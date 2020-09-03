#' Conditional probability list
#'
#' A check and conversion of cpts to be used in the junction tree algorithm
#'
#' @param x A named list with cpts in form of array-like object(s).
#' The name must be the child.
#' @examples
#' print(asia2)
#' cpt_list(asia2)
#' @export
cpt_list <- function(x) { 
  # x: list of cpts with dimnames
    
  if (!is_named_list(x)) {
    stop("x must be a named list of cpts. A name should be the name of the corresponding child node.")
  }

  dim_names <- list()
  
  y <- lapply(seq_along(x), function(i) {
    l <- x[[i]]
    class_allowed <- any(.map_lgl(sparta::allowed_class_to_sparta(), function(x) inherits(l, x)))
    if (!class_allowed) stop("one ore more elements in x is not an array-like object")
    spar <- sparta::as_sparta(l)
    dim_names <<- push(dim_names, attr(spar, "dim_names"))
    spar
  })

  dim_names <- unlist(dim_names, FALSE)
  dim_names <- dim_names[unique(names(dim_names))]
  
  structure(y, names = names(x), dim_names = dim_names, class = c("cpt_list", class(x)))
}

# TODO: Make a vanilla print method avoiding all the sparta tables to print?

#' Compile information
#'
#' Compiled objects are used as building blocks for junction tree inference
#'
#' @param x Either a \code{data.frame} or an object returned from \code{cpt_list}
#' @param g Either a directed acyclic graph (DAG) as an igraph object or a
#' decomposable graph obtained from \code{ess::fit_graph} or an
#' adjacency list - a named list where an element is a character vector defining
#' the neigbors of the element. If \code{x} is a \code{cpt_list},
#' \code{g} must be \code{NULL}. The procdure then deduce the graph
#' from the conditional probability tables that was inputtet in \code{cpt_list}
#' @param root_node A node for which we require it to live in the root
#' clique (the first clique).
#' @export
compile <- function(x, g = NULL, root_node = "") UseMethod("compile")

#' @rdname compile
#' @export
compile.cpt_list <- function(x, g = NULL, root_node = "") {
  # x is validated in cpt_list
  if (!is.null(g)) stop("g must be 'NULL'")
  g       <- graph_from_cpt_list(x)
  parents <- parents_igraph(g)
  adj     <- adjacency_list_from_moralization_and_triangulation_igraph(g, parents)
  cliques <- construct_cliques_and_parents(adj, root_node)$cliques
  charge  <- new_charge(x, cliques, parents)
  structure(
    list(charge = charge, cliques = cliques),
    root_node = root_node,
    dim_names = attr(x, "dim_names"),
    class = c("charge", "list")
  )
}

#' @rdname compile
#' @export
compile.data.frame <- function(x, g, root_node = "") {

  # TODO: Make a vanilla print method
  attr(x, "dim_names") <- lapply(x, unique)

  # Used to store parents in construct_cliques_and_parents if g != igraph.
  pe <- new.env()
  pe[["parents"]] <- NULL
  
  adj     <- adjacency_list_from_graph(g, pe)
  cp      <- construct_cliques_and_parents(adj, root_node)
  parents <- if (is.null(pe$parents))  cp$parents else pe$parents
  charge  <- new_charge(x, cp$cliques, parents)

  structure(
    list(charge = charge, cliques = cp$cliques),
    root_node = root_node,
    dim_names = attr(x, "dim_names"),
    class = c("charge", "list")
  )
}
