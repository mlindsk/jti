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

  if (inherits(x, "sptable")) return(x)
  
  if (!is_named_list(x)) {
    stop("x must be a named list of cpts. A name should be the name of the corresponding child node.")
  }

  lookup <- vector("list")
  
  y <- lapply(seq_along(x), function(i) {

    l <- x[[i]]

    class_allowed <- any(.map_lgl(.allowed_cpt_classes(), function(x) inherits(l, x)))
    if (!class_allowed) stop("one ore more elements in x is not an array-like object")

    cal  <- char_array(l)
    sptl <- as_sptable(cal, validate = FALSE)
    lu   <- lookup(cal) # used to populate/create empty/unity potentials in new_charge

    for (k in seq_along(lu)) {
      if (names(lu)[k] %ni% names(lookup)) {
        lookup <<- push(lookup, lu[[k]], names(lu)[k])  
      }
    }
    
    return(sptl)
  })

  class(lookup) <- c("lookup", class(lookup))
  
  structure(y, names = names(x), lookup = lookup, class = c("cpt_list", class(x)))
}

# TODO: Make a vanilla print method avoiding all the sptables

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
#' @param root_node A node for which we require it to live in the root clique (the first clique).
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
  # TODO: Also output the lookup!
  structure(
    list(charge = charge, cliques = cliques),
    lookup = attr(x, "lookup"),
    root_node = root_node,
    class = c("charge", "list")
  )
}

#' @rdname compile
#' @export
compile.data.frame <- function(x, g, root_node = "") {

  # TODO: Make a vanilla print method

  x <- char_frame(x)

  # Used to store parents in construct_cliques_and_parents if g != igraph.
  pe <- new.env()
  pe[["parents"]] <- NULL
  
  adj     <- adjacency_list_from_graph(g, pe)
  cp      <- construct_cliques_and_parents(adj, root_node)
  parents <- if (is.null(pe$parents))  cp$parents else pe$parents
  charge  <- new_charge(x, cp$cliques, parents)

  structure(
    list(charge = charge, cliques = cp$cliques),
    lookup = lookup(x),
    root_node = root_node,
    class = c("charge", "list")
  )
}
