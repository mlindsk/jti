#' Conditional probability list
#'
#' A check and conversion of cpts to be used in the junction tree algorithm
#'
#' @param x Either a named list with cpts in form of array-like object(s)
#' where names must be the child node or a \code{data.frame}
#' @param g Either a directed acyclic graph (DAG) as an igraph object or a
#' decomposable graph as an igraph object. If \code{x} is a list,
#' \code{g} must be \code{NULL}. The procdure then deduce the graph
#' from the conditional probability tables.
#' @examples
#'
#' library(igraph)
#' el <- matrix(c(
#' "A", "T",
#' "T", "E",
#' "S", "L",
#' "S", "B",
#' "L", "E",
#' "E", "X",
#' "E", "D",
#' "B", "D"),
#'  nc = 2,
#'  byrow = TRUE
#' )
#' 
#' g <- igraph::graph_from_edgelist(el) 
#' cl <- cpt_list(asia, g)
#' 
#' print(cl)
#' dim_names(cl)
#' names(cl)
#' plot(get_graph(cl))
#' @export
cpt_list <- function(x, g = NULL) UseMethod("cpt_list")


#' @rdname cpt_list
#' @export
cpt_list.list <- function(x, g = NULL) { 

  if (!is_named_list(x)) {
    stop(
      "x must be a named list of cpts. ",
      "A name should be the name of the corresponding child node."
    )
  }

  if (!is.null(g)) stop("g must be 'NULL'")

  dim_names <- list()
  
  y <- lapply(seq_along(x), function(i) {
    l <- x[[i]]
    spar <- sparta::as_sparta(l)
    # This ensures, that the CPTs and dim_names have the same ordering of the lvls!
    dim_names <<- push(dim_names, attr(spar, "dim_names"))
    spar
  })

  names(y)  <- names(x)
  dim_names <- unlist(dim_names, FALSE)
  dim_names <- dim_names[unique(names(dim_names))]

  g <- graph_from_cpt_list(y)
  if (!igraph::is_dag(g)) stop("The cpts does not induce an acyclic graph.")

  structure(
    y,
    nodes     = names(x),
    dim_names = dim_names,
    parents   = parents_cpt_list(y), # NOTE: Needed?
    graph     = g,
    class     = c("cpt_list", "list")
  )
}


#' @rdname cpt_list
#' @export
cpt_list.data.frame <- function(x, g) {

  if (!igraph::is.igraph(g)) stop("g must be an igraph object")

  is_dag <- igraph::is_dag(g)
  if (!is_dag) {
    if (!igraph::is_chordal(g)$chordal) {
      stop("undirected graphs must be be decomposable", call. = FALSE)
    }
    message(
      "Consider using 'pot_list' for markov random fields. ",
      "It is faster and more idiomatic."
    )
  }

  parents <- if (is_dag) {
    parents_igraph(g)
  } else {
    rip(as_adj_lst(igraph::as_adjacency_matrix(g, sparse = FALSE)), check = FALSE)$P 
  }
  
  dns <- list()

  y <- lapply(seq_along(parents), function(i) {
    child <- names(parents)[i]
    pars  <- parents[[i]]
    spar  <- sparta::as_sparta(x[, c(child, pars), drop = FALSE])
    spar  <- sparta::as_cpt(spar, pars)
    # This ensures, that the CPTs and dim_names have the same ordering of the lvls!
    dns   <<- push(dns, sparta::dim_names(spar))
    spar
  })

  dns <- unlist(dns, FALSE)
  dns <- dns[unique(names(dns))]
  
  structure(
    structure(y, names = names(parents)),
    nodes     = names(parents),
    dim_names = dns,
    parents   = parents,
    graph     = g,
    class     = c("cpt_list", "list")
  )
}

# TODO:
# + Implement new construction of sparse tables


#' A check and extraction of clique potentials from a Markov random field
#' to be used in the junction tree algorithm
#'
#' @param x Character \code{data.frame}
#' @param g A decomposable Markov random field as an igraph object.
#' @examples
#'
#' # Typically one would use the ess package:
#' # library(ess)
#' # g  <- ess::fit_graph(derma)
#' # pl <- pot_list(derma, ess::as_igraph(g))
#' # pl
#'
#' # Another example
#' g <- igraph::sample_gnm(ncol(asia), 12)
#' while(!igraph::is.chordal(g)$chordal) g <- igraph::sample_gnm(ncol(asia), 12, FALSE)
#' igraph::V(g)$name <- colnames(asia)
#' plot(g)
#' pot_list(asia, g)
#' 
#' @export
pot_list <- function(x, g) UseMethod("pot_list")


#' @rdname pot_list
#' @export
pot_list.data.frame <- function(x, g) {

  if (!igraph::is.igraph(g)) stop("g must be an igraph object", call. = FALSE)

  cliques <- rip(as_adj_lst(igraph::as_adjacency_matrix(g, sparse = FALSE)), check = TRUE)$C
  dns <- list()

  y <- lapply(seq_along(cliques), function(i) {
    clique <- cliques[[i]]
    spar  <- sparta::as_sparta(x[, clique, drop = FALSE])
    spar  <- sparta::normalize(spar)
    # This ensures, that the potentials and dim_names have the same ordering of the lvls!
    dns   <<- push(dns, sparta::dim_names(spar))
    spar
  })

  dns <- unlist(dns, FALSE)
  dns <- dns[unique(names(dns))]
  
  structure(
    structure(y, names = paste("C", 1:length(cliques), sep = "")),
    nodes     = colnames(x),
    cliques   = cliques,
    dim_names = dns,
    graph     = g,
    class     = c("pot_list", "list")
  )
}


#' Compile information
#'
#' Compiled objects are used as building blocks for junction tree inference
#'
#' @param x An object returned from \code{cpt_list} (baeysian network) or
#' \code{pot_list} (decomposable markov random field)
#' @param evidence A named vector. The names are the variabes and the elements
#' are the evidence.
#' @param root_node A node for which we require it to live in the root
#' clique (the first clique).
#' @param joint_vars A vector of variables for which we require them
#' to be in the same clique. Edges between all these variables are added
#' to the moralized graph.
#' @param tri The optimization strategy used for triangulation if x originates
#' from a Baeysian network. One of
#'  * 'min_fill'
#'  * 'min_rfill'
#'  * 'min_efill'
#'  * 'min_sfill'
#'  * 'min_sp'
#'  * 'min_esp'
#'  * 'min_nei'
#'  * 'minimal'
#'  * 'alpha'
#' @md
#' @param pmf_evidence A named vector of frequencies. The names should
#' correspond to the evidence that is expected to see over time. Relevant
#' in connection to \code{min_efill} and \code{min_esp} triangulations.
#' @param alpha Character vector. A permutation of the nodes
#' in the graph. It specifies a user-supplied eliminination ordering for
#' triangulation of the moral graph.
#' 
#' @details The Junction Tree Algorithm performs both a forward and inward
#' message pass (collect and distribute). However, when the forward
#' phase is finished, the root clique potential is guaranteed to be the
#' joint pmf over the variables involved in the root clique. Thus, if
#' it is known in advance that a specific variable is of interest, the
#' algortihm can be terminated after the forward phase. Use the \code{root_node}
#' to specify such a variable and specify \code{propagate = "collect"} in
#' the juntion tree algortihm function \code{jt}.
#'
#' Moreover, if interest is in some joint pmf for variables that end up
#' being in different cliques these variables must be specified in advance
#' using the \code{joint_vars} argument. The compilation step then
#' adds edges between all of these variables to ensure that at least one
#' clique contains all of them.
#'
#' Evidence can be entered either at compile stage
#' or after compilation. Hence, one can also combine
#' evidence from before compilation with evidence
#' after compilation. Before refers to entering
#' evidence in the 'compile' function and after
#' refers to entering evidence in the 'jt' function.
#'
#' Finally, one can either use a Bayesian network or a decomposable
#' Markov random field (use the \code{ess} package to fit these). Bayesian
#' networks must be constructed with \code{cpt_list} and decomposable MRFs
#' should be constructed with \code{pot_list}, but can also be constructed
#' using \code{cpt_list}.
#' 
#' @examples
#' cptl <- cpt_list(asia2)
#' cp1  <- compile(cptl, evidence = c(bronc = "yes"), joint_vars = c("bronc", "tub"))
#' print(cp1)
#' names(cp1)
#' dim_names(cp1)
#' plot(get_graph(cp1))
#' @export
compile <- function(x,
                    evidence       = NULL,
                    root_node      = "",
                    joint_vars     = NULL,
                    tri            = "min_fill",
                    pmf_evidence   = NULL,
                    alpha          = NULL
                    ) {
  UseMethod("compile")
}

#' @rdname compile
#' @export
compile.cpt_list <- function(x,
                             evidence       = NULL,
                             root_node      = "",
                             joint_vars     = NULL,
                             tri            = "min_fill",
                             pmf_evidence   = NULL,
                             alpha          = NULL                             
                             ) {

  .defense_compile(tri, pmf_evidence, alpha, names(x))
  
  g       <- attr(x, "graph")
  parents <- attr(x, "parents")

  gm      <- moralize_igraph(g, parents)
  if (!is.null(joint_vars)) gm <- add_joint_vars_igraph(gm, joint_vars)

  # Note here: if sparse = TRUE, the run time explodes! Wonder why...
  M  <- igraph::as_adjacency_matrix(gm, sparse = FALSE)
  
  tri_obj <- switch(tri,
    "min_fill"  = new_min_fill_triang(M),
    "min_rfill" = new_min_rfill_triang(M),
    "min_efill" = new_min_efill_triang(M, .map_int(dim_names(x), length), pmf_evidence),
    "min_sfill" = new_min_sfill_triang(M, .map_int(dim_names(x), length)),
    "min_sp"    = new_min_sp_triang(M, .map_int(dim_names(x), length)),
    "min_esp"   = new_min_esp_triang(M, .map_int(dim_names(x), length), pmf_evidence),
    "min_nei"   = new_min_nei_triang(M),
    "minimal"   = new_minimal_triang(M),
    "alpha"     = new_alpha_triang(M, alpha)
  )
  
  gmt     <- .triang(tri_obj)
  adj_lst <- as_adj_lst(gmt)
  cliques <- construct_cliques(adj_lst, root_node) # just use cliques_int ?

  # cliques_int is needed to construct the junction tree in new_jt -> new_schedule
  dimnames(gmt) <- lapply(dimnames(gmt), function(x) 1:nrow(gmt))
  adj_lst_int   <- as_adj_lst(gmt)
  root_node_int <- ifelse(root_node != "", as.character(match(root_node, names(adj_lst))), "")
  cliques_int   <- lapply(rip(adj_lst_int, root_node_int)$C, as.integer)

  if (!is.null(evidence)) {
    if (!valid_evidence(attr(x, "dim_names"), evidence)) {
      stop("Evidence is not on correct form", call. = FALSE)
    }

    # x looses its attributes in set_evidence
    att_ <- attributes(x)
    x    <- set_evidence_(x, evidence)
    attributes(x) <- att_
  }
  
  charge  <- new_charge_cpt(x, cliques, parents)

  structure(
    list(charge = charge, cliques = cliques),
    root_node   = root_node,
    dim_names   = attr(x, "dim_names"),
    evidence    = evidence,
    graph       = g,
    cliques_int = cliques_int,
    class       = c("charge", "list")
  )
}


#' @rdname compile
#' @export
compile.pot_list <- function(x,
                             evidence       = NULL,
                             root_node      = "",
                             joint_vars     = NULL,
                             tri            = "min_fill",
                             pmf_evidence   = NULL,
                             alpha          = NULL                             
                             ) {

  .defense_compile(tri, pmf_evidence, alpha, names(x))
  
  g       <- attr(x, "graph")
  gmat    <- igraph::as_adjacency_matrix(g, sparse = FALSE)
  adj_lst <- as_adj_lst(gmat)
  cliques <- attr(x, "cliques")

  # cliques_int is needed to construct the junction tree in new_jt -> new_schedule
  dimnames(gmat) <- lapply(dimnames(gmat), function(x) 1:nrow(gmat))
  adj_lst_int   <- as_adj_lst(gmat)
  root_node_int <- ifelse(root_node != "", as.character(match(root_node, names(adj_lst))), "")
  cliques_int   <- lapply(rip(adj_lst_int, root_node_int)$C, as.integer)

  if (!is.null(evidence)) {
    if (!valid_evidence(attr(x, "dim_names"), evidence)) {
      stop("Evidence is not on correct form", call. = FALSE)
    }
    # x looses its attributes in set_evidence
    att_ <- attributes(x)
    x    <- set_evidence_(x, evidence)
    attributes(x) <- att_
  }

  charge  <- new_charge_pot(x)
  structure(
    list(charge = charge, cliques = cliques),
    root_node   = root_node,
    dim_names   = attr(x, "dim_names"),
    evidence    = evidence,
    graph       = g,
    cliques_int = cliques_int,
    class       = c("charge", "list")
  )
}



#' Variable getters
#'
#' Getter methods for information about variables and their statespaces
#' 
#' @param x \code{cpt_list} or a compiled object

#' @rdname getters
#' @export
dim_names <- function(x) UseMethod("dim_names")

#' @rdname getters
#' @export
dim_names.cpt_list <- function(x) attr(x, "dim_names")

#' @rdname getters
#' @export
names.cpt_list <- function(x) attr(x, "nodes")

#' @rdname getters
#' @export
dim_names.pot_list <- function(x) attr(x, "dim_names")

#' @rdname getters
#' @export
names.pot_list <- function(x) attr(x, "nodes")

#' @rdname getters
#' @export
dim_names.charge <- function(x) attr(x, "dim_names")

#' @rdname getters
#' @export
names.charge <- function(x) names(attr(x, "dim_names"))

#' @rdname getters
#' @export
dim_names.jt <- function(x) attr(x, "dim_names")

#' @rdname getters
#' @export
names.jt <- function(x) names(attr(x, "dim_names"))


#' Get graph
#'
#' Retrieve the graph from
#'
#' @param x \code{cpt_list} or a compiled object
#' @return A graph as an \code{igraph} object

#' @rdname get-graph
#' @export
get_graph <- function(x) UseMethod("get_graph")

#' @rdname get-graph
#' @export
get_graph.charge <- function(x) attr(x, "graph")

#' @rdname get-graph
#' @export
get_graph.cpt_list <- function(x) attr(x, "graph")

#' @rdname get-graph
#' @export
get_graph.pot_list <- function(x) attr(x, "graph")

#' A print method for cpt lists
#'
#' @param x A \code{cpt_list} object
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{compile}}
#' @export
print.cpt_list <- function(x, ...) {
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  nn  <- length(names(x))
  cat(" List of CPTs",
    "\n -------------------------\n")

  for (child in names(x)) {
    parents <- setdiff(names(sparta::dim_names(x[[child]])), child)
    if (neq_empt_chr(parents)) {
      cat("  P(", child, "|" , paste(parents, collapse = ", "), ")\n")      
    } else {
      cat("  P(", child, ")\n")
    }
  }
  
  cat(paste0("\n  ", cls),
    "\n -------------------------\n"
  )
}

#' A print method for pot lists
#'
#' @param x A \code{pot_list} object
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{compile}}
#' @export
print.pot_list <- function(x, ...) {
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  nn  <- length(names(x))
  cat(" List of potentials",
    "\n -------------------------\n")

  for (k in seq_along(x)) {
    pot_vars <- names(x[[k]])
    cat("  pot(", paste(pot_vars, collapse = ", "), ")\n")      
  }
  
  cat(paste0("\n  ", cls),
    "\n -------------------------\n"
  )
}


#' A print method for compiled objects
#'
#' @param x A compiled object
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{jt}}
#' @export
print.charge <- function(x, ...) {
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  nn  <- length(names(x))
  clique_sizes <- .map_int(x$cliques, length)
  max_C <- max(clique_sizes)
  min_C <- min(clique_sizes)
  avg_C <- mean(clique_sizes)
  cat(" Compiled network",
    "\n -------------------------",
    "\n  Nodes:", nn,
    "\n  Cliques:", length(x$cliques),
    "\n   - max:", max_C,
    "\n   - min:", min_C,
    "\n   - avg:", round(avg_C, 2)
  )

  e <- attr(x, "evidence")
  if (!is.null(e)) {
    cat("\n  Evidence:")
    for (i in seq_along(e)) {
      cat(
        "\n   -", paste0(names(e[i]), ":"), unname(e[i])
      )
    }
  }

  cat(paste0("\n  ", cls),
    "\n -------------------------\n"
  ) 
}
