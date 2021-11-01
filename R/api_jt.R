#' Junction Tree
#'
#' Construction of a junction tree and message passing
#' 
#' @param x An object return from \code{compile}
#' @param evidence A named vector. The names are the variabes and the elements
#' are the evidence
#' @param flow Either "sum" or "max"
#' @param propagate Either "no", "collect" or "full".
#' @return A \code{jt} object
#' @details Evidence can be entered either at compile stage
#' or after compilation. Hence, one can also combine
#' evidence from before compilation with evidence
#' after compilation. Before refers to entering
#' evidence in the 'compile' function and after
#' refers to entering evidence in the 'jt' function.
#' @seealso \code{\link{query_belief}}, \code{\link{mpe}},
#' \code{\link{get_cliques}}, \code{\link{get_clique_root}},
#' \code{\link{propagate}}
#' @examples
#'
#' # Setting up the network
#' # ----------------------
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
#' plot(g)
#' # -----------------------
#'
#' # Data
#' # ----
#' # We use the asia data; see the man page (?asia)
#'
#' # Compilation
#' # -----------
#' cl <- cpt_list(asia, g) # Checking and conversion
#' cp <- compile(cl)
#'
#' # After the network has been compiled, the graph has been triangulated and
#' # moralized. Furthermore, all conditional probability tables (CPTs) has been
#' # designated one of the cliques (in the triangulated and moralized graph).
#'
#' # Example 1: sum-flow without evidence
#' # ------------------------------------
#' jt1 <- jt(cp)
#' plot(jt1)
#' print(jt1)
#' query_belief(jt1, c("E", "L", "T"))
#' query_belief(jt1, c("B", "D", "E"), type = "joint")
#'
#'
#' # Notice, that jt1 is equivalent to:
#' # jt1 <- jt(cp, propagate = "no")
#' # jt1 <- propagate(jt1, prop = "full")
#'
#' # That is; it is possible to postpone the actual propagation
#' # In this setup, the junction tree is saved in the jt1 object,
#' # and one can repeadetly enter evidence for new observations
#' # using the set_evidence function on jt1 and then query
#' # several probabilites without repeadetly calculating the
#' # the junction tree over and over again. One just needs
#' # to use the propagate function on jt1.
#' 
#' # Example 2: sum-flow with evidence
#' # ---------------------------------
#'
#' e2  <- c(A = "y", X = "n")
#' jt2 <- jt(cp, e2) 
#' query_belief(jt2, c("B", "D", "E"), type = "joint")
#'
#' # Notice that, the configuration (D,E,B) = (y,y,n) has changed
#' # dramatically as a consequence of the evidence
#'
#' # We can get the probability of the evidence:
#' query_evidence(jt2)
#' 
#' # Example 3: max-flow without evidence
#' # ------------------------------------
#' jt3 <- jt(cp, flow = "max")
#' mpe(jt3)
#'
#' 
#' # Example 4: max-flow with evidence
#' # ---------------------------------
#' e4  <- c(T = "y", X = "y", D = "y")
#' jt4 <- jt(cp, e4, flow = "max")
#' mpe(jt4)
#' 
#' # Notice, that T, E, S, B, X and D has changed from "n" to "y"
#' # as a consequence of the new evidence e4
#'
#' 
#' # Example 5: specifying a root node and only collect to save run time
#' # -------------------------------------------------------------------------
#'
#' \donttest{
#'   cp5 <- compile(cpt_list(asia, g), root_node = "X")
#'   jt5 <- jt(cp5, propagate = "collect")
#'   query_belief(jt5, get_clique_root(jt5), "joint")
#' }
#'
#' # We can only query from the variables in the root clique now
#' # but we have ensured that the node of interest, "X", does indeed live in
#' # this clique. The variables are found using 'get_clique_root'
#' 
#' # Example 6: Compiling from a list of conditional probabilities
#' # -------------------------------------------------------------------------
#'
#' # * We need a list with CPTs which we extract from the asia2 object
#' #    - the list must be named with child nodes
#' #    - The elements need to be array-like objects
#'
#' cl  <- cpt_list(asia2)
#' cp6 <- compile(cl)
#'
#' # Inspection; see if the graph correspond to the cpts
#' # g <- get_graph(cp6)
#' # plot(g) 
#'
#' # This time we specify that no propagation should be performed
#' jt6 <- jt(cp6, propagate = "no")
#'
#' # We can now inspect the collecting junction tree and see which cliques
#' # are leaves and parents
#' plot(jt6)
#' get_cliques(jt6)
#' get_clique_root(jt6)
#'
#' leaves(jt6)
#' unlist(parents(jt6))
#'
#' # That is;
#' # - clique 2 is parent of clique 1
#' # - clique 3 is parent of clique 4 etc.
#'
#' # Next, we send the messages from the leaves to the parents
#' jt6 <- send_messages(jt6)
#'
#' # Inspect again
#' plot(jt6)
#'
#' # Send the last message to the root and inspect
#' jt6 <- send_messages(jt6)
#' plot(jt6)
#'
#' # The arrows are now reversed and the outwards (distribute) phase begins
#' leaves(jt6)
#' parents(jt6)
#'
#' # Clique 2 (the root) is now a leave and it has 1, 3 and 6 as parents.
#'
#' # Finishing the message passing
#' jt6 <- send_messages(jt6)
#' jt6 <- send_messages(jt6)
#'
#' # Queries can now be performed as normal
#' query_belief(jt6, c("either", "tub"), "joint")
#' 
#' @export
jt <- function(x, evidence = NULL, flow = "sum", propagate = "full") UseMethod("jt")

#' @rdname jt
#' @export
jt.charge <- function(x, evidence = NULL, flow = "sum", propagate = "full") {

  if (!attr(x, "cpts_initialized")) {
    stop("The CPTs are not yet initialized. Use either 'set_evidence' or 'initialize'.")
  }
  
  if (!is.null(evidence)) {
    if (!valid_evidence(attr(x, "dim_names"), evidence)) {
      stop("evidence is not on correct form", call. = FALSE)
    }
    attr(x, "evidence") <- c(attr(x, "evidence"), evidence)
  }

  j <- new_jt(x, evidence, flow)
  attr(j, "propagated") <- "no"
  attr(j, "type") <- ifelse(inherits(x, "bn"), "bn", "mrf")

  # A junction tree with a single node with flow = max
  if (length(j$charge$C) == 1L && attr(j, "flow") == "max") {
      max_cell <- sparta::which_max_cell(j$charge$C$C1)
      attr(j, "mpe")[names(max_cell)] <- max_cell
  }
  
  if (propagate == "no") {
    return(j)
  } else if (propagate == "collect") {
    m <- send_messages(j)  
    while (attr(m, "direction") != "distribute") m <- send_messages(m)
    attr(m, "propagated") <- "collect"
    return(m)
  } else {
    m <- send_messages(j)
    while (attr(m, "direction") != "full") m <- send_messages(m)
    attr(m, "propagated") <- "full"
    if (attr(m, "inconsistencies")) {
      m$charge$C <- lapply(m$charge$C, sparta::normalize)
      m$charge$S <- lapply(m$charge$S, function(s) {
        if (is.null(s) || is_scalar(s)) return(s)
        sparta::normalize(s)
      })
    }
    return(m)
  }
  stop("propagate must be either 'no', 'collect' or full", call. = TRUE)
}

#' Propagation of junction trees
#'
#' Given a junction tree object, propagation is conducted
#' 
#' @param x A junction tree object \code{jt}
#' @param prop Either "collect" or "full".
#' @seealso \code{\link{jt}}
#' @examples
#' # See Example 1 in the 'jt' function
#' @export
propagate <- function(x, prop = "full") UseMethod("propagate")

#' @rdname propagate
#' @export
propagate.jt <- function(x, prop = "full") {

  if (prop == "collect") {

    if (attr(x, "propagated") == "collect") return(x)

    if (attr(x, "propagated") == "full") {
      stop("the junction tree is already propageted fully", call. = FALSE)
    }

    m <- send_messages(x)
    while (attr(m, "direction") != "distribute") m <- send_messages(m)

    attr(m, "propagated") <- "collect"
    return(m)
    
  } else if (prop == "full") {

    if (attr(x, "propagated") == "full") return(x)

    m <- send_messages(x)

    while (attr(m, "direction") != "full") m <- send_messages(m)

    attr(m, "propagated") <- "full"

    if (attr(m, "inconsistencies")) {
      m$charge$C <- lapply(m$charge$C, sparta::normalize)
      m$charge$S <- lapply(m$charge$S, function(s) {
        if (is.null(s) || is_scalar(s)) return(s)
        sparta::normalize(s)
      })
    }
    
    return(m)
  } else {
    
    stop("propagate must be either 'collect' or full", call. = TRUE)  
  }
}

#' Most Probable Explanation
#'
#' Returns the most probable explanation given the evidence entered in the
#' junction tree
#' 
#' @param x A junction tree object, \code{jt}, with max-flow.
#' @seealso \code{\link{jt}}
#' @examples
#' # See the 'jt' function
#' @export
mpe <- function(x) UseMethod("mpe")

#' @rdname mpe
#' @export
mpe.jt <- function(x) {
  if (attr(x, "flow") != "max") stop("The flow of the junction tree is not 'max'.")
  attr(x, "mpe")
}

#' Return the cliques of a junction tree
#'
#' @param x A junction tree object, \code{jt}.
#' @seealso \code{\link{jt}}
#' @examples
#' # See Example 5  and 6 of the 'jt' function 
#'
#' @rdname get_cliques
#' @export
get_cliques <- function(x) UseMethod("get_cliques")

#' @rdname get_cliques
#' @export
get_cliques.jt <- function(x) x$cliques

#' @rdname get_cliques
#' @export
get_cliques.charge <- function(x) x$cliques

#' @rdname get_cliques
#' @export
get_cliques.pot_list <- function(x) attr(x, "cliques")

#' @rdname get_cliques
#' @export
get_clique_root_idx <- function(x) UseMethod("get_clique_root_idx")

#' @rdname get_cliques
#' @export
get_clique_root_idx.jt <- function(x) as.integer(gsub("C","",attr(x, "clique_root")))

#' @rdname get_cliques
#' @export
get_clique_root <- function(x) UseMethod("get_clique_root")

#' @rdname get_cliques
#' @export
get_clique_root.jt <- function(x) x$cliques[[get_clique_root_idx(x)]]

#' Query Evidence 
#'
#' Get the probability of the evidence entered in the junction tree object
#'
#' @param x A junction tree object, \code{jt}.
#' @seealso \code{\link{jt}}, \code{\link{mpe}}
#' @export
query_evidence <- function(x) UseMethod("query_evidence")

#' @rdname query_evidence
#' @export
query_evidence.jt <- function(x) {

  if (has_inconsistencies(x)) {
    stop(
      "The probability of evidence is not meaningful ",
      "when there are inconsistencies in the evidence.",
      call. = FALSE
    )
  }
  
  if(attr(x, "flow") != "sum") {
    stop(
      "The flow of the junction tree must be 'sum'.",
      call. = FALSE
    )
  }
  
  if (attr(x, "propagated") == "no") {
    stop("In order to query the probabilty of evidence, ",
      "the junction tree must at least be propagted to ",
      "the root node (collect).",
      call. = FALSE
    )
  }
  
  return(attr(x, "probability_of_evidence"))
}

# cptl <- cpt_list(asia2)
# cp <- compile(cptl, initialize_cpts = FALSE)
# cp2 <- set_evidence(cp, evidence = c(bronc = "yes"))

#' Query Parents or Leaves in a Junction Tree
#'
#' Return the clique indices of current parents or leaves
#' in a junction tree
#'
#' @param jt A junction tree object, \code{jt}.
#' @seealso \code{\link{jt}}, \code{\link{get_cliques}}
#' @examples
#' # See example 6 in the help page for the jt function
#' @rdname par_lvs
#' @export
leaves <- function(jt) UseMethod("leaves")

#' @rdname par_lvs
#' @export
leaves.jt <- function(jt) {
  direction <- attr(jt, "direction")
  if (direction == "full") {
    message("The junction tree is already fully propagated. NULL is returned")
    return(NULL)
  }
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  lvs               <- attr(x$tree, "leaves")
  true_clique_names <- names(x$cliques)[lvs]
  true_lvs_indicies <- as.integer(gsub("C", "", true_clique_names))
  return(true_lvs_indicies)
}


#' @rdname par_lvs
#' @export
parents <- function(jt) UseMethod("parents")

#' @rdname par_lvs
#' @export
parents.jt <- function(jt) {
  direction <- attr(jt, "direction")
  if (direction == "full") {
    message("The junction tree is already fully propagated. NULL is returned")
    return(NULL)
  }
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  par               <- attr(x$tree, "parents")
  true_clique_names <- lapply(par, function(p) names(x$cliques)[p])
  true_par_indicies <- lapply(true_clique_names, function(tcn) {
    as.integer(gsub("C", "", tcn))
  })
  return(true_par_indicies)
}


#' A print method for junction trees
#'
#' @param x A junction tree object, \code{jt}.
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{jt}}
#' @export
print.jt <- function(x, ...) {
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  direction <- attr(x, "direction")
  flow <- attr(x, "flow")
  nv  <- ncol(x$clique_graph)
  ne  <- sum(x$clique_graph)/2  
  clique_sizes <- .map_int(x$cliques, length)
  max_C <- max(clique_sizes)
  min_C <- min(clique_sizes)
  avg_C <- mean(clique_sizes)

  cat(" Junction Tree",
    "\n -------------------------",
    "\n  Propagated:", attr(x, "propagated"),
    "\n  Flow:", flow,
    "\n  Cliques:", length(x$cliques),
    "\n   - max:", max_C,
    "\n   - min:", min_C,
    "\n   - avg:", round(avg_C, 2)
  )

  inc <- attr(x, "inconsistencies")
  e <- attr(x, "evidence")
  if (!is.null(e)) {
    if (inc) cat("\n  Evidence: (inconsistencies)") else cat("\n  Evidence:")
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

#' A plot method for junction trees
#'
#' @param x A junction tree object, \code{jt}.
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{jt}}
#' @export
plot.jt <- function(x, ...) {
  direction <- attr(x, "direction")
  y <- if (direction == "collect") {
    list(
      cliques   = x$schedule$collect$cliques,
      tree      = x$schedule$collect$tree,
      type      = "directed"
    )
  } else if (direction == "distribute") {
    list(
      cliques = x$schedule$distribute$cliques,
      tree    = x$schedule$distribute$tree,
      type    = "directed"
    )
  } else {
    list(
      cliques = x$cliques,
      tree    = x$clique_graph,
      type    = "undirected"
    )
  }

  .names <- unlist(lapply(y$cliques, function(z) paste(z, collapse = "\n")))
  dimnames(y$tree) <- list(.names, .names)
  g <- igraph::graph_from_adjacency_matrix(y$tree, y$type)
  graphics::plot(g, ...)
}


#' A plot method for junction trees
#'
#' @param x A compile object
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{compile}}
#' @export
plot.charge <- function(x, ...) {
  .names <- unlist(lapply(x$cliques, function(z) paste(z, collapse = "\n")))
  dimnames(x$schedule$clique_graph) <- list(.names, .names)
  g <- igraph::graph_from_adjacency_matrix(x$schedule$clique_graph, "undirected")
  graphics::plot(g, ...)
}
