#' Number of Binary Operations 
#'
#' Number of binary operations needed to propagate in a junction tree
#' given evidence, using the Lauritzen-Spiegelhalter scheme
#'
#' @param x A junction tree object or an object returned from
#' the triangulation function
#' @param evidence List of character vectors with evidence nodes
#' @param root Integer specifying the root node in the junction tree
#' @param nc Integer. The number of cores to be used in parallel
#' @export
jt_nbinary_ops <- function(x, evidence = list(), root = NULL, nc = 1) {
  UseMethod("jt_nbinary_ops")
}

#' @rdname jt_nbinary_ops
#' @export
jt_nbinary_ops.triangulation <- function(x, evidence = list(), root = NULL, nc = 1) {
  sp       <- .map_int(x$dim_names, length)
  root_idx <- if (is.null(root)) x$clique_root else root
  tree     <- x$junction_tree_collect
  tree     <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)
  
  dn <- dimnames(x$new_graph)
  sp_int <- .map_int(x$dim_names[dn[[1]]], length)
  names_ <- names(sp_int)
  dimnames(x$new_graph) <- lapply(dn, function(x) 1:length(x))
  cliques_int <- lapply(rip(as_adj_lst(x$new_graph))$C, as.integer)

  unlist(parallel::mclapply(mc.cores = nc, X = evidence, FUN = function(e) {
    e_int <- match(e, names_)
    cliques_int_e <- lapply(cliques_int, function(x) setdiff(x, e_int))
    nbinary_ops_int_(cliques_int_e, tree, sp, root_idx)
  }))
}
  
# #' Number of Binary Operations 
# #'
# #' Number of binary operations needed to propagate in a junction tree
# #' given evidence, using the Lauritzen-Spiegelhalter scheme
# #'
# #' @param x A junction tree object or an object returned from
# #' the triangulation function
# #' @param evidence List of character vectors with evidence nodes
# #' @param root Integer specifying the root node in the junction tree
# #' @export
# jt_nbinary_ops <- function(x, evidence = character(0), root = NULL) {
#   UseMethod("jt_nbinary_ops")
# }

# #' @rdname jt_nbinary_ops
# #' @export
# jt_nbinary_ops.jt <- function(x, evidence = character(0), root = NULL) {
#   stopifnot(attr(x, "propagate") == "no")
#   sp       <- .map_int(dim_names(x), length)
#   cr       <- attr(x, "clique_root")
#   root_idx <- if (is.null(root)) as.integer(substr(cr, 2, nchar(cr))) else root
#   tree     <- x$schedule$collect$tree
#   tree     <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)
#   cliques  <- lapply(x$cliques, function(x) setdiff(x, evidence))
#   .nbinary_ops(cliques, tree, sp, root_idx)
# }

# #' @rdname jt_nbinary_ops
# #' @export
# jt_nbinary_ops.triangulation <- function(x, evidence = character(0), root = NULL) {
#   sp       <- .map_int(x$dim_names, length)
#   root_idx <- if (is.null(root)) x$clique_root else root
#   tree     <- x$junction_tree_collect
#   tree     <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)
#   cliques  <- lapply(x$cliques, function(x) setdiff(x, evidence))

#   .nbinary_ops(cliques,  tree, sp, root_idx)  
# }

# .nbinary_ops <- function(cliques, collect_tree, sp, root_idx) {
#   s <- sum(.map_dbl(1:nrow(collect_tree), function(k) {
#     if (k == root_idx) return(0)    
#     Cleave  <- cliques[[k]]
#     Cparent <- cliques[[which(collect_tree[k, ] == 1)]]
#     S       <- intersect(Cleave, Cparent)
#     3*prod(sp[Cleave]) + 2*prod(sp[Cparent]) - 2*prod(sp[S])
#   }))

#   C0 <- prod(sp[cliques[[root_idx]]])
#   s + 2*C0 - 1    
# }
