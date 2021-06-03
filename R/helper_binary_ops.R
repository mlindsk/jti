#' Number of Binary Operations 
#'
#' Number of binary operations needed to propagate in a junction tree
#' given evidence, using the Lauritzen-Spiegelhalter scheme
#'
#' @param x A junction tree object or an object returned from
#' the triangulation function
#' @param evidence Character vector of evidence nodes
#' @param root Integer specifying the root node in the junction tree
#' @export
jt_nbinary_ops <- function(x, evidence = character(0), root = NULL) {
  UseMethod("jt_nbinary_ops")
}

#' @rdname jt_nbinary_ops
#' @export
jt_nbinary_ops.jt <- function(x, evidence = character(0), root = NULL) {
  stopifnot(attr(x, "propagate") == "no")
  sp       <- .map_int(dim_names(x), length)
  cr       <- attr(x, "clique_root")
  root_idx <- if (is.null(root)) as.integer(substr(cr, 2, nchar(cr))) else root
  tree     <- x$schedule$collect$tree
  tree     <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)
  cliques  <- lapply(x$cliques, function(x) setdiff(x, evidence))
  .nbinary_ops(cliques, tree, sp, root_idx)
}

#' @rdname jt_nbinary_ops
#' @export
jt_nbinary_ops.triangulation <- function(x, evidence = character(0), root = NULL) {
  sp       <- .map_int(x$dim_names, length)
  root_idx <- if (is.null(root)) x$clique_root else root
  tree     <- x$junction_tree_collect
  tree     <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)

  # TODO: Let e be a list of evidence. Then we can save the above (e.g. wont have to calc sp each time)
  
  cliques  <- lapply(x$cliques, function(x) setdiff(x, evidence))
  .nbinary_ops(cliques,  tree, sp, root_idx)  
}

.nbinary_ops <- function(cliques, collect_tree, sp, root_idx) {
  s <- sum(.map_dbl(1:nrow(collect_tree), function(k) {
    if (k == root_idx) return(0)    
    Cleave  <- cliques[[k]]
    Cparent <- cliques[[which(collect_tree[k, ] == 1)]]
    S       <- intersect(Cleave, Cparent)
    3*prod(sp[Cleave]) + 2*prod(sp[Cparent]) - 2*prod(sp[S])
  }))

  C0 <- prod(sp[cliques[[root_idx]]])
  s + 2*C0 - 1    
}


# .nbinary_ops2 <- function(cliques, collect_tree, sp, root_idx) {
#   s <- sum(.map_dbl(1:nrow(collect_tree), function(k) {
#     if (k == root_idx) return(0)

#     Cleave  <- cliques[[k]]
#     Cpar_idx <- which(collect_tree[k, ] == 1)

#     Cparent <- cliques[[Cpar_idx]]
#     S       <- intersect(Cleave, Cparent)
#     Rlv     <- setdiff(Cleave, S)
#     Rpa     <- setdiff(Cparent, S)
    
#     S_  <- ifelse(neq_empt_chr(S), prod(sp[S]), 1)
#     R1_ <- ifelse(neq_empt_chr(Rlv), prod(sp[Rlv]), 1)
#     R2_ <- ifelse(neq_empt_chr(Rpa), prod(sp[Rpa]), 1)
    
#     S_ * (3*R1_ + 2*R2_ - 2)      
#   }))

#   C0 <- prod(sp[cliques[[root_idx]]])
#   s + 2*C0 - 1    
# }
