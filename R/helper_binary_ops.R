#' Number of Binary Operations 
#'
#' Number of binary operations needed to propagate in a junction tree
#' given evidence, using the Lauritzen-Spiegelhalter scheme
#'
#' @param j A junction tree object
#' @param evidence Character vector of evidence nodes
#' @param root Integer specifying the root node in the junction tree
#' @export
jt_nbinary_ops <- function(j, evidence, root = NULL) {
  sp   <- .map_int(dim_names(j), length)
  cr   <- attr(j, "clique_root")

  root_idx <- if (is.null(root)) as.integer(substr(cr, 2, nchar(cr))) else root

  cliques <- j$cliques
  tree    <- j$schedule$collect$tree
  tree    <- if (is.null(root)) tree else root_clique_tree(tree + t(tree), root)
  
  cliques <- lapply(cliques, function(x) setdiff(x, evidence))

  s <- sum(.map_dbl(1:nrow(tree), function(k) {

    if (k == root_idx) return(0)
    
    Cleave  <- cliques[[k]]
    Cpar_idx <- which(tree[k, ] == 1)

    Cparent <- cliques[[Cpar_idx]]
    S       <- intersect(Cleave, Cparent)
    Rlv     <- setdiff(Cleave, S)
    Rpa     <- setdiff(Cparent, S)
    
    S_  <- ifelse(neq_empt_chr(S), prod(sp[S]), 1)
    R1_ <- ifelse(neq_empt_chr(Rlv), prod(sp[Rlv]), 1)
    R2_ <- ifelse(neq_empt_chr(Rpa), prod(sp[Rpa]), 1)
    
    S_ * (3*R1_ + 2*R2_ - 2)      
  }))

  C0 <- prod(sp[cliques[[root_idx]]])
  s + 2*C0 - 1
}

# dist_ev_nodes <- function(je, j, e) {
#   sp <- .map_int(dim_names(j), length)
#   sp <- sp[names(e)]
#   je_ <- .map_int(names(e), function(e_) {
#     .map_lgl(je$cliques, function(x) {
#       e_ %in% x
#     }) %>% sum()
#   })

#   j_ <- .map_int(names(e), function(e_) {
#     .map_lgl(j$cliques, function(x) {
#       e_ %in% x
#     }) %>% sum()
#   })

#   rbind(je_, j_, sp)
# }

# prop <- function(data_miss, j) {
#   for (k in 1:nrow(data_miss)) {
#     ek <- data_miss[k, ] %>%
#       as.character() %>%
#       structure(names = colnames(data_miss)) %>%
#       na.omit()
#     jk <- set_evidence(j, ek)
#     p  <- propagate(jk)
#   }
# }

# prop2 <- function(data_miss) {
#   for (k in 1:nrow(data_miss)) {
#     ek <- data_miss[k, ] %>%
#       as.character() %>%
#       structure(names = colnames(data_miss)) %>%
#       na.omit()

#     cpe <- compile(cl, ek,  tri = "evidence", evidence_nodes = names(ek))
#     je  <- jt(cpe)
#   }
# }


# compare_binary_ops_to_min_fill <- function(js, j_min_fill, data_miss) {
#   # js: list of junction trees
#   lapply(js, function(j) {
#     nops <- .map_dbl(1:nrow(data_miss), function(k) {
#       ek <- data_miss[k, ] %>%
#         as.character() %>%
#         structure(names = colnames(data_miss)) %>%
#         na.omit()
#       jt_nbinary_ops(j, names(ek)) / jt_nbinary_ops(j_min_fill, names(ek))
#     })
#   })
# }
