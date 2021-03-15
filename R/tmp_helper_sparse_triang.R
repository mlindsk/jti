# new_sparse_triang <- function(graph, cl) {

#   vars       <- colnames(graph)
#   mpd_       <- new_mpd(graph)
#   ps         <- mpd_$primes
#   flawed     <- mpd_$flawed
#   flawed_idx <- which(flawed)
#   nvars      <- .map_int(ps[flawed_idx], length)

#   # Order them according to nvars (root the flawed prime with most variables)
#   # ord             <- order(nvars, decreasing = TRUE)
#   flawed_idx        <- flawed_idx[order(nvars, decreasing = TRUE)]
#   flawed_graphs     <- lapply(flawed_idx, function(k) graph[ps[[k]], ps[[k]], drop = FALSE])
#   flawed_root_idx   <- flawed_idx[1L]
#   flawed_root_graph <- flawed_graphs[[1L]]
  
#   # These flawed primes are triangulated with a fast/easy heuristic
#   remaining_flawed_idx <- flawed_idx[-1L]
#   flawed_graphs[1L]    <- NULL
  
#   # Initialize prime potentials
#   primes_chr     <- lapply(ps, function(p) vars[p])
#   tmp_potentials <- initialize_prime_pots(cl, primes_chr, flawed)

#   spt <- new.env()

#   # MEMBER VARIABLES
#   # ----------------
#   spt[["dns"]]                  <- attr(cl, "dim_names")
#   spt[["org_len"]]              <- length(ps) # This is needed to delete intermediate triangulated primes
#   spt[["cur_len"]]              <- length(ps) # These are used to maintain the structure
#   spt[["old_len"]]              <- length(ps) # during the process
#   spt[["vars"]]                 <- vars

#   spt[["flawed"]]               <- flawed
#   spt[["flawed_graphs"]]        <- flawed_graphs
#   spt[["flawed_root_idx"]]      <- flawed_root_idx
#   spt[["flawed_root_graph"]]    <- flawed_root_graph

#   spt[["remaining_flawed_idx"]] <- remaining_flawed_idx
#   spt[["remaining_flawed_primes_int"]] <- ps[remaining_flawed_idx] # For updating structure when root has been triangulated
  
#   spt[["primes_int"]]           <- ps
#   spt[["tmp_potentials"]]       <- tmp_potentials # Sweep specific, altered untill the root. Reset for next root.

#   spt[["fill_edges"]]           <- list() # The final fill in the triangulation
#   spt[["potentials"]]           <- list() # Final version - only altered in the root.

#   # The junction tree (collect)
#   spt[["tmp_tmpd"]]             <- mpd_[["jt_collect"]]
#   spt[["tmpd"]]                 <- spt[["tmp_tmpd"]]

#   # MEMBER FUNCTIONS
#   # ----------------
#   # update_partial_structure.sparse_triang
#   # update_tmpd_structure.sparse_triang
#   # update_prime_pots.sparse_triang
#   # update_tmp_prime_pots.sparse.triang
#   # propagate_perfect_primes
  
#   structure(spt, class = c("sparse_triang", "environment"))
# }

# cliques_mat_int_ <- function(mat, prime_ints = NULL) {
#   # prime_ints are the true indices of mat columns
#   # if NULL no conversion is made (as in new_mpd)

#   dimnames(mat)[[1]] <- 1:nrow(mat)
#   dimnames(mat)[[2]] <- dimnames(mat)[[1]]
#   lapply(rip(as_adj_lst(mat), "")$C, function(x) {
#     if (is.null(prime_ints)) {
#       return(as.integer(x))
#     } else {
#       # Convert to original indices
#       return(prime_ints[as.integer(x)])
#     }
#   })
# }

# new_cliques_int_ <- function(prime_graph, prime_ints) {
#   # This function triangulates a flawed prime and find the cliques
#   # prime_ints are used to convert to original indicies
#   # prime_ints must ofcourse correspond to the prime_graph
#   new_prime_graph <- elim_game(new_min_fill_triang(prime_graph))[["new_graph"]]
#   cliques_int     <- cliques_mat_int_(new_prime_graph)
#   # Convert to original indices
#   lapply(cliques_int, function(x) prime_ints[x])
# }


# update_partial_structure <- function(spt, new_cliques_int, j) UseMethod("update_partial_structure")

# update_partial_structure.sparse_triang <- function(spt, new_cliques_int, j) {
#   # This function inserts new_cliques[[1]] at index j, and the remaining ones
#   # are pushed to the end of primes_int. New NULL slots for the potentials
#   # are created as well.
  
#   # j: current flawed prime index in remaining_flawed_idx

#   # NOTE: tmp_flawed uncommment on trial
  
#   tmp_flawed                       <- spt$flawed
#   # tmp_flawed[j]                    <- FALSE
#   spt$primes_int[[j]]              <- new_cliques_int[[1]]
#   spt$tmp_potentials$prime_pots[j] <- list(NULL)

#   for (i in 2:length(new_cliques_int)) {
#     spt$primes_int                    <- push(spt$primes_int, new_cliques_int[[i]])
#     spt$tmp_potentials$prime_pots     <- push(spt$tmp_potentials$prime_pots, NULL)
#     tmp_flawed[length(tmp_flawed)+1L] <- FALSE
#   }

#   spt$flawed  <- tmp_flawed
#   spt$old_len <- spt$cur_len
#   spt$cur_len <- length(spt$primes_int)
#   invisible(NULL)
# }


# update_tmp_prime_pots    <- function(spt, new_cliques_int, j) UseMethod("update_tmp_prime_pots")

# update_tmp_prime_pots.sparse_triang <- function(spt, new_cliques_int, j) {
#   # This function identify cpts in non_allocated_cpts that can now be placed
#   # in the new cliques and allocates them.

#   # j: current flawed prime index in remaining_flawed_idx
#   # FIXME: j!
  
#   new_clique_idx      <- c(j, seq(spt$old_len + 1, length(spt$primes_int)))
#   new_cliques_chr     <- lapply(new_cliques_int, function(p) spt$vars[p])
#   new_cliques_vars    <- unique(unlist(new_cliques_chr))
#   new_dims            <- spt$dns[new_cliques_vars]

#   update_tmp_prime_pots_engine(
#     spt,
#     new_cliques_chr,
#     new_dims,
#     new_clique_idx,
#     at_root = j == spt$flawed_root_idx
#   )
  
#   invisible(NULL)
# }

# propagate_perfect_primes <- function(spt) UseMethod("propagate_perfect_primes")

# propagate_perfect_primes.sparse_triang <- function(spt) {

#   rjt                <- spt$tmp_tmpd
#   jt_collect         <- rjt
#   index_vector_jt    <- 1:ncol(rjt)
#   index_vector_prune <- 1:ncol(rjt)

#   lvs         <- leaves_jt(rjt)
#   par         <- parents_jt(rjt, lvs)  
#   perfect_idx <- .map_lgl(par, function(x) x != spt$flawed_root_idx)
#   perfect_lvs <- lvs[perfect_idx]
#   perfect_par <- unlist(par[perfect_idx])

#   while (any(perfect_idx)) {

#     for (k in seq_along(perfect_lvs)) {

#       # Convert index in jt_collect to true index in rjt
#       i <- index_vector_prune[perfect_lvs[k]]
#       j <- index_vector_prune[perfect_par[[k]]]

#       # Find the true clique index in jt_collect
#       lvs_k <- match(i, index_vector_jt)
#       par_k <- match(j, index_vector_jt)
      
#       all_vars_par_k  <- spt$vars[spt$primes_int[[par_k]]]
#       pot_lvs_k       <- spt$tmp_potentials$prime_pots[[lvs_k]]
#       pot_par_k       <- spt$tmp_potentials$prime_pots[[par_k]]
#       message_k_names <- setdiff(names(pot_lvs_k), all_vars_par_k)

#       par_is_unity <- inherits(pot_par_k, "sparta_unity")
#       message_k    <- sparta::marg(pot_lvs_k, message_k_names)

#       spt$tmp_potentials$prime_pots[[par_k]] <- if (par_is_unity) message_k else sparta::mult(pot_par_k, message_k)
#       spt$tmp_potentials$prime_pots[[lvs_k]] <- sparta::div(pot_lvs_k, message_k)      
#     }

#     index_vector_prune  <- index_vector_prune[-perfect_lvs]
#     jt_collect          <- jt_collect[-perfect_lvs, -perfect_lvs, drop = FALSE]
    
#     lvs         <- leaves_jt(jt_collect)
#     par         <- unlist(parents_jt(jt_collect, lvs))

#     # Using index_vector_prune to convert par to true indices
#     perfect_idx <- .map_lgl(index_vector_prune[par], function(x) x != spt$flawed_root_idx)
#     perfect_lvs <- lvs[perfect_idx]
#     perfect_par <- par[perfect_idx]
    
#   }  
#   invisible(NULL)
# }


# update_tmpd_structure.sparse_triang <- function(spt, new_cliques_int, j, root) {
#   # j: the index of the current flawed prime under consideration
#   #     (spt$flawed_root_idx in the last update)
#   fct                <- rooted_junction_tree(new_cliques_int)
#   flawed_clique_tree <- fct[["collect"]] + fct[["distribute"]]

#   N                   <- length(spt$primes_int)
#   new_tmpd            <- matrix(0L, N, N)
#   new_cliques_mat_idx <- c(spt$flawed_root_idx, (spt$org_len + 1L):N)

#   # Update the new cliques
#   for (j in seq_along(new_cliques_mat_idx)) {
#     v     <- flawed_clique_tree[, j]
#     nei_v <- which(v == 1L)
#     if (neq_empt_int(nei_v)) {
#       new_tmpd[new_cliques_mat_idx[nei_v], new_cliques_mat_idx[j]] <- 1L
#       new_tmpd[new_cliques_mat_idx[j], new_cliques_mat_idx[nei_v]] <- 1L      
#     }
#   }

#   # update the old cliques
#   old_cliques_mat_idx <- (1:spt$org_len)[-spt$flawed_root_idx]
#   for (j in old_cliques_mat_idx) {
#     v <- spt$tmpd[, j]
#     nei_v <- which(v == 1L)
#     if (neq_empt_int(nei_v)) {
#       new_tmpd[nei_v, j] <- 1L
#       new_tmpd[j, nei_v] <- 1L      
#     }
#   }  

#   # which of the old cliques should be connected to the new ones? And which ones?
#   new_primes <- spt$primes_int[new_cliques_mat_idx]
#   for (k in old_cliques_mat_idx) {
#     psk <- spt$primes_int[[1]]
#     connect_idx <- which.max(.map_int(new_primes, function(x) length(intersect(x, psk))))
#     new_tmpd[k, connect_idx] <- 1L
#     new_tmpd[connect_idx, k] <- 1L
#   }

#   spt$tmp_tmpd <- root_clique_tree(new_tmpd, root)
  
# }


# # g <- igraph::graph_from_adjacency_matrix(spt$tmp_tmpd, "directed")
# # g %>% plot(vertex.size = 1)
# # g %>% igraph::is.chordal()
