# allocate_child_to_prime_potential <- function(cpt, prime_pots, prime_chr, non_allocated_cpts, flawed) {
#   # prime_pots: environment with clique potentials
#   for (k in seq_along(prime_chr)) {
#     if (flawed[k]) next
#     family_in_Ck <- all(names(cpt) %in% prime_chr[[k]])
#     if (family_in_Ck) {
#       if (is.null(prime_pots$pp[[k]])) {
#         prime_pots$pp[[k]] <- cpt
#         return(NULL)
#       } else {
#         prime_pots$pp[[k]] <- sparta::mult(prime_pots$pp[[k]], cpt)
#         return(NULL)
#       }
#     } 
#   }
#   # If the cpt was not fully contained in any perfect prime, record it.
#   non_allocated_cpts$cpts <- push(non_allocated_cpts$cpts, cpt)
#   invisible(NULL)
# }

# initialize_prime_pots <- function(cl, primes_chr, flawed) {
#   # cl         : cpt_list (sparta objects)
#   # primes_chr : obtained from mpd function (but as character names)
#   # flawed     : a vector of flags indicating which primes_chr are flawed
  
#   prime_pots              <- new.env()
#   prime_pots$pp           <- vector("list", length(primes_chr))
#   non_allocated_cpts      <- new.env()
#   non_allocated_cpts$cpts <- vector("list")
  
#   for (k in seq_along(cl)) {
#     cpt <- cl[[k]]
#     allocate_child_to_prime_potential(cpt, prime_pots, primes_chr, non_allocated_cpts, flawed)
#   }

#   # NOTE: Why is this here?
#   names(prime_pots$pp) <- names(primes_chr)

#   return(
#     structure(list(
#       prime_pots                          = prime_pots$pp,
#       non_allocated_cpts                  = non_allocated_cpts$cpts,
#       remaining_flawed_non_allocated_cpts = list(),
#       flawed_root_msg                     = list()
#     ),
#     class = c("prime_pots", "list")
#     )
#   )
# } 


# assign_pot_at_idx <- function(spt, cpt, idx) UseMethod("assign_pot_at_idx")

# assign_pot_at_idx.prime_pots <- function(spt, cpt, idx) {
#   if (is.null(spt$tmp_potentials$prime_pots[[idx]])) {
#     spt$tmp_potentials$prime_pots[[idx]] <- cpt
#   } else {
#     spt$tmp_potentials$prime_pots[[idx]] <- sparta::mult(spt$tmp_potentials$prime_pots[[idx]], cpt)
#   }
#   invisible(NULL)
# }


# update_tmp_prime_pots_engine <- function(spt, new_cliques_chr, new_dim_names, new_clique_idx, at_root) {
#   # new_cliques_chr : the new cliques as characters
#   # new_dim_names   : dimnames of all variables included in new_cliques_chr
#   # new_clique_idx  : indices of new cliques
#   # at_root         : boolean indicating if we are updating the root
  
#   rm_cpt_idx <- vector("integer")
#   for (q in seq_along(spt$tmp_potentials$non_allocated_cpts)) {
#     cpt <- spt$tmp_potentials$non_allocated_cpts[[q]]
#     for (k in seq_along(new_clique_idx)) {
#       npk    <- new_cliques_chr[[k]]
#       in_npk <- all(names(cpt) %in% npk)
#       if (in_npk) {
#         idx_k <- new_clique_idx[k]
#         assign_pot_at_idx.prime_pots(spt, cpt, idx_k)
#         rm_cpt_idx <- c(rm_cpt_idx, q)
#         break # Must only live in one clique
#       }
#     }
#   }  

#   if (!at_root) {
#     # Allocate the now added cpts to a temporary structure in order to use them again later
#     # When we are at the root, the non_allocated just used should not be used again
#     spt$tmp_potentials$remaining_flawed_non_allocated_cpts <- c(
#       spt$tmp_potentials$remaining_flawed_non_allocated_cpts,
#       spt$tmp_potentials$non_allocated_cpts[rm_cpt_idx]
#     )
#   }
  
#   # Remove those cpts from non_allocated that are now assigned
#   spt$tmp_potentials$non_allocated_cpts[rm_cpt_idx] <- NULL

#   # Now make unities if no cpts are available!
#   for (k in seq_along(new_clique_idx)) {
#     idx_k <- new_clique_idx[k]
#     if (is.null(spt$tmp_potentials$prime_pots[[idx_k]])) {
#       spt$tmp_potentials$prime_pots[[idx_k]] <- sparta::sparta_unity_struct(new_dim_names[new_cliques_chr[[k]]])
#     }
#   }

#   invisible(NULL)
# }
