# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #            SETUP DATA AND LIBRARIES
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # alarm
# # andes
# # asia
# # barley
# # cancer
# # child
# # diabetes
# # earthquake
# # ecoli70
# # hailfinder
# # hepar2
# # insurance
# # link
# # mildew
# # munin
# # pathfinder
# # pigs
# # sachs
# # survey
# # water
# # win95pts

# library(igraph)
# library(magrittr)

# read_net <- function(net) readRDS(glue::glue("../../../../sandbox/r/bns/", net, ".rds"))
# l <- read_net("link")

# cpts <- bnfit_to_cpts(l)
# cl   <- cpt_list(cpts)
# dns  <- attr(cl, "dim_names")
# g    <- attr(cl, "graph")

# mg   <- moralize_igraph(g, attr(cl, "parents"))
# moral_graph <- igraph::as_adjacency_matrix(mg, sparse = FALSE)
# spt <- new_sparse_triang(moral_graph, cl)

# par(mfrow = c(1, 2))
# plot(g, vertex.size = 15)
# plot(mg, vertex.size = 15)

# while (any(spt$flawed)) {

#   for (k in seq_along(spt$remaining_flawed_idx)) {
#     prime_int_k     <- spt$remaining_flawed_primes_int[[k]]
#     prime_graph_k   <- spt$flawed_graphs[[k]]
#     new_cliques_int <- new_cliques_int_(prime_graph_k, prime_int_k)

#     j <- spt$remaining_flawed_idx[k]
#     update_partial_structure.sparse_triang(spt, new_cliques_int, j)
#     update_tmpd_structure.sparse_triang(spt, new_cliques_int, j, spt$flawed_root_idx)
#     update_tmp_prime_pots.sparse_triang(spt, new_cliques_int, j)
#   }


#   propagate_perfect_primes.sparse_triang(spt)

#   # Find all "neiboring" nodes of the flawed prime
#   flawed_nei_idx   <- which(spt$tmp_tmpd[, spt$flawed_root_idx] == 1L) # lvs[flawed_idx]
#   flawed_root_vars <- spt$vars[spt$primes_int[[spt$flawed_root_idx]]]
  
#   # Calculate all messages that will eventually be send to the root from the flawed neibors
#   for (k in flawed_nei_idx) {
#     pot_lvs_k       <- spt$tmp_potentials$prime_pots[[k]]
#     message_k_names <- setdiff(names(pot_lvs_k), flawed_root_vars)
#     message_k       <- sparta::marg(pot_lvs_k, message_k_names)
#     spt$tmp_potentials$flawed_root_msg <- push(spt$tmp_potentials$flawed_root_msg, message_k)
#   }

#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   #               END OF PROPAGATE
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   # eg <- elim_game(new_min_nei_triang(spt$flawed_root_graph))

#   # Triangulate the flawed root
#   eg             <- elim_game(spt) # length(eg[[2]])
#   new_root_graph <- eg[["new_graph"]]

#   # Convert fill edges to original indices
#   fills <- lapply(eg[["fill_edges"]], function(e) {
#     spt$primes_int[[spt$flawed_root_idx]][e]
#   })
  
#   spt$fill_edges <- c(spt$fill_edges, fills)

#   # Add fills to the moral graph
#   for (f in fills) {
#     moral_graph[f[1], f[2]] <- 1L
#     moral_graph[f[2], f[1]] <- 1L
#   }

#   # link_triangulated_graph <- moral_graph
#   # saveRDS(link_triangulated_graph, "link_triangulated_graph.rds")
  
#   # Reset messages
#   spt$tmp_potentials$flawed_root_msg <- list()


#   # Update structure so remaining flawed primes are back again (only if len remaining > 0L!)
#   if (neq_empt_int(spt$remaining_flawed_idx)) {
#     spt$primes_int[spt$remaining_flawed_idx] <- spt$remaining_flawed_primes_int
#     spt$primes_int[(spt$org_len + 1):spt$cur_len] <- NULL
    
#     spt$tmp_potentials$prime_pots[spt$remaining_flawed_idx] <- list(NULL)
#     spt$tmp_potentials$prime_pots[(spt$org_len + 1):spt$cur_len] <- NULL

#     # update cur_len
#     spt$cur_len <- spt$org_len
#     spt$old_len <- spt$org_len # neccessary? YES :D !
#   }

#   # Get the new cliques of the root graph and add them to spt$prime_int
#   new_root_cliques_int <- cliques_mat_int_(new_root_graph, spt$primes_int[[spt$flawed_root_idx]])

#   # Insert the new cliques resulting from triangulating the flawed root
#   update_partial_structure.sparse_triang(spt, new_root_cliques_int, spt$flawed_root_idx)
  
#   # Populate the new flawed root cliques with non_allocated_cpts
#   update_tmp_prime_pots.sparse_triang(spt, new_root_cliques_int, spt$flawed_root_idx)

#   # Update the final potentials
#   spt$potentials <- spt$tmp_potentials$prime_pots

#   # Update flag for flawed root
#   spt$flawed[spt$flawed_root_idx] <- FALSE
  
#   if (any(spt$flawed)) {

#     # Update the junction tree structure to obey the way we optimized.
#     update_tmpd_structure.sparse_triang(spt, new_root_cliques_int, spt$flawed_root_idx, spt$remaining_flawed_idx[1L])
#     spt$tmpd <- spt$tmp_tmpd
    
#     # Update non_allocated_cpts and remaining ones
#     spt$tmp_potentials$non_allocated_cpts <- spt$tmp_potentials$remaining_flawed_non_allocated_cpts
    
#     # Reset the temporary remaining cpts
#     spt$tmp_potentials$remaining_flawed_non_allocated_cpts <- list()
    
#     # Update member variables
#     spt$flawed_root_idx      <- spt$remaining_flawed_idx[1L]
#     spt$flawed_root_graph    <- spt$flawed_graphs[[1L]]
#     spt$flawed_graphs[1L]    <- NULL
    
#     spt$remaining_flawed_idx <- spt$remaining_flawed_idx[-1L]
#     spt$remaining_flawed_primes_int <- spt$primes_int[spt$remaining_flawed_idx]

#     spt$org_len              <- length(spt$primes_int)
#     spt$cur_len              <- spt$org_len
#     spt$old_len              <- spt$org_len
#   }
  
# }


# # # ----------
# # # inspection
# library(magrittr)
# g <- igraph::graph_from_adjacency_matrix(moral_graph, "undirected")
# g %>% plot()
# g %>% igraph::is.chordal()


# spt$primes_int

# igraph::graph_from_adjacency_matrix(spt$tmpd, "directed") %>% plot()
# # # -----------
