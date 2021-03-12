# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            SETUP DATA AND LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(igraph)
library(magrittr)

l    <- readRDS("../../../../sandbox/r/bns/munin.rds")
l    <- readRDS("../../../../sandbox/r/bns/link.rds")
l    <- readRDS("../../../../sandbox/r/bns/diabetes.rds")
l    <- readRDS("../../../../sandbox/r/bns/barley.rds")
l    <- readRDS("../../../../sandbox/r/bns/hailfinder.rds")
l    <- readRDS("../../../../sandbox/r/bns/asia.rds")

cpts <- bnfit_to_cpts(l)
cl   <- cpt_list(cpts)
dns  <- attr(cl, "dim_names")
g    <- attr(cl, "graph")

par(mfrow = c(1, 2))
plot(g, vertex.size = 15)

mg   <- moralize_igraph(g, attr(cl, "parents"))
moral_graph <- igraph::as_adjacency_matrix(mg, sparse = FALSE)
spt <- new_sparse_triang(moral_graph, cl)

plot(mg, vertex.size = 15)

while (any(spt$flawed)) {

  for (k in seq_along(spt$remaining_flawed_idx)) {
    prime_int_k     <- spt$remaining_flawed_primes_int[[k]]
    prime_graph_k   <- spt$flawed_graphs[[k]]
    new_cliques_int <- new_cliques_int_(prime_graph_k, prime_int_k)

    j <- spt$remaining_flawed_idx[k]
    update_partial_structure.sparse_triang(spt, new_cliques_int, j)
    update_tmp_prime_pots.sparse_triang(spt, new_cliques_int, j)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   propagate 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # hack since otherwise sorted and hence altered in spt due to reference semantics!
  tmp_primes     <- spt[["primes_int"]]; tmp_primes[[length(tmp_primes)+1]] <- NULL
  tmp_jt_collect <- rooted_junction_tree(tmp_primes, spt$flawed_root_idx)$collect
  propagate_perfect_primes.sparse_triang(spt, tmp_jt_collect)

  # g <- igraph::graph_from_adjacency_matrix(rjt, "directed")
  # g %>% plot()

  # Find all "neiboring" nodes of the flawed prime
  flawed_nei_idx   <- which(tmp_jt_collect[, spt$flawed_root_idx] == 1L) # lvs[flawed_idx]
  flawed_root_vars <- spt$vars[spt$primes_int[[spt$flawed_root_idx]]]
  
  # Calculate all messages that will eventually be send to the root from the flawed neibors
  for (k in flawed_nei_idx) {
    pot_lvs_k       <- spt$tmp_potentials$prime_pots[[k]]
    message_k_names <- setdiff(names(pot_lvs_k), flawed_root_vars)
    message_k       <- sparta::marg(pot_lvs_k, message_k_names)
    spt$tmp_potentials$flawed_root_msg <- push(spt$tmp_potentials$flawed_root_msg, message_k)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #               END OF PROPAGATE
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # eg <- elim_game(new_min_nei_triang(spt$flawed_root_graph))
  # eg <- elim_game(new_min_fill_triang(spt$flawed_root_graph))

  # Triangulate the flawed root
  eg             <- elim_game(spt) # length(eg[[2]])
  new_root_graph <- eg[["new_graph"]]

  # Convert fill edges to original indices
  fills <- lapply(eg[["fill_edges"]], function(e) {
    spt$primes_int[[spt$flawed_root_idx]][e]
  })
  
  spt$fill_edges <- c(spt$fill_edges, fills)

  # Add fills to the moral graph
  for (f in fills) {
    moral_graph[f[1], f[2]] <- 1L
    moral_graph[f[2], f[1]] <- 1L
  }
  
  # Reset messages
  spt$tmp_potentials$flawed_root_msg <- list()


  # Update structure so remaining flawed primes are back again (only if len remaining > 0L!)
  if (neq_empt_int(spt$remaining_flawed_idx)) {
    spt$primes_int[spt$remaining_flawed_idx] <- spt$remaining_flawed_primes_int
    spt$primes_int[(spt$org_len + 1):spt$cur_len] <- NULL
    
    spt$tmp_potentials$prime_pots[spt$remaining_flawed_idx] <- list(NULL)
    spt$tmp_potentials$prime_pots[(spt$org_len + 1):spt$cur_len] <- NULL

    # update cur_len
    spt$cur_len <- spt$org_len
    spt$old_len <- spt$org_len # neccessary? YES :D !
  }

  # Get the new cliques of the root graph and add them to spt$prime_int
  new_root_cliques_int <- cliques_mat_int_(new_root_graph, spt$primes_int[[spt$flawed_root_idx]])

  # Insert the new cliques resulting from triangulating the flawed root
  update_partial_structure.sparse_triang(spt, new_root_cliques_int, spt$flawed_root_idx)
  
  # Populate the new flawed root cliques with non_allocated_cpts
  update_tmp_prime_pots.sparse_triang(spt, new_root_cliques_int, spt$flawed_root_idx)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #        
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # TODO: Update spt$tmpd
  # THAT IS:
  # ----------------------
  # 1) transpose spt$tmpd first, this gives the parent of the flawed prime.
  # 2) make a junction tree of the new cliques in the flawed root
  #    with root given in 1)
  # 3) find all leaves that must be connected to the new cliques
  # 4) root this new tmpd.


  update_clique_tree_structure <- function(spt, new_root_cliques_int, j) {
    # j: the index of the current flawed prime under consideration
    #     (spt$flawed_root_idx in the last update)
  }
  
  # spt$tmpd <- root_junction_tree(spt$tmpd, spt$flawed_root_idx)
  flawed_clique_tree <- rooted_junction_tree(new_root_cliques_int)$collect
  flawed_clique_tree <- flawed_clique_tree + t(flawed_clique_tree)

  N                   <- length(spt$primes_int)
  new_tmpd            <- matrix(0L, N, N)
  new_cliques_mat_idx <- c(spt$flawed_root_idx, (spt$org_len + 1L):N)

  # Update the new cliques
  for (j in seq_along(new_cliques_mat_idx)) {
    v     <- flawed_clique_tree[, j]
    nei_v <- which(v == 1L)
    if (neq_empt_int(nei_v)) {
      new_tmpd[new_cliques_mat_idx[nei_v], new_cliques_mat_idx[j]] <- 1L
      new_tmpd[new_cliques_mat_idx[j], new_cliques_mat_idx[nei_v]] <- 1L      
    }
  }

  # update the old cliques
  old_cliques_mat_idx <- (1:spt$org_len)[-spt$flawed_root_idx]
  for (j in old_cliques_mat_idx) {
    v <- spt$tmpd[, j]
    nei_v <- which(v == 1L)
    if (neq_empt_int(nei_v)) {
      new_tmpd[nei_v, j] <- 1L
      new_tmpd[j, nei_v] <- 1L      
    }
  }  

  # which of the old cliques should be connected to the new ones? And which ones?
  new_primes <- spt$primes_int[new_cliques_mat_idx]
  for (k in old_cliques_mat_idx) {
    psk <- spt$primes_int[[1]]
    connect_idx <- which.max(.map_int(new_primes, function(x) length(intersect(x, psk))))
    new_tmpd[k, connect_idx] <- 1L
    new_tmpd[connect_idx, k] <- 1L
  }


  A <- root_clique_tree(new_tmpd, 27)
  
  g <- igraph::graph_from_adjacency_matrix(A, "directed")
  g %>% plot(vertex.size = 1)
  g %>% igraph::is.chordal()
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #        
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  # Update the final potentials
  spt$potentials <- spt$tmp_potentials$prime_pots

  # Update flag for flawed root
  spt$flawed[spt$flawed_root_idx] <- FALSE
  
  if (any(spt$flawed)) {
    # Update non_allocated_cpts and remaining ones
    spt$tmp_potentials$non_allocated_cpts <- spt$tmp_potentials$remaining_flawed_non_allocated_cpts
    
    # Reset the temporary remaining cpts
    spt$tmp_potentials$remaining_flawed_non_allocated_cpts <- list()
    
    # Update member variables
    spt$flawed_root_idx      <- spt$remaining_flawed_idx[1L]
    spt$flawed_root_graph    <- spt$flawed_graphs[[1L]]
    spt$flawed_graphs[1L]    <- NULL
    
    spt$remaining_flawed_idx <- spt$remaining_flawed_idx[-1L]
    spt$remaining_flawed_primes_int <- spt$primes_int[spt$remaining_flawed_idx]

    spt$org_len              <- length(spt$primes_int)
    spt$cur_len              <- spt$org_len
    spt$old_len              <- spt$org_len
  }
  
}


# ----------
# inspection
library(magrittr)
g <- igraph::graph_from_adjacency_matrix(moral_graph, "undirected")
g %>% plot()
g %>% igraph::is.chordal()
# -----------
