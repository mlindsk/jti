#' Simulate data from a Bayesian network
#' 
#' @param net A Bayesian network as an igraph object
#' @param lvls Named integer vector where each element is the size of the
#' statespace of the corresponding variable
#' @param nsims Number of simulations
#' @export
sim_data_from_network <- function(net, lvls, nsims = 1000) {

  nodes <- igraph::V(net)$name

  stopifnot(identical(sort(nodes), sort(names(lvls))))
  
  N       <- length(nodes)
  pars    <- parents_igraph(net)
  sims    <- structure(vector("list", length = N), names = nodes)
  visited <- structure(vector("logical", length = N), names = nodes)

  founders <- which(
    .map_lgl(pars, function(x) identical(character(0), x)) == TRUE
  )

  # magic parameters
  s1 <- 3; s2 <- 5
  pmf_founders <- lapply(lvls[founders], function(n) {
    rb <- rbeta(n, s1, s2)
    rb / sum(rb)
  })

  founders_sim <- lapply(pmf_founders, function(x) {
    sample(1:length(x), nsims, replace = TRUE, prob = x)
  })

  # update sims and visited
  sims[names(founders_sim)] <- founders_sim
  visited[names(founders_sim)] <- TRUE

  while (sum(visited) != N) {
    cand_idx <- .map_lgl(pars[!visited], function(ps) all(ps %in% names(which(visited))))
    candidates <- pars[!visited][cand_idx]
    
    complete_pars_candidates <- which(.map_lgl(candidates, function(ps) {
      all(ps %in% names(which(visited)))
    }))

    while (!all(visited[names(candidates)])) {

      for (child in names(complete_pars_candidates)) {

        child_pars <- pars[[child]]
        family     <- c(child_pars, child)
        parray     <- expand.grid(lapply(lvls[family], function(z) 1:z))
        n_par_comb <- prod(lvls[child_pars])
        tmp_p      <- rep(0, nrow(parray))

        for (k in 1:n_par_comb) {
          idx    <- vector("logical", length = n_par_comb)
          idx[k] <- TRUE
          idx    <-rep(idx, lvls[child])
          rb     <- rbeta(lvls[child], s1, s2)
          tmp_p[idx] <- rb / sum(rb)
        }
        
        parray$p   <- tmp_p

        parray_facet_child <- split(parray, parray[[child]])

        parray_facet_child[] <- lapply(parray_facet_child, function(x) {
          prob <- x[, "p", drop = TRUE]
          .names <- apply(x[, child_pars, drop = FALSE], 1L, function(y) {
            paste(y, collapse = ":")
          })
          structure(prob, names = .names)
        })
        
        par_cells <- apply(parray, 1L, function(x) {
          paste(x[1:length(child_pars)], collapse = ":")
        })

        par_sim <- apply(as.data.frame(sims[child_pars]), 1L, function(x) {
          paste(x, collapse = ":")
        })
        
        child_sims <- .map_int(par_sim, function(ps) {
          idx <- match(ps, names(parray_facet_child[[1]]))
          cond_prob <- .map_dbl(parray_facet_child, function(x) x[ps])
          sample(1:length(cond_prob), 1L, prob = cond_prob)
        })

        sims[[child]] <- unname(child_sims)
      }
      
      # update so new children are now possible and set current ones as visited
      visited[names(complete_pars_candidates)] <- TRUE
      complete_pars_candidates <- which(.map_lgl(candidates, function(ps) {
        all(ps %in% names(which(visited)))
      }))
    }
  }
  
  out <- as.data.frame(sims)
  colnames(out) <- names(pars)
  return(out)
}

#' Simulate data from a decomposable discrete markov random field
#' 
#' @param net A decomposable discrete markov random field as an igraph object
#' @inheritParams sim_data_from_net
#' @export
sim_data_from_dmrf <- function(mrf, lvls, nsims = 1000) {

  is_dag <- igraph::is_dag(mrf)
  if (!is_dag) {
    if (!igraph::is_chordal(mrf)$chordal) {
      stop("mrf must be be decomposable")
    }
  }
  
  x   <- rip(as_adj_lst(igraph::as_adjacency_matrix(mrf, sparse = FALSE)), check = FALSE)$P
  net <- igraph::make_empty_graph(n = length(x))
  net <- igraph::set_vertex_attr(net, "label", value = names(x))
  net <- igraph::set_vertex_attr(net, "name", value = names(x))
  edges <- lapply(seq_along(x), function(i) {
    child   <- names(x)[i]
    parents <- setdiff(x[[i]], child)
    as.matrix(expand.grid(parents, child, stringsAsFactors = FALSE))
  })
  edges <- do.call(rbind, edges)
  for (k in 1:nrow(edges)) {
    net <- igraph::add_edges(net, unname(edges[k, ]))
  }
  return(sim_data_from_network(net, lvls, nsims))
}
