#' Simulate data from a Bayesian network
#' 
#' @param net A Bayesian network as an igraph object
#' @param lvls Named integer vector where each element is the size of the
#' statespace of the corresponding variable
#' @param nsims Number of simulations
#' distributions from which the simulatios are drawn.
#' @param increasing_prob Logical. If true, probabilities in the underlying CPTs
#' increases with as the number of levels increses.
#' @param p1 Probability
#' @param p2 Probability
#' @examples
#' net <- igraph::graph(as.character(c(1,2,1,3,3,4,3,5,5,4,2,6,6,7,5,7)), directed = TRUE)
#' nodes_net <- igraph::V(net)$name
#' lvls_net  <- structure(sample(3:9, length(nodes_net)), names = nodes_net)
#' lvls_net  <- structure(rep(3, length(nodes_net)), names = nodes_net)
#' sim_data_from_bn(net, lvls_net, 10)
#' @export
sim_data_from_bn <- function(net, lvls, nsims = 1000, increasing_prob = FALSE, p1 = .8, p2 = 1) {

  nodes <- igraph::V(net)$name

  stopifnot(identical(sort(nodes), sort(names(lvls))))
  
  N       <- length(nodes)
  pars    <- parents_igraph(net)
  sims    <- structure(vector("list", length = N), names = nodes)
  visited <- structure(vector("logical", length = N), names = nodes)

  founders <- which(
    .map_lgl(pars, function(x) identical(character(0), x)) == TRUE
  )

  # Magic numbers!
  # alpha <- 1
  # beta  <- 1

  pmf_founders <- lapply(lvls[names(founders)], function(n) {
    if (n == 2) {
      z <- stats::runif(1, p1, p2)
      c(1-z, z)
    } else {
      ru <- stats::runif(n) # stats::rueta(n, alpha, beta)
      if (increasing_prob) ru <- sort(ru)
      ru / sum(ru)      
    }
  })

  founders_sim <- lapply(pmf_founders, function(x) {
    sample(0:(length(x)-1), nsims, replace = TRUE, prob = x)
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
        parray     <- expand.grid(lapply(lvls[family], function(z) 0:(z-1)))
        n_par_comb <- prod(lvls[child_pars])
        tmp_p      <- rep(0, nrow(parray))

        for (k in 1:n_par_comb) {
          idx    <- vector("logical", length = n_par_comb)
          idx[k] <- TRUE
          idx    <- rep(idx, lvls[child])

          
          if (lvls[child] == 2) {
            z <- stats::runif(1, p1, p2)
            tmp_p[idx] <- c(1-z, z)
          } else {
            ru     <- stats::runif(lvls[child]) # stats::rueta(lvls[child], alpha, beta)
            if (increasing_prob) ru <- sort(ru)
            tmp_p[idx] <- ru / sum(ru)            
          }
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
          sample(0:(length(cond_prob) -1), 1L, prob = cond_prob)
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
  out[] <- lapply(out, as.character)
  return(out)
}

#' Simulate data from a decomposable discrete markov random field
#' 
#' @param graph A decomposable discrete markov random field as an igraph object
#' @inheritParams sim_data_from_bn
#' @export
sim_data_from_dmrf <- function(graph, lvls, nsims = 1000, increasing_prob = FALSE, p1 = .8, p2 = 1) {

  is_dag <- igraph::is_dag(graph)
  if (!is_dag) {
    if (!igraph::is_chordal(graph)$chordal) {
      stop("graph must be decomposable")
    }
  }
  
  x   <- rip(as_adj_lst(igraph::as_adjacency_matrix(graph, sparse = FALSE)), check = FALSE)$P
  net <- igraph::make_empty_graph(n = length(x))
  net <- igraph::set_vertex_attr(net, "label", value = names(x))
  net <- igraph::set_vertex_attr(net, "name", value = names(x))
  edges <- lapply(seq_along(x), function(i) {
    child   <- names(x)[i]
    parents <- setdiff(x[[i]], child)
    as.matrix(expand.grid(parents, child, stringsAsFactors = FALSE))
  })

  edges <- do.call(rbind, edges)
  if (nrow(edges) > 0) {
    for (k in 1:nrow(edges)) {
      net <- igraph::add_edges(net, unname(edges[k, ]))
    }    
  } else {
    net <- igraph::as.directed(net)
  }

  return(sim_data_from_bn(net, lvls, nsims, increasing_prob))
}
