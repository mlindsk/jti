new_schedule <- function(cliques) {

  nc <- length(cliques)
  clique_graph <- matrix(0L, nc, nc)
  coll_tree   <- clique_graph
  dist_tree   <- clique_graph

  # Alg. 4.8 - Probabilistic Expert Systems (p.55)
  for (i in seq_along(cliques)[-1L]) {
    for (j in 1:(i-1L)) {
      Ci   <- cliques[[i]]
      Cj   <- cliques[[j]]
      Hi_1 <- unlist(cliques[1:(i-1L)])
      Si   <- intersect(Ci, Hi_1)
      if (all(Si %in% Cj)) {
        clique_graph[i, j] <- 1L
        clique_graph[j, i] <- 1L
        is_new_directed_edge <- !neq_empt_int(which(coll_tree[i, ] == 1L))
        if (is_new_directed_edge) {
          coll_tree[i, j] <- 1L
          dist_tree[j, i] <- 1L
        }
      }
    }
  }

  coll_lvs <- leaves_jt(coll_tree)
  dist_lvs <- leaves_jt(dist_tree)

  attr(coll_tree, "leaves")  <- coll_lvs
  attr(dist_tree, "leaves")  <- dist_lvs

  attr(coll_tree, "parents") <- parents_jt(coll_tree, coll_lvs)
  attr(dist_tree, "parents") <- parents_jt(dist_tree, dist_lvs)

  collect    <- list(cliques = cliques, tree = coll_tree)
  distribute <- list(cliques = cliques, tree = dist_tree)

  # TODO: Should we return a schedule as an environment?
  # - that would reduce the number of arguments to input/output everywhere?
  
  return(list(collect = collect , distribute = distribute, clique_graph = clique_graph))
  
}

leaves_jt <- function(x) {
  # x: rooted tree structure of a junctions tree (jt$schedule$collect$tree)
  which(colSums(x) == 0L)
}

parents_jt <- function(x, lvs) {
  # x:   rooted tree structure of a junctions tree (jt$schedule$collect$tree)
  # lvs: leaves of the junction tree
  par <- vector("list", length = length(lvs))
  for (i in seq_along(lvs)) {
    pari <- which(x[lvs[i], ] == 1L)
    par[[i]] <- pari
  }
  return(par)
}

prune_jt <- function(jt) {

  direction <- attr(jt, "direction")
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute

  if (identical(x, "FULL")) {
    stop("The junction tree has already been propagated in this direction!")  
  }

  leaves <- attr(x$tree, "leaves")
  pars   <- attr(x$tree, "parents")

  
  if (length(leaves) == ncol(x$tree)) { # If all nodes left are singletons in distribute
    x$cliques <- NULL
  } else {
    x$cliques <- x$cliques[-leaves]
    x$tree    <- x$tree[-leaves, -leaves]   
  }
  
  has_arrived_at_root <- length(x$cliques) < 2L
  if (has_arrived_at_root) {
    if (direction == "collect") {
      jt$schedule$collect    <- "FULL"
      attr(jt, "direction")  <- "distribute"
      probability_of_evidence <- sum(jt$charge$C[["C1"]])
      attr(jt, "probability_of_evidence") <- probability_of_evidence
      jt$charge$C[["C1"]] <- jt$charge$C[["C1"]] / probability_of_evidence
    } else {
      jt$schedule$distribute <- "FULL"
      attr(jt, "direction")  <- "FULL"
    }
    return(jt)
  }
  
  attr(x$tree, "leaves")  <- leaves_jt(x$tree)
  attr(x$tree, "parents") <- parents_jt(x$tree, attr(x$tree, "leaves"))

  if (direction == "collect") {
    jt$schedule$collect <- list(cliques = x$cliques, tree = x$tree)
  } else {
    jt$schedule$distribute <- list(cliques = x$cliques, tree = x$tree)
  }
  
  return(jt)
}

set_evidence_jt <- function(charge, cliques, evidence) {
  # We dont loop over the cliques now, since some clique
  # potentials may not have as many variables as the
  # corresponding clique in the triangulated graph.
  # Say, C = {a,b,c}, then potC may be {b,c} if a is
  # already put in another clique. But {b,c} is still
  # "implicitly a function" of {a}. But we must take this
  # into account when setting evidence.
  # for (k in seq_along(cliques)) {
  for (k in seq_along(charge$C)) {
    Ck <- attr(charge$C[[k]], "vars")
    for (i in seq_along(evidence)) {
      e     <- evidence[i]
      e_var <- names(e)
      e_val <- unname(e)
      if (e_var %in% Ck) {
        e_pos_charge_k <- match(e_var, attr(charge$C[[k]], "vars"))
        charge_k_by_e_pos <- .find_cond_configs(charge$C[[k]], e_pos_charge_k)
        idx_to_keep   <- which(charge_k_by_e_pos == e_val)
        charge$C[[k]] <- charge$C[[k]][idx_to_keep]
      }
    }
  }
  return(charge)
}


new_jt <- function(x, evidence = NULL, flow = "sum", validate = TRUE) {
  # x: a charge object returned from compile
  #  - a list with the charge and the cliques
  
  charge  <- x$charge
  cliques <- x$cliques

  if (!is.null(evidence)) charge <- set_evidence_jt(charge, cliques, evidence)
  
  schedule  <- new_schedule(cliques)
  jt        <- list(
    schedule    = schedule[1:2], # collect and distribute
    charge      = charge,
    cliques     = cliques,
    clique_graph = schedule$clique_graph
  )
  
  class(jt)             <- c("jt", class(jt))
  attr(jt, "direction") <- "collect" # collect, distribute or full
  attr(jt, "flow")      <- flow
  if (flow == "max") {
    # most probable explanation
    all_vars <- unique(unlist(cliques))
    attr(jt, "mpe") <- structure(
      vector("character", length = length(all_vars)), names = all_vars
    )
  }
  return(jt)
}

.get_max_info <- function(pot) {
  # Helper function to locate the maximum configuration
  max_idx    <- which.max(pot)
  max_config <- pot[max_idx]
  max_vars   <- attr(max_config, "vars")
  max_vals   <- .split_chars(names(max_config))
  return(structure(max_vals, names = max_vars, max_idx = max_idx))
}


send_messages <- function(jt, flow = "sum") {

  # TODO: wrap jt in an environment and make more small helper functions

  direction <- attr(jt, "direction")
  x   <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  lvs <- attr(x$tree, "leaves")
  par <- attr(x$tree, "parents")

  for (k in seq_along(lvs)) {

    lvs_k <- lvs[k]
    par_k <- par[[k]]
    
    # Skip if the leave has no parents (can occur in distribute)    
    # if (!neq_empt_int(par_k)) next
    
    for (pk in par_k) {
      
      C_lvs_k <- x$cliques[[lvs_k]]
      C_par_k <- x$cliques[[pk]]
      Sk      <- intersect(C_lvs_k, C_par_k)
      C_lvs_k_name <- names(x$cliques)[lvs_k]
      C_par_k_name <- names(x$cliques)[pk]

      if (neq_empt_chr(Sk)) { # if empty, no messages should be sent unless flow = max, then we bookkeep the max config

        message_k_names <- setdiff(C_lvs_k, Sk)

        if (direction == "collect") {

          message_k <- marginalize(jt$charge$C[[C_lvs_k_name]], message_k_names, attr(jt, "flow"))

          # e1 <- as_env.sptable(jt$charge$C[[C_lvs_k_name]])
          # message_k <- marginalize.sptable_env(e1, message_k_names, attr(jt, "flow"))
          # e2 <- as_env.sptable(message_k)
          # hh <- merge.sptable_env(e1, e2, "*", validate = FALSE)
          
          jt$charge$C[[C_par_k_name]] <- merge(jt$charge$C[[C_par_k_name]], message_k, "*", validate = FALSE)
          jt$charge$C[[C_lvs_k_name]] <- merge(jt$charge$C[[C_lvs_k_name]], message_k, "/", validate = FALSE)
        }

        if (direction == "distribute") {

          if (attr(jt, "flow") == "max") {
            ## The child:
            max_idx <- which.max(jt$charge$C[[C_lvs_k_name]])
            # Here we must change the leave potential and send this new message to the parent
            # - and so we can't use .get_max_info
            jt$charge$C[[C_lvs_k_name]] <- jt$charge$C[[C_lvs_k_name]][max_idx]
            max_vars <- attr(jt$charge$C[[C_lvs_k_name]], "vars")
            max_vals <- .split_chars(names(jt$charge$C[[C_lvs_k_name]]))
            attr(jt, "mpe")[max_vars] <- max_vals
          }

          message_k <- marginalize(jt$charge$C[[C_lvs_k_name]], message_k_names, attr(jt, "flow"))
          jt$charge$C[[C_par_k_name]] <- merge(jt$charge$C[[C_par_k_name]], message_k, "*", validate = FALSE)

          if (attr(jt, "flow") == "max") {
            ## The parent:
            max_info_par <- .get_max_info(jt$charge$C[[C_par_k_name]])
            attr(jt, "mpe")[names(max_info_par)] <- unname(max_info_par)
          }
          
          S_k_name <- paste("S", pk, sep = "") # str_rem(C_par_k_name, 1L)
          jt$charge$S[[S_k_name]] <- message_k
        }
        
      } else if (direction == "distribute") { # This else if clause, ensures that parents with no leaves are taken care of
        # TODO: double "direction == 'distribute'" ? FIX: put the if (attr(jt, ..)) inside the above if clause!
        if (attr(jt, "flow") == "max") {
          ## The parent:
          max_info_par <- .get_max_info(jt$charge$C[[C_par_k_name]])
          attr(jt, "mpe")[names(max_info_par)] <- unname(max_info_par)

          ## The child:
          max_info_lvs <- .get_max_info(jt$charge$C[[C_lvs_k_name]])
          attr(jt, "mpe")[names(max_info_lvs)] <- unname(max_info_lvs)
        }
      }      
    }
  }
  prune_jt(jt)
}


