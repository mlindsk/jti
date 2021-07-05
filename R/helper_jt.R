check_query <- function(prop, type, inconsistent, flow, nodes, evidence, clique_root) {
  if (prop == "no") {
    stop("It is not possible to query from a junction tree that ",
      "hasn't been propagated.", call. = FALSE
    )
  }
  
  if (type %ni% c("marginal", "joint")) {
    stop("Type must be 'marginal' or 'joint'.", call. = FALSE)
  }

  if (type == "joint" && inconsistent) {
    stop(
      "It is not possible to make joint queries when ",
      "there is inconsistent evidence", call. = FALSE
    )
  }
  
  if (flow == "max") {
    stop(
      "It does not make sense to query probablities from a junction",
      "tree with flow = 'max'. ",
      "Use 'mpe' to obtain the max configuration.", call. = FALSE
    )
  }

  if (any(nodes %in% evidence)) {
    stop("It is not possible to query probabilities from",
      "evidence nodes", call. = FALSE)
  }
  
  if (prop == "collect") if (!all(nodes %in% clique_root)) {
    stop(
      "All nodes must be in the root clique",
      "since the junction tree has only collected! ",
      "See get_clique_root(x) to find the nodes in the root clique.",
      call. = FALSE
    )
  }    
}

# note: Rows only have a single 1 in collect
#       this indicates the parent. A column with all
#       zeroes indicates a leave node.

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

valid_evidence <- function(dim_names, e) {
  nemvc <- neq_empt_vector_chr(e)
  e_conforms_with_dim_names <- !anyNA(mapply(match, e, dim_names[names(e)]))
  if (e_conforms_with_dim_names && nemvc) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

has_root_node_jt <- function(x) attr(x, "root_node") != ""

new_schedule <- function(cliques_chr, cliques_int, root_node, joint_vars = NULL) {

  clique_root_int <- if (!is.null(joint_vars)) {
    as.integer(unname(which(.map_lgl(cliques_chr, function(x) all(joint_vars %in% x)))))
  } else {
    if (root_node != "") {
      idx <- which(.map_lgl(cliques_chr, function(z) root_node %in% z))
      idx[which.min(.map_int(cliques_chr[idx], length))]  
    } else {
      0L
    }
  }
  
  rjt <- rooted_junction_tree(cliques_int, clique_root_int)

  coll_tree    <- rjt$collect
  dist_tree    <- rjt$distribute
  clique_root  <- rjt$clique_root
  clique_graph <- coll_tree + dist_tree


  coll_lvs <- leaves_jt(coll_tree)
  dist_lvs <- leaves_jt(dist_tree)

  attr(coll_tree, "leaves")  <- coll_lvs
  attr(dist_tree, "leaves")  <- dist_lvs

  attr(coll_tree, "parents") <- parents_jt(coll_tree, coll_lvs)
  attr(dist_tree, "parents") <- parents_jt(dist_tree, dist_lvs)

  collect    <- list(cliques = cliques_chr, tree = coll_tree)
  distribute <- list(cliques = cliques_chr, tree = dist_tree)
  
  return(
    list(
      collect      = collect ,
      distribute   = distribute,
      clique_graph = clique_graph,
      clique_root  = paste("C", clique_root, sep = "") # TODO: Just return the index
    )
  )
}

prune_jt <- function(jt) {

  direction <- attr(jt, "direction")
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute

  if (identical(x, "full")) {
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
      attr(jt, "direction")  <- "distribute"
      attr(jt, "propagated") <- "collect"
      jt$schedule$collect    <- "full"
      cr <- attr(jt, "clique_root")
      attr(jt, "probability_of_evidence") <- sum(jt$charge$C[[cr]])
      jt$charge$C[[cr]] <- sparta::normalize(jt$charge$C[[cr]])
      
    } else {
      jt$schedule$distribute <- "full"
      attr(jt, "direction")  <- "full"
      attr(jt, "propagated") <- "distribute"
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

set_evidence_ <- function(x, evidence, inc) {
  # x: list of (sparse) tables
  for (k in seq_along(x)) {
    pot_k <- names(x[[k]])
    if (inherits(x[[k]], "sparta_unity")) next
    es_in_ck <- which(names(evidence) %in% pot_k)

    if (neq_empt_int(es_in_ck)) {
      e        <- evidence[es_in_ck]
      conform  <- length(pot_k) > length(e)
      m <- if (conform) {
        try(sparta::slice(x[[k]], e, drop = TRUE), silent = TRUE)  # possibly a sparta_unity
      } else {
        try(sparta::slice(x[[k]], e, drop = FALSE), silent = TRUE)
      }

      if (inherits(m, "try-error")) {
        # Now the best guess is a uniform prior
        m <- sparta::sparta_unity_struct(sparta::dim_names(x[[k]]))
        inc$inc <- TRUE
      }
      x[[k]] <- m      
    }
  }
  return(x)
}


new_jt <- function(x, evidence = NULL, flow = "sum") {
  # x: a charge object returned from compile
  #  - a list with the charge and the cliques

  charge  <- x$charge
  cliques <- x$cliques

  inc <- new.env()
  inc$inc <- attr(x, "inconsistencies")
  if (!is.null(evidence)) charge$C <- set_evidence_(charge$C, evidence, inc)

  schedule  <- new_schedule(cliques, attr(x, "cliques_int"), attr(x, "root_node"), attr(x, "joint_vars"))

  jt <- list(
    schedule = schedule[1:2], # collect and distribute
    charge   = charge,
    cliques  = cliques,
    clique_graph = schedule$clique_graph
  )
  
  class(jt)               <- c("jt", class(jt))
  attr(jt, "direction")   <- "collect" # collect, distribute or full
  attr(jt, "flow")        <- flow
  attr(jt, "root_node")   <- attr(x, "root_node")
  attr(jt, "clique_root") <- schedule$clique_root
  attr(jt, "evidence")    <- attr(x, "evidence")  # The aggregated evidence
  attr(jt, "dim_names")   <- attr(x, "dim_names") # To verify valid evidence in set_evidence.jt
  attr(jt, "inconsistencies") <- inc$inc # Record whether or not the evidence has produced inconsistencies
  
  if (flow == "max") {
    # most probable explanation
    all_vars <- names(attr(x, "dim_names"))
    attr(jt, "mpe") <- structure(
      vector("character", length = length(all_vars)),
      names = all_vars
    )
    if (!is.null(evidence)) attr(jt, "mpe")[names(evidence)] <- evidence
  }
  return(jt)
}


#' Send Messages in a Junction Tree
#'
#' Send messages from the current leaves to the current parents
#' in a junction tree
#' 
#' @param jt A \code{jt} object return from the \code{jt} function
#' @seealso \code{\link{jt}}, \code{\link{get_cliques}}, \code{\link{leaves}},
#' \code{\link{parents}}
#' @examples
#' # See example 6 in the help page for the jt function
#' @export
send_messages <- function(jt) {

  direction <- attr(jt, "direction")
  if (direction == "full") {
    message("The junction tree is already fully propagated. jt is returned")
    return(jt)
  }

  x   <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  lvs <- attr(x$tree, "leaves")
  par <- attr(x$tree, "parents")
  
  for (k in seq_along(lvs)) {

    lvs_k <- lvs[k]
    par_k <- par[[k]]
    
    for (pk in par_k) {

      ## C_lvs_k <- x$cliques[[lvs_k]]
      C_par_k      <- x$cliques[[pk]]
      C_lvs_k_name <- names(x$cliques)[lvs_k]
      C_par_k_name <- names(x$cliques)[pk]
      
      message_k_names <- setdiff(names(jt$charge$C[[C_lvs_k_name]]), C_par_k)

      # Unities can occur both as a product of triangulation but also
      # because of of inconsistent evidence wich are transformed to unities.
      # This means, that we just put a uniform prior on the respective potential.
      par_eq_unity <- inherits(jt$charge$C[[C_par_k_name]], "sparta_unity")
      lvs_eq_unity <- inherits(jt$charge$C[[C_lvs_k_name]], "sparta_unity")
      
      if (direction == "collect") {

        message_k <- sparta::marg(jt$charge$C[[C_lvs_k_name]], message_k_names, attr(jt, "flow"))

        # Update parent potential
        if (!lvs_eq_unity && !par_eq_unity) {
          jt$charge$C[[C_par_k_name]] <- sparta::mult(jt$charge$C[[C_par_k_name]], message_k)
          # If the parent becomes the null-potential we make it a unity afterall
          # This can happen when there is inconsistent evidence or just if data
          # is very sparse
          if (ncol(jt$charge$C[[C_par_k_name]]) < 1) {
            attr(jt, "inconsistencies") <- TRUE
            dn <- sparta::dim_names(jt$charge$C[[C_par_k_name]])
            jt$charge$C[[C_par_k_name]] <- sparta::sparta_unity_struct(dn, rank = 1)
          }
          
        } else if (!lvs_eq_unity && par_eq_unity) {
          # Note here, that the new potential is possibly not defined over all variables in the clique
          # We must take this into account wheen we query variables that are no longer in the potential
          if (is_scalar(message_k)) {
            # Can be a scalar because of evidence reduction.
            jt$charge$C[[C_par_k_name]] <- sparta::mult(jt$charge$C[[C_par_k_name]], message_k)
          } else {
            jt$charge$C[[C_par_k_name]] <- message_k
          }
          # in the last cases, just leave the parent potential as is since we
          # only multiply with ones. This will result in "wrong" factors; but it
          # can only occur if there is inconsistent evidence, and here we have
          # already taken this into account by saying that prob(evidence) is no longer valid.
        }

        # Update leave potential
        jt$charge$C[[C_lvs_k_name]] <- sparta::div(jt$charge$C[[C_lvs_k_name]], message_k)
        
      }

      if (direction == "distribute") {

        if (attr(jt, "flow") == "max") {
          # Find the max cell and change the potential
          # before sending the information:

          max_cell <- sparta::which_max_cell(jt$charge$C[[C_lvs_k_name]])
          max_dn   <- sparta::dim_names(jt$charge$C[[C_lvs_k_name]])

          jt$charge$C[[C_lvs_k_name]] <- if (!inherits(jt$charge$C[[C_lvs_k_name]], "sparta_unity")) {
            max_idx  <- sparta::which_max_idx(jt$charge$C[[C_lvs_k_name]])
            max_mat  <- jt$charge$C[[C_lvs_k_name]][, max_idx, drop = FALSE]
            max_val  <- attr(jt$charge$C[[C_lvs_k_name]], "vals")[max_idx]
            sparta::sparta_struct(
              max_mat,
              max_val,
              max_dn
            )
          } else {
            lvs_eq_unity <- FALSE # no longer a unity
            sparta::sparta_struct(
              matrix(1L, nrow = length(max_cell), ncol = 1),
              sparta::sparta_rank(jt$charge$C[[C_lvs_k_name]]),
              max_dn
            )
          }

          attr(jt, "mpe")[names(max_cell)] <- max_cell
        }

        # Send the message (can be scalar if lvs_var = sep_var)
        message_k <- sparta::marg(jt$charge$C[[C_lvs_k_name]], message_k_names, attr(jt, "flow"))

        # Update parent potential
        if (!lvs_eq_unity && !par_eq_unity) {
          jt$charge$C[[C_par_k_name]] <- sparta::mult(jt$charge$C[[C_par_k_name]], message_k)
          if (ncol(jt$charge$C[[C_par_k_name]]) < 1) {
            attr(jt, "inconsistencies") <- TRUE
            dn <- sparta::dim_names(jt$charge$C[[C_par_k_name]])
            jt$charge$C[[C_par_k_name]] <- sparta::sparta_unity_struct(dn, rank = 1)
          }
        } else if (!lvs_eq_unity && par_eq_unity) {
          if (is_scalar(message_k)) {
            jt$charge$C[[C_par_k_name]] <- sparta::mult(jt$charge$C[[C_par_k_name]], message_k)
          } else {
            jt$charge$C[[C_par_k_name]] <- message_k  
          }
        }         
        jt$charge$S[[paste("S", pk, sep = "")]] <- message_k
        
        if (attr(jt, "flow") == "max") {
          # Record the max cell for the parent potential
          max_cell <- sparta::which_max_cell(jt$charge$C[[C_par_k_name]])
          attr(jt, "mpe")[names(max_cell)] <- max_cell
        }
      }
    }
  }
  prune_jt(jt)
}
