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
  lookup <- mapply(match, e, dim_names[names(e)])
  if (anyNA(lookup)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

has_root_node <- function(x) UseMethod("has_root_node")

has_root_node.jt <- function(x) attr(x, "root_node") != ""

new_schedule <- function(cliques_chr, cliques_int, root_node) {
  
  # mcs promise that the root_node lives in clique one
  jrn <- if (root_node != "") 1L else 0L
  
  rjt <- rooted_junction_tree(cliques_int, jrn)

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
      jt$schedule$collect    <- "full"
      attr(jt, "direction")  <- "distribute"

      # Normalize clique_root
      cr <- attr(jt, "clique_root")
      attr(jt, "probability_of_evidence") <- sum(sparta::vals(jt$charge$C[[cr]]))
      jt$charge$C[[cr]] <- sparta::normalize(jt$charge$C[[cr]])

    } else {
      jt$schedule$distribute <- "full"
      attr(jt, "direction")  <- "full"
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


# set_evidence_jt <- function(charge, cliques, evidence) {
#   for (k in seq_along(charge$C)) {
#     Ck <- names(charge$C[[k]])
#     if (inherits(Ck, "sparta_unity")) next
#     es_in_ck <- which(names(evidence) %in% Ck)
#     for (i in es_in_ck) {
#       e     <- evidence[i]
#       e_var <- names(e)
#       e_val <- unname(e)
#       if (e_var %in% Ck) {
#         m <- try(sparta::slice(charge$C[[k]], e), silent = TRUE)
#         if (inherits(m, "try-error")) {
#           stop(
#             "inconsistent evidence",
#             call. = FALSE
#           )
#         }
#         charge$C[[k]] <- m
#       }
#     }
#   }
#   return(charge)
# }


set_evidence <- function(x, cliques, evidence) {
  # x: list of (sparse) potentials
  for (k in seq_along(x)) {
    Ck <- names(x[[k]])
    if (inherits(x[[k]], "sparta_unity")) next
    es_in_ck <- which(names(evidence) %in% Ck)
    for (i in es_in_ck) {
      e     <- evidence[i]
      e_var <- names(e)
      e_val <- unname(e)
      if (e_var %in% Ck) {
        m <- try(sparta::slice(x[[k]], e), silent = TRUE)
        if (inherits(m, "try-error")) {
          stop(
            "inconsistent evidence",
            call. = FALSE
          )
        }
        x[[k]] <- m
      }
    }
  }
  return(x)
}



new_jt <- function(x, evidence = NULL, flow = "sum") {
  # x: a charge object returned from compile
  #  - a list with the charge and the cliques

  charge  <- x$charge
  cliques <- x$cliques

  if (!is.null(evidence)) charge$C <- set_evidence(charge$C, cliques, evidence)

  schedule  <- new_schedule(cliques, attr(x, "cliques_int"), attr(x, "root_node"))
  attr(x, "cliques_int") <- NULL

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
  attr(jt, "evidence")    <- attr(x, "evidence") # The aggregated evidence
  
  if (flow == "max") {
    # most probable explanation
    all_vars <- names(attr(x, "dim_names"))
    attr(jt, "mpe") <- structure(
      vector("character", length = length(all_vars)),
      names = all_vars
    )
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

      pot_lvs_k <- jt$charge$C[[C_lvs_k_name]]
      pot_par_k <- jt$charge$C[[C_par_k_name]]
      message_k_names <- setdiff(names(pot_lvs_k), C_par_k)
      
      if (direction == "collect") {

        par_is_unity <- inherits(pot_par_k, "sparta_unity")
        message_k <- sparta::marg(pot_lvs_k, message_k_names, attr(jt, "flow"))

        jt$charge$C[[C_par_k_name]] <- if (par_is_unity) message_k else sparta::mult(pot_par_k, message_k)
        jt$charge$C[[C_lvs_k_name]] <- sparta::div(pot_lvs_k, message_k)
        
      }

      if (direction == "distribute") {
        if (attr(jt, "flow") == "max") {

          # Find the max cell and change the potential
          # before sending the information:
          max_idx  <- sparta::which_max_idx(pot_lvs_k)
          max_cell <- sparta::which_max_cell(pot_lvs_k)
          max_mat  <- jt$charge$C[[C_lvs_k_name]][, max_idx, drop = FALSE]
          max_val  <- attr(pot_lvs_k, "vals")[max_idx]
          max_dn   <- attr(pot_lvs_k, "dim_names")

          jt$charge$C[[C_lvs_k_name]] <- sparta::sparta_struct(
            max_mat,
            max_val,
            max_dn
          )
          attr(jt, "mpe")[names(max_cell)] <- max_cell
        }

        # Send the message
        message_k <- sparta::marg(pot_lvs_k, message_k_names, attr(jt, "flow"))
        jt$charge$C[[C_par_k_name]] <- sparta::mult(pot_par_k, message_k)
        jt$charge$S[[paste("S", pk, sep = "")]] <- message_k
        
        if (attr(jt, "flow") == "max") {
          # Record the max cell for the parent potential
          max_cell <- sparta::which_max_cell(pot_par_k)
          attr(jt, "mpe")[names(max_cell)] <- max_cell
        }
      }
    }
  }
  prune_jt(jt)
}
