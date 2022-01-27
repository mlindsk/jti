allocate_child_to_potential <- function(potC, x, cliques, child, parents) {
  # potC: environment with clique potentials
  cpt <- x[[child]] # extract_or_make_cpt(x, child, parents)
  for (k in seq_along(cliques)) {
    family_in_Ck <- all(c(child, parents) %in% cliques[[k]])
    if (family_in_Ck) {
      if (is.null(potC$C[[k]])) {
        potC$C[[k]] <- cpt
      } else {
        # Must take into account that CPTs may have turned into unities by eps-smoothing
        if (inherits(cpt, "sparta_unity")) {
          potC$C[[k]] <- sparta::mult(potC$C[[k]], sparta::sparta_rank(cpt))
        } else if (inherits(potC$C[[k]], "sparta_unity")) {
          potC$C[[k]] <- sparta::mult(cpt, sparta::sparta_rank(potC$C[[k]]))
        } else {
          potC$C[[k]] <- sparta::mult(potC$C[[k]], cpt)          
        }
      }
      break # Must only live in one clique
    }
  }
  NULL
}

new_charge <- function(x, cliques, parents) {
  potC <- new.env()
  potC[["C"]] <- vector("list", length(cliques))

  children <- names(parents)
  for (child in children) {
    allocate_child_to_potential(potC, x, cliques, child, parents[[child]])
  }

  # Some clique potentials may be empty due to triangulation
  # We set these as the identity = 1 for all configurations
  is_null <- .map_lgl(potC[["C"]], is.null)
  
  if (any(is_null)) {
    which_is_null <- which(is_null)
    for (k in which_is_null) {
      pck <- sparta::sparta_unity_struct(attr(x, "dim_names")[cliques[[k]]])
      potC$C[[k]] <- pck
    }
  }

  names_potS <- paste("S", 1:length(cliques), sep = "")
  potS <- structure(vector("list", length(cliques)), names = names_potS)
  names(potC$C) <- names(cliques)
  pots <- list(C = potC$C, S = potS)
  return(pots)
}

# new_charge_pot <- function(x) {
#   attributes(x) <- NULL
#   names(x) <- paste("C", 1:length(x), sep = "")
#   names_potS <- paste("S", 1:length(x), sep = "")
#   potS <- structure(vector("list", length(x)), names = names_potS)
#   pots <- list(C = x, S = potS)
#   return(pots)
# }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Experimental
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# allocate_to_minimal_clique_cover <- function(potC, x, minimal_cover) {
#   # potC: environment with clique potentials
#   for (k in seq_along(minimal_cover)) {
#     clique_name <- names(minimal_cover[k])
#     min_cov_k   <- minimal_cover[[k]]
#     potC$C[[clique_name]]  <- if (length(min_cov_k) > 1) {
#       Reduce(sparta::mult, x[min_cov_k])
#     } else {
#       x[[min_cov_k]]
#     }
#   }
# }


# new_charge_cpt2 <- function(x, cliques, parents) {
#   potC          <- new.env()
#   potC[["C"]]   <- vector("list", length(cliques))
#   names(potC$C) <- names(cliques)

#   W <- lapply(x, names)
  
#   covered <- vector(length = length(W))
  
#   clique_cover <- lapply(cliques, function(clique) {
#     unname(which(.map_lgl(W, function(w) {
#       all(w %in% clique)
#     })))
#   })

#   minimal_cover <- vector("list")

#   while (!all(covered)) {
#     lens          <- .map_int(clique_cover, length)
#     max_lens      <- which.max(lens)
#     idx           <- clique_cover[[max_lens]]
#     covered[idx]  <- TRUE
#     max_clique    <- clique_cover[max_lens]
#     minimal_cover <- push(minimal_cover, max_clique[[1]], names(max_clique))
#     clique_cover[max_lens] <- NULL 
#   }
    
#   allocate_to_minimal_clique_cover(potC, x, minimal_cover)

#   # Some clique potentials may be empty due to triangulation
#   # We set these as the identity = 1 for all configurations
#   is_null <- .map_lgl(potC[["C"]], is.null)
  
#   if (any(is_null)) {
#     which_is_null <- which(is_null)
#     for (k in which_is_null) {
#       pck <- sparta::sparta_unity_struct(attr(x, "dim_names")[cliques[[k]]])
#       potC$C[[k]] <- pck
#     }
#   }

#   names_potS <- paste("S", 1:length(cliques), sep = "")
#   potS <- structure(vector("list", length(cliques)), names = names_potS)
#   pots <- list(C = potC$C, S = potS)
#   return(pots)
# }
