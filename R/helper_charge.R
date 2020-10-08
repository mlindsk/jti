extract_or_make_cpt <- function(x, child, parents) {
  # x: data.frame or cpt_list
  if (inherits(x, "data.frame")) {
    spt <- sparta::as_sparta(x[, c(child, parents), drop = FALSE])
    return(sparta::as_cpt(spt, parents))
  } else {
    return(x[[child]])
  }
}

allocate_child_to_potential <- function(potC, x, cliques, child, parents) {
  # potC: environment with clique potentials
  cpt <- extract_or_make_cpt(x, child, parents)
  for (k in seq_along(cliques)) {
    family_in_Ck <- all(c(child, parents) %in% cliques[[k]])
    if (family_in_Ck) {
      if (is.null(potC$C[[k]])) {
        unity <- sparta::sparta_unity_struct(attr(x, "dim_names")[cliques[[k]]])
        potC$C[[k]] <- sparta::mult(cpt, unity)
      } else {
        potC$C[[k]] <- sparta::mult(potC$C[[k]], cpt)
      }
      break # Must only live in one clique
    }
  }
  NULL
}


new_charge <- function(x, cliques, parents) {
  # x: data.frame or cpt_list
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
