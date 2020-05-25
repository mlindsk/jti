make_unity_sptable_from_cpt_list <- function(x, k) {
  # x: cpt_list
  vars <- attr(x[[k]], "vars")
  lvls <- attr(x, "dim_names")[vars]
  lvls_comb <- expand.grid(lvls, stringsAsFactors = FALSE)
  .names <- apply(lvls_comb, 1L, paste, collapse = "")
  spt <- structure(rep(1, length(.names)), names = .names)
  attr(spt, "vars") <- vars
  class(spt) <- c("sptable", class(spt))
  spt
}


extract_or_make_probability_table <- function(x, child, parents) {
  # x: data.frame or cpt_list
  if (inherits(x, "data.frame")) {
    spt  <- sptable(as.matrix(x[, c(child, parents), drop = FALSE]))
    return(parray(spt, parents))
  } else {
    return(x[[child]])
  }
}

allocate_child_to_potential <- function(potC, x, cliques, child, parents) {
  pspt <- extract_or_make_probability_table(x, child, parents)
  for (k in seq_along(cliques)) {
    family_in_Ck <- all(c(child, parents) %in% cliques[[k]])
    if (family_in_Ck) {
      if (is.null(potC$C[[k]])) {
        potC$C[[k]] <- pspt
      } else {
        potC$C[[k]] <- merge(potC$C[[k]], pspt)
      }
      break # Must only live in one clique
    }
  }
}

make_unity_sptable <- function(potC, k, x, cliques) {
  if (inherits(x, "data.frame")) {
    sptk <- sptable(as.matrix(x[, cliques[[k]], drop = FALSE]))
    sptk[1:length(sptk)] <- 1L
    potC$C[[k]] <- sptk    
  } else {        
    potC$C[[k]] <- make_unity_sptable_from_cpt_list(x, k)
  }
}


new_charge <- function(x, cliques, parents) {

  potC <- new.env()
  potC[["C"]] <- vector("list", length(cliques))

  children <- names(parents)

  for (child in children) {
    .parents <- parents[[child]]
    allocate_child_to_potential(potC, x, cliques, child, .parents)
  }

  # Some clique potentials may be empty due to triangulation
  # We set these to the identity = 1 for all configurations
  which_is_null <- .map_lgl(potC[["C"]], is.null)
  if (any(which_is_null)) {
    for (k in which(which_is_null)) {
      make_unity_sptable(potC, k, x, cliques)
    }
  }

  potS <- vector("list", length(cliques))
  names(potS) <- paste("S", 1:length(potS), sep = "")
  names(potC$C) <- names(cliques)
  pots <- list(C = potC$C, S = potS)
  return(pots)
}
