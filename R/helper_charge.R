extract_or_make_cpt <- function(x, child, parents) {
  # x: data.frame or cpt_list
  if (inherits(x, "data.frame")) {
    spt  <- sptable(as.matrix(x[, c(child, parents), drop = FALSE]))
    return(as_parray(spt, parents))
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
        potC$C[[k]] <- cpt
      } else {
        potC$C[[k]] <- merge(potC$C[[k]], cpt)
      }
      break # Must only live in one clique
    }
  }
}

make_unity_sptable_from_cpt_list <- function(x, k) {
  # x: cpt_list
  # TODO: dont input x! Just input x_k and dim_names!
  vars <- attr(x[[k]], "vars")
  lvls <- attr(x, "dim_names")[vars]
  # lvls_comb <- expand.grid(lvls, stringsAsFactors = FALSE)
  # .names <- apply(lvls_comb, 1L, paste, collapse = "")
  spt <- numeric(0) # structure(rep(1, length(.names)), names = .names)
  attr(spt, "vars") <- vars
  attr(spt, "lvls") <- lvls
  class(spt) <- c("unity_table","sptable", class(spt)) # c("sptable", class(spt))
  spt
}

make_unity_sptable <- function(potC, k, x, clique) {
  if (inherits(x, "data.frame")) {

    ## TODO: All this is redundant, if we just know the levels!!!!
    sptk <- sptable(as.matrix(x[, clique, drop = FALSE]))
    sptk[1:length(sptk)] <- 1L # TODO: Change to numeric(0L)
    potC$C[[k]] <- sptk    
  } else {        
    potC$C[[k]] <- make_unity_sptable_from_cpt_list(x, k)
  }
}


new_charge <- function(x, cliques, parents) {
  # x: data.frame or cpt_list
  potC <- new.env()
  potC[["C"]] <- vector("list", length(cliques))

  children <- names(parents)

  for (child in children) {
    .parents <- parents[[child]]
    allocate_child_to_potential(potC, x, cliques, child, .parents)
  }

  # Some clique potentials may be empty due to triangulation
  # We set these to the identity = 1 for all configurations
  is_null <- .map_lgl(potC[["C"]], is.null)

  if (any(is_null)) {
    which_is_null <- which(is_null)
    for (k in which_is_null) {
      make_unity_sptable(potC, k, x, cliques[[k]])
    }
  }

  potS <- vector("list", length(cliques))
  names(potS) <- paste("S", 1:length(potS), sep = "")
  names(potC$C) <- names(cliques)
  pots <- list(C = potC$C, S = potS)
  return(pots)
}
