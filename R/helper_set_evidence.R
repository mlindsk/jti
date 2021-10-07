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
        m <- sparta::sparta_unity_struct(dim_names(x[[k]])[es_in_ck])
        inc$inc <- TRUE
      }
      x[[k]] <- m      
    }
  }
  return(x)
}

set_evidence_cpt <- function(x, evidence, inc, eps_smooth = 0.1) {
  # x: a cpt_list object
  for (k in seq_along(x)) {

    child   <- names(x)[k]
    parents <- attr(x, "parents")[[child]]
    family  <- c(child, parents)

    if (inherits(x[[k]], "sparta_unity")) next

    es_in_child   <- which(names(evidence) %in% child)
    es_in_parents <- which(names(evidence) %in% parents)
    es_in_family  <- c(es_in_child, es_in_parents) 
    e             <- evidence[es_in_family]
    
    if (neq_empt_int(es_in_family)) {
      
      # parent evidence
      if (neq_empt_int(es_in_parents)) {
        m <- try(sparta::slice(x[[k]], e[es_in_parents], drop = TRUE), silent = TRUE)  # possibly a sparta_unity
        if (inherits(m, "try-error")) {
          inc$inc <- TRUE
          x[[k]] <- sparta::sparta_unity_struct(
            sparta::dim_names(x[[k]])[setdiff(names(x[[k]]), names(e[es_in_parents]))],
            rank = 1/length(sparta::dim_names(x[[k]])[[child]])
          )
          next
        }
        x[[k]] <- m
      }

      # child evidence
      if (neq_empt_int(es_in_child)) {
        m <- if (length(names(x[[k]])) > 1) {
          try(sparta::slice(x[[k]], e[es_in_child], drop = TRUE), silent = TRUE)
        } else {
          try(sparta::slice(x[[k]], e[es_in_child], drop = FALSE), silent = TRUE)
        }

        # epsilon-smoothing
        if (inherits(m, "try-error")) {
          inc$inc <- TRUE
          new_names <- setdiff(names(x[[k]]), names(e[es_in_child]))
          x[[k]] <- sparta::sparta_unity_struct(
            sparta::dim_names(x[[k]])[new_names],
            eps_smooth
          )
          next
        }
        x[[k]] <- m
      }
      
    } # end es_in_family
  } # end for loop
  
  return(x)
}
