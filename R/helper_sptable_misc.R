## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
deep_copy <- function(x) {
  new <- list2env(as.list(x))
  new_struct <- structure(new, class = class(x), vars = attr(x, "vars"))
  if (inherits(x, "unity_sptable")) attr(new_struct, "lvls") <- attr(x, "lvls")
  return(new_struct)
}

.find_cond_configs <- function(x, pos) {
  # x  : sptable
  # pos: the position of the conditional variables

  # TODO: Should we test for variable names containing "@"?
  skeleton <- paste(rep("@", nchar(names(x)[1])))
  .map_chr(names(x), function(s) {
    sk <- skeleton
    s_pos_val <- .map_chr(pos, function(l) substr(s, l, l))
    sk[pos] <- s_pos_val
    paste(gsub("@", "", sk), collapse = "")
  })
}

.reposition_names <- function(x, pos) {
  # x : named list / sptable

  # TODO: This is just 'str_rem' in disguise? Fix!
  # - or is it 'str_extract' ?
  structure(x, names =.map_chr(names(x), function(y) {
    paste(.split_chars(y)[pos], collapse = "")
  }))
}

.set_as_sptable <- function(x) {
  structure(x, class = c("sptable", class(x)))
}


.allowed_cpt_classes <- function() {
  c(.map_chr(utils::methods("as_sptable"), function(generic) sub("as_sptable.", "", generic)), "sptable")  
}

