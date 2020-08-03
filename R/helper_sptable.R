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
  # x  : sptable / sptable
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

merge_two_unities <- function(x, y) {

  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep_var <- intersect(vx, vy)

  if (length(sep_var) == length(vx)) {
    # If they are identical

    new_lvls <- mapply(function(a, b) {
      if (length(a) <= length(b)) return(a) else return(b)
    }, attr(x, "lvls"), attr(y, "lvls"), SIMPLIFY = FALSE)
    
    spt <- deep_copy(x)
    attr(spt, "lvls") <- new_lvls
    return(spt)
  }

  if (!neq_empt_chr(sep_var)) {
    # If no vars in commong
    spt <- deep_copy(x)
    attr(spt, "vars") <- c(attr(spt, "vars"), attr(y, "vars"))
    attr(spt, "lvls") <- c(attr(spt, "lvls"), attr(y, "lvls"))
    return(spt)
  }
  
  sep_lvl_x   <- attr(x, "lvls")[sep_var]
  sep_lvl_y   <- attr(y, "lvls")[sep_var]

  sep_lvl_new <- mapply(function(a, b) {
    if (length(a) <= length(b)) return(a) else return(b)
  }, sep_lvl_x, sep_lvl_y, SIMPLIFY = FALSE)

  spt <- deep_copy(x)

  attr(spt, "lvls")[sep_var] <- sep_lvl_new

  res_var <- setdiff(vy, vx)
  res_lvl <- attr(y, "lvls")[res_var]

  for (k in 1:length(res_lvl)) {
    attr(spt, "lvls") <- push(attr(spt, "lvls"), res_lvl[[k]], names(res_lvl)[k])
  }
  attr(spt, "vars") <- c(attr(spt, "vars"), res_var)
  
  return(spt)
}

merge_unity_sptable <- function(x, y, op = "*", validate = TRUE, ...) {

  stopifnot(op %in% c("*", "/", "+", "-"))

  if (inherits(x, "unity_sptable") && inherits(y, "unity_sptable")) {
    return(merge_two_unities(x, y))
    print("Test")
  }

  if (inherits(x, "unity_sptable")) {
    # assumming that x is sptable and y is unity_sptable
    tmp <- x
    x <- y
    y <- tmp
  }
  
  vx  <- attr(x, "vars")
  vy  <- attr(y, "vars")
  sep <- intersect(vx, vy)

  posx <- match(sep, vx)

  # The if statement ensures correctness even if no vars in common
  cfx <- if (neq_empt_int(posx)) .find_cond_configs(x, posx) else x
  y_res_lvl  <- if (neq_empt_int(posx)) attr(y, "lvls")[-which(vy %in% sep)] else attr(y, "lvls")
  y_res_comb <- expand.grid(y_res_lvl, stringsAsFactors = FALSE)
  y_res_comb <- apply(y_res_comb, 1L, paste0, collapse = "")

  spt <- list2env(as.list(unlist(lapply(names(cfx), function(string) {
    structure(rep(x[string, "value"], length(y_res_comb)), names = paste(string, y_res_comb, sep = ""))
  }))))
  
  attr(spt, "vars") <- c(vx, setdiff(vy, vx))
  class(spt) <- c("sptable", "environment")
  return(spt)
}


## ---------------------------------------------------------
##                  EXPORTED HELPERS
## ---------------------------------------------------------

#' @export
make_unity_sptable <- function(vars, lvls) {
  # Just input lvls? If vars is the names of lvls anyway!
  stopifnot(setequal(vars, names(lvls)))
  spt <- new.env()
  attr(spt, "vars") <- vars
  attr(spt, "lvls") <- lvls
  class(spt) <- c("unity_sptable","sptable", class(spt))
  spt
}

