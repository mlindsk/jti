## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------

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

merge_unity_sptable <- function(x, y, op = "*") {

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

#' Sparse unity table
#' 
#' @param x Named list with the levels of the corresponding variables
#' @export
make_unity_sptable <- function(x) {
  spt <- new.env()
  attr(spt, "vars") <- names(x)
  attr(spt, "lvls") <- x
  class(spt) <- c("unity_sptable","sptable", class(spt))
  spt
}

