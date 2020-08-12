munin <- readRDS("../inst/extdata/munin.rds")

munin <- lapply(munin, function(x) {
  arr <- as(x$prob, "array")
  if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
    dimnames(arr) <- structure(dimnames(arr), names = x$node)
  }
  arr
}) 

usethis::use_data(munin)
