library(igraph)

munin <- readRDS("../inst/extdata/munin.rds")

cpts <- lapply(munin[1:35], function(x) {
  arr <- as(x$prob, "array")
  if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
    dimnames(arr) <- structure(dimnames(arr), names = x$node)
  }
  arr
}) 

cpts <- dimnames_to_single_chars(cpts)

cl   <- cpt_list(cpts)
cp   <- compile(cl)
j    <- jt(cp, propagate = TRUE)
plot(j, vertex.size = 0.1, vertex.label = NA)

j <- send_messages(j); plot(j)
