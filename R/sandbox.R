library(igraph)

munin <- readRDS("../inst/extdata/munin.rds")

cpts <- lapply(munin[1:50], function(x) {
  arr <- as(x$prob, "array")
  if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
    dimnames(arr) <- structure(dimnames(arr), names = x$node)
  }
  arr
}) 

cpts <- dimnames_to_single_chars(cpts)
cl   <- cpt_list(cpts)
cp   <- compile(cl)
j    <- jt(cp, propagate = FALSE)
plot(j, vertex.size = 0.1, vertex.label = NA)

j <- send_messages(j); plot(j)





## ---------------------------------------------------------
##                   TRYOUTS
## ---------------------------------------------------------
# NOTE: they dont have reference semantics. This is important!
# - Should they have that?

e1 <- as_env.sptable(cl[[1]])
e2 <- as_env.sptable(cl[[10]])
merge.sptable_env(e1, e2)

as_parray(e2, "DIFFN_PATHO")
marginalize(e2, "DIFFN_PATHO", "sum")

## x <- cpts[[10]]
## microbenchmark::microbenchmark(
##   as_sptable_env.array(x, validate = FALSE),
##   as_sptable.array(x, validate = FALSE)  
## )

## e <- new.env()
## class(e) <- c(class(e), "A")
## e$a <- 1L

## f(e)

## f <- function(e) {
##   old_class <- class(e)
##   class(e) <- "environment"
##   e$a <- e$a + 1L
##   class(e) <- old_class
##   NULL
## }
