## library(igraph)

## munin <- readRDS("../inst/extdata/munin.rds")

## cpts <- lapply(munin[1:25], function(x) {
##   arr <- as(x$prob, "array")
##   if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
##     dimnames(arr) <- structure(dimnames(arr), names = x$node)
##   }
##   arr
## }) 

## cpts <- dimnames_to_single_chars(cpts)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl)

## j    <- jt(cp, propagate = FALSE)

## plot(j, vertex.size = 0.1, vertex.label = NA)
## j <- send_messages(j); plot(j)


## ## ---------------------------------------------------------
## ##                   TRYOUTS
## ## ---------------------------------------------------------

## e <- new.env()
## e$a <- 1L
## class(e) <- c("sptable_env", "environment")
## e["ddff", "value"]

## spt <- sptable_env(asia[, 1:3] %>% as.matrix())
## u1  <- make_unity_sptable_env(colnames(asia[, 2:5]), lapply(asia[, 2:5], unique))
## u2  <- make_unity_sptable_env(colnames(asia[, 6:7]), lapply(asia[, 6:7], unique))

## mm <- merge_two_unities_env(u1, u1)
## merge(u1, spt)
## merge_unity_sptable_env(spt, u1)

# NOTE: they dont have reference semantics. This is important!
# - Should they have that?

## e1 <- as_env.sptable(cl[[1]])
## e2 <- as_env.sptable(cl[[10]])
## merge.sptable_env(e1, e2)

## as_parray(e2, "DIFFN_PATHO")
## marginalize(e2, "DIFFN_PATHO", "sum")

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


## ---------------------------------------------------------
##                COMPARE TABLES
## ---------------------------------------------------------
## library(microbenchmark)
## library(dplyr)

## compare_tables <- function(spt, spt_env) {
##   .map_lgl(names(spt), function(x) {
##     spt[x] == spt_env[x, "value"]
##   }) %>% all()
## }


## x1 <- sptable(asia[, c(3,1,5)] %>% as.matrix())
## x2 <- sptable_env(asia[, c(3,1,5)] %>% as.matrix())

## y1 <- sptable(asia[, c(1,4,5,7)] %>% as.matrix())
## y2 <- sptable_env(asia[, c(1,4,5,7)] %>% as.matrix())

## m1 <- merge(x1, y1, "/")
## m2 <- merge(x2, y2, "/")

## compare_tables(m1, m2)

# microbenchmark(merge(x1, y1, "/"), merge(x2, y2, "/"))



## library(igraph)
## el <- matrix(c(
##   "A", "T",
##   "T", "E",
##   "S", "L",
##   "S", "B",
##   "L", "E",
##   "E", "X",
##   "E", "D",
##   "B", "D"),
##   nc = 2,
##   byrow = TRUE
## )

## g <- igraph::graph_from_edgelist(el)
## cp <- compile(asia, g)
