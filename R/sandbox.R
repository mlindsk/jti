## library(igraph)

## ---------------------------------------------------------
##                       MUNIN
## ---------------------------------------------------------

## munin <- readRDS("../inst/extdata/munin.rds")

## cpts <- lapply(munin[1:20], function(x) {
##   arr <- as(x$prob, "array")
##   if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
##     dimnames(arr) <- structure(dimnames(arr), names = x$node)
##   }
##   arr
## }) 

## cl <- cpt_list(cpts)
## cp <- compile(cl)
## j <- jt(cp, propagate = TRUE)

## plot(j, vertex.size = 5)
## get_cliques(j)
## qb <- query_belief(j, c("DIFFN_DISTR", "DIFFN_SENS_SEV", "DIFFN_S_SEV_DIST"), "joint")

## ftable(qb)

## ## ---------------------------------------------------------
## ##                   TRYOUTS
## ## ---------------------------------------------------------

## e <- new.env()
## e$a <- 1L
## class(e) <- c("sptable", "environment")
## e["ddff", "value"]

## spt <- sptable(asia[, 1:3] %>% as.matrix())
## u1  <- make_unity_sptable(colnames(asia[, 2:5]), lapply(asia[, 2:5], unique))
## u2  <- make_unity_sptable(colnames(asia[, 6:7]), lapply(asia[, 6:7], unique))

## mm <- merge_two_unities(u1, u1)
## merge(u1, spt)
## merge_unity_sptable(spt, u1)

# NOTE: they dont have reference semantics. This is important!
# - Should they have that?

## e1 <- as.sptable(cl[[1]])
## e2 <- as.sptable(cl[[10]])
## merge.sptable(e1, e2)

## to_cpt(e2, "DIFFN_PATHO")
## marginalize(e2, "DIFFN_PATHO", "sum")

## x <- cpts[[10]]
## microbenchmark::microbenchmark(
##   as_sptable.array(x, validate = FALSE),
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
##                        DERMA
## ---------------------------------------------------------

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


## d <- derma
## g <- ess::fit_graph(d[, -35])
## cp <- compile(d[, -35], g)
## j <- jt(cp, structure("3", names = "h31"))
## query_belief(j, "h31")
## methods(class = "jt")

## library(Rcpp)
## sourceCpp("src/sptab_.cpp")
## aa <- asia
## aa[1, 2] <- "FAIL ME!"
## matpr(as.matrix(aa))


## ---------------------------------------------------------
##                           CAR
## ---------------------------------------------------------

## car <- read.table(
##   "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
##   header = FALSE, sep = ",", dec = ".")

## names(car) <- c(
##   "buying",
##   "maint",
##   "doors",
##   "persons",
##   "lug_boot",
##   "safety",
##   "class"
## )

## car[] <- lapply(car, as.character)
## cp <- compile(car, ess::fit_graph(car))
## j <- jt(cp)


## get_cliques(j)
## query_belief(j, c("buying", "class", "maint"), "joint")

## as_sptable(char_array(query_belief(j, c("buying", "class", "maint"), "joint")))
