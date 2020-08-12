## g <- ess::fit_graph(asia, q = 0)

## cp1 <- compile(asia, g)
## cp2 <- compile(asia, g, "D")


## microbenchmark::microbenchmark(
##   jt(cp1),
##   jt(cp1, propagate = "collect")
## )

## ---------------------------------------------------------
##                       MUNIN
## ---------------------------------------------------------

## library(igraph)
## munin <- readRDS("../inst/extdata/munin.rds")

## cpts <- lapply(munin[1:31], function(x) {
##   arr <- as(x$prob, "array")
##   if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
##     dimnames(arr) <- structure(dimnames(arr), names = x$node)
##   }
##   arr
## }) 

## cl  <- cpt_list(cpts)
## cp1 <- compile(cl)
## cp2 <- compile(cl, root_node = "DIFFN_SENS_SEV")

## microbenchmark::microbenchmark(
##   jt(cp1),
##   jt(cp2, propagate = "collect"),
##   times = 2,
##   unit  = "s"
## )

## j1 <- jt(cp1)
## j2 <- jt(cp2)

## plot(j1, vertex.size = 5)
## get_cliques(j1)
## qb <- query_belief(j1, c("DIFFN_DISTR", "DIFFN_SENS_SEV", "DIFFN_S_SEV_DIST"), "joint")

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


## ---------------------------------------------------------
##                  GRAIN COMPARISON
## ---------------------------------------------------------
## library(gRbase)
## x1 <- Reduce(tabMult, cpts[1:12])
## x2 <- Reduce(tabMult, cpts[5:17])
## length(x1)
## object.size(x1)[1] / 1e6

## xx <- tabMult(x1, x2)

## # BIG ARRAYS
## a  <- lapply(cpts, function(x) as_sptable(char_array(x)))
## a1 <- Reduce(merge, a[1:7])
## a2 <- Reduce(merge, a[4:10])
## length(a2)
## object.size(a2)[1] / 1e6

## aa <- merge(a1, a2)

## microbenchmark::microbenchmark(
##   x1 %a*% x2, 
##   a1 %m*% a2,
##   times = 3,
##   unit = "s"
## )


## dim  <- c(2,2,2)
## cell <- c(1,1,1)

## cell <- next_cell(cell, dim)
## cell


## cell2entry(cell, dim)

## cellf <- cell[1:2]
## cellg <- cell[c(1,3)]

## cf <- cell2entry(cellf, dim[1:2])
## cell2entry(cellg, dim[c(1,3)])

## tabf <- c(10, 200)
## tabfi <- c(2, 4)

## tabf[which(tabfi == cf)]

