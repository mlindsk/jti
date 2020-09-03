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

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }


## library(igraph)
## munin <- readRDS("../inst/extdata/munin.rds")

## cpts <- lapply(munin[1:100], function(x) {
##   arr <- as(x$prob, "array")
##   if (length(dim(arr)) == 1L) { # The onedimensional ones are not named
##     dimnames(arr) <- structure(dimnames(arr), names = x$node)
##   }
##   arr
## }) 

## cl  <- cpt_list(cpts)
## cp1 <- compile(cl)
## size_mb(cp1)

## j <- jt(cp1)
## size_mb(j)

## for (k in 1:12) j <- send_messages(j)
## j <- send_messages(j)


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

## ---------------------------------------------------------
##                        DERMA
## ---------------------------------------------------------
## library(molic)

## d <- derma
## g <- ess::fit_graph(d)
## cp <- compile(d, g)

## unlist(lapply(d, function(x) length(unique(x))))

## j <- jt(cp, propagate = "no")

## microbenchmark::microbenchmark(
##   jt(cp),
##   times = 10
## )

# query_belief(j, c("h18", "h20"), "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  gRain
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## library(igraph)
## library(gRbase)
## library(gRain)
## library(dplyr)

## Ag <- g$G_A
## gi <- igraph::graph_from_adjacency_matrix(Ag)
## pars <- ess::rip(g$G_adj)$P

## ## igraph::is_dag(gi)

## for (parent in names(pars)) {
##   children <- pars[[parent]]
##   for (ch in children) {
##     gi <- gi %>% igraph::delete_edges(paste(ch,parent, sep = "|"))
##   }
## }

## ## igraph::is_dag(gi)
## plot(gi)

## dg <- d %>% mutate_all(as.factor)
## gi <- as(gi, "graphNEL")

## jt_gr <- grain(compileCPT(extractCPT(dg, gi), smooth = 0.1))
## jt_gr <- gRbase::compile(jt_gr, propagate = TRUE, smooth = 0.1)

## # querygrain(jt_gr, nodes = c(""), type = "joint")
