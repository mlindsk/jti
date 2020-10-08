## gR <- function(l, qnodes, type = "marginal") {
##   gr <- bnlearn::as.grain(l)
##   jt_gr <- gRbase::compile(gr, propagate = TRUE)
##   gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")  
## }

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~    DATA DRIVEN     ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ---------------------------------------------------------
##                       DERMA
## ---------------------------------------------------------
## library(molic)

## d <- derma
## g <- ess::fit_graph(d)
## cp <- compile(d, g)

## unlist(lapply(d, function(x) length(unique(x))))

## j <- jt(cp, propagate = "full")

## microbenchmark::microbenchmark(
##   jt(cp),
##   times = 10
## )

## query_belief(j, c("h18", "h20"), "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~    CPT DRIVEN    ~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alarm.rds
# andes.rds
# barley.rds
# child.rds
# diabetes.rds
# hailfinder.rds
# hepar2.rds
# insurance.rds
# link.rds
# mildew.rds
# pathfinder.rds
# pigs.rds
# water.rds


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         MUNIN: 1041-1397-80592
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/munin.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile2.cpt_list(cl, save_graph = TRUE)

## tictoc::tic()
## j  <- jt(cp, propagate = "no") # This includes finding the junction tree which is slow!
## tictoc::toc()

## cs  <- get_cliques(j)
## qnodes <- cs$C1
## query_belief(j, qnodes[1:2], "joint")

## gr <- bnlearn::as.grain(l)

## tictoc::tic()
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## tictoc::toc()

## gRain::querygrain(jt_gr, nodes = c("DIFFN_SENS_SEV", "DIFFN_TYPE"), type = "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         LINK: 724-1125-14211
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/link.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)

## tictoc::tic()
## j  <- jt(cp, propagate = "full") # This includes finding the junction tree which is slow!
## tictoc::toc()

## gr <- bnlearn::as.grain(l)

## tictoc::tic()
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## tictoc::toc()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                        DIABETES: 413-602-429409
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/diabetes.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)
## j    <- jt(cp, propagate = "full")

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"),
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1
## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes)

## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      MILDEW: 35-46-540150
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../sandbox/r/bns/mildew.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"),
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1[1]

## query_belief(j, qnodes)
## query_belief(j2, qnodes)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      HAILFINDER: 56-66-2656
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/hailfinder.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"),
##   j2 <- jt(cp2, propagate = "full"),
##   times = 10
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1

## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes)


## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      BARLEY: 48-84-114005
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## l <- readRDS("../../../../sandbox/r/bns/barley.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), # Error: cannot allocate vector of size 2.5 Gb
##   j2 <- jt(cp2, propagate = "full"), # Process R killed at Thu Sep 24 10:16:29 2020
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1[1]

## query_belief(j, qnodes)
## query_belief(j2, qnodes)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      CHILD: 20-25-230
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/child.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)

## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## cp2$charge$C$C2 <- sparta:::sparta_unity(sparta::dim_names(cp2$charge$C$C2))

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## ## ## par(mfrow = c(1, 2))
## ## ## plot(j)
## ## ## plot(j2)

## qnodes <- c("Disease", "LungFlow")

## # TODO: Different results?
## # - only reasonable explanation is the idenity tables?
## # - try reverting again?
## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes, "joint")

## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      HEPAR2: 70-123-1453
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/hepar2.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## j  <- jt(cp, propagate = "no")
## plot(j, vertex.size = 5, vertex.label = NA)
## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## cp2$charge$C[[2]]

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1

## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes)


## library(igraph)
## library(gRbase)
## library(bnlearn)
## library(gRain)
## library(dplyr)

## gr <- as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## querygrain(jt_gr, nodes = qnodes, type = "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      INSURANCE: 27-52-984
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## l <- readRDS("../../../../sandbox/r/bns/insurance.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 5
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## ## par(mfrow = c(1, 2))
## ## plot(j)
## ## plot(j2)

## qnodes <- cs$C1[1]

## query_belief(j, qnodes)
## query_belief(j2, qnodes)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ASIA: 8-8-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/hailfinder.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## adj <- list(
##   asia   = c("tub", "dysp"),
##   tub    = c("asia", "lung", "either", "dysp"),
##   smoke  = c("lung", "bronc", "either"),
##   lung   = c("tub", "smoke", "either"),
##   bronc  = c("smoke", "either", "dysp"),
##   either = c("tub", "smoke", "lung", "bronc", "xray", "dysp"),
##   xray   = c("either"),
##   dysp   = c("asia", "tub", "bronc", "either")
## )

## ess:::rip(adj)
## ess:::mcs(adj)

## ig <- igraph::graph_from_adjacency_matrix(ess::as_adj_mat(adj), "undirected")
## igraph::is.chordal(ig)
## plot(ig)


## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## ## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 5
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## j <- jt(cp, propagate = "no")
## plot(j)
## j <- send_messages(j)


## qnodes <- cs$C4
## query_belief(j, qnodes, "joint")
## gR(l, qnodes, "joint")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      CANCER: 5-4-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/cancer.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1

## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes, "joint")

# gR(l, qnodes)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      EARTHQUAKE: 5-4-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/earthquake.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1

## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes, "joint")


## gr <- as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## querygrain(jt_gr, nodes = qnodes, type = "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      SACHS: 11-17-178
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/sachs.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)

## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- cs$C1
## query_belief(j, qnodes, "joint")

## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ALARM: 37-46-509
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/alarm.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- compile(cl, save_graph = TRUE)
## cp2  <- compile2.cpt_list(cl, save_graph = TRUE)
## plot(jti::dag(cp), vertex.size = 0.2, arrow.size = 0.5)

## ##  <dim_names>
## ## CATECHOL: NORMAL, HIGH 
## ## CO: LOW, NORMAL, HIGH 
## ## TPR: LOW, NORMAL, HIGH 


## ## .map_int(cp$charge$C, function(x) length(sparta::vals(x)))
## ## .map_int(cp2$charge$C, function(x) length(sparta::vals(x)))

## microbenchmark::microbenchmark(
##   j  <- jt(cp, propagate = "full"), 
##   j2 <- jt(cp2, propagate = "full"),
##   times = 1
## )


## j <- jt(cp, propagate = "full")
## j <- send_messages(j)

## cs  <- get_cliques(j)
## cs2 <- get_cliques(j2)

## par(mfrow = c(1, 2))
## plot(j)
## plot(j2)

## qnodes <- c("FIO2", "PVSAT") # cs$C1

## query_belief(j, qnodes, "joint")
## query_belief(j2, qnodes, "joint")

## gr <- gR(l, qnodes)

## gr    <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

## gRain::querygrain(jt_gr, nodes = names(sparta::dim_names(j$charge$C$C15)), "joint")
## query_belief(j, names(sparta::dim_names(j$charge$C$C15)), "joint")
## j$charge$C$C15

# FIO2: normal
# VENTALV: normal
# PVSAT: low

## unlist(j$cliques)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         gRain
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




## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## library(igraph)       
## library(sparta)
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #                     CREATE GRAPH
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## el <- matrix(
##   c(
##     1, 2,
##     2, 3,
##     3, 4,
##     3, 5,
##     5, 6,
##     1, 6 # try remove and see the tree
##   ),
##   ncol = 2,
##   byrow = TRUE
## )

## g <- graph_from_edgelist(el)
## plot(g, layout = layout_as_tree)

## # moralize:
## gm <- as.undirected(add_edges(g, c(1,5)))

## # triangulate:
## gt <- add_edges(gm, c(1,3, 4,5))

## par(mfrow = c(1,2))
## plot(g, layout = layout_as_tree)
## plot(gt)

## # So: 1,3,5 (a,c,e) is possibly an empty clique!

## V(g)$name   <- letters[1:6]
## V(g)$label  <- letters[1:6]
## V(gt)$name  <- letters[1:6]
## V(gt)$label <- letters[1:6]

## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #                 CREATE PORTENTIALS
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set.seed(300718)
## sprob <- function() {p <- runif(1); c(p, 1-p)}
## s <- c(0,1)
## a     <- array(sprob(), dim = 2L, dimnames = list(a = c(0,1)))
## b_a   <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(b = s, a = s))
## c_b   <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(c = s, b = s))
## d_c   <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(d = s, c = s))
## e_c   <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(e = s, c = s))
## f_ae  <- array(c(sprob(), sprob(), sprob(), sprob()), dim = c(2L, 2L, 2L), dimnames = list(f = s, a = s, e = s))

## sa    <- sparta::as_sparta(a)
## sb_a  <- sparta::as_sparta(b_a)
## sc_b  <- sparta::as_sparta(c_b)
## sd_c  <- sparta::as_sparta(d_c)
## se_c  <- sparta::as_sparta(e_c)
## sf_ae <- sparta::as_sparta(f_ae)


## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #        INITIALIZING POTENTIALS
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## parents_igraph(g)

## gt_adj_mat <- as_adjacency_matrix(gt)
## gt_adj_lst <- as_adj_lst(gt_adj_mat)

## cliques    <- rip(gt_adj_lst)$C
## names(cliques) <- paste("C", 1:length(cliques), sep = "")

## pots <- lapply(cliques, function(x) NULL)
## ## pots$C1 <- mult(sc_b, sb_a)
## ## pots$C2 <- sa
## pots$C1 <- mult(mult(sa, sb_a), sc_b)
## pots$C2 <- sparta_unity_struct(list(a = s, c = s, e = s))
## pots$C3 <- mult(sd_c, se_c)
## pots$C4 <- sf_ae


## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #               MESSAGE PASSING
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## C3_msg  <- marg(pots$C3, c("d"))
## pots$C3 <- div(pots$C3, C3_msg)

## pots$C2 <- mult(pots$C2, C3_msg)

## C4_msg  <- marg(pots$C4, c("f"))
## pots$C4 <- div(pots$C4, C4_msg)

## pots$C2 <- mult(pots$C2, C4_msg)

## C2_msg  <- marg(pots$C2, c("e"))
## pots$C2 <- div(pots$C2, C2_msg)

## pots$C1 <- mult(pots$C1, C2_msg)
## pots$C1 <- sparta::normalize(pots$C1)

## C1_msg  <- marg(pots$C1, c("b"))
## pots$C2 <- mult(pots$C2, C1_msg)

## C2_msg  <- marg(pots$C2, c("a"))
## pots$C3 <- mult(pots$C3, C2_msg)

## C2_msg  <- marg(pots$C2, c("c"))
## pots$C4 <- mult(pots$C4, C2_msg)


## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## #                INFERENCE
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## pots$C2

## library(gRain)
## cpt_list <- list(
##   a,
##   b_a,
##   c_b,
##   d_c,
##   e_c,
##   f_ae
## )

## plist <- gRain::compileCPT(cpt_list)
## gr    <- gRain::grain(plist)
## gr    <- gRbase::propagate(gr)
## gRain::querygrain(gr, nodes = c("a", "c", "e"), type = "joint")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## library(dplyr)
## library(sparta)
## set.seed(300718)
## sprob <- function() {p <- runif(1); c(p, 1-p)}
## s <- c(0,1)
## b_a <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(b = s, a = s))
## c_b <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(c = s, b = s))
## d_c <- array(c(sprob(), sprob()), dim = c(2L, 2L), dimnames = list(d = s, c = s))

## pots <- lapply(1:3, function(x) NULL)
## pots[[1]] <- sparta::as_sparta(b_a)
## pots[[2]] <- sparta::as_sparta(c_b)
## pots[[3]] <- sparta::as_sparta(d_c)


## pots[[1]] <- mult(pots[[1]], marg(pots[[2]], "c"))
## pots[[1]] <- normalize(pots[[1]])
## pots[[3]] <- mult(pots[[3]], marg(pots[[2]], "b"))
## pots[[3]] <- normalize(pots[[3]])

## pots[[2]] <- mult(pots[[2]], marg(pots[[1]], "a"))
## pots[[2]] <- mult(pots[[2]], marg(pots[[3]], "d"))

## normalize(mult(mult(pots[[1]], pots[[2]]), pots[[3]]))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## cl  <- cpt_list(asia2)
## cp6 <- compile(cl, root_node = "xray", save_graph = TRUE)
## jt6 <- jt(cp6, propagate = "full")
## plot(jt6)
## sum(jt6$charge$C$C3)
## attr(jt6, "clique_root")
## attr(jt6, "root_node")
