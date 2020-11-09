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
## library(gRain)
## library(gRbase)
## library(pryr)

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }

## l <- readRDS("../../../sandbox/r/bns/munin.rds")
## cpts <- munin # bnlearn_to_cpts(munin)
## cl   <- cpt_list(cpts)

## plist <- gRain::compileCPT(munin)
## grn   <- grain(plist)
## jt_gr <- gRbase::compile(grn)
## prop  <- propagate(jt_gr)
## size_mb(jt_gr)
## size_mb(munin)
## mem_used()


# gRain::querygrain(jt_gr, nodes = c("DIFFN_SENS_SEV", "DIFFN_TYPE"), type = "joint")

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
## library(gRain)
## library(gRbase)
## library(pryr)

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }

## l <- readRDS("../../../../sandbox/r/bns/diabetes.rds")
## cpts <- bnlearn_to_cpts(l)

## ## # jti:
## cl <- cpt_list(cpts)
## cp <- compile_grbase(cl)
## j <- jt(cp)

## query_belief(j, "straaling_4")

## size_mb(cl)
## size_mb(j$charge)

## ## # gRain:
## plist <- gRain::compileCPT(cpts)
## size_mb(plist)
## grn   <- gRain::grain(plist)
## jt_gr <- gRbase::compile(grn)
## prop  <- gRbase::propagate(jt_gr)
## gRain::querygrain(prop, "straaling_4")


## size_mb(prop$potential$pot_equi)
## size_mb(prop$potential$pot_orig)
## size_mb(prop$potential$pot_temp)

## mapply(function(x,y) setequal(x,y), prop$rip$cliques, j$cliques)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      MILDEW: 35-46-540150
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## library(gRain)
## library(gRbase)
## library(pryr)
## library(jti)
## library(dplyr)
## library(sparta)

## nodes <- c(
##   "meldug_1",
##   "meldug_2",
##   "middel_1",
##   "lai_0",
##   "lai_1",
##   "nedboer_1",
##   "mikro_1",
##   "temp_1",
##   "foto_1",
##   "straaling_1",
##   "dm_1",
##   "dm_2", # From here it fucks up!
##   "foto_2",
##   "straaling_2",
##   "temp_2",
##   "lai_2"
## )

## l <- readRDS("../../../../sandbox/r/bns/mildew.rds")
## cpts <- bnlearn_to_cpts(l)
## ## cpts <- cpts[nodes]
## cl <- cpt_list(cpts)
## plot(attr(cl, "graph"), vertex.size = 10)
## cp <- compile(cl, save_graph = TRUE, "")
## j <- jt(cp)
## sapply(seq_along(nodes), function(i) sum(query_belief(j, nodes[i])[[1]]))
## query_belief(j, nodes[12])[[1]]


## plist <- gRain::compileCPT(cpts)
## gr    <- gRain::grain(plist)
## gr    <- gRbase::propagate(gr)
## gRain::querygrain(gr, nodes[12])[[1]]


## ## ## ## # Now use gRain's cliques!
## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## cp   <- compile.cpt_list(cl, jt_gr$rip$cliques)
## gRain::querygrain(jt_gr, nodes)

## gRain::querygrain(jt_gr, "straaling_4")

## ## # Now use gRain's junction tree!
## j <- jt(cp, jt_gr$rip)

## grain_to_junction_tree <- function(x) {

##   # 1) We must change the cliques already before new_charge
##   # in the compile function!!!

##   # 2) Make collect and distribute matrices
##   # and extract cliques
  
##   # return(list(collect = collect, ...))
## }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      HAILFINDER: 56-66-2656
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/hailfinder.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- jti::compile(cl)
## lapply(cp$charge$C, function(x) inherits(x, "sparta_unity"))
## j <- jt(cp)
## plot(j)

## ## # gr
## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)

## for (k in 1:56) {
##   qnodes <- names(attr(cp, "dim_names"))[k]
##   print(query_belief(j, qnodes, "joint"))
##   print(gRain::querygrain(jt_gr, nodes = qnodes, type = "joint"))
## }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      BARLEY: 48-84-114005
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/barley.rds")
## cpts <- bnlearn_to_cpts(l)

## # jti
## cl   <- cpt_list(cpts)
## cp   <- jti::compile(cl)
## j <- jt(cp)
## plot(j)

## # gr
## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)

## for (k in 1:48) {
##   qnodes <- names(attr(cp, "dim_names"))[k]
##   print(query_belief(j, qnodes, "joint"))
##   print(gRain::querygrain(jt_gr, nodes = qnodes, type = "joint"))
## }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      CHILD: 20-25-230
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/child.rds")
## cpts <- bnlearn_to_cpts(l)
## cl   <- cpt_list(cpts)
## cp   <- jti::compile(cl)
## j <- jt(cp)
## plot(j)

## qnodes <- c("Disease", "LungFlow")
## query_belief(j, qnodes, "joint")

## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      HEPAR2: 70-123-1453
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/hepar2.rds")
## cpts <- bnlearn_to_cpts(l)

## # jti
## cl   <- cpt_list(cpts)
## cp   <- jti::compile(cl)
## j <- jt(cp)
## plot(j)

## # gr
## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)

## for (k in 1:70) {
##   qnodes <- names(attr(cp, "dim_names"))[k]
##   print(query_belief(j, qnodes, "joint"))
##   print(gRain::querygrain(jt_gr, nodes = qnodes, type = "joint"))
## }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      INSURANCE: 27-52-984
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## l <- readRDS("../../../../sandbox/r/bns/insurance.rds")
## cpts <- bnlearn_to_cpts(l)

## # jti
## cl   <- cpt_list(cpts)
## cp   <- jti::compile(cl)
## j <- jt(cp)
## plot(j)

## # gr
## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)

## for (k in 1:27) {
##   qnodes <- names(attr(cp, "dim_names"))[k]
##   print(query_belief(j, qnodes, "joint"))
##   print(gRain::querygrain(jt_gr, nodes = qnodes, type = "joint"))
## }




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
## cp   <- jti::compile(cl)
## j <- jt(cp)
## plot(j)


## gr <- bnlearn::as.grain(l)
## jt_gr <- gRbase::compile(gr, propagate = TRUE)

## qnodes <- names(attr(cp, "dim_names"))[37]
## query_belief(j, qnodes, "joint")
## gRain::querygrain(jt_gr, nodes = qnodes, type = "joint")

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       SIM DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## library(sparta)
## library(igraph)
## library(dplyr)

## sim_data <- function(N = 15000,
##                      lvlsB = 3,
##                      lvlsE = 3,
##                      k = 2L,
##                      l = 2L,
##                      sig = 1L,
##                      s1  = 2,
##                      s2  = 3) {
##   set.seed(1)
##   N <- 15000
##   A <- rmultinom(N, size = 1, prob = c(0.1,0.2,0.7))
##   m <- c(-3, 0, 3)
##   B <- sapply(1:N, function(i) rnorm(1, sum(A[, i]*m), sig))

##   k <- 2L
##   D <- sapply(B, function(b) rpois(1, abs(b) / k))
##   E <- rbeta(N, s1, s2)

##   l <- 2
##   C <- sapply(1:N, function(i) {
##     rpois(1, (abs(B[i] + D[i] + E[i])) / l)
##   }) %>% as.character()

##   A <- apply(A, 2, function(a) which(a == 1L)) %>% as.character(A)
##   D <- as.character(D)
##   d <- data.frame(A, B, C, D, E)
##   d$B <- as.character(cut(B, lvlsB, labels = paste("b", 1:lvlsB, sep = "")))
##   d$E <- as.character(cut(E, lvlsE, labels = paste("e", 1:lvlsE, sep = "")))
##   d
## }

## g <- igraph::make_graph(c("A", "B", "B", "D", "B", "C", "D", "C", "E", "C"))
## plot(g)

## predict_jt <- function(train, test, g) {
##   class_int <- 3
##   cp <- jti::compile(jti::cpt_list(train, g))

##   score <- vapply(1:nrow(test), function(i) {
##     cls <- test[i, class_int]
##     j  <- jti::jt(cp, test[i, setdiff(1:ncol(test), class_int)] %>% unlist(), "max")
##     pred_cls <- unname(jti::mpe(j)[colnames(test)[class_int]])
##     pred_cls == cls
##   }, 1) %>% mean()

##   sparsity <- lapply(cp$charge$C, function(x) {
##     ncol(x) / prod(vapply(sparta::dim_names(x), length, 1L))
##   })

##   list(score = score, sparsity = c(C1 = sparsity[[1]], C2 = sparsity[[2]]))
## }


## validate_performance <- function(d, g, nfold = 5) {
##   kf <- modelr::crossv_kfold(d, k = nfold)

##   scores <- vector("double", length = nrow(kf))
##   sparsity <- structure(vector("double", length = 2), names = c("C1", "C2"))
##   for (i in 1:nrow(kf)) {
##     train_idx <- kf$train[[i]] %>% as.integer()
##     test_idx <- kf$test[[i]] %>% as.integer()
##     pjt <- predict_jt(d[train_idx, ], d[test_idx, ], g)
##     scores[i] <- pjt$score
##     sparsity[1] <- sparsity[1] + pjt$sparsity[1]
##     sparsity[2] <- sparsity[2] + pjt$sparsity[2]
##   }
##   sparsity[1] <- sparsity[1] / nrow(kf)
##   sparsity[2] <- sparsity[2] / nrow(kf)
##   list(cv_score = mean(scores), sparsity = sparsity)
## }


## d <- sim_data(lvlsB = 15, lvlsE = 15)
## vp <- validate_performance(d, g)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        FIXING MILDEW
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## get_ <- function(cell, charge) {
##   structure(sapply(
##     seq_along(dim_names(charge)),
##     function(i) (dim_names(charge)[[i]])[cell[i]]
##   ), names = names(charge))
## }


## library(dplyr)
## library(sparta)

## nodes <- c(
##   "meldug_1",
##   "meldug_2",
##   "middel_1",
##   "lai_0",
##   "lai_1",
##   "nedboer_1",
##   "mikro_1",
##   "temp_1",
##   "foto_1",
##   "straaling_1",
##   "dm_1",
##   "dm_2", # From here it fucks up!
##   "foto_2",
##   "straaling_2",
##   "temp_2",
##   "lai_2"
## )

## l    <- readRDS("../../../../sandbox/r/bns/mildew.rds")
## cpts <- bnlearn_to_cpts(l)
## cpts <- cpts[nodes]
## cl   <- cpt_list(cpts)
## par(mfrow = c(1, 1))
## plot(attr(cl, "graph"), vertex.size = 10)


## cp <- compile(cl)
## j <- jt(cp, propagate = "no")
## plot(j)

## plist <- gRain::compileCPT(cpts)
## gr <- gRain::grain(plist)

## R_given_S_correct <- function(charge, R, S, gr) {
##   for (i in 1:ncol(charge)) {
##     for (r in R) {
##       print(i)
##       cell <- charge[, i]
##       e <- get_(cell, charge)
##       sp <- sparta::slice(charge, e[S])
##       xx <- marg(sp, setdiff(names(sp), r)) %>% as_array()
##       grp <- gRbase::propagate(gr)
##       grp <- gRain::setEvidence(gr, S, e[S])
##       yy <- gRain::querygrain(grp, r, "joint")[names(xx)]
##       ae <- try(all.equal(as.vector(xx), as.vector(yy)))
##       # if (inherits(ae, "try-error")) browser()
##       if (!ae) return(FALSE)
##     }
##   }
##   return(TRUE)
## }


## ## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## # C10 ~ C8       
## ## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## R <- setdiff(names(j$charge$C$C10), names(j$charge$C$C8))
## S <- intersect(names(j$charge$C$C10), names(j$charge$C$C8))
## m_10_8 <- marg(j$charge$C$C10, R)
## j$charge$C$C8  <- mult(j$charge$C$C8, m_10_8)
## j$charge$C$C10 <- div(j$charge$C$C10, m_10_8)

## ## R_given_S_correct(j$charge$C$C10, R, S, gr)

## ## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## # C9 ~ C8       
## ## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## R <- setdiff(names(j$charge$C$C9), names(j$charge$C$C8))
## S <- intersect(names(j$charge$C$C9), names(j$charge$C$C8))
## m_9_8 <- marg(j$charge$C$C9, R)
## j$charge$C$C8 <- mult(j$charge$C$C8, m_9_8)
## j$charge$C$C9 <- div(j$charge$C$C9, m_9_8)

## R_given_S_correct(j$charge$C$C9, R, S, gr)
## ## get_(c(21, 12, 9), j$charge$C$C9)

## ## dm_2   21 22 23 24 25 26
## ## foto_2 12 12 12 12 12 12
## ## dm_1    9  9  9  9  9  9
## ## "dm_2" | "foto_2" "dm_1"  
## uni <- sparta::sparta_unity_struct(dim_names(j$charge$C[[9]]))
## ss <- mult(cl$dm_2, uni)


## sapply(1:ncol(j$charge$C$C9), function(i) {
##   sparta::slice(j$charge$C$C9, get_(j$charge$C$C9[, i], j$charge$C$C9)[S]) %>% sum()
## })


## cpts$dm_2 %>% as_sparta() %>% sum()

## e <- get_(j$charge$C$C9[, 2382], j$charge$C$C9)[S]


## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## TODO: SLICE IS WRONG!!!!!!!!!???

## s <- c(foto_2 = "0_55_kg_m2", dm_1 = "0_16_kg_m2")
## sparta::slice(j$charge$C$C9, e)
## sparta::slice(cl$dm_2, e[1:2])
## sparta::slice(cl$dm_2, e[2:1])
## gRbase::tabSlice(
##   cpts$dm_2,
##   slice = list(foto_2 = "0_55_kg_m2", dm_1 = "0_16_kg_m2")
## )[21:26]
## # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## grp <- gRbase::propagate(gr)
## grp <- gRain::setEvidence(gr, S, e[S])
## gRain::querygrain(grp, R)

## gRbase::tabDiv(cpts$dm_2, gRbase::tabMarg(cpts$dm_2, R)) %>% sparta::as_sparta() %>% sum()
## gRbase::tabDiv(cpts$dm_2, gRbase::tabMarg(cpts$dm_2, R))
