# library(ess)
# derma_igraph <- as_igraph(fit_graph(derma, q = 0, sparse_qic = TRUE))
# saveRDS(derma_igraph, "../extdata/derma_igraph.rds")
# saveRDS(derma, "../extdata/derma.rds")

# derma is a markov random field

automatic_test <- TRUE
derma <- if (automatic_test) readRDS("../extdata/derma.rds") else readRDS("../inst/extdata/derma.rds")
derma_igraph <- if (automatic_test) readRDS("../extdata/derma_igraph.rds") else readRDS("../inst/extdata/derma_igraph.rds")

cl <- cpt_list(derma, derma_igraph)
cp <- compile(cl, initialize_cpts = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Inconsistent evidece where p(parents) = 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Evidence - a parent configuration that is never seen in h20
e1 <- c(c2 = "0", c4 = "1")

# Test that e1 is never seen:
cpt_e1 <- sparta::marg(cl$h20, setdiff(names(cl$h20), names(e1)))
expect_equal(sparta::get_val(cpt_e1, e1), 0)

# Test that the reduced cpt is the uniform prior
cpe1 <- set_evidence(cp, e1, initialize_cpts = FALSE)

cpt1 <- cpe1$charge$cpts$h20
expect_true(inherits(cpt1, "sparta_unity")) # uniform unity

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Inconsistent evidece where p(parents) > 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Evidence
e2   <- c(h20 = "2", ES = "chronic dermatitis", age = "1", c2 = "1")

# Test that e1 is never seen:
cpt_e2 <- sparta::marg(cl$h20, setdiff(names(cl$h20), names(e2)))
expect_equal(sparta::get_val(cpt_e2, e2), 0)
