cl  <- readRDS("../extdata/derma_cpt_list.rds")
# cl  <- readRDS("../inst/extdata/derma_cpt_list.rds") # locally testing
cp  <- compile(cl, initialize_cpts = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Inconsistent evidece where p(parents) = 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Evidence - a parent configuration that is never seen in h20
e1 <- c(c2 = "0", c4 = "1")

# Test that e1 is never seen:
cpt_e1 <- sparta::marg(cl$h20, setdiff(names(cl$h20), names(e1)))
expect_equal(sparta::get_val(cpt_e1, e1), 0)

# Test that the reduced cpt is the uniform prior
cpe1 <- set_evidence(cp, e1, initialize_cpts = TRUE)
cpt1 <- cpe1$charge$cpts$h20
expect_true(inherits(cpt1, "sparta_unity")) # uniform unity

# Test that the rank is indeed uniform: 1/|I_h20|
r1 <- sparta::sparta_rank(cpt1)
expect_equal(r1, 1/length(sparta::dim_names(cpt1)[["h20"]]))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Inconsistent evidece where p(parents) > 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Evidence
e2   <- c(h20 = "2", ES = "chronic dermatitis", age = "1", c2 = "1")

# Test that e1 is never seen:
cpt_e2 <- sparta::marg(cl$h20, setdiff(names(cl$h20), names(e2)))
expect_equal(sparta::get_val(cpt_e2, e2), 0)

# Test that the reduced cpt is the epsilon-smoothed version
cpe2 <- set_evidence(cp, e2, initialize_cpts = FALSE)
cpt2 <- cpe2$charge$cpts$h20 # epsilon smooting

# Test that the rank is indeed eps_smooth = 0.1 (default)
r2 <- sparta::sparta_rank(cpt2)
expect_equal(r2, formals(compile)[["eps_smooth"]])
