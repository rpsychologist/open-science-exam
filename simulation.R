
# Packages ----------------------------------------------------------------
library(powerlmm)

# Setup -------------------------------------------------------------------
p <- study_parameters(n1 = 11, 
                      n2 = 50,
                      icc_pre_subject = 0.5,
                      cor_subject = -0.4,
                      var_ratio = c(0, 0.01, 0.02, 0.03, 0.04),
                      cohend = 0.5
                      )



# Run simulation ----------------------------------------------------------
## Model syntaxes
f <- list("correct" = "y ~ time * treatment + (1 + time | subject)",
          "wrong" = "y ~ time * treatment + (1 | subject)")

res_H0 <- simulate(update(p, cohend = 0), 
                   nsim = 5000, 
                   formula = f, 
                   satterthwaite = FALSE, 
                   cores = 16)

# not really used in the paper
res_Ha <- simulate(p, 
                   nsim = 5000, 
                   formula = f, 
                   satterthwaite = FALSE, 
                   cores = 16)

# Save
save(res_H0, res_Ha, file = "data/sim.Rdata")

