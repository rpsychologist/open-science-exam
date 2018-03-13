
library(powerlmm)
library(ggplot2)
library(dplyr)
library(tidyr)

p <- study_parameters(n1 = 11, 
                      n2 = 50,
                      icc_pre_subject = 0.5,
                      cor_subject = -0.4,
                      var_ratio = c(0, 0.01, 0.02, 0.03, 0.04),
                      cohend = 0.5
                      )

get_power(p)

# Power curve

pcurve <- get_power_table(p, n2 = 20:100, var_ratio = c(0, 0.01, 0.02, 0.03, 0.04))

plot(pcurve) + 
  geom_vline(xintercept = 100, linetype = "dotted") +
  guides(linetype = "none") +
  labs(color = "Variance ratio \n(random_slope²/error²)")

# Simulation
f <- list("correct" = "y ~ time * treatment + (1 + time | subject)",
          "wrong" = "y ~ time * treatment + (1 | subject)")

res_H0 <- simulate(update(p, cohend = 0), nsim = 5000, formula = f, satterthwaite = FALSE, cores = 16)
res_Ha <- simulate(p, nsim = 5000, formula = f, satterthwaite = FALSE, cores = 16)

# Save
save(res_H0, res_Ha, file = "data/sim.Rdata")

summary(res_H0)

# Plots
type1 <- summary(res_H0, model = "wrong")
type1$var_ratio <- get_var_ratio(p)
type1$type1_correct <- summary(res_H0, model = "correct")$Power

d <- type1 %>% 
  select(var_ratio, type1_wrong = Power, type1_correct) %>% 
  gather(model, type1, -var_ratio) %>% 
  mutate(model = factor(model, labels = c("Random intercept-slope", "Random-intercept only")))
  


ggplot(d, aes(var_ratio, type1, color = model)) + geom_point() + geom_line() +
  labs(x = "Variance ratio \n(random_slope²/error²)",
       y = "Type I Error") +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.2))


summary(res_H0, model = "wrong", type = "random", "subject_intercept")
summary(res_H0, model = "wrong", type = "random", "error")
