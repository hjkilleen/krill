# This script generates fake data with a hump shape (quadratic) for a number of different groups/plots, then fits simple and hierarchical quadratic models to them. 

library(ggplot2)
library (lme4)
library(brms)

# Make fake nonlinear data 
n_indivs <- 40 
n_plots <- 5
nlfun <- function(x){return(rnorm(1, 0.5, 0.1)*x - rnorm(1, 0.25, 0.05)*x^2)}
noisefun <- function(n){return(rnorm(n, 0, 2))}
plot_var_fun <- function(){return(runif(1, -2, 2))}
x <- runif(n_indivs, 0, 5)
y <- {}
for (i in 1:n_plots) y <- c(y, nlfun(x) + noisefun(n_indivs) + runif(1, -2, 2) + plot_var_fun())
x <- rep(x, n_plots)
d <- data.frame(x, y, plot = as.factor(rep(1:n_plots, rep(n_indivs, n_plots))))

# Check
ggplot(d, aes(x, y, colour=plot)) + geom_point() + theme_minimal()

# Test mixed models
m1 <- lmer(y~x+(1|plot), d) # linear
m2 <- lmer(y~x + I(x^2) + (1|plot), d) # quadratic
m3 <- lmer(y~x + I(x^2)  + (1 + x + I(x^2)|plot), d) # quadratic with random slopes
summary(m3)
anova(m1, m2, m3)

# Bayesian version 
bm1 <- brm(y~x + I(x^2) + (1|plot), d) # quadratic
bm2 <- brm(y~x + I(x^2) + (1 + x + I(x^2)|plot), d) # quadratic with random slopes
bm3 <- brm(y ~ s(x) + (1|plot), d) # nonparametric spline fit -- can't make it hierarchical
summary(bm1); conditional_effects(bm1)
summary(bm2); conditional_effects(bm2)
summary(bm3); conditional_effects(bm3)
waic(bm1, bm2, bm3)
