## Replication code for paper accompanying 'SimEngine' R package
##
## Avi Kenny and Charles J. Wolock
##
## The code follows the organization of the paper, divided by section. 

## Required packages for replication
library("SimEngine")
library("ggplot2")

## Set seed for reproducibility
set.seed(72724)

## Section 2: Introduction

## Section 3: Parallelization

## Section 4: Advanced functionality

# Chunk 4.7.1
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat) {
  return(mean(dat))
}
sim %<>% set_levels(n=c(10,100,1000))
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summarize(
  list(stat = "mse", name="lambda_mse", estimate="lambda_hat", truth=5), 
  mc_se = TRUE)

## Appendix A: Simulation-based power calculation

# Chunk A.1
sim <- new_sim()

create_rct_data <- function(n, mu_0, mu_1, sigma_0, sigma_1) {
  group <- sample(rep(c(0,1),n))
  outcome <- (1-group) * rnorm(n=n, mean=mu_0, sd=sigma_0) +
    group * rnorm(n=n, mean=mu_1, sd=sigma_1)
  return(data.frame("group"=group, "outcome"=outcome))
}

create_rct_data(n=3, mu_0=3, mu_1=4, sigma_0=0.1, sigma_1=0.1)

# Chunk A.2 (no output)
run_test <- function(data) {
  test_result <- t.test(outcome~group, data=data)
  return(as.integer(test_result$p.value<0.05))
} 

# Chunk A.3 (no output)
sim %<>% set_script(function() {
  data <- create_rct_data(n=L$n, mu_0=17, mu_1=18, sigma_0=2, sigma_1=2)
  reject <- run_test(data)
  return (list("reject"=reject))
})
sim %<>% set_levels(n=c(20,40,60,80))
sim %<>% set_config(num_sim=1000)

# Chunk A.4
sim %<>% run()

power_sim <- sim %>% summarize(
  list(stat="mean", name="power", x="reject")
)

print(power_sim)

# Chunk A.5
power_formula <- sapply(c(20,40,60,80), function(n) {
  pnorm(sqrt((n*(17-18)^2)/(2^2+2^2)) - qnorm(0.025, lower.tail=F))
})

ggplot(data.frame(
  n = rep(c(20,40,60,80), 2),
  power = c(power_sim$power, power_formula),
  which = rep(c("Simulation","Formula"), each=4)
), aes(x=n, y=power, color=factor(which))) +
  geom_line() +
  labs(color="Method", y="Power", x="Sample size (per group)")



## Appendix B: Comparing two standard error estimators

# Chunk B.1
sim <- new_sim()

create_regression_data <- function(n) {
  beta <- c(-1, 10)
  x <- rnorm(n)
  sigma2 <- exp(x)
  y <- rnorm(n=n, mean=(beta[1]+beta[2]*x), sd = sqrt(sigma2))
  return(data.frame(x=x, y=y))
}

# Chunk B.2
dat <- create_regression_data(n=500)
linear_model <- lm(y~x, data=dat)
dat$residuals <- linear_model$residuals

ggplot(dat, aes(x=x, y=residuals)) +
  geom_point() +
  theme_bw() +
  labs(x="x", y="residual")

# Chunk B.3
model_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcov(mod))))
}

sandwich_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcovHC(mod))))
}

# Chunk B.4
sim %<>% set_script(function() {
  data <- create_regression_data(n=L$n)
  estimates <- use_method(L$estimator, list(data))
  return(list(
    "beta0_est" = estimates$coef[1],
    "beta1_est" = estimates$coef[2],
    "beta0_se_est" = sqrt(estimates$vcov[1]),
    "beta1_se_est" = sqrt(estimates$vcov[2])
  ))
})

sim %<>% set_levels(
  estimator = c("model_vcov", "sandwich_vcov"),
  n = c(50, 100, 500, 1000)
)

sim %<>% set_config(
  num_sim = 500,
  seed = 24,
  packages = c("sandwich")
)

sim %<>% run()

# Chunk B.5
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)

print(summarized_results)

# Chunk B.6
plot_results <- function(which_graph, n_est) {
  if (n_est == 3) {
    values <- c("#999999", "#E69F00", "#56B4E9")
    breaks <- c("model_vcov", "sandwich_vcov", "bootstrap_vcov")
    labels <- c("Model-based", "Sandwich", "Bootstrap")
  } else {
    values <- c("#999999", "#E69F00")
    breaks <- c("model_vcov", "sandwich_vcov")
    labels <- c("Model-based", "Sandwich")
  }
  if (which_graph == "width") {
    summarized_results %>%
      pivot_longer(
        cols = c("mean_se_beta0", "mean_se_beta1"),
        names_to = "parameter",
        names_prefix = "mean_se_"
      ) %>%
      dplyr::mutate(value_j = jitter(value, amount = 0.01)) %>%
      ggplot(aes(x=n, y=1.96*value_j, color=estimator)) +
      geom_line(aes(linetype=parameter)) +
      geom_point() +
      theme_bw() +
      ylab("Average CI width") +
      scale_color_manual(
        values = values,
        breaks = breaks,
        name = "SE estimator",
        labels = labels
      ) +
      scale_linetype_discrete(
        breaks = c("beta0", "beta1"),
        name = "Parameter",
        labels = c(expression(beta[0]), expression(beta[1]))
      )
  } else {
    summarized_results %>%
      pivot_longer(
        cols = c("cov_beta0","cov_beta1"),
        names_to = "parameter",
        names_prefix = "cov_"
      ) %>%
      dplyr::mutate(value_j = jitter(value, amount = 0.01)) %>%
      ggplot(aes(x=n, y=value, color=estimator)) +
      geom_line(aes(linetype = parameter)) +
      geom_point() +
      theme_bw() +
      ylab("Coverage") +
      scale_color_manual(
        values = values,
        breaks = breaks,
        name = "SE estimator",
        labels = labels
      ) +
      scale_linetype_discrete(
        breaks = c("beta0", "beta1"),
        name = "Parameter",
        labels = c(expression(beta[0]), expression(beta[1]))
      ) +
      geom_hline(yintercept=0.95)
  }
}

# Chunk B.7
plot_results("width", 2)
plot_results("coverage", 2)

# Chunk B.8
bootstrap_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  boot_ests <- matrix(NA, nrow=100, ncol=2)
  for (j in 1:100) {
    indices <- sample(1:nrow(data), size=nrow(data), replace=TRUE)
    boot_dat <- data[indices,]
    boot_mod <- lm(y~x, data=boot_dat)
    boot_ests[j,] <- boot_mod$coefficients
  }
  boot_v1 <- var(boot_ests[,1])
  boot_v2 <- var(boot_ests[,2])
  return(list("coef"=mod$coefficients, "vcov"=c(boot_v1, boot_v2)))
}
sim %<>% set_levels(
  estimator = c("model_vcov", "sandwich_vcov", "bootstrap_vcov"),
  n = c(50, 100, 500, 1000)
)
sim %<>% set_config(
  num_sim = 500,
  seed = 24,
  parallel = TRUE,
  n_cores = 2,
  packages = c("sandwich")
)
sim %<>% update_sim()

# Chunk B.9
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)

plot_results("width", 3)
plot_results("coverage", 3)


sessionInfo()