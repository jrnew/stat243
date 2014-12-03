## @knitr q1a
set.seed(0)
m <- 10000
x <- - abs(rnorm(m)) - 4 # samples from a half-normal distri centered/trunacted at -4
f <- dt(x, df = 3)/pt(-4, df = 3)  # density of x under f, a t distri with df 3, truncated at -4
g <- dnorm(x + 4)/pnorm(0)  # density of x under g, a half-normal distri centered/truncated at -4
w <- f/g # weights
samples <- x*w
summary(samples)
par(cex = 0.8)
hist(samples, main = expression(frac(h(x)*f(x), g(x))))
summary(w)
hist(w, main = expression(frac(f(x),g(x))))
mean(samples) # estimated mean of a t distri with df 3, truncated at -4

## @knitr q1b
set.seed(0)
m <- 10000
x <- - abs(rt(m, df = 1)) - 4 # samples from a t distri centered/truncated at -4
f <- dt(x, df = 3)/pt(-4, df = 3) # density of x under f, a t distri with df 3, truncated at -4
g <- dt(x + 4, df = 1)/pt(0, df = 1)  # density of x under g, a t distri centered/truncated at -4
w <- f/g # weights
samples <- x*w
summary(samples)
par(cex = 0.8)
hist(samples, main = expression(frac(h(x)*f(x), g(x))))
summary(w)
hist(w, main = expression(frac(f(x),g(x))))
mean(samples) # estimated mean of a t distri with df 3, truncated at -4

## @knitr q2
theta <- function(x1, x2) atan2(x2, x1)/(2*pi)
f <- function(x1, x2, x3) {
  f1 <- 10*(x3 - 10*theta(x1, x2))
  f2 <- 10*(sqrt(x1^2+x2^2)-1)
  f3 <- x3
  return(f1^2+f2^2+f3^2)
}

library(fields)
x <- seq(-100, 100, 1)
par(mfrow = c(7, 3), mar = c(1.5, 1.5, 2, 1), oma = c(3, 3, 0.5, 1))
X_all <- expand.grid(x, x, x)
result_all <- f(X_all[, 1], X_all[, 2], X_all[, 3])
for (x3 in x[seq(1, length(x), 10)]) {
  result <- outer(x, x, function(x1, x2) f(x1, x2, x3))
  image(x, x, result, col = tim.colors(32))
  contour(x, x, result, levels = seq(min(result_all), max(result_all), by = 100000),
          add = TRUE, col = "black")
  mtext(paste("x3 =", x3), side = 3, line = 0.5)
}
mtext("x1", side = 1, line = 1, outer = TRUE)
mtext("x2", side = 2, line = 1, outer = TRUE)

fnew <- function(x) f(x[1], x[2], x[3])
X_init <- matrix(c(c(0, 0, 0),
                   c(-50, -50, -50),
                   c(50, 50, 50)), 3, 3, byrow = TRUE)
for (i in 1:nrow(X_init)) {
  cat("Starting values of x are:", paste(X_init[i, ], collapse = ", "), "\n")
  res <- optim(par = X_init[i, ], fnew)
  cat("Using optim...\n")
  cat("Values of x are that minimize the function:\n", paste(res$par, collapse = ", "), "\n")
  cat("Minimum of the function:", res$value, "\n")
  res2 <- nlm(fnew, p =  X_init[i, ])
  cat("Using nlm\n")
  cat("Values of x are that minimize the function:\n", paste(res2$estimate, collapse = ", "), "\n")
  cat("Minimum of the function:", res2$minimum, "\n")
  cat("--------------------------------------------------\n")
}

## @knitr q3c
# Simulate data
set.seed(1)
n <- 100
x <- rnorm(n)
beta0_true <- 2
beta1_true <- 0.5
sigma_true <- beta1_true*sqrt(sum((x - mean(x))^2))/3 # for signal to noise ratio of 3
ytemp <- beta0_true + beta1_true*x + rnorm(n, 0, sigma_true)
tau1 <- quantile(ytemp, 0.8) # tau for 20% exceedances
tau2 <- quantile(ytemp, 0.2) # tau for 80% exceedances
truncated1 <- ytemp > tau1
truncated2 <- ytemp > tau2
y1 <- pmin(tau1, ytemp)
y2 <- pmin(tau2, ytemp)
data1 <- list(x = x, y = y1, truncated = truncated1, tau = tau1)
data2 <- list(x = x, y = y2, truncated = truncated2, tau = tau2)

# Check signal to noise ratio
mod_temp <- summary(lm(ytemp ~ x))
signal_to_noise_ratio <- mod_temp$coefficients[2, 1]/mod_temp$coefficients[2, 2]
signal_to_noise_ratio

#' Perform the EM algorithm to estimate linear regression parameters for truncated data.
#' 
#' @param data List with variables x, y, truncated and tau.
#' @param num_max_iterations Maximum number of iterations before EM algorithm 
#' stops if convergence is still not reached.
#' @return List containing results of EM algorithm.
em_algorithm <- function(
  data, 
  num_max_iterations = 10000
) {
  # Initialise parameter values
  mod <- summary(lm(data$y ~ data$x))
  beta0_all <- beta0_prev <- mod$coefficients[1, 1]
  beta1_all <- beta1_prev <- mod$coefficients[2, 1]
  sigma_all <- sigma_prev <- mod$sigma*(n-2)/n
  
  x <- data$x
  xtrunc <- x[data$truncated == 1]
  ystar <- data$y
  q_theta_all <- NA
  q_theta_prev <- 0
  q_theta <- -10000
  i <- 1
  
  while (abs(q_theta - q_theta_prev) > 1e-10 & i <= num_max_iterations) {
    if (i > 1) {
      beta0_prev <- beta0
      beta1_prev <- beta1
      sigma_prev <- sigma
      q_theta_prev <- q_theta
    }
    
    # Compute E(Y_{trunc, i} | theta_t) for i = 1, ..., c
    mu_trunc <- beta0_prev + beta1_prev*xtrunc
    tau_star <- (data$tau - mu_trunc)/sigma_prev
    rho_tau_star <- dnorm(tau_star)/(1 - pnorm(tau_star))
    mean_ytrunc <- mu_trunc + sigma_prev*rho_tau_star
    ystar[data$truncated == 1] <- mean_ytrunc
    var_ytrunc <- (sigma_prev^2)*(1 + tau_star*rho_tau_star - rho_tau_star^2)
    
    # Regress ystar = {Yobs, E(Y_{trunc, i} | theta_t)} on {x}
    mod <- lm(ystar ~ x)
    beta0 <- mod$coefficients[1]
    beta1 <- mod$coefficients[2]
    
    # Compute sigma and q_theta
    rss_star <- sum((ystar - beta0 - beta1*x)^2) + sum(var_ytrunc) # corresponds to A + B + C in derivation
    sigma <- sqrt(rss_star/n)
    q_theta <- -(n/2)*log(2*pi*sigma^2) - rss_star/(2*sigma^2)

    # Save current values
    beta0_all <- c(beta0_all, beta0)
    beta1_all <- c(beta1_all, beta1)
    sigma_all <- c(sigma_all, sigma)
    q_theta_all <- c(q_theta_all, q_theta)
    i <- i + 1
  }
  cat(paste0("Results of EM algorithm:\nbeta0: ", tail(beta0_all, 1), "\n",
             "beta1: ", tail(beta1_all, 1), "\n",
             "sigma: ", tail(sigma_all, 1), "\n",
             "Q(theta|theta_t): ", tail(q_theta_all, 1), "\n"))
  return(list(beta0 = beta0_all, beta1 = beta1_all, sigma = sigma_all, 
              q_theta = q_theta_all))
}

em1 <- em_algorithm(data = data1)
em2 <- em_algorithm(data = data2)

## @knitr q3d
# Get initial parameter values
init1 <- c(beta0 = em1$beta0[1], beta1 = em1$beta1[1], log_sigma = log(em1$sigma[1]))
init2 <- c(beta0 = em2$beta0[1], beta1 = em2$beta1[1], log_sigma = log(em2$sigma[1]))

negloglikelihood <- function(par, data) {
  beta0 <- par[1]
  beta1 <- par[2]
  log_sigma <- par[3]
  mu <- beta0 + beta1*x 
  # Log likelihood function for censored data
  lik <- sum(dnorm(data$y[!data$truncated], mu[!data$truncated], exp(log_sigma), 
                   log = TRUE),
             pnorm(data$tau, mu[data$truncated], exp(log_sigma), 
                   log = TRUE, lower.tail = FALSE))
  return(-lik)
}

solve1 <- optim(par = init1, fn = negloglikelihood, data = data1, method = "BFGS", hessian = TRUE)
cov1 <- solve(solve1$hessian) # estimate of covariance matrix
sqrt(diag(cov1)) # standard errors of parameter estimates
solve2 <- optim(par = init2, fn = negloglikelihood, data = data2, method = "BFGS", hessian = TRUE)
cov2 <- solve(solve2$hessian) # estimate of covariance matrix
sqrt(diag(cov2)) # standard errors of parameter estimates
