# ps7.R

# Question 4g
GetMatrices <- function(n, p) {
  Z <- matrix(rnorm(n^2), n, n)
  A <- crossprod(Z) # t(Z) %*% Z
  X <- matrix(rnorm(n*p), n, p)
  B <- A %*% X
  return(list(A = A, X = X, B = B))
}

set.seed(123)
for (n in c(100, 3000)) {
  for (p in c(1, 100, 3000)) {
    cat("n = ", n, "; p = ", p, "\n")
    matrices <- GetMatrices(n = n, p = p)
    # Method 1: Solve with LU decomposition
    print(system.time({
      X1 <- solve(matrices$A, matrices$B)
    }))
    
    # Method 2: Solve to find the inverse followed by matrix multiplication
    print(system.time({
      X2 <- solve(matrices$A) %*% matrices$B
    }))
    
    # Method 3: Cholesky decomposition
    print(system.time({
      U <- chol(matrices$A)
      X3 <- backsolve(U, backsolve(U, matrices$B, transpose = TRUE))
    }))
  }
}

#----------------------------------------------------------------------
# Question 5
GetBeta <- function(X, Y, A, b) {
  X_qr <- qr(X)
  C <- crossprod(qr.R(X_qr), qr.R(X_qr))
  Cinvd <- backsolve(qr.R(X_qr), crossprod(qr.Q(X_qr), Y))
  ARinv <- A %*% solve(qr.R(X_qr))
  ACinvA <- tcrossprod(AR, AR)
  ACinvd <- A %*% Cinvd
  betahat <- Cinvd + solve(C, crossprod(A, solve(ACinvA, -ACinvd + b)))
}
#----------------------------------------------------------------------
# Question 6
set.seed(6)
n <- 100
Z <- matrix(rnorm(n), n, n)
A <- crossprod(Z)
eigen_list <- eigen(A)
gamma <- cbind(eigen_list$vectors)

CreateMatrix <- function(gamma, eigenvalues) {
  lambda <- diag(eigenvalues)
  return(gamma %*% lambda %*% solve(gamma))
}

norm2 <- function(x)
  sqrt(sum(x^2))

num_trials <- 12
results_list <- list()
eigenvalues_ij <- results_ij <- matrix(NA, num_trials, n)
A_ijk <- array(NA, c(num_trials, n, n))
condition_number_i <- rep(NA, num_trials)

# Specify sets of eigenvalues to test with
eigenvalues_ij[1, ] <- rep(0.00001, n)
eigenvalues_ij[2, ] <- rep(1, n)
eigenvalues_ij[3, ] <- rep(100000, n)
eigenvalues_ij[4, ] <- seq(1, n)/100000
eigenvalues_ij[5, ] <- seq(1, n)
eigenvalues_ij[6, ] <- seq(1, n)*100000
eigenvalues_ij[7, ] <- seq(1, n*1000000000000, length.out = n)/100000
eigenvalues_ij[8, ] <- seq(1, n*1000000000000, length.out = n)
eigenvalues_ij[9, ] <- seq(1, n*1000000000000, length.out = n)*100000
eigenvalues_ij[10, ] <- seq(1, n*10000000000000, length.out = n)/100000
eigenvalues_ij[11, ] <- seq(1, n*10000000000000, length.out = n)
eigenvalues_ij[12, ] <- seq(1, n*10000000000000, length.out = n)*100000

# Create a new matrix A given eigenvectors and eigenvalues,
# then perform an eigendecomposition and calculate condition number
for (i in 1:num_trials) {
  A_ijk[i, , ] <- CreateMatrix(gamma = gamma, eigenvalues = eigenvalues_ij[i, ])
  results_ij[i, ] <- eigen(A_ijk[i, , ])$values
  condition_number_i[i] <- abs(max(eigenvalues_ij[i, ]))/abs(min(eigenvalues_ij[i, ]))
}

# Summarise results
is_positive_definite_i <- apply(Re(results_ij), 1, function(x) all(x > 0))
error_i <- apply(results_ij - eigenvalues_ij, 1, norm2)
condition_number_i
data.frame(condition_number = condition_number_i,
           magnitude = rep(c(1/100000, 1, 100000), nrow(eigenvalues_ij)/3),
           is_positive_definite = is_positive_definite_i,
           error = error_i)
