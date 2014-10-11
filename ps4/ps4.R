#----------------------------------------------------------------------
# ps4.R
# JR New, 2014
#----------------------------------------------------------------------
# Problem 1
f1 <- function(x = {
    y <- 1
    2
  }, y = 0) {
  x + y
}
f1()

f2 <- function(x = {
  y <- 1
  2
}, y = 0) {
  y + x
}
f2()
#======================================================================
# Problem 2
CompareTime <- function(length_vector, num_select) {
  test_vector <- 1:length_vector
  indices_vector <- 1:num_select
  boolean_vector <- c(rep(TRUE, num_select), rep(FALSE, length_vector-num_select))
  time_indices <- system.time(test_vector[indices_vector])["elapsed"]
  time_boolean <- system.time(test_vector[boolean_vector])["elapsed"]
  return(c(time_indices = time_indices, time_boolean = time_boolean))
}
library(ggplot2)
# Examine how length of vector to subset affects timing
lengths <- c(seq(1, 9)*10000000, 100000000)
times_list <- lapply(lengths, CompareTime, num_select = 5000000)
theme_set(theme_bw(base_size = 10) + 
            theme(plot.title = element_text(vjust = 1),
                  axis.title.x = element_text(vjust = -0.3),
                  axis.title.y = element_text(vjust = 1.1)))
p <- ggplot(data.frame(length = rep(lengths, 2),
                  type = rep(c("indices", "boolean"), each = length(lengths)),
                  system_time = c(sapply(times_list, "[[", 1), sapply(times_list, "[[", 2))),
            aes_string(x = "length", y = "system_time", colour = "type")) +
  geom_line() + geom_point() +
  ggtitle("Elapsed time vs length of vector to subset\nfor a subset of 5000000 elements") +
  xlab("Length of vector to subset") + ylab("Elapsed time to subset vector (seconds)") + 
  scale_colour_discrete(name = "Subsetting based on",
                        breaks = c("indices", "boolean"),
                        labels = c("indices", "boolean"))
print(p)

# Examine how number of elements in subset affects timing
nums_select <- c(seq(1, 9)*10000000)
times_list2 <- lapply(nums_select, CompareTime, length_vector = 100000000)
p2 <- ggplot(data.frame(num_select = rep(nums_select, 2),
                        type = rep(c("indices", "boolean"), each = length(nums_select)),
                        system_time = c(sapply(times_list2, "[[", 1), sapply(times_list2, "[[", 2))),
             aes_string(x = "num_select", y = "system_time", colour = "type")) +
  geom_line() + geom_point() +
  ggtitle("Elapsed time vs number of elements to subset\nfor a vector of length 100000000") +
  xlab("Number of elements in subset") + ylab("Elapsed time to subset vector (seconds)") + 
  scale_colour_discrete(name = "Subsetting based on",
                        breaks = c("indices", "boolean"),
                        labels = c("indices", "boolean"))
print(p2)
#======================================================================
# Problem 3
# (a)
data_file <- "data/cpds.csv"
data <- read.csv(data_file)
.Internal(inspect(data))
data[1, colnames(data) == "year"] <- data[1, colnames(data) == "year"] + 1
.Internal(inspect(data))
# Memory addresses of all components of the data frame except for that changed column 
# remain the same.

# (b)
data <- read.csv(data_file)
data_list <- as.list(data)
.Internal(inspect(data_list))
data_list[["year"]][1] <- data_list[["year"]][1] + 1
.Internal(inspect(data_list))
# Check that data frame has been converted to a list
# str(data)
# str(data_list)
# Same
#======================================================================
# Problem 4
# Setting an rw class
setClass("rw",
         slots = c(x = "numeric", y = "numeric", num_steps = "numeric"),
         prototype = list(x = numeric(), y = numeric(), num_steps = numeric()),
         validity = function(object) {
           if (length(object@num_steps) != 1)
             return("Error: num_steps must be a scalar.")
           if (object@num_steps <= 0)
             stop("num_steps has to be a positive integer.")
           if (object@num_steps %% 1 != 0)
             stop("num_steps has to be an integer.")
           if (!(all(length(object@x) == length(object@y), length(object@x) == (object@num_steps + 1))))
             return("Error: Lengths of x and y must be equal to num_steps+1.")
           return(TRUE)
         })
#' Generate a 2D random walk.
#' 
#' Generate a discrete random walk in two dimensions with uniform probability of moving up, down,
#' left, right by 1 unit at each step (+/- 1 on the x- or y-axis).
#' 
#' @param num_steps Number of steps of the random walk to generate.
#' @param full_walk \code{TRUE} return full random walk, \code{FALSE} to return final position.
#' @return A list object containing: 
#' \describe{
#'   \item{\code{x}}{Integer vector of length \code{num_steps+1} of x-coordinates of the random walk 
#'   if \code{full_walk} is TRUE and of length \code{1} of final x-coordinate if \code{full_walk} is FALSE.}
#'   \item{\code{y}}{Integer vector of length \code{num_steps+1} of y-coordinates of the random walk
#'   if \code{full_walk} is TRUE and of length \code{1} of final y-coordinate if \code{full_walk} is FALSE.}
#' }
Generate2DRandomWalk <- function(num_steps, full_walk = TRUE) {
  # Return error if num_steps is not a scalar, is negative or zero and warning if it is not an integer
  if (length(num_steps) != 1)
    stop("num_steps must be a scalar.")
  if (num_steps <= 0)
    stop("num_steps has to be a positive integer.")
  if (num_steps %% 1 != 0) {
    num_steps <- as.integer(num_steps)
    warning("num_steps has been converted to an integer.")
  }
  # Return error if full_walk is not logical
  if (!is.logical(full_walk))
    stop("full_walk has to be TRUE or FALSE")
  # Do random walk
  units_x <- c(0, 0, -1, 1)
  units_y <- c(-1, 1, 0, 0)
  step_indices <- sample(1:4, num_steps, replace = TRUE)
  steps_x <- units_x[step_indices]
  steps_y <- units_y[step_indices]
  x <- c(0, cumsum(steps_x))
  y <- c(0, cumsum(steps_y))
  if (full_walk) {
    rw <- new("rw", x = x, y = y, num_steps = num_steps)
  } else {
    rw <- list(x = tail(x, n = 1), y = tail(y, n = 1), num_steps = num_steps)
  }
  return(rw)
}
# Method print
print.rw <- function(x) {
  cat(paste0("Final x-coordinate: ", tail(x@x, n = 1), "\n",
             "Final y-coordinate: ", tail(x@y, n = 1), "\n",
             "Maximum distance moved along x-axis: ", 
             max(x@x) - min(x@x), "\n",
             "Maximum distance moved along y-axis: ", 
             max(x@y) - min(x@y), "\n"))
}
setMethod("print", signature(x = "rw"), definition = print.rw)

# Method plot
plot.rw <- function(x, y) {
  theme_set(theme_bw(base_size = 10) + 
              theme(plot.title = element_text(vjust = 1),
                    axis.title.x = element_text(vjust = -0.3),
                    axis.title.y = element_text(vjust = 1.1)))
  p <- ggplot(data.frame(x = x@x, y = x@y),
              aes(x = x, y = y)) + geom_path(alpha = 0.5) + 
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_point(x = x@x[1], y = x@y[1]) + # starting position
    geom_point(x = x@x[x@num_steps+1], y = x@y[x@num_steps+1], colour = "red") + # ending position
    ggtitle(paste0("2D random walk of length ", x@num_steps)) +
    xlim(range(x@x)) + ylim(range(x@y)) + 
    coord_fixed()
  print(p)
}
setMethod("plot", signature(x = "rw", y = "missing"), definition = plot.rw)

# Method [
setMethod("[", signature(x = "rw"),
          function(x, i) {
            if (i < 0 | i > x@num_steps)
              stop("i must be between 0 and num_steps (inclusive).")
            return(list(x = x@x[i+1], y = x@y[i+1]))
          })

# Method start
setGeneric("start<-", function(x, ...) {
  standardGeneric("start<-")
})
`start.rw<-` <- function(x, value) {
  x@x <- x@x + value[1]
  x@y <- x@y + value[2]
  return(x)
}
setReplaceMethod("start", signature(x = "rw"),
                 definition = `start.rw<-`)

# Extra: method animate
# With help from http://assemblingnetwork.wordpress.com/2014/03/24/random-walks-in-r/
animate.rw <- function(x, gif_file) {
  if (!require(animation))
    install.packages("animation")
  library(animation)
  if (file.exists(gif_file))
    stop("gif_file already exists! Specify a new file name.")
  range_x <- max(x@x) - min(x@x)
  range_y <- max(x@y) - min(x@y)
  saveGIF({
    for (i in 2:(x@num_steps+1)) {
      p <- ggplot(data.frame(x = x@x[1:i], y = x@y[1:i]),
                  aes(x = x, y = y)) + geom_path(alpha = 0.5) + 
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        geom_point(x = x@x[1], y = x@y[1]) + # starting position
        geom_point(x = x@x[i], y = x@y[i], colour = "red") + # ending position
        ggtitle(paste0("2D random walk of length ", x@num_steps)) +
        xlim(range(x@x)) + ylim(range(x@y)) + 
        coord_fixed()
      print(p)
    }},
    movie.name = gif_file, interval = 0.1, nmax = 1000, 
    ani.width = (range_x/max(range_x, range_y))*400, 
    ani.height = (range_y/max(range_x, range_y))*400,
    outdir = getwd()
  )
}
setGeneric("animate", function(x, ...) {
  standardGeneric("animate")
})
setMethod("animate", signature(x = "rw"), definition = animate.rw)
#----------------------------------------------------------------------
rw_object <- Generate2DRandomWalk(num_steps = 1000)
# Call class-specific methods
print(rw_object)
plot(rw_object)
rw_object[rw_object@num_steps]
start(rw_object) <- c(1000, 2000)
plot(rw_object)

# Extra: Animating the plot
rw_object_small <- Generate2DRandomWalk(num_steps = 100)
animate(rw_object_small, gif_file = "rw_small.gif")
#======================================================================
# Problem 5
library(inline)
# this code is simply a placeholder to demonstrate that I can
# modify the input arguments as desired in C;
# in reality 'src' would contain substantive computations
src <- '
tablex[0] = 7;
'

dummyFun <- cfunction(signature(tablex = "integer", tabley = "integer",
                                xvar = "integer", yvar = "integer", useline = "integer",
                                n = "integer"), src, convention = ".C")

fastcount <- function(xvar,yvar) {
    print(gc())
    print(paste("Object size of xvar:", object.size(xvar)))
    print(paste("Object size of yvar:", object.size(yvar)))
  nalineX <- is.na(xvar)
    print("nalineX <- is.na(xvar)")
    print(gc())
    print(paste("Object size of nalineX:", object.size(nalineX)))
  nalineY <- is.na(yvar)
    print("nalineY <- is.na(yvar)")
    print(gc())
    print(paste("Object size of nalineY:", object.size(nalineY)))
  xvar[nalineX | nalineY] <- 0
    print("xvar[nalineX | nalineY] <- 0")
    print(gc())
    print(paste("Object size of nalineX | nalineY:", object.size(nalineX | nalineY)))
    print(paste("Object size of xvar[nalineX | nalineY]:", object.size(xvar[nalineX | nalineY])))
  yvar[nalineX | nalineY] <- 0
    print("yvar[nalineX | nalineY] <- 0")
    print(gc())
    print(paste("Object size of yvar[nalineX | nalineY]:", object.size(yvar[nalineX | nalineY])))
  useline <- !(nalineX | nalineY)
    print("useline <- !(nalineX | nalineY)")
    print(gc())
    print(paste("Object size of useline:", object.size(useline)))
  tablex <- numeric(max(xvar)+1)
    print("tablex <- numeric(max(xvar)+1)")
    print(gc())
    print(paste("Object size of tablex:", object.size(tablex)))
  tabley <- numeric(max(yvar)+1)
    print("tabley <- numeric(max(yvar)+1)")
    print(gc())
    print(paste("Object size of tabley:", object.size(tabley)))
  stopifnot(length(xvar) == length(yvar))
    print("stopifnot(length(xvar) == length(yvar))")
    print(gc())
    print(paste("Object size of length(xvar) == length(yvar):", object.size(length(xvar) == length(yvar))))
  res <- dummyFun(
    tablex = as.integer(tablex), tabley = as.integer(tabley),
    as.integer(xvar), as.integer(yvar), as.integer(useline),
    as.integer(length(xvar)))
    print("res <- dummyFun()")
    print(gc())
    print(paste("Object size of as.integer(tablex):", object.size(as.integer(tablex))))
    print(paste("Object size of as.integer(tabley):", object.size(as.integer(tabley))))
    print(paste("Object size of as.integer(xvar):", object.size(as.integer(xvar))))
    print(paste("Object size of as.integer(yvar):", object.size(as.integer(yvar))))
    print(paste("Object size of as.integer(useline):", object.size(as.integer(useline))))
  xuse <- which(res$tablex > 0)
    print("xuse <- which(res$tablex > 0)")
    print(gc())
    print(paste("Object size of res$tablex > 0:", object.size(res$tablex > 0)))
    print(paste("Object size of xuse:", object.size(xuse)))
  xnames <- xuse - 1
    print("xnames <- xuse - 1")
    print(gc())
    print(paste("Object size of xnames:", object.size(xnames)))
  resb <- rbind(res$tablex[xuse], res$tabley[xuse])
    print("resb <- rbind(res$tablex[xuse], res$tabley[xuse])")
    print(gc())
    print(paste("Object size of res$tablex[xuse]:", object.size(res$tablex[xuse])))
    print(paste("Object size of res$tabley[xuse]:", object.size(res$tabley[xuse])))
    print(paste("Object size of resb:", object.size(resb)))
  colnames(resb) <- xnames
    print("colnames(resb) <- xnames")
    print(gc())
  return(resb)
}

n <- 1e7
set.seed(1)
xvar <- sample(c(seq(1, 20, by = 1), NA), n, replace = TRUE)
yvar <- sample(c(seq(1, 20, by = 1), NA), n, replace = TRUE)
resb <- fastcount(xvar, yvar)
#======================================================================
# Problem 6
load('ps4prob6.Rda') # should have A, n, K

# Original code
ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}

oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) { 
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (z in 1:K) {
        if (theta.old[i, z]*theta.old[j, z] == 0){
          q[i, j, z] <- 0
        } else {
          q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /
            Theta.old[i, j]
        }
      }
    }
  }
  theta.new <- theta.old
  for (z in 1:K) {
    theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z]))
  }
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check)) 
}

# Improved and optimised code
likelihood <- function(Theta, A) {
  sum.ind <- which(A == 1)
  log.likelihood <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(log.likelihood)
}

update_once <- function(A, n, K, theta.old, threshold = 0.1) { 
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- likelihood(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  theta.new <- theta.old
  for (z in 1:K) {
    q[, , z] <- outer(theta.old[, z], theta.old[, z])/Theta.old
    theta.new[, z] <- rowSums(A*q[, , z])/sqrt(sum(A*q[, , z]))
  }
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- likelihood(Theta.new, A)
  converge.check <- abs(L.new - L.old) < threshold
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check)) 
}

# Initialize the parameters at random starting values
set.seed(1234)
temp <- matrix(runif(n*K), n, K)
theta.init <- temp/rowSums(temp)

# Comparison
system.time(out <- oneUpdate(A, n, K, theta.init))
system.time(out2 <- update_once(A, n, K, theta.init))
identical(out, out2)

