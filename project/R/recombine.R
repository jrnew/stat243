#' Recombine.
#' 
#' Carry out crossover of parent chromosomes in a mating pool.
#' 
#' @param pop_mating Matrix of population of chromosomes that form the mating pool.
#' @param pop_size Integer; Number of chromosomes in a generation.
#' @param method String; "onepoint", "twopoint", "uniform" (default); 
#' Type of crossover, at one point, at two points or uniformly (at all possible points).
#' @param prob_recombine Numeric, between 0 and 1; Default is 0.6; 
#' Probability of recombination.
#' @param do_parallel Logical; Default FALSE; Do in parallel?
#' @return Matrix of population of chromosomes resulting from recombination.
recombine <- function(
  pop_mating,
  pop_size,
  method = "uniform",
  prob_recombine = 0.6,
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop_mating))
  stopifnot(all(c(pop_mating) %in% c(0, 1)))
  stopifnot(is.numeric(pop_size))
  stopifnot(method %in% c("onepoint", "twopoint", "uniform"))
  stopifnot(is.logical(do_parallel))
  stopifnot(is.numeric(prob_recombine))
  stopifnot(prob_recombine >= 0 & prob_recombine <= 1)
  
  do_recombine <- as.logical(rbinom(pop_size, 1, prob_recombine))
  if (do_parallel) {
    pop_new <- foreach (i = 1:pop_size, .combine = rbind) %dopar% {
      indices_parents <- sample(1:nrow(pop_mating), size = 2)
      if (do_recombine[i]) {
        recombine_once(parent1 = pop_mating[indices_parents[1], ], 
                       parent2 = pop_mating[indices_parents[2], ], 
                       method = method)
      } else {
        pop_mating[indices_parents[1], ]
      }
    }
  } else {
    pop_new <- matrix(NA, pop_size, ncol(pop_mating))
    for (i in 1:pop_size) {
      indices_parents <- sample(1:nrow(pop_mating), size = 2)
      if (do_recombine[i]) {
        pop_new[i, ] <- recombine_once(parent1 = pop_mating[indices_parents[1], ], 
                                       parent2 = pop_mating[indices_parents[2], ], 
                                       method = method)
      } else {
        pop_new[i, ] <- pop_mating[indices_parents[1], ]
      }
    }
  }
  return(pop_new)
}

#' Recombine once.
#' 
#' Carry out crossover of two parent chromosomes to produce one child chromosome.
#' 
#' @param parent1 Integer vector of 1st parent chromosome containing 1's and 0's.
#' @param parent2 Integer vector of 2nd parent chromosome containing 1's and 0's.
#' @param method String; "onepoint", "twopoint", "uniform" (default); 
#' Type of crossover, at one point, at two points or uniformly (at all possible points).
#' @return Integer vector of child chromosome containing 1's and 0's.
recombine_once <- function(
  parent1,
  parent2,
  method = "uniform"
) {
  if (all(parent1 == parent2)) {
    # Crossover not necessary (has no effect) if both parents are identical
    child <- parent1
  } else {
    num_genes <- length(parent1)
    if (method == "onepoint") {
      breakpoint <- sample(1:num_genes, size = 1)
      if (breakpoint == num_genes) {
        child <- parent1
      } else {
        child <- c(parent1[1:breakpoint], parent2[(breakpoint + 1):num_genes])
      }
    } else if (method == "twopoint") {
      breakpoints <- sort(sample(1:num_genes, size = 2))
      if (breakpoints[2] == num_genes) {
        child <- c(parent1[1:breakpoints[1]], parent2[(breakpoints[1] + 1):breakpoints[2]])
      } else {
        child <- c(parent1[1:breakpoints[1]], parent2[(breakpoints[1] + 1):breakpoints[2]], 
                   parent1[(breakpoints[2] + 1):num_genes])
      }
    } else if (method == "uniform") {
      child <- ifelse(rbinom(num_genes, 1, 0.5) == 1, parent1, parent2)
    }
  }
  return(child)
}
