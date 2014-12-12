#' Select chromosomes for recombination.
#' 
#' Select chromosomes for recombination based on fitness.
#' 
#' @param pop Matrix; Population of chromosomes.
#' @param evaluation Numeric vector; Evaluation values of all chromosomes in population.
#' @param method String; "rank" (linear rank selection) (default) or
#' "tournament"; Method to select chromosomes for inclusion in mating pool.
#' @param do_parallel Logical; Default FALSE; Do in parallel?
#' @return Matrix of population of chromosomes that form the mating pool.
select_for_mating <- function(
  pop,
  evaluation,
  method = "rank",
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop))
  stopifnot(all(c(pop) %in% c(0, 1)))
  stopifnot(is.vector(evaluation))
  stopifnot(is.numeric(evaluation))
  stopifnot(method %in% c("rank", "tournament"))
  stopifnot(is.logical(do_parallel))
  
  mating_pool_size <- nrow(pop)
  indices_select <- rep(NA, mating_pool_size)
  if (method  == "rank") {
    # Select chromosomes randomly with probability proportional to 
    # their relative rank
    # Assign rank 1 to chromosome with highest (worst) evaluation criterion
    # and vice versa
    rank <- order(evaluation, decreasing = TRUE)
    fitness <- rank/sum(rank)
    cum_fitness <- cumsum(fitness)
    # Generate a random number rand in range [0, 1]. 
    # If all elements of cum_fitness < rand, select last index.
    # Else select first index of cum_fitness with element >= rand 
    if (do_parallel) {
      indices_select <- foreach (i = 1:mating_pool_size, .combine = c) %dopar% {
        rand <- runif(1)
        ifelse(all(cum_fitness < rand), length(cum_fitness), 
               which(cum_fitness >= rand)[1])
      }
    } else {
      for (i in 1:mating_pool_size) {
        rand <- runif(1)
        indices_select[i] <- ifelse(all(cum_fitness < rand), 
                                    length(cum_fitness), 
                                    which(cum_fitness >= rand)[1])
      }
    }
  } else if (method == "tournament") {
    # Select with replacement for each tournament two chromosomes
    indices_tournament <- replicate(mating_pool_size, 
                                    sample(1:nrow(pop), 2, replace = FALSE))
    indices_select <- ifelse(evaluation[indices_tournament[1, ]] >= 
                               evaluation[indices_tournament[2, ]],
                             indices_tournament[1, ], indices_tournament[2, ])
  }
  pop_mating <- pop[indices_select, ]
  return(pop_mating)
}
