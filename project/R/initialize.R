#' Initialize first generation of chromosomes.
#' 
#' Initialize first generation of chromosomes completely randomly.
#' 
#' @param pop_size Non-negative integer; Number of chromosomes in population.
#' @param num_vars Non-negative integer; Number of variables in model under consideration/
#' number of genes in each chromosome. 
#' @return A matrix of size pop_size x num_vars with 1's and 0's.
initialize <- function(
  pop_size,
  num_vars
) {
  stopifnot(is.numeric(pop_size))
  stopifnot(pop_size > 0)
  stopifnot(is.numeric(num_vars))
  stopifnot(num_vars > 0)
  
  pop <- matrix(sample(c(0, 1), size = pop_size*num_vars,
                       replace = TRUE),
                nrow = pop_size,
                ncol = num_vars)
  return(pop)
}
