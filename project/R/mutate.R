#' Mutate genes in the population.
#' 
#' Mutate each gene in the population at a pre-defined rate.
#' 
#' @param pop Matrix; Population of chromosomes.
#' @param prob_mutate Numeric, between 0 and 1; Default is 0.01; Probability of mutation.
#' @return Matrix of population of chromosomes that have undergone mutation.
mutate <- function(
  pop,
  prob_mutate = 0.01
) {
  stopifnot(is.matrix(pop))
  stopifnot(all(c(pop) %in% c(0, 1)))
  stopifnot(prob_mutate >= 0 & prob_mutate <= 1)
  
  num_genes_total <- prod(dim(pop))
  # Probabilty of getting a 1 is prob_mutate if gene is currently 0 and vice versa
  pop_mutate <- matrix(rbinom(num_genes_total, rep(1, num_genes_total), 
                              ifelse(c(pop) == 0, prob_mutate, 1 - prob_mutate)),
                       nrow(pop), ncol(pop))
  return(pop_mutate)
}
