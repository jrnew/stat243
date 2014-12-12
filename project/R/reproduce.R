#' Wrapper function for reproduction stage.
#' 
#' Wrapper function for reproduction stage.
#' 
#' @param ga Object of class ga.
#' @param iteration Iteration number.
#' @return Updated ga list object.
reproduce <- function(
  ga,
  iteration,
  do_parallel = FALSE
) {
  stopifnot(class(ga) == "ga")
  stopifnot(is.numeric(iteration))
  stopifnot(iteration > 0)
  stopifnot(is.logical(do_parallel))
  
  # Create mating pool
  pop_mating <- select_for_mating(pop = ga$pop, 
                                  evaluation = ga$evaluation,
                                  method = ga$settings$method_select,
                                  do_parallel = do_parallel)
  # Do recombination/crossover
  pop_recombined <- recombine(pop_mating = pop_mating,
                              pop_size = ga$settings$pop_size,
                              method = ga$settings$method_recombine,
                              prob_recombine = ga$settings$prob_recombine,
                              do_parallel = do_parallel)
  # Carry out mutation
  pop_mutated <- mutate(pop = pop_recombined, 
                        prob_mutate = ga$settings$prob_mutate)
  #----------------------------------------------------------------------
  # Replace entire generation with child chromosomes
  ga$pop <- pop_mutated
  # Save results of current iteration
  evaluation_child <- evaluate(pop = ga$pop,
                               model_data = ga$model_data,
                               model = ga$settings$model,
                               glm_family = ga$settings$glm_family,
                               criterion = ga$settings$criterion,
                               criterion_function = ga$settings$criterion_function,
                               do_parallel = do_parallel)
  ga$evaluation <- evaluation_child
  # Save only the first "best" model as there may be multiple "best" models
  ga$log$models[iteration, ] <- ga$pop[which(ga$evaluation == min(ga$evaluation))[1], ] 
  ga$log$evaluation_best[iteration] <- min(ga$evaluation)[1]
  ga$log$evaluation_mean[iteration] <- mean(ga$evaluation)
  return(ga)
}
