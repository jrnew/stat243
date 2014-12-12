#' Plots results from the genetic algorithim.
#' 
#' Plots the best model evaluation criterion in each generation
#' against the generation iteration.
#' 
#' @param ga Object of class ga
#' @return Plots the best model evaluation criterion in each generation
#' against the generation iteration.
plot.ga <- function(
  ga
) {
  stopifnot(class(ga) == "ga")
  generation <- 1:ga$settings$num_max_iterations
  range_evaluation <- range(ga$log$evaluation_best)
  plot(ga$log$evaluation_best ~ generation,
       main = "Evolution of best model\nthrough generations",
       xlab = "Generation", 
       ylab = paste0("Best ", ga$settings$criterion),
       ylim = range_evaluation, type = "l", cex = 0.8)
  return(invisible())
}
