#' Display summary of results from the genetic algorithim.
#' 
#' Outputs the top models selected from the genetic algorithm.
#' 
#' @param ga Object of class \code{ga}.
#' @param num_view Number of top models to display, default 5.
#' @return Prints summary of top models and associated value of 
#' model selection criterion.
summary.ga <- function(
  ga, 
  num_view = 5
) {
  stopifnot(class(ga) == "ga")
  stopifnot(is.numeric(num_view))
  
  models_evaluation <- unique(cbind(ga$log$models, ga$log$evaluation_best))
  models <- models_evaluation[, -ncol(models_evaluation)]
  evaluation <- models_evaluation[, ncol(models_evaluation)]
  if (nrow(models) < num_view) {
    cat("Not enough unique models to print, try use a smaller num_view value.")
  } else {
    rank <- order(evaluation)
    top_models <- models[rank, ][1:num_view, ]
    top_evaluation <- sort(evaluation)[1:num_view]
    for (i in 1:num_view) {
      if (all(top_models[i, ] == 0)) {
        xvars <- "Intercept"
      } else {
        x <- ga$model_data$xvars[top_models[i, ] == 1]
        xvars <- paste(x, collapse = " + ")
      }
      y <- ga$model_data$yvar
      cat("Model", i, ":\n",
          paste(c(y, xvars), collapse = " ~ "), "\n",
          ga$settings$criterion, "=", top_evaluation[i], "\n",
          "--------------------------------------------------\n")
      if (i == 1) {
        res <- list(formula = paste(c(y, xvars), collapse = " ~ "), 
                    xvars = x,
                    chromosome = top_models[i, ], 
                    evaluation = top_evaluation[i])
      }
    }
  }
  return(res)
}

