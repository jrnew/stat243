#' Process data for input into genetic algorithm.
#' 
#' Process data for input into genetic algorithm.
#' 
#' @param data Data frame
#' @param yvar Character; Name of column containing response variable.
#' @param xvars Character vector; Default is all column names that are not yvar;
#' Name(s) of column(s) containing set of explanatory variables to select on.
#' @return A list object named model_data containing:
#' \describe{
#'   \item{data}{Data frame; Processed data with only relevant columns.}
#'   \item{yvar}{Character; Name of column containing response variable.}
#'   \item{xvars}{Character vector; Name(s) of column(s) containing 
#'   set of explanatory variables to select on.}
#'   \item{num_vars}{Integer; Length of xvars.}   
#' } 
process_data <- function(
  data,
  yvar,
  xvars = NULL
) {
  vars_all <- colnames(data)
  stopifnot(yvar %in% vars_all)
  if (!is.null(xvars)) {
    stopifnot(is.character(xvars))
  }
  
  vars <- vars_all[!(vars_all %in% yvar)]
  # If xvars is NULL, set xvars = vars, else check that all
  # xvars can be found in column names of data.
  if (is.null(xvars)) {
    xvars <- vars
  } else if (any(!(xvars %in% vars))) {
    stop(paste("The following xvars do not correspond to", 
               "any column names in data:",
               xvars[!(xvars %in% vars)], collapse = ", "))
  }
  stopifnot(length(xvars) > 1)
  data_proc <- subset(data, select = c(yvar, xvars))
  model_data <- list(data = data_proc,
                     yvar = yvar,
                     xvars = xvars,
                     num_vars = length(xvars))
  return(model_data)
}

