# Load all libraries
if (!require(doParallel)) {
  install.packages("doParallel")
}
if (!require(foreach)) {
  install.packages("foreach")
}
if (!require(MASS)) {
  install.packages("MASS")
}
if (!require(testthat)) {
  install.packages("testthat")
}
library(doParallel)
library(foreach)
library(MASS)
library(testthat)

#' Do evaluation.
#' 
#' Do evaluation for chromosomes in population by calculating model selection criterion.
#' 
#' @param pop Matrix of population of chromosomes.
#' @param model_data Object of class model_data.
#' @param model Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param glm_family Character if model is "glm", NULL otherwise; 
#' "binomial", "gaussian" (default), "Gamma", "inverse.gaussian", "poisson", "quasi",
#' "quasibinomial", "quasipoisson"; A family function that gives the error 
#' distribution and link function to be used in the model.
#' @param criterion Character; "AIC" (default), "BIC" or some user-defined criterion; 
#' Model selection criterion to minimize.
#' @param criterion_function NULL if criterion is "AIC" or "BIC", user-defined
#' function in the global environment otherwise.
#' @param do_parallel Logical; Default FALSE; Do in parallel?
#' @return Numeric vector; Evaluation values for all chromosomes 
#' in the current generation.
evaluate <- function(
  pop,
  model_data,
  model = "lm",
  glm_family = NULL,
  criterion = "AIC",
  criterion_function = NULL,
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop))
  stopifnot(all(c(pop) %in% c(0, 1)))
  stopifnot(is.list(model_data))
  stopifnot(model %in% c("lm", "glm"))
  if (!(criterion %in% c("AIC", "BIC"))) {
    stopifnot(is.function(criterion_function))
  }
  stopifnot(is.logical(do_parallel))
  if (model == "glm") {
    stopifnot(!is.null(glm_family))
    stopifnot(is.character(glm_family))
    stopifnot(glm_family %in%
                c("binomial", "gaussian", "Gamma", "inverse.gaussian",
                  "poisson", "quasi", "quasibinomial", "quasipoisson"))
  }
  if (do_parallel) {
    evaluation <- foreach (i = 1:nrow(pop), .combine = c) %dopar%
      evaluate_once(model_data = model_data, 
                    xvars_select = as.logical(pop[i, ]),
                    model = model, glm_family = glm_family,
                    criterion = criterion, 
                    criterion_function = criterion_function)
  } else {
    evaluation <- rep(NA, nrow(pop))
    for (i in 1:nrow(pop))
      evaluation[i] <- evaluate_once(model_data = model_data, 
                                     xvars_select = as.logical(pop[i, ]),
                                     model = model, glm_family = glm_family,
                                     criterion = criterion,
                                     criterion_function = criterion_function)
  }  
  return(evaluation)
}

#' Do evaluation once.
#' 
#' Do evaluation for a chromosome by calculating model selection criterion.
#' 
#' @param model_data; Object of class model_data.
#' @param xvars_select; Logical vector; 
#' @param model Character; "lm" (default) or "glm"; Linear model or generalized linear model.
#' @param glm_family Character if model is "glm", NULL otherwise; 
#' "binomial", "gaussian" (default), "Gamma", "inverse.gaussian", "poisson", "quasi",
#' "quasibinomial", "quasipoisson"; A family function that gives the error 
#' distribution and link function to be used in the model.
#' @param criterion Character; "AIC" (default), "BIC" or some user-defined criterion; 
#' Model selection criterion to minimize.
#' @param criterion_function NULL if criterion is "AIC" or "BIC", user-defined
#' function in the global environment otherwise.
#' @return Numeric; Value of criterion.
evaluate_once <- function(
  model_data,
  xvars_select,
  model = "lm",
  glm_family = NULL,
  criterion = "AIC",
  criterion_function = NULL
) {
  stopifnot(model %in% c("lm", "glm"))
  if (!(criterion %in% c("AIC", "BIC"))) {
    stopifnot(is.function(criterion_function))
  }
  if (model == "glm") {
    stopifnot(!is.null(glm_family))
    stopifnot(is.character(glm_family))
    stopifnot(glm_family %in%
                c("binomial", "gaussian", "Gamma", "inverse.gaussian",
                  "poisson", "quasi", "quasibinomial", "quasipoisson"))
  }
  
  mod_formula <- as.formula(paste(
    model_data$yvar, "~", 
    ifelse(sum(xvars_select) == 0, 1,
           paste(model_data$xvars[xvars_select], collapse = " + "))))
  if (model == "lm") {
    mod <- lm(mod_formula, data = model_data$data)
  } else if (model == "glm") {
    mod <- glm(mod_formula, family = glm_family, data = model_data$data)
  }
  if (criterion == "AIC") {
    result <- AIC(mod)
  } else if (criterion == "BIC") {
    result <- BIC(mod)
  } else {
    fun <- function(fun, mod) return(fun(mod))
    result <- try(fun(criterion_function, mod))
    if (class(result) == "try-error")
      stop(paste0("Error in ", criterion_function, ". ",
                  "Please check that function inputs and outputs are valid."))
    if (!is.numeric(result))
      stop(paste0(criterion_function, " does not return a numeric value!"))
  }
  return(result)
}
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
#' Carry out model selection with a genetic algorithm.
#' 
#' Main function for carrying out model selection with a genetic algorithm.
#' 
#' @param data Data frame
#' @param yvar Character; Name of column containing response variable
#' @param xvars Character vector; Default is all column names that are not yvar;
#' Name(s) of column(s) containing set of explanatory variables to select on.
#' @param model Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param glm_family Character if model is "glm", NULL otherwise; 
#' "binomial", "gaussian" (default), "Gamma", "inverse.gaussian", "poisson", "quasi",
#' "quasibinomial", "quasipoisson"; A family function that gives the error 
#' distribution and link function to be used in the model.
#' @param criterion Character; "AIC" (default), "BIC" or some user-defined criterion; 
#' Model selection criterion to minimize.
#' @param criterion_function NULL if criterion is "AIC" or "BIC", user-defined
#' function in the global environment otherwise.
#' @param pop_size Integer; Default is 100; Number of chromosomes per generation.
#' @param method_select String; "rank" (linear rank selection) (default) or
#' "tournament"; Method to select chromosomes for inclusion in mating pool.
#' @param method_recombine String; "onepoint", "twopoint", "uniform" (default); 
#' Type of crossover, at one point, at two points or uniformly (at all possible points).
#' @param prob_recombine Numeric, between 0 and 1; Default is 0.6; 
#' Probability of recombination.
#' @param prob_mutate Numeric, between 0 and 1; Default is 0.01; 
#' Probability of mutation.
#' @param num_max_iterations Non-negative integer; Default is 100; 
#' Maximum number of iterations before algorithm is stopped.
#' @param seed Non-negative integer; Default is 123; Random seed 
#' for reproducibility.
#' @param do_parallel Logical; Default is FALSE; Do in parallel?
select <- function(
  data,
  yvar,
  xvars = NULL,
  model = "lm",
  glm_family = NULL,
  criterion = "AIC",
  criterion_function = NULL,
  pop_size = 100L,
  method_select = "rank",
  method_recombine = "uniform",
  prob_recombine = 0.6,
  prob_mutate = 0.01,
  num_max_iterations = 100L,
  seed = 123,
  do_parallel = FALSE
) {
  stopifnot(is.data.frame(data))
  stopifnot(model %in% c("lm", "glm"))
  if (!(criterion %in% c("AIC", "BIC"))) {
    stopifnot(is.function(criterion_function))
  }
  stopifnot(is.numeric(pop_size))
  stopifnot(method_select %in% c("rank", "tournament"))
  stopifnot(method_recombine %in% c("onepoint", "twopoint", "uniform"))
  stopifnot(is.numeric(prob_mutate))
  stopifnot(prob_mutate >= 0 & prob_mutate <= 1)
  stopifnot(is.numeric(prob_recombine))
  stopifnot(prob_recombine >= 0 & prob_recombine <= 1)
  stopifnot(is.numeric(num_max_iterations))
  stopifnot(num_max_iterations >= 10)
  stopifnot(is.logical(do_parallel))
  if (model == "glm") {
    stopifnot(!is.null(glm_family))
    stopifnot(is.character(glm_family))
    stopifnot(glm_family %in%
                c("binomial", "gaussian", "Gamma", "inverse.gaussian",
                  "poisson", "quasi", "quasibinomial", "quasipoisson"))
  }
  
  set.seed(seed)
  if (do_parallel)
    registerDoParallel(cores = detectCores())
  settings <- list(model = model,
                   glm_family = glm_family,
                   criterion = criterion,
                   criterion_function = criterion_function,
                   pop_size = pop_size,
                   method_select = method_select,
                   method_recombine = method_recombine,
                   prob_recombine = prob_recombine,
                   prob_mutate = prob_mutate,
                   num_max_iterations = num_max_iterations,
                   seed = seed)
  model_data <- process_data(data = data, yvar = yvar, xvars = xvars)
  class(model_data) <- "model_data"
  cat(paste0("Initializing population...\n"))
  pop <- initialize(pop_size = pop_size, num_vars = model_data$num_vars)
  log <- list(models = matrix(NA, nrow = num_max_iterations, 
                              ncol = model_data$num_vars),
              evaluation_best = rep(NA, num_max_iterations),
              evaluation_mean = rep(NA, num_max_iterations))
  evaluation <- evaluate(pop = pop,
                         model_data = model_data,
                         model = settings$model,
                         glm_family = settings$glm_family,
                         criterion = settings$criterion,
                         criterion_function = settings$criterion_function,
                         do_parallel = do_parallel)
  ga <- list(settings = settings,
             model_data = model_data,
             pop = pop,
             evaluation = evaluation,
             log = log)
  class(ga) <- "ga"
  iteration <- 1L
  while (iteration <= num_max_iterations) {
    cat(paste0("Producing generation ", iteration, "...\n"))
    ga <- reproduce(ga = ga, iteration = iteration, do_parallel = do_parallel)
    iteration <- iteration + 1L
  }
  cat("Model selection complete!\n")
  return(ga)
}
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

#' Testing function.
#' 
#' Conduct unit tests and compare output of test data with output of stepAIC function.
#' 
#' @return Prints summary of top models and associated value of 
#' model selection criterion.
test <- function() {
  test_dir("testing")
  data <- read.table("data/video.txt", header = TRUE, quote = "\"")
  ga <- select(data, 
               yvar = "grade",
               pop_size = nrow(data)*2,
               num_max_iterations = 50, 
               model = "glm",
               glm_family = "gaussian")
  res <- summary(ga)
  plot(ga)
  mod <- glm(grade ~ ., data = data) 
  res_step <- stepAIC(mod)
  cat("Testing done!\n")
  return(invisible())
}


