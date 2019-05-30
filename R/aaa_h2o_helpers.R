# h2o helpers --------------------------------------------------------------

#' Modeling via h2o
#'
#' `h2o_train` is a wrapper for `h2o` models to start an H2O cluster,
#'  convert data.frame input to H2OFrames and
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param h2o_fun The name of the h2o modeling function.
#' @param ... Other options to pass to `fun`.
#' @return A fitted `h2o` object.
#' @export
h2o_train <- function(
  x, y, h2o_fun = c("h2o.randomForest"), ...) {

  # If an H2O cluster is not already started by the user, start one
  h2o.init()
  #if (is.data.frame(x))  #what else could this be, do we need to check for matrix?
  # assumes input is data.frame for x and data.frame or vector for y
  x_h2oframe <- as.h2o(x)
  y_h2oframe <- as.h2o(y)

  # TO DO: check if we have to do something extra for classificaion (as.factor)
  train <- h2o.cbind(y_h2oframe, x_h2oframe)

  main_args <- list(
    y = 1,  #first column is response
    training_frame = quote(train)
  )

  call <- make_call(fun = h2o_fun, ns = "h2o", main_args)

  eval_tidy(call, env = current_env())
}
