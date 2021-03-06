% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_reg.R
\name{logistic_reg}
\alias{logistic_reg}
\alias{update.logistic_reg}
\title{General Interface for Logistic Regression Models}
\usage{
logistic_reg(mode = "classification", regularization = NULL,
  mixture = NULL, others = list(), ...)

\method{update}{logistic_reg}(object, regularization = NULL, mixture = NULL,
  others = list(), fresh = FALSE, ...)
}
\arguments{
\item{mode}{A single character string for the type of model.
The only possible value for this model is "classification".}

\item{regularization}{An non-negative number representing the
total amount of regularization (\code{glmnet} and \code{spark} only).}

\item{mixture}{A number between zero and one (inclusive) that
represents the proportion of regularization that is used for the
L2 penalty (i.e. weight decay, or ridge regression) versus L1
(the lasso) (\code{glmnet} and \code{spark} only).}

\item{others}{A named list of arguments to be used by the
underlying models (e.g., \code{stats::glm},
\code{rstanarm::stan_glm}, etc.). These are not evaluated
until the model is fit and will be substituted into the model
fit expression.}

\item{...}{Used for S3 method consistency. Any arguments passed to
the ellipses will result in an error. Use \code{others} instead.}

\item{object}{A logistic regression model specification.}

\item{fresh}{A logical for whether the arguments should be
modified in-place of or replaced wholesale.}
}
\value{
An updated model specification.
}
\description{
\code{logistic_reg} is a way to generate a \emph{specification} of a model
before fitting and allows the model to be created using
different packages in R, Stan, or via Spark. The main arguments for the
model are:
\itemize{
\item \code{regularization}: The total amount of regularization
in the model. Note that this must be zero for some engines.
\item \code{mixture}: The proportion of L1 regularization in
the model. Note that this will be ignored for some engines.
}
These arguments are converted to their specific names at the
time that the model is fit. Other options and argument can be
set using the \code{others} argument. If left to their defaults
here (\code{NULL}), the values are taken from the underlying model
functions.

If parameters need to be modified, this function can be used
in lieu of recreating the object from scratch.
}
\details{
The data given to the function are not saved and are only used
to determine the \emph{mode} of the model. For \code{logistic_reg},the
mode will always be "classification".

The model can be created using the \code{fit()} function using the
following \emph{engines}:
\itemize{
\item \pkg{R}:  \code{"glm"} or \code{"glmnet"}
\item \pkg{Stan}:  \code{"stan"}
\item \pkg{Spark}: \code{"spark"}
}
}
\examples{
logistic_reg()
# Parameters can be represented by a placeholder:
logistic_reg(regularization = varying())
model <- logistic_reg(regularization = 10, mixture = 0.1)
model
update(model, regularization = 1)
update(model, regularization = 1, fresh = TRUE)
}
\seealso{
\code{\link[=varying]{varying()}}, \code{\link[=fit]{fit()}}
}
