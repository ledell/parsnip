#' @import rlang


make_classes <- function(prefix) {
  c(prefix, "model_spec")
}


check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms))
    stop("Please pass other arguments to the model function via `others`", call. = FALSE)
  terms
}

all_modes <- c("classification", "regression")


deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}

print_arg_list <- function(x, ...) {
  others <- c("name", "call", "expression")
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  res <- paste0("  ", names(x2), " = ", x2, collaspe = "\n")
  cat(res, sep = "")
}

model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    cat(print_arg_list(non_null_args), "\n", sep = "")
  }
  if (length(x$others) > 0) {
    cat("Engine-Specific Arguments:\n")
    cat(print_arg_list(x$others), "\n", sep = "")
  }
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$library) > 0) {
        if (length(x$method$library) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$library, collapse = ", "), "\n")
      }
    }
  }
}

load_libs <- function(x, quiet) {
  if (quiet) {
    for (pkg in x$method$library)
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  } else {
    for (pkg in x$method$library)
      library(pkg, character.only = TRUE)
  }
  invisible(x)
}

is_missing_arg <- function(x)
  identical(x, quote(missing_arg()))

show_call <- function(object) {
  call2(object$method$fit$func["fun"], !!!object$method$fit$args,
        .ns = object$method$fit$func["pkg"])
}

make_call <- function(fun, ns, args, ...) {

  # remove any null or placeholders (`missing_args`) that remain
  discard <-
    vapply(args, function(x)
      is_missing_arg(x) | is.null(x), logical(1))
  args <- args[!discard]

  if (!is.null(ns)) {
    out <- call2(fun, !!!args, .ns = ns)
  } else
    out <- call2(fun, !!!args)
  out
}

resolve_args <- function(args, ...) {
  for (i in seq(along = args)) {
    if (!is_missing_arg(args[[i]]))
      args[[i]] <- eval_tidy(args[[i]], ...)
  }
  args
}

levels_from_formula <- function(f, dat) {
  levels(eval_tidy(f[[2]], dat))
}

is_spark <- function(x)
  isTRUE(unname(x$method$fit$func["pkg"] == "sparklyr"))


has_exprs <- function(x) {
  if(is.null(x) | does_it_vary(x) | is_missing_arg(x))
    return(FALSE)
  is_symbolic(x) 
}

make_descr <- function(object) {
  if (length(object$args) > 0)
    expr_main <- map_lgl(object$args, has_exprs)
  else
    expr_main <- FALSE
  if (length(object$others) > 0)
    expr_others <- map_lgl(object$others, has_exprs)
  else
    expr_others <- FALSE
  any(expr_main) | any(expr_others)
}
