#' Prints code Summary
#'
#' @param x a codeExplainer object created with the `summarize_code` function
#' @param ... other parameters
#'
#' @export
print.codeExplorer <- function(x, ...) {
  if (class(x)[2] == "single") {
    print_single_codeExplorer(x)
  } else {
      print_multiple_codeExplorer(x)
    }
  return(invisible(NULL))
}

print_single_codeExplorer <- function(x){
  cat("Libraries:", "\n    ", paste(x$libraries, collapse = ", "),"\n")
  cat("Functions:", "\n    ", paste(x$functions, collapse = ", "),"\n")
  cat("Variables:", "\n    ", paste(c(x$arrow_assignments[[2]], x$equality_assignments[[2]]), collapse = ", "),"\n\n")
}

print_multiple_codeExplorer <- function(x){
  libraries <- unlist(sapply(y[-1], function(x) x$libraries))
  functions <- unlist(sapply(y[-1], function(x) x$functions))
  arrow_assignments <- unlist(sapply(y[-1], function(x) x$arrow_assignments[[2]]))
  equality_assignments <- unlist(sapply(y[-1], function(x) x$equality_assignments[[2]]))
  cat("Libraries:", "\n    ", paste(libraries, collapse = ", "),"\n")
  cat("Functions:", "\n    ", paste(functions, collapse = ", "),"\n")
  cat("Variables:", "\n    ", paste(c(arrow_assignments,equality_assignments), collapse = ", "),"\n")
}
