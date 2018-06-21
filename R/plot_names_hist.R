#' @title Plot histogram names length
#'
#' @description histgram of unique names
#' 
#' @param codeExplorer an object of class codeExplorer to be plotted
#' @param ... other codeExplorers to be plotted
#'
#' @import ggplot2
#'
#' @export
plot_names_hist <- function(codeExplorer, ...){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_nameshist_df(codeExplorer)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind(df, get_nameshist_df(resp))
    }
  }
  ggplot(df, aes(x = length, fill = label), alpha = 0.5) +
    geom_histogram(position = 'identity') +
    ggtitle("Distribution of names length")
  
}

get_nameshist_df <- function(codeExplorer){
  if(class(codeExplorer)[2]=="single") {
    funcs <- unlist(c(codeExplorer$arrow_assignments, unique(codeExplorer$equality_assignents)))
  } else {
    funcs <- unlist(sapply(codeExplorer[-1], function(x) c(unique(x$arrow_assignments), unique(x$quality_assignents))))
  }
  funcs <- data.frame(variable = funcs)
  funcs$label <- codeExplorer$label
  funcs$length <- nchar(as.character(funcs$variable))
  
  return(funcs)
}



