#' @title Plot of used functions
#'
#' @description plot
#' 
#' @param codeExplorer an object of class codeExplorer to be plotted
#' @param ... other codeExplorers to be plotted
#' @param position position of bars
#'
#' @import ggplot2
#'
#' @export
plot_functions <- function(codeExplorer, ..., position = "stack"){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_functions_df(codeExplorer)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind(df, get_functions_df(resp))
    }
  }
  
  ggplot(df, aes(funcs, Freq, fill = label)) +
    geom_col(position = position) + 
    ggtitle("Used Functions") +
    coord_flip()
  
}

get_functions_df <- function(codeExplorer){
  if(class(codeExplorer)[2]=="single") {
    funcs <- codeExplorer$functions
  } else {
    funcs <- unlist(sapply(codeExplorer[-1], function(x) x$functions))
  }
  res <- as.data.frame(table(funcs))
  res$label <- codeExplorer$label
  
  return(res)
}
