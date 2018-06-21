#' @title Plot of used libraries
#'
#' @description plot
#' 
#' @param codeExplorer an object of class codeExplorer to be plotted
#' @param ... other codeExplorers to be plotted
#' @param position position of bars
#' @param ploly indicates whenever plotly version should be plotted
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @export
plot_libraries <- function(codeExplorer, ..., position = "stack", plotly = FALSE){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_libraries_df(codeExplorer)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind(df, get_libraries_df(resp))
    }
  }
  
  p <- ggplot(df, aes(libs, Freq, fill = label)) +
      geom_col(position = position) + 
      ggtitle("Used Libraries")
  if(plotly==TRUE) return(ggplotly(p))
  return(p)
}

get_libraries_df <- function(codeExplorer){
  if(class(codeExplorer)[2]=="single") {
    libs <- codeExplorer$libraries
  } else {
    libs <- unlist(sapply(codeExplorer[-1], function(x) x$libraries))
  }
  res <- as.data.frame(table(libs))
  res$label <- codeExplorer$label
  
  return(res)
}
