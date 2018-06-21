#' @title Plot of naming convention functions
#'
#' @description plot
#' 
#' @param codeExplorer an object of class codeExplorer to be plotted
#' @param ... other codeExplorers to be plotted
#'
#' @import ggplot2
#'
#' @export
plot_naming_convention <- function(codeExplorer, ...){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_namingconv_df(codeExplorer)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind(df, get_namingconv_df(resp))
    }
  }
  ggplot(df, aes(x = convention, y = Freq, fill = label)) +
    geom_boxplot() +
    ggtitle("Convencions of naming functions")
  
}

get_namingconv_df <- function(codeExplorer){
  if(class(codeExplorer)[2]=="single") {
    funcs <- as.data.frame(t(codeExplorer$naming_convention))
  } else {
    funcs <- t(unlist(sapply(codeExplorer[-1], function(x) x$naming_convention)))
    funcs <- as.data.frame(funcs)
    if (length(rownames(funcs)) ==  length(unique(names(funcs))))rownames(funcs) <- names(codeExplorer)[-1]
  }
  funcs$label <- codeExplorer$label
  res <- tidyr::gather(funcs, convention, Freq, dot:mixed)
  
  return(res)
}



