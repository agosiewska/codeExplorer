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
plot_assignments <- function(codeExplorer, ...){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_assignments_df(codeExplorer)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind(df, get_assignments_df(resp))
    }
  }
  ggplot(df, aes(x = assign, y = Freq,  fill = label)) +
    geom_boxplot() +
    ggtitle("Assign types")
  
}

get_assignments_df <- function(codeExplorer){
  if(class(codeExplorer)[2]=="single") {
    arrows <- codeExplorer$arrow_assignments
    equality <- codeExplorer$equality_assignments
    funcs <- data.frame(arrow = length(arrows$functions)+length(arrows$variables), equality = length(equality$functions)+length(equality$variables))
  } else {
    funcs <- sapply(codeExplorer[-1], function(x) data.frame(arrow = length(x$arrow_assignments$functions)+length(x$arrow_assignments$variables), equality = length(x$equality_assignments$functions)+length(x$equality_assignments$variables)))
    funcs <- as.data.frame(t(funcs))
    funcs$arrow <- unlist(funcs$arrow)
    funcs$equality <- unlist(funcs$equality)
  }
  funcs <- tidyr::gather(funcs, assign, Freq, arrow:equality)
  funcs$label <- codeExplorer$label
  
  return(funcs)
}



