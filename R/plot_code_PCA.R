#' @title PCA of codes
#' @description Principal Component Analysis
#' 
#' @param codeExplorer an object of class codeExplorer to be plotted
#' @param ... other codeExplorers to be plotted
#' @param variables vector of variables to include
#'
#' @import ggplot2 factoextra FactoMineR
#'
#' @export
plot_codePCA <- function(codeExplorer, ..., variables = c("libraries", "functions", "mean_length_var", "naming")){
  if(class(codeExplorer)=="list"){
    dfl <- c(codeExplorer[-1],list(...))
    codeExplorer <- codeExplorer[[1]]
  } else{
    dfl <- list(...)
  }
  
  df <- get_codePCA_df(codeExplorer, variables)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- gtools::smartbind(df, get_codePCA_df(resp, variables))
    }
  }
  
  res.pca <- PCA(df[,!colnames(df)=="label"], graph=FALSE)
  fviz_pca_ind(res.pca, label="none", habillage =  factor(df$label), addEllipses = TRUE)
  
  
}

get_codePCA_df <- function(codeExplorer, variables){
  if(class(codeExplorer)[2]=="single") {
      results <- get_single_plotPCA_df(codeExplorer, variables)
    } else {
      results <- get_single_plotPCA_df(codeExplorer[[2]], variables)
      for(i in 3:length(codeExplorer)){
        results <- gtools::smartbind(results, get_single_plotPCA_df(codeExplorer[[i]], variables))
        results[is.na(results)] <- 0
        
      }
  }
  results$label <- codeExplorer$label
  return(results)
}

get_single_plotPCA_df <- function(codeExplorer, variables){
  naming <- functions <- libraries <- nchar_var <- mean_length_var <- NULL
  
  if("naming" %in% variables) naming <- codeExplorer$naming_convention
  if("libraries" %in% variables) libraries <- add_to_results(codeExplorer$libraries, "LIB_")
  if("functions" %in% variables) functions <- add_to_results(codeExplorer$functions, "FUNC_")
  nchar_var <- c(nchar(codeExplorer$arrow_assignments[[1]]), nchar(codeExplorer$arrow_assignments[[2]]),
                    nchar(codeExplorer$equality_assignments[[1]]), nchar(codeExplorer$equality_assignments[[2]]))
  if("mean_length_var" %in% variables) mean_length_var <- mean(nchar_var) 
    
  as.data.frame(t(c(mean_length_var, naming, libraries, functions)))
}


add_to_results <- function(values, prefix=""){
  if(length(values) == 0) return(NULL)
  results_df <- as.data.frame(table(values))
  results <- results_df$Freq
  names(results) <- paste0(prefix, results_df$values)
  return(results)
}












