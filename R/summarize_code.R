#' @title Summary of the code
#' 
#' @description summarize_code extracts libraries, functions and variables used in code.
#' 
#' @param code Vector of stings, each code line should be a separate string.
#' @param ... additional vectors with code codes 
#' @param label character - the name of the explorer. By default it's extracted from the first line of code.
#' @param code_names vector of names of codes
#' 
#' @return object of class codeExplorer
#'   
#' @export
summarize_code <- function(code, ..., label = NULL, code_names = NULL){
  if (class(code) == "list") {
    dfl <- c(code[-1],list(...))
    code <- code[[1]]
  } else{
    dfl <- list(...)
  }
  
  result <- create_single_codeExplorer(code, label)
  if (length(dfl) > 0) {
    result$label <- code_names[1]
    result <- list(label = label, result)
    for (i in 1:length(dfl)) {
      result[[i+2]] <- create_single_codeExplorer(dfl[[i]], code_names[i-2])
    }
  }
  
  class(result) <- c("codeExplorer", "single")
  if (length(dfl) > 0) {
    class(result)[2] <- "multiple"
    if(!is.null(code_names))names(result)[-1] <- code_names
  }  
  return(result)
}


create_single_codeExplorer <- function(code, label){
  if(is.null(label)) label <- code[1]
  code <- gsub("#.*|\\\\", "", code)
  
  arrow_assignments <- extract_arrow_assignments(code)
  equality_assignments <- extract_equality_assignments(code)
  
  result <- list(
    label = label,
    libraries = extract_libraries(code), 
    functions = extract_functions(code),
    arrow_assignments = arrow_assignments,
    equality_assignments = equality_assignments,
    naming_convention = summarize_notation(c(arrow_assignments, equality_assignments))
  )
  class(result) <- c("codeExplorer", "single")
  return(result)
}



#'@export
summarize_notation <- function(names, prefix=""){
  if(length(names)==0) {
    result <- rep(0,5)
    names(result) <- paste0(prefix, c("dot", "underscore", "camelCase", "snake" ,"mixed"  ))
    return(result)
  }
  
  notation <- data.frame(dot = grepl("\\.", names),
                         underscore = grepl("\\_", names),
                         camelCase = grepl("[A-Z]", names))
  row_sums <- rowSums(notation[,1:3] == 0)
  notation$regular <- ifelse(row_sums == 3, 1, 0)
  notation$mixed <- ifelse(row_sums < 2, 1, 0)
  
  result <- c()
  result["dot"] <- sum(ifelse(notation$mixed==0, notation$dot, 0))
  result["underscore"] <- sum(ifelse(notation$mixed==0, notation$underscore, 0))
  result["camelCase"] <- sum(ifelse(notation$mixed==0, notation$camelCase, 0)) 
  result["snake"] <- sum(notation$regular)
  result["mixed"] <- sum(notation$mixed)
  
  names(result) <- paste0(prefix, names(result))
  
  return(result)
}


