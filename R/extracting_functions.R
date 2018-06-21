#' @importFrom stringr str_extract_all
#' @export
extract_libraries <- function(text){
  lib <- text %>% 
    paste0(collapse="") %>%
    str_extract_all(pattern = "library\\([^)]*\\)") %>%
    unlist()
  lib <- gsub('library\\(|\\)|"',"",lib)
  lib <- gsub("'","",lib)
  return(lib)
}

#' @import dplyr
#' @export
extract_arrow_assignments <- function(text){
  removed_ifs_and_loops <- gsub("if\\([^)]*\\)|for\\([^)]*\\)|while\\([^)]*\\)|\\{|`.*", "", text)
  is_assignment <- unlist(lapply(text,grepl,pattern="<-"))
  assignments <- text[is_assignment]
  
  funkcje1 <- unlist(lapply(assignments, grepl,pattern = "<- function"))
  funkcje2 <- unlist(lapply(assignments, grepl,pattern = "<-function"))
  variables<-!(funkcje1 | funkcje2)
  
  function_names <- assignments[c(funkcje1,funkcje2)] %>%
    lapply(gsub,pattern="<.*", replacement="")
  function_names <- gsub(".*\\{", "", function_names) %>%
    lapply(trimws,which="both") %>%
    unlist()
  
  
  variable_names <- assignments[variables] %>%
    lapply(gsub,pattern="<.*|\\[.*|if.*", replacement="")
  variable_names <- gsub(".*\\{", "", variable_names) %>%
    lapply(trimws,which="both") %>%
    unlist()
  
  
  return(list(functions=function_names, variables=variable_names))
}

#' @import dplyr
#' @export
extract_equality_assignments <- function(text){
  removed_ifs_and_loops <- gsub("if\\([^)]*\\)|for\\([^)]*\\)|while\\([^)]*\\)|\\{|`.*|!=","", text)
  potential_assignments <- gsub("\\(.*|==.*" ,"" , removed_ifs_and_loops)
  is_assignment <- unlist(lapply(potential_assignments, grepl, pattern="="))
  assignments <- removed_ifs_and_loops[is_assignment]
  
  funkcje1 <- unlist(lapply(assignments, grepl,pattern = "= function"))
  funkcje2 <- unlist(lapply(assignments, grepl,pattern = "=function"))
  variables<-!(funkcje1 | funkcje2)
  
  function_names <- assignments[c(funkcje1,funkcje2)] %>%
    lapply(gsub,pattern="=.*", replacement="")
  function_names <- gsub(".*\\{", "", function_names) %>%
    lapply(trimws,which="both") %>%
    unlist()
  
  
  variable_names <- assignments[variables] %>%
    lapply(gsub,pattern="=.*|\\[.*|if.*", replacement="")
  variable_names <- gsub(".*\\{", "", variable_names) %>%
    lapply(trimws,which="both") %>%
    unlist()
  
  return(list(functions=function_names, variables=variable_names))
}


#' @export
extract_functions <- function(text){
  func <- text  %>%
    str_extract_all(pattern = "[^(]*.\\(") %>%
    unlist()
  
  func_names <- gsub(".*\\[|.*-|.*=|.*:|.*%>%|.*\\)|.*\\{|.*, |.*,|.* `|.*\\*|.*<", "", func) 
  func_names <- gsub('\\(', "", func_names) %>%
    trimws()
  func_names <- func_names[which(!(func_names %in% c("", "'", "\\")))]
  return(func_names)
}