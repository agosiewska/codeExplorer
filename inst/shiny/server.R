function(input,output) { 
  
  summarized_files<-reactive({
    files <- lapply(input$file$datapath, readLines)
    summarize_code(files, label = "group1")
  })
  
  output$summary <- renderPrint(
    summarized_files()
    )
  
  ids <<- NULL
  observeEvent(input$addInput,{
    print(ids)
    if (is.null(ids)){
      ids <<- 1
    }else{
      ids <<- c(ids, max(ids)+1)
    }
    output$inputs <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          fileInput(paste0("file",ids[i]), sprintf("Codes for group #%d",ids[i]), multiple = TRUE)
        })
      )
    })
  })
  
  
  

  observeEvent(input$addInput,{
    output$summaries <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          verbatimTextOutput(paste0("summary",ids[i]))
        })
      )
    })
    ########## TO DO poprawic na cos eleganckiego
    summarized_files1<<-reactive({
      files1 <- lapply(input$file1$datapath, readLines)
      summarize_code(files1, label = "group2")
    })
    output$summary1 <- renderPrint(
      summarized_files1()
    )
    summarized_files2<<-reactive({
      files2 <- lapply(input$file2$datapath, readLines)
      summarize_code(files2, label = "group3")
    })
    output$summary2 <- renderPrint(
      summarized_files2()
    )
    summarized_files3<<-reactive({
      files3 <- lapply(input$file3$datapath, readLines)
      summarize_code(files3, label = "group4")
    })
    output$summary3 <- renderPrint(
      summarized_files3()
    )
    ###############
  
  })
  
  selected_library_position <- reactive({
    input$library_position
  })
  output$libraries <- renderPlot({
    ############ TO DO zrobic elegancko
    if(is.null(ids)) return(plot_libraries(summarized_files(), position = selected_library_position()))
    if(length(ids) == 1) return( plot_libraries(summarized_files(), summarized_files1(), position = selected_library_position()))
    if(length(ids) == 2) return( plot_libraries(summarized_files(), summarized_files1(), summarized_files2(), position = selected_library_position()))
    if(length(ids) == 3) return( plot_libraries(summarized_files(), summarized_files1(), summarized_files2(), summarized_files3(), position = selected_library_position()))
  })
  
  selected_function_position <- reactive({
    input$function_position
  })
  output$functions <- renderPlot({
    ############ TO DO zrobic elegancko
    if(is.null(ids)) return(plot_functions(summarized_files(), position = selected_function_position()))
    if(length(ids) == 1) return( plot_functions(summarized_files(), summarized_files1(), position = selected_function_position()))
    if(length(ids) == 2) return( plot_functions(summarized_files(), summarized_files1(), summarized_files2(), position = selected_function_position()))
    if(length(ids) == 3) return( plot_functions(summarized_files(), summarized_files1(), summarized_files2(), summarized_files3(), position = selected_function_position()))
  })
  
  output$namingconv <- renderPlot({
    selected_function_position()
    ############ TO DO zrobic elegancko
    if(is.null(ids)) return(plot_naming_convention(summarized_files()))
    if(length(ids) == 1) return( plot_naming_convention(summarized_files(), summarized_files1()))
    if(length(ids) == 2) return( plot_naming_convention(summarized_files(), summarized_files1(), summarized_files2()))
    if(length(ids) == 3) return( plot_naming_convention(summarized_files(), summarized_files1(), summarized_files2(), summarized_files3()))
  })
  
  output$naminghist <- renderPlot({
    selected_function_position()
    ############ TO DO zrobic elegancko
    if(is.null(ids)) return(plot_names_hist(summarized_files()))
    if(length(ids) == 1) return( plot_names_hist(summarized_files(), summarized_files1()))
    if(length(ids) == 2) return( plot_names_hist(summarized_files(), summarized_files1(), summarized_files2()))
    if(length(ids) == 3) return( plot_names_hist(summarized_files(), summarized_files1(), summarized_files2(), summarized_files3()))
  })
  
  output$assignments <- renderPlot({
    selected_function_position()
    ############ TO DO zrobic elegancko
    if(is.null(ids)) return(plot_assignments(summarized_files()))
    if(length(ids) == 1) return( plot_assignments(summarized_files(), summarized_files1()))
    if(length(ids) == 2) return( plot_assignments(summarized_files(), summarized_files1(), summarized_files2()))
    if(length(ids) == 3) return( plot_assignments(summarized_files(), summarized_files1(), summarized_files2(), summarized_files3()))
  })
  
}