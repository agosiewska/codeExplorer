navbarPage("codeExplorer",
            tabPanel("Upload codes",
              sidebarLayout(
                sidebarPanel( 
                  fileInput("file","Upload files with R code",multiple = TRUE),
                  actionButton("addInput","Add Input"),
                  uiOutput("inputs")
                ),
                mainPanel(
                  verbatimTextOutput("summary"),
                  uiOutput("summaries")
                )
              )
            ),
           tabPanel("Libraries",
              sidebarLayout(
                sidebarPanel(
                  selectInput("library_position","Select bar position",choices =as.list(c("stack","dodge", "fill")),multiple = FALSE)
                ),
                mainPanel(
                  plotOutput("libraries")
                )
              )
            ),
           tabPanel("Functions",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("function_position","Select bar position",choices =as.list(c("stack","dodge", "fill")),multiple = FALSE)
                      ),
                      mainPanel(
                        plotOutput("functions", height = "600px")
                      )
                    )
           ),
           tabPanel("Naming conventions",
                    sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                        plotOutput("namingconv"),
                        plotOutput("naminghist")
                      )
                    )
           ),
           tabPanel("Assignments",
                    sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                        plotOutput("assignments")
                      )
                    )
           ),
           tabPanel("PCA",
                    sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                      )
                    )
           ),
           tabPanel("Models",
                    sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                      )
                    )
           ),
           tabPanel("About",
                    fluidPage(
                      titlePanel(
                        h3("Alicja Gosiewska")
                      )
                    )
           )
)