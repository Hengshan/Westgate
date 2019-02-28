library(shiny)
library(ggplot2)  # for the diamonds dataset
library(DT)
library(markdown)
library(stringr)

ui <- tagList(
  navbarPage(
    theme = "cerulean","Westgate Data Visualization",
    tabPanel("Survey Data",
             sidebarPanel(
               fileInput("file1", "Choose CSV File",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               checkboxInput("header", "Header", TRUE),
               tags$hr(),
               uiOutput("toCol"),
               uiOutput("fromCol"),
               uiOutput("allCols")
             ),
             mainPanel(
               tabsetPanel(
                 id = 'dataset',
                 
                 tabPanel("SAM",tabsetPanel(id='sams',
                                            tabPanel("Pre_Study",DT::dataTableOutput("mytable_SAM0")),
                                            tabPanel("Trial1",DT::dataTableOutput("mytable_SAM1")),
                                            tabPanel("Trial2",DT::dataTableOutput("mytable_SAM2")),
                                            tabPanel("Trial3",DT::dataTableOutput("mytable_SAM3")),
                                            tabPanel("Trial4",DT::dataTableOutput("mytable_SAM4"))
                                            )),
                
                 tabPanel("ShortEmotion",tabsetPanel(id='emotions',
                                             tabPanel("Pre_Study",DT::dataTableOutput("mytable_ShortEmotion0")),
                                             tabPanel("Post_Study",DT::dataTableOutput("mytable_ShortEmotion1"))
                                            )),
                 tabPanel("DSSQ", tabsetPanel(id='dssqs',
                                               tabPanel("Pre_Study",DT::dataTableOutput("mytable_DSSQ0")),
                                               tabPanel("Post_Study",DT::dataTableOutput("mytable_DSSQ1"))
                 )),
                 tabPanel("SOD", DT::dataTableOutput("mytable_SOD")),
                 tabPanel("Crowd Avoidance", DT::dataTableOutput("mytable_Crowd")),
                 tabPanel("Demography", DT::dataTableOutput("mytable_Demography"))
               )
             )
    ),
    tabPanel("Route Choice", "This panel is intentionally left blank"),
    tabPanel("Physio", "This panel is intentionally left blank"),
    navbarMenu("More",
               tabPanel("Contact Us",
                        "Contact Us"
               ),
               tabPanel("About",
                        fluidRow(
                          column(6,
                                 includeMarkdown("about.md")
                          )
                        )
               )
    )
  )
)

server <- function(input, output, session) {
  
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, header = input$header)
  })
  
  cleanData <- function(x){
      if (!grepl("^[[:digit:]]+$", x)){
        str_sub(x,-1)}
      else{
        x
      }
    }
  
  # Read specific columns
  output$mytable_SAM0 <- DT::renderDataTable({
    df=filedata()
    df[,18:21]
  })
  
  output$mytable_ShortEmotion0 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,22:32)]
  })
  
  output$mytable_DSSQ0 <- DT::renderDataTable({
    df=filedata()
    sapply(df[,c(18,33:62)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        str_sub(x,-1)}
      else{
        x
      }
    })
  })

  
  output$mytable_SAM1 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,63:65)]
  })
  
  output$mytable_SAM2 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,66:68)]
  })
  
  output$mytable_SAM3 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,69:71)]
  })
  
  output$mytable_SAM4 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,72:74)]
  })
  
  output$mytable_ShortEmotion1 <- DT::renderDataTable({
    df=filedata()
    df[,c(18,75:85)]
  })
  
  output$mytable_DSSQ1 <- DT::renderDataTable({
    df=filedata()
    sapply(df[,c(18,86:115)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        str_sub(x,-1)}
      else{
        x
      }})
  })
  
  output$mytable_SOD <- DT::renderDataTable({
    df=filedata()
    sapply(df[,c(18,116:130)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        str_sub(x,1,1)}
      else{
        x
      }})
  })
  
  output$mytable_Crowd <- DT::renderDataTable({
    df=filedata()
    df[,c(18,131:135)]
  })
  
  output$mytable_Demography <- DT::renderDataTable({
    df=filedata()
    df[,c(18,136:140)]
  })
  
  output$fromCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("from", "From:",items)
    
  })
  
  output$toCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("to", "To:",items)
  })
  
  output$allCols <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    checkboxGroupInput("show_vars", "Choose Columns to show:",
                       names(items))
  })

  selectedCols <- reactive({
    
    fromCol <- input$from
    fromTo <- input$to
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, header = input$header)
  })

  
}

shinyApp(ui, server)