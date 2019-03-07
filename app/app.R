library(shiny)
library(ggplot2)  # for the diamonds dataset
library(DT)
library(markdown)
library(stringr)
library(dplyr)
library(MASS)
library(nnet)
library(lme4)
library(data.table)
library(ez)
library(multcomp)
library(plotly)
library(sf)

expBalance <- read.csv("Balance_Exp1.csv")

# UI ----------------------------------------------------------------------
ui <- navbarPage(
    theme = "cerulean","Westgate Data Visualization",
    id = "datatype",

# SurveyData --------------------------------------------------------------
    tabPanel("Survey Data",
           sidebarLayout(
             # SurveyData_Sidebar ------------------------------------------------------
             sidebarPanel(
             wellPanel(
             fileInput("file1", "Choose Questionnaire CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
             checkboxInput("header", "Header", TRUE)
             ),
             wellPanel(
               "Select Indepedent Variables",
               checkboxInput("crowd_indep","Crowdedness",TRUE),
               checkboxInput("startloc_indep","Start Location",TRUE)
               ),
             wellPanel(
               HTML("<h5>Select Depedent Variables</h5>"),
               uiOutput("selectcol"),
               conditionalPanel("input.dataset=='SAM'||input.dataset=='ShortEmotion'||input.dataset=='DSSQ'",
                                checkboxInput("compare_indep","Compare Trials",FALSE)),
               
               uiOutput("conditionalInput"),
               selectInput('stat_method',"Choose Analysis Method:", c("Ordered Logistic Regression", 
                                                                      "Multinomial Logistic Regression",
                                                                      "Binomial Logistic Regression",
                                                                      "ANOVA"))
               ),
               actionButton("do","Data Analysis")
           ),
           # SurveyData_mainpage ------------------------------------------------------
           mainPanel(
             tabsetPanel(
               id = 'dataset',
               
               tabPanel("SAM",
                        tabsetPanel(id='sams',
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
               tabPanel("CrowdAvoidance", DT::dataTableOutput("mytable_Crowd")),
               tabPanel("Demography", DT::dataTableOutput("mytable_Demography"))
             ),
             tabsetPanel(
               id = 'report',
               tabPanel("Plots",
                        selectInput("plot_type","Plot Tyle:", c("Histgram","Box graph","Bar chart")),
                        plotlyOutput("plot2")
                        ),
               tabPanel("Stats",verbatimTextOutput("stat")),
               tabPanel("Summary",verbatimTextOutput("summary"))
             )
        ))),

# RouteChoice -------------------------------------------------------------
    tabPanel("Route Choice", 
             sidebarPanel(
            fileInput("file2", "Choose Route Choice CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
      checkboxInput("header2", "Header", TRUE),
      fileInput("file_shape", "Choose ShapeFile",
                accept = c(".shp")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset_route',
        tabPanel("Data",DT::dataTableOutput("mytable_route")),
        tabPanel("Plot 3D",plotlyOutput("plot3D"))
      )
    )),

# Physio and More ---------------------------------------------------------

    tabPanel("Physio", 
             "This panel is intentionally left blank"),

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



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  df <- data.frame()  #questionnare data
  df_route <- data.frame()  #survey data
  df_current <- data.frame()
  
  # read questionnare data
  filedata <- reactive({
    # infile <- input$file1
    # if (is.null(infile)) {
    #   # User has not uploaded a file yet
    #   return(NULL)
    # }
    # df <<- read.csv(infile$datapath, header = input$header)
    df <<- read.csv("data.csv", header = input$header)
  })
  
  # read survey data
  filedata2 <- reactive({
    infile <- input$file2
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df_route <<- read.csv(infile$datapath, header = input$header2)
  })

# Phrase Data -------------------------------------------------------------

  setdata <- reactive({
    df <- filedata()
    if (!is.data.frame(df) || nrow(df)==0 ||!is.data.frame(expBalance) || nrow(expBalance)==0) return(NULL)
    df_show <- list()
    df_show[[1]] <- merge(df[,18:21],expBalance[,1:5],by="SubID")
    df_show[[2]] <- merge(df[,c(18,22:32)],expBalance[,1:5],by="SubID")
    
    temp <- sapply(df[,c(18,86:115)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        as.integer(str_sub(x,-1))}
      else{
        as.integer(x)
      }})
    df_show[[3]] <-merge(temp,expBalance[,1:5],by="SubID")
    df_show[[4]] <-merge(df[,c(18,63:65)],expBalance[,1:5],by="SubID")
    df_show[[5]] <-merge(df[,c(18,66:68)],expBalance[,1:5],by="SubID")
    df_show[[6]] <-merge(df[,c(18,69:71)],expBalance[,1:5],by="SubID")
    df_show[[7]] <-merge(df[,c(18,72:74)],expBalance[,1:5],by="SubID")
    df_show[[8]] <-merge(df[,c(18,75:85)],expBalance[,1:5],by="SubID")
    temp2 <- sapply(df[,c(18,86:115)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        as.integer(str_sub(x,-1))}
      else{
        as.integer(x)
      }})
    df_show[[9]] <-merge(temp2,expBalance[,1:5],by="SubID")
    
    temp3 <- sapply(df[,c(18,116:130)],function(x) {
      if (!grepl("^[[:digit:]]+$", x)){
        as.integer(str_sub(x,1,1))}
      else{
        as.integer(x)
      }})
    
    df_show[[10]] <-merge(temp3,expBalance[,1:5],by="SubID")
    df_show[[11]] <-merge(df[,c(18,131:135)],expBalance[,1:5],by="SubID")
    df_show[[12]] <-merge(df[,c(18,136:140)],expBalance[,1:5],by="SubID")
    df_show
  })
  
# Show All table panels ---------------------------------------------------
  output$mytable_SAM0 <- DT::renderDataTable({
    getcurdata()
  })
  
  output$mytable_ShortEmotion0 <- DT::renderDataTable({
    getcurdata()
  })

  output$mytable_DSSQ0 <- DT::renderDataTable({
    getcurdata()
  })


  output$mytable_SAM1 <- DT::renderDataTable({
    getcurdata()

  })

  output$mytable_SAM2 <- DT::renderDataTable({
    getcurdata()

  })

  output$mytable_SAM3 <- DT::renderDataTable({
    getcurdata()

  })

  output$mytable_SAM4 <- DT::renderDataTable({
    getcurdata()
  })

  output$mytable_ShortEmotion1 <- DT::renderDataTable({
    getcurdata()

  })

  output$mytable_DSSQ1 <- DT::renderDataTable({
    getcurdata()
  })

  output$mytable_SOD <- DT::renderDataTable({
    getcurdata()
  })

  output$mytable_Crowd <- DT::renderDataTable({
    getcurdata()
  })

  output$mytable_Demography <- DT::renderDataTable({
    getcurdata()
  })
  
  output$mytable_route <- DT::renderDataTable({
    getroutedata()
  })
  
  getroutedata <- eventReactive(input$file2,{
    filedata2()
  })
  
# Get Current Data --------------------------------------------------------
  getcurdata <- eventReactive(c(input$dataset,input$sams,input$emotions,input$dssqs,input$file1), {
    mydata <- setdata()
    if(input$dataset=="SAM")
    {
      if(input$sams=="Pre_Study")
      {
        df_current <<-  mydata[[1]]
        return(df_current)
      }else if (input$sams=="Trial1")
      {
        df_current <<-  mydata[[4]]
      }else if (input$sams=="Trial2")
      {
        df_current <<-  mydata[[5]]
      }else if (input$sams=="Trial3")
      {
        df_current <<-  mydata[[6]]
      }else if (input$sams=="Trial4")
      {
        df_current <<-  mydata[[7]]
      }
      
    }else if (input$dataset=="ShortEmotion")
    {
      if(input$emotions=="Pre_Study")
      {
        df_current <<-  mydata[[2]]
        
      }else if (input$emotions=="Post_Study")
      {
        df_current <<-  mydata[[8]]
      }
    }else if (input$dataset=="DSSQ")
    {
      if(input$dssqs=="Pre_Study")
      {
        df_current <<-  mydata[[3]]
      }else if (input$dssqs=="Post_Study")
      {
        df_current <<-  mydata[[9]]
      }
      
    }else if (input$dataset=="SOD")
    {
      df_current <<-  mydata[[10]]
    }
    else if (input$dataset=="CrowdAvoidance")
    {
      df_current <<-  mydata[[11]]
    }else if (input$dataset=="Demography")
    {
      df_current <<-  mydata[[12]]
    }
  })



# Update UI ---------------------------------------------------------------
  renderCondInput <- eventReactive(c(input$dataset),{
    print(input$dataset)
    if(input$dataset == 'SAM')
    {
      checkboxGroupInput("comp1","Compare Between Trials",c("Pre_Study","Trial1","Trial2","Trial3","Trial4"))
    }else if (input$dataset == 'ShortEmotion'||input$dataset == 'DSSQ'){
      checkboxGroupInput("comp2","Compare Pre/Post",c("Pre_Study","Post_Study"))
    }
  })
  
  output$conditionalInput <- renderUI({
    if(input$compare_indep){
      renderCondInput()
    }
  })

  output$selectcol <- renderUI({
    tempdf <- renderSum()
    if(length(names(tempdf))>1)
    {selectInput("selected", "Choose Column:",
                names(tempdf),selected = names(tempdf)[2])}
    else{
      selectInput("selected", "Choose Column:",
                  names(tempdf))
    }
  })

  #only summarize and select numberic fields
  renderSum <- eventReactive(c(input$dataset,input$sams,input$emotions,input$dssqs,input$file1),{
    select_if(df_current, is.numeric)
    })
  
  output$summary <- renderPrint({
    summary(renderSum())
  })
  

# PlotlyGraph -------------------------------------------------------------

  rePlot2 <- eventReactive(c(input$selected,input$crowd_indep,input$startloc_indep,input$plot_type),{
    #clear stat first
    
    if(input$plot_type=="Histgram")
    {
      p <- ggplot(df_current, aes(x=df_current[,input$selected]))
      p <- p+geom_histogram(color="black", fill="blue")
      p <- p+
        if(input$crowd_indep&&input$startloc_indep)
        {facet_grid(Start_Location~Crowdedness)}
      else if (input$crowd_indep){
        facet_grid(.~Crowdedness)
      }else if (input$startloc_indep){
        facet_grid(.~Start_Location)
      }
      return(ggplotly(p))
    }else if (input$plot_type=="Bar chart"){
      Animals <- c("giraffes", "orangutans", "monkeys")
      SF_Zoo <- c(20, 14, 23)
      LA_Zoo <- c(12, 18, 29)
      data <- data.frame(Animals, SF_Zoo, LA_Zoo)
      
      p <- plot_ly(df_current, x = ~Crowdedness, y = ~df_current[,input$selected], type = 'bar', name = input$selected) %>%
        layout(yaxis = list(title = 'Count'), barmode = 'group')
    }
  }) 
  
  output$plot2 <- renderPlotly({
    rePlot2()
  })
  

# Plotly3D Graph ----------------------------------------------------------
  rePlot3D <- eventReactive(c(input$file_shape,input$dataset_route),{
    shapedata <- sf::st_read("mygeodata/Westgate_Floorplans32_changedFloor-line.shp", quiet = TRUE)
    shapedata <- highlight_key(shapedata)
    data <- shapedata$data()
    plot_ly(data, z = ~Z) %>% 
      layout(autosize = T, width = 800, height = 800)
  }) 
  output$plot3D <- renderPlotly({
    rePlot3D()
  })
  

# Data analysis -----------------------------------------------------
  reStatPlot <- eventReactive(input$do,{
    # choose different data analysis
    
    if(input$stat_method=="Ordered Logistic Regression"){  #for example, SAM data, 1-9 scale 
      df_current[,input$selected] <- as.factor(df_current[,input$selected])
      
      #indepdent variables
      if(input$crowd_indep&&input$startloc_indep)
        m <- polr(df_current[,input$selected] ~ Crowdedness + Start_Location, data = df_current, Hess=TRUE)
      else if (input$crowd_indep){
        m <- polr(df_current[,input$selected] ~ Crowdedness, data = df_current, Hess=TRUE)
      }else if (input$startloc_indep){
        m <- polr(df_current[,input$selected] ~ Start_Location, data = df_current, Hess=TRUE)
      }
      
    }else if (input$stat_method=="Multinomial Logistic Regression"){ # may not be used in this study, for unordered categorical data
      df_current[,input$selected] <- as.factor(df_current[,input$selected])
      
      
      if(input$crowd_indep&&input$startloc_indep)
        m <- multinom(df_current[,input$selected] ~Crowdedness + Start_Location,data = df_current)
      else if (input$crowd_indep){
        m <- multinom(df_current[,input$selected] ~Crowdedness,data = df_current)
      }else if (input$startloc_indep){
        m <- multinom(df_current[,input$selected] ~Start_Location,data = df_current)
      }
      
    }else if (input$stat_method=="Binomial Logistic Regression"){# for route choice
      df_current[,input$selected] <- as.factor(df_current[,input$selected])
      
      if(input$crowd_indep&&input$startloc_indep)
        m <- glm(df_current[,input$selected] ~ Crowdedness + Start_Location,data = df_current, family = binomial)
      else if (input$crowd_indep){
        m <- glm(df_current[,input$selected] ~ Crowdedness,data = df_current, family = binomial)
      }else if (input$startloc_indep){
        m <- glm(df_current[,input$selected] ~Start_Location,data = df_current, family = binomial)
      }
      
    }else if (input$stat_method=="ANOVA"){  # for continous depedent variables, for example, for user perception of crowdedness 1-100 can be treated as continous
      if(input$crowd_indep&&input$startloc_indep)
        m = lm(df_current[,input$selected]~Crowdedness+Start_Location, data=df_current)
      else if (input$crowd_indep){
        m = lm(df_current[,input$selected]~Crowdedness, data=df_current)
      }else if (input$startloc_indep){
        m = lm(df_current[,input$selected]~Start_Location, data=df_current)
      }
    }
    m
  })
  
  output$stat <- renderPrint({
    ctable <- summary(reStatPlot())
    
    if(input$stat_method=="Ordered Logistic Regression"){  #for example, SAM data, 1-9 scale 
      p <- pnorm(abs(ctable$coefficients[, "t value"]), lower.tail = FALSE) * 2
      ctable$coefficients <- cbind(ctable$coefficients, "p value" = p)
      ctable$coefficients <- cbind(ctable$coefficients,"exp value"=exp(ctable$coefficients[,1]))
      return(ctable)
      
    }else if (input$stat_method=="Multinomial Logistic Regression"){ # may not be used in this study, for unordered categorical data
      
      tvalue <- as.data.frame(ctable$coefficients/ctable$standard.errors)
      pvalue <- sapply(tvalue,function(x){
        pnorm(abs(x), lower.tail = FALSE) * 2
      })
      return(cbind(ctable$coefficients,"tvalue"=tvalue, "pvalue"=pvalue, "expvalue"=exp(ctable$coefficients)))
      
    }else if (input$stat_method=="Binomial Logistic Regression"){# for route choice
      return(ctable)
      
    }else if (input$stat_method=="ANOVA"){  # for continous depedent variables, for example, for user perception of crowdedness 1-100 can be treated as continous
      return(anova(reStatPlot()))
    }
  })
}

shinyApp(ui, server)