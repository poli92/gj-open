## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)

setwd('/Users/josephwood/Dropbox/R/HFC/DecisionCenter')

# Import supporting data frames
topics <- readRDS('data/topics.Rda')
topics <- data.frame(topic = as.character(topics$topic), description = as.character(topics$description), stringsAsFactors = FALSE)

predictions <- readRDS('data/predictions.Rda')
predictions <- data.frame(topic = as.character(predictions$topic), date = as.character(predictions$date), outcome = as.character(predictions$outcome), probability = predictions$probability, comment = as.character(predictions$comment), stringsAsFactors = FALSE)

sources <- readRDS('data/sources.Rda')
sources <- data.frame(topic = as.character(sources$topic), title = as.character(sources$title), location = as.character(sources$location), stringsAsFactors = FALSE)

notes <- readRDS('data/notes.Rda')
notes <- data.frame(topic = as.character(notes$topic), entry = as.character(notes$entry), stringsAsFactors = FALSE)

subquestions <- readRDS('data/subquestions.Rda')
subquestions <- data.frame(topic = as.character(subquestions$topic), question = as.character(subquestions$question), answer = as.character(subquestions$answer), stringsAsFactors = FALSE)

# -------------------------------------------------
# UI

ui <- dashboardPage(
  dashboardHeader(title = "DecisionCenter"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = 'Topic',
        status = 'primary',
        solidHeader = TRUE,
        width = 12,
        selectInput('topic',"Choose a topic:",
                    topics$topic),
        actionButton("loadtpc", "Load Topic"),
        br(), br(),
        verbatimTextOutput('description')
      )
    ),
    fluidRow(
      box(
        title = 'Predictions',
        status = 'success',
        width = 12, 
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        
        # content
        # display reactive table
        rHandsontableOutput("hot.predictions")#,
        
        # # Add observation
        # div(style = "display:inline-block", textInput('dateinput', label = 'Date', value = "", width = 100)),
        # div(style = "display:inline-block", textInput('outcomeinput', label = 'Outcome', width = 100)),
        # div(style = "display:inline-block", numericInput('probinput', label = 'Probability',value = 0, min = 0, max = 1, step=.05, width = 100)),
        # div(style = "display:inline-block", textInput('commentinput', label = 'Comment', width = 550)),
        # br(),
        # actionButton('addprediction',"Add Prediction")        
      )
    ),
    fluidRow(
      box(
        title = 'Sub-questions',
        solidHeader = TRUE,
        DT::dataTableOutput('subquestions.table'),
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE
      )      
    ),
    fluidRow(
      box(
        title = 'Notes',
        solidHeader = TRUE,
        DT::dataTableOutput('notes.table'),
        width = 6, 
        collapsible = TRUE,
        collapsed = TRUE
      ),
      box(
        title = 'Sources',
        solidHeader = TRUE,
        DT::dataTableOutput('sources.table'),
        width = 6, 
        collapsible = TRUE,
        collapsed = TRUE
      )
    )
  )
)

# -------------------------------------------------
# Server 

server <- function(input, output) { 
  
  observeEvent(input$loadtpc, {
    # This will load the relevant data based on the selected topic
    
    desc.text <- as.character(topics$description[topics$topic == input$topic])
    
    output$description <- renderText(desc.text)
    #... # do some more work
    
    # Render predictions table
    values = reactiveValues()
    
    pred.data = reactive({
      if (!is.null(input$hot.predictions)) {
        DF = hot_to_r(input$hot.predictions)
      } else {
        if (is.null(values[["DF"]]))
          DF = data.frame(date = predictions$date[predictions$topic == input$topic],
                          outcome = predictions$outcome[predictions$topic == input$topic],
                          probability = predictions$probability[predictions$topic == input$topic],
                          comment = predictions$comment[predictions$topic == input$topic],
                          stringsAsFactors = FALSE)
        else
          DF = values[["DF"]]
      }

      values[["DF"]] = DF
      save.data <- data.frame(topic = input$topic, 
                              date = DF$date, 
                              outcome = DF$outcome, 
                              probability = DF$probability,
                              comment = DF$comment,
                              stringsAsFactors = TRUE)
      
      # Add new rows to existing rows while removing duplicates
      save.data <- unique(rbind(save.data,predictions))
      
      saveRDS(save.data,'data/predictions.Rda')
      DF
    })
    
    output$hot.predictions <- renderRHandsontable({
      DF = pred.data()
      rhandsontable(DF, useTypes = TRUE)
    })
    
    
    # output$predictions.table = DT::renderDataTable({
    # data.frame(Date = predictions$date[predictions$topic == input$topic],
    #            Outcome = predictions$outcome[predictions$topic == input$topic],
    #            Probability = predictions$probability[predictions$topic == input$topic],
    #            Comment = predictions$comment[predictions$topic == input$topic],
    #            stringsAsFactors = FALSE)
    # })  
        
    # Render subquestions table
    output$subquestions.table = DT::renderDataTable({
      data.frame(Entry = subquestions$question[subquestions$topic == input$topic], 
                 Answer = subquestions$answer[subquestions$topic == input$topic],
                 stringsAsFactors = FALSE)
    })
    
    
    output$notes.table = DT::renderDataTable({
      data.frame(Entry = notes$entry[notes$topic == input$topic],
                 stringsAsFactors = FALSE)
    })
    
    output$sources.table = DT::renderDataTable({
      data.frame(Title = sources$title[sources$topic == input$topic], 
                 Location = sources$location[sources$topic == input$topic],
                 stringsAsFactors = FALSE)
    })
    
  })
  
  }

shinyApp(ui, server)
