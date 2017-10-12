## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)

setwd('X:/Personal Development/Data Science/HFC/DecisionCenter')

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
        textInput('newtopic', "Enter new topic"),
        actionButton("loadtpc", "Load topic"),
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
        
      )
    ),
    fluidRow(
      box(
        title = 'Sub-questions',
        solidHeader = TRUE,
        rHandsontableOutput("hot.subq"),
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE
      )      
    ),
    fluidRow(
      box(
        title = 'Notes',
        solidHeader = TRUE,
        rHandsontableOutput("hot.notes"),
        width = 6, 
        collapsible = TRUE,
        collapsed = TRUE
      ),
      box(
        title = 'Sources',
        solidHeader = TRUE,
        rHandsontableOutput("hot.sources"),
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
    
    values = reactiveValues()
    
    # Render predictions table
    pred.data = reactive({
      if (!is.null(input$hot.predictions)) {
        pred.DF = hot_to_r(input$hot.predictions)
      } else {
        if (is.null(values[["pred.DF"]]))
          pred.DF = data.frame(Date = predictions$date[predictions$topic == input$topic],
                          Outcome = predictions$outcome[predictions$topic == input$topic],
                          Probability = predictions$probability[predictions$topic == input$topic],
                          Comment = predictions$comment[predictions$topic == input$topic],
                          stringsAsFactors = FALSE)
        else
          pred.DF = values[["pred.DF"]]
      }

      values[["pred.DF"]] = pred.DF
      pred.save.data <- data.frame(topic = input$topic, 
                              date = pred.DF$Date, 
                              outcome = pred.DF$Outcome, 
                              probability = pred.DF$Probability,
                              comment = pred.DF$Comment,
                              stringsAsFactors = TRUE)
      
      # Add new rows to existing rows while removing duplicates
      pred.save.data <- unique(rbind(pred.save.data,predictions))
      
      saveRDS(pred.save.data,'data/predictions.Rda')
      pred.DF
    })
    
    output$hot.predictions <- renderRHandsontable({
      pred.DF = pred.data()
      rhandsontable(pred.DF, useTypes = TRUE)
    })
        
    # Render subquestions table
    subq.data = reactive({
      if (!is.null(input$hot.subq)) {
        subq.DF = hot_to_r(input$hot.subq)
      } else {
        if (is.null(values[["subq.DF"]]))
          subq.DF = data.frame(Question = subquestions$question[subquestions$topic == input$topic],
                          Answer = subquestions$answer[subquestions$topic == input$topic],
                          stringsAsFactors = FALSE)
        else
          subq.DF = values[["subq.DF"]]
      }
      
      values[["subq.DF"]] = subq.DF
      subq.save.data <- data.frame(topic = input$topic, 
                              question = subq.DF$Question, 
                              answer = subq.DF$Answer, 
                              stringsAsFactors = TRUE)
      
      # Add new rows to existing rows while removing duplicates
      subq.save.data <- unique(rbind(subq.save.data,subquestions))
      
      saveRDS(subq.save.data,'data/subquestions.Rda')
      subq.DF
    })
    
    output$hot.subq <- renderRHandsontable({
      subq.DF = subq.data()
      rhandsontable(subq.DF, useTypes = TRUE)
    })
    
    # Render notes table
    notes.data = reactive({
      if (!is.null(input$hot.notes)) {
        notes.DF = hot_to_r(input$hot.notes)
      } else {
        if (is.null(values[["notes.DF"]]))
          notes.DF = data.frame(Entry = notes$entry[notes$topic == input$topic],
                               stringsAsFactors = FALSE)
        else
          notes.DF = values[["notes.DF"]]
      }
      
      values[["notes.DF"]] = notes.DF
      notes.save.data <- data.frame(topic = input$topic, 
                                   entry = notes.DF$Entry, 
                                   stringsAsFactors = TRUE)
      
      # Add new rows to existing rows while removing duplicates
      notes.save.data <- unique(rbind(notes.save.data,notes))
      
      saveRDS(notes.save.data,'data/notes.Rda')
      notes.DF
    })
    
    output$hot.notes <- renderRHandsontable({
      notes.DF = notes.data()
      rhandsontable(notes.DF, useTypes = TRUE)
    })   
    
    # Render sources table
    sources.data = reactive({
      if (!is.null(input$hot.sources)) {
        sources.DF = hot_to_r(input$hot.sources)
      } else {
        if (is.null(values[["sources.DF"]]))
          sources.DF = data.frame(Title = sources$title[sources$topic == input$topic],
                                  Location = sources$location[sources$topic == input$topic],
                                  stringsAsFactors = FALSE)
        else
          sources.DF = values[["sources.DF"]]
      }
      
      values[["sources.DF"]] = sources.DF
      sources.save.data <- data.frame(topic = input$topic,
                                      title = sources.DF$Title,
                                      location = sources.DF$Location, 
                                      stringsAsFactors = TRUE)

      # Add new rows to existing rows while removing duplicates
      sources.save.data <- unique(rbind(sources.save.data,sources))

      saveRDS(sources.save.data,'data/sources.Rda')
      sources.DF
    })
    
    output$hot.sources <- renderRHandsontable({
      sources.DF = sources.data()
      rhandsontable(sources.DF, useTypes = TRUE)
    })      
    
    # output$sources.table = DT::renderDataTable({
    #   data.frame(Title = sources$title[sources$topic == input$topic], 
    #              Location = sources$location[sources$topic == input$topic],
    #              stringsAsFactors = FALSE)
    # })
    
  })
  
  }

shinyApp(ui, server)
