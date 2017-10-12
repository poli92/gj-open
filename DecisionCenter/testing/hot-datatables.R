library(rhandsontable)

rhandsontable(
  data.frame(Date = predictions$date[predictions$topic == input$topic],
             Outcome = predictions$outcome[predictions$topic == input$topic],
             Probability = predictions$probability[predictions$topic == input$topic],
             Comment = predictions$comment[predictions$topic == input$topic],
             stringsAsFactors = FALSE)
)

unique(rbind(predictions, predictions))


test2 <- unique(testdata)

testdata <- readRDS('data\\predictions.Rda')
