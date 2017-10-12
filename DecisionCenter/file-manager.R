setwd('/Users/josephwood/Dropbox/R/HFC/DecisionCenter')

# Import supporting data frames
topics <- readRDS('data/topics.Rda')
topics <- data.frame(topic = as.character(topics$topic), description = as.character(topics$description), stringsAsFactors = FALSE)
saveRDS(topics,'data/topics.Rda')

predictions <- readRDS('data/predictions.Rda')
predictions <- data.frame(topic = as.character(predictions$topic), date = as.character(predictions$date), outcome = as.character(predictions$outcome), probability = predictions$probability, comment = as.character(predictions$comment), stringsAsFactors = FALSE)
saveRDS(predictions,'data/predictions.Rda')

sources <- readRDS('data/sources.Rda')
sources <- data.frame(topic = as.character(sources$topic), title = as.character(sources$title), location = as.character(sources$location), stringsAsFactors = FALSE)
saveRDS(sources,'data/sources.Rda')

notes <- readRDS('data/notes.Rda')
notes <- data.frame(topic = as.character(notes$topic), entry = as.character(notes$entry), stringsAsFactors = FALSE)
saveRDS(notes,'data/notes.Rda')


subquestions <- readRDS('data/subquestions.Rda')
subquestions <- data.frame(topic = as.character(subquestions$topic), question = as.character(subquestions$question), answer = as.character(subquestions$answer), stringsAsFactors = FALSE)
saveRDS(subquestions,'data/subquestions.Rda')
