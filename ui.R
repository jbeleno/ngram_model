library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Text: "),
      sliderInput("num_results",
                  "How many words do you want to see?",
                  min = 1, max = 50, value = 1),
      checkboxInput("show_stop_words", "Show Stop Words", value = TRUE),
      checkboxInput("show_swear_words", "Show Swear Words", value = TRUE),
      submitButton("Predict")
    ),
    mainPanel(
      h3("Documentation"),
      div("This web application predicts the next word given a text."),
      div("Stop words are the most common words in a language. You can remove them from the predictions to find more interesting suggestions. "),
      div("Sometimes you don't want to receive predictions of swear words. However, sometimes you want them, so we let the decision up to you."),
      h3("Results"),
      textOutput("result")
    )
  )
))
