library(shiny)

english = 'en_US'
language <- english
filename_unigrams <- sprintf('./assets/1-grams-%s.csv', language)[1]
filename_bigrams <- sprintf('./assets/2-grams-%s.csv', language)[1]
filename_trigrams <- sprintf('./assets/3-grams-%s.csv', language)[1]
filename_unigram_gte <- sprintf('./assets/unigram_estimations-%s.csv', language)[1]
filename_bigram_gte <- sprintf('./assets/bigram_estimations-%s.csv', language)[1]
filename_trigram_gte <- sprintf('./assets/trigram_estimations-%s.csv', language)[1]
filename_model <- sprintf('./assets/ngrams_model-%s.csv', language)[1]

load_env_from_csv <- function(filename) {
  data <- read.csv(filename, header = TRUE, stringsAsFactors=FALSE)
  keys <- as.vector(data['key'])
  values <- as.vector(data['value'])
  data_list <- as.list(values[[1]])
  names(data_list) <- keys[[1]]
  
  data_size <- length(data_list)
  chunk_size <- 10000
  num_loops <- ceiling(data_size/chunk_size)
  dictionary <- new.env(hash=TRUE)
  
  for (index in 1:num_loops) {
    start <- (index - 1) * chunk_size + 1
    end <- (index - 1) * chunk_size + chunk_size
    if (index == num_loops) {
      end <- data_size
    }
    print(start)
    dictionary <- list2env(data_list[start:end], envir=dictionary, hash=TRUE)
  }
  
  dictionary
}


ngram_model <- load_env_from_csv(filename_model)
ngram_keys <- ls(ngram_model)
index <- 0
for (key in ngram_keys) {
  print(index)
  index <- index + 1
  ngram_model[[key]] <- unlist(strsplit(ngram_model[[key]], "|", fixed = TRUE))
}
rm(ngram_keys)

unigrams <- load_env_from_csv(filename_unigrams)
bigrams <- load_env_from_csv(filename_bigrams)
trigrams <- load_env_from_csv(filename_trigrams)
unigram_estimations <- load_env_from_csv(filename_unigram_gte)
bigram_estimations <- load_env_from_csv(filename_bigram_gte)
trigram_estimations <- load_env_from_csv(filename_trigram_gte)

clean_text <- function(text) {
  clean_line <- tolower(text)
  # Get rid of anything not a-z, ', or whitespace
  clean_line <- gsub('[^a-z\'[:space:]]', ' ', clean_line)
  # Collapse whitespace
  clean_line <- gsub('[[:space:]]+', ' ', clean_line)
  clean_line <- gsub("^\\s+|\\s+$", "", clean_line)
  clean_line
}

katz_backoff_model <- function(ngram){
  ngram_size <- length(ngram)
  n <- ngram_size
  prediction <- new.env(hash=TRUE)
  
  if (ngram_size >= 2) {
    bigram <- paste(ngram[n-1], ngram[n])
    
    if (exists(bigram, envir=ngram_model, inherits=FALSE)) {
      word_candidates <- ngram_model[[bigram]]
      frequency_bigram <- bigrams[[bigram]]
      cumulative_probability <- 0
      for (word in word_candidates) {
        trigram_candidate <- paste(bigram, word)
        frequency_trigram <- trigrams[[trigram_candidate]]
        gte_estimation <- trigram_estimations[[toString(frequency_trigram)]]
        probability <- gte_estimation/frequency_bigram
        prediction[[word]] <- probability
        
        cumulative_probability <- cumulative_probability + probability
      }
      beta <- 1 - cumulative_probability
      
      if (length(word_candidates) < 3) {
        prediction_bigrams <- katz_backoff_model(c(ngram[n]))
        cumulative_estimations <- 0
        for (word in ls(prediction_bigrams)) {
          cumulative_estimations <- cumulative_estimations + prediction_bigrams[[word]]
        }
        alpha <- beta / cumulative_estimations
        for (word in ls(prediction_bigrams)) {
          if (!exists(word, envir=prediction, inherits=FALSE)) {
            prediction[[word]] <- alpha * prediction_bigrams[[word]]
          }
        }
      }
    } else {
      prediction <- katz_backoff_model(c(ngram[n]))
    }
  } else if (ngram_size == 1) {
    unigram <- ngram[n]
    if (exists(unigram, envir=ngram_model, inherits=FALSE)) {
      word_candidates <- ngram_model[[unigram]]
      frequency_unigram <- unigrams[[unigram]]
      for (word in word_candidates) {
        bigram_candidate <- paste(unigram, word)
        frequency_bigram <- bigrams[[bigram_candidate]]
        gte_estimation <- bigram_estimations[[toString(frequency_bigram)]]
        probability <- gte_estimation/frequency_bigram
        prediction[[word]] <- probability
      }
    }
  }
  prediction
}

shinyServer(function(input, output) {
  
  predict_next_word <- reactive({
    text <- input$text
    num_results <- input$num_results
    # https://gist.github.com/sebleier/554280
    stop_words <- c('i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', "you're", "you've", "you'll", "you'd", 'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', "it's", 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don', "don't", 'should', "should've", 'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', "mustn't", 'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren', "weren't", 'won', "won't", 'wouldn', "wouldn't")
    # http://www.slate.com/blogs/lexicon_valley/2013/09/11/top_swear_words_most_popular_curse_words_on_facebook.html
    swear_words <- c("shit", "fuck", "damn", "bitch", "crap", "piss",  "dick", "darn",  "cock", "pussy", "asshole", "fag",  "bastard", "slut", "douche", "bloody", "cunt", "bugger", "bollocks", "arsehole")
    word_predictions <- NULL
    if (text != '') {
      word_list <- unlist(strsplit(clean_text(text), " ", fixed = TRUE))
      predictions <- katz_backoff_model(word_list)
      word_list <- as.list(predictions)
      if (length(word_list) != 0) {
        score_predictions <- word_list[order(-unlist(word_list))]
        temp_word_predictions <- names(score_predictions)
        for (word in temp_word_predictions) {
          flag <- (input$show_stop_words || !(word %in% stop_words)) && (input$show_swear_words || !(word %in% swear_words))
          if (flag) {
            word_predictions <- c(word_predictions, word)
          }
        }
      }
    }
    head(word_predictions, num_results)
  })
  
  output$result <- renderText({
    prediction <- predict_next_word()
    output <- NULL
    if (!is.null(prediction)) {
      output <- paste(prediction)
    } else {
      output <- 'No results found'
    }
    output
  })
  
})
