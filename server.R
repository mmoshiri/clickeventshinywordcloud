
server <- function(session, input, output){
  
  dataStorage <- shiny::reactiveValues(
    frequencies_original = data.table::data.table(),
    frequencies_filtered = data.table::data.table()
  )
  
  # observeEvent doesn;t need to be assigned to a variable - it starts a 'listening' function
  # so it will watch if input$go changes and do something if it did change
  
  observeEvent(input$go, {
    # IF RUNNING runApp, comment the input = ... lines out! They are only for testing
     #input = list(
       #filter = c("medical", "numbers", "stopwords", "metabolomics"),
       #searchTerm = "glucose",
       #dateRange = c(2000, 2020),
       #absFreq = 100,
       #topWords=200
       # )
    
    shiny::withProgress({ # progress bar
      abstracts = getAbstracts(searchTerms = input$searchTerm,
                               mindate = input$dateRange[1],
                               maxdate = input$dateRange[2],
                               retmax = input$absFreq)
      shiny::setProgress(0.5)
      dataStorage$frequencies_original <- getWordFrequency(abstracts)
      dataStorage$frequencies_filtered <- dataStorage$frequencies_original
    }
    , message = "Searching...", max = 1)
  })
  observeEvent(input$filter, {
    print("changed")
    # get all the lists of filter words that the user wants, and join them together into a big list
    filterList <- data.table::rbindlist(filter_storage[input$filter], fill=T)
    # remove single character words
    # start= ^ , any character = . , end = $"
    singleChar <- grep(dataStorage$frequencies_original$word, pattern = "^.{1,3}$", value = T) 
    # remove verbs ending on -es and -ed (differentiated, etc.)
    verbs <- grep(dataStorage$frequencies_original$word, pattern = ".*[ed|es]$", value = T) 
    # remove numbers (p-values and the like)
    numericals = dataStorage$frequencies_original$word[which(!is.na(as.numeric(dataStorage$frequencies_original$word)))]
    # make an extra filter list for the stuff that comes specifically from this search term
    additionalFilters <- data.table::data.table(word = c(strsplit(input$searchTerm, 
                                                                  # dont include the words themselves
                                                                  split = " ")[[1]],
                                                         singleChar,
                                                         numericals,
                                                         verbs))
    # merge into final filter list
    filterList <- rbind(filterList,
                        additionalFilters, fill = T)
    
    without_stopwords <- getFilteredWordFreqency(dataStorage$frequencies_original, filterList)
    dataStorage$frequencies_filtered <- without_stopwords
  })
  
  observe({
    output$cloud <- renderWordcloud2({ 
      if(nrow(dataStorage$frequencies_filtered) > 0){
        wordcloud2(dataStorage$frequencies_filtered[1:input$topWords,], color = "random-light", size=.7, shape = "circle")
      }
    })  
  })
  if(interactive()){
    print(input$selected_word)
  }
}






