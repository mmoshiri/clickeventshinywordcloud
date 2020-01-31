
  
  ui <- fluidPage(
    titlePanel("Word Cloud Plot"),
    sidebarLayout(
    sidebarPanel(
      helpText("Type a metabolite below and search PubMed to find documents that contain that word in the text."),
      textInput("searchTerm", label = h3("Enter your search terms"), placeholder = "enter your search terms"),
      hr(),
      #dateRangeInput('dateRange',label = 'Date range input: yyyy-mm-dd',start = Sys.Date() - 2, end = Sys.Date() + 2),
      helpText("You can specify the start and end years of your search, use the format YYYY"),
      sliderInput(inputId = "dateRange", label = "Date range input:",min = 2000, max = 2019,value = c(2010, 2020), sep = ""),
      sliderInput("absFreq", "Amount of abstracts to use:", min = 1,  max = 4000, value = 500),
      sliderInput("topWords", "Top words used for plot:", min = 1,  max = 10000, value = 100),
      selectizeInput("filter",
                     "Filter",
                     choices = c("medical","stopwords","metabolomics"), 
                     multiple = T),
     actionButton("go", "Go! :)"),
     hr(),
     textOutput(outputId="caption")
    ), 
    mainPanel(
      verbatimTextOutput("absFreq"),
      verbatimTextOutput("filter"),
      verbatimTextOutput("dateRange"),
      wordcloud2Output(outputId = "cloud", height = "800px"),
      verbatimTextOutput("value"),
      tags$script(HTML(
        "$(document).on('click', '#canvas', function() {",
        'word = document.getElementById("wcSpan").innerHTML;',
        "Shiny.onInputChange('selected_word', word);",
        "});"
      ))
      
    )
  )
)

