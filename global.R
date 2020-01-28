setwd("C:/Users/A   S    U    S/Desktop/GitGub/maryam_shiny")
#getwd()
library(rstudioapi)    
#script_path = rstudioapi::getActiveDocumentContext()$path
#script_dir = dirname(script_path)
#setwd(script_dir)
library(shiny)
library(RISmed)
library(dplyr)
library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud2)
library(data.table)
filterList <- data.table::fread("filterList.csv")
getAbstracts <- function(searchTerms, retmax=500, mindate=2000, maxdate=2019){
  searchTerms = strsplit(searchTerms, " ")[[1]]
  # ==== SEARCH A METABOLITE TERM =====
  SUMMARYx <- EUtilsSummary(paste0(searchTerms, collapse="+"),type = "esearch", db = "pubmed",
                            datetype = "edat",retmax = 500, 
                            mindate = 2000, maxdate = 2019)
  Idsx <- QueryId(SUMMARYx)
  # Get Download(Fetch)
  MyDownloadsx <- EUtilsGet(SUMMARYx, type = "efetch", db = "pubmed")
  #Make Data.frame of MyDownloads
  abstractsx <- data.frame(title = MyDownloadsx@ArticleTitle,
                           abstract = MyDownloadsx@AbstractText,
                           journal = MyDownloadsx@Title,
                           DOI = MyDownloadsx@PMID,
                           year = MyDownloadsx@YearPubmed)
  #Constract a charaterized variable for abstract of abstracts data.frame
  abstractsx <- abstractsx %>% mutate(abstract = as.character(abstract))
  #abstractsx$abstract <- as.character(abstractsx$abstract) # alternative for above line
  return(abstractsx)
}

#wordfrequency of whole abstract #frequencies = getWordFrequency(abstracts)#it should be # from here
getWordFrequency <- function(abstractsx){
  #Split a column into tokens using the tokenizers package
  CorpusofMyCloudx <- unique(abstractsx %>% 
                               unnest_tokens(word, abstract)) %>% count(word, sort = TRUE)
  CorpusofMyCloudx$word <- gsub("^\\d+$", "", CorpusofMyCloudx$word)
  return(CorpusofMyCloudx)
}

getFilteredWordFreqency <- function(frequencies, filterList){
  filterWords <- unique(filterList$word)
  filteredWords <- frequencies[!(frequencies$word %in% filterWords),]
  return(filteredWords)
}

filter_storage <- list(
  medical = filterList,
  stopwords = tidytext::stop_words,
  metabolomics = data.table::data.table(word=c("metabolism", "metabolic", 
                                               "metabolomic", "metabolomics",
                                               "biochemical", "mass", "spectrometry", 
                                               "nmr", "direct", "infusion"))
)


