library(tm)
library(dplyr)
library(stringr)
library(wordcloud)
library(quanteda)
library(syuzhet)

# setwd("~/Desktop/BDA/Project_3")
setwd("/Users/bhavya/Academics/ThirdSem/BDA/Project3")
getwd()
tarzan <- VCorpus(DirSource('.', pattern = "\\.txt$", ignore.case = TRUE))
str(tarzan)
tarzan

text <- tarzan[[1]]
text
text[1]

# Read the entire book into R
book_lines <- readLines("TarzanOfTheApes.txt", warn = FALSE)
book_lines

# Find the indices of chapter beginnings
chapter_indices <- grep("^Chapter [IVXLCDM]+", book_lines)

# Make sure we only get the first 15 chapters
if (length(chapter_indices) > 15) {
  chapter_indices <- chapter_indices[1:16]
}

# Create a directory for the chapters
dir.create("chapters", showWarnings = FALSE)

# Write the first 15 chapters to files
for (i in 1:(length(chapter_indices) - 1)) {
  # Extract the chapter text
  book_chapter <- book_lines[chapter_indices[i]:(chapter_indices[i + 1] - 1)]
  
  # Define the filename
  file_name <- sprintf("chapters/Chapter%d.txt", i)
  
  # Write the chapter text to a file
  write.table(book_chapter, file = file_name, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# Assuming the chapters have been written into separate files in the "chapters" directory
chapter_files <- list.files("chapters", pattern = "\\.txt$", full.names = TRUE)

# Extract the chapter numbers and sort the files based on these
chapter_numbers <- as.numeric(sub("chapters/Chapter(\\d+)\\.txt", "\\1", chapter_files))
chapter_files <- chapter_files[order(chapter_numbers)]


# Initialize lists to store the longest words and sentences
longest_words_list <- list()
longest_sentences_list <- list()

# Define a function to find the 10 longest words
find_longest_words <- function(text) {
  words <- unlist(strsplit(text, "\\W+"))
  unique_words <- unique(words)
  sorted_words <- unique_words[order(nchar(unique_words), decreasing = TRUE)]
  return(head(sorted_words, 10))
}

# Define a function to find the 10 longest sentences
find_longest_sentences <- function(text) {
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  sorted_sentences <- sentences[order(nchar(sentences), decreasing = TRUE)]
  return(head(sorted_sentences, 10))
}

# Process each chapter
for (i in seq_along(chapter_files)) {
  chapter_text <- tolower(readLines(chapter_files[i], warn = FALSE))
  longest_words <- find_longest_words(chapter_text)
  longest_sentences <- find_longest_sentences(chapter_text)
  
  # Store the results
  longest_words_list[[i]] <- longest_words
  longest_sentences_list[[i]] <- longest_sentences
}

# Combine results into data frames
longest_words_df <- data.frame(Chapter = seq_along(longest_words_list), Words = I(longest_words_list))
longest_sentences_df <- data.frame(Chapter = seq_along(longest_sentences_list), Sentences = I(longest_sentences_list))

# Print the tables
print(longest_words_df)
print(longest_sentences_df)


## Corpus Cleaning - Data Wrangling

while (!is.null(dev.list())) dev.off()

chapter_files
for (i in seq_along(chapter_files)) {
  
  chapter_number <- gsub("chapters/Chapter(\\d+)\\.txt", "\\1", chapter_files[i])
  
  print("Processing Chapter: ",chapter_number)
  
  # Read the text of the current chapter
  chapter_text <- tolower(readLines(chapter_files[i], warn = FALSE))
  chapter_corpus <- VCorpus(VectorSource(chapter_text))
  inspect(chapter_corpus)
  
  print("Cleaning Text of Chapter ", chapter_number)
  
  # Remove numbers and punctuation
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","", x)
  chapter_corpus <- tm_map(chapter_corpus, content_transformer(removeNumPunct))
  
  # Remove stopwords
  chapter_corpus <- tm_map(chapter_corpus, removeWords, stopwords("english"))
  
  # Strip whitespace
  chapter_corpus <- tm_map(chapter_corpus, stripWhitespace)
  inspect(chapter_corpus)
  
  # Create a DTM 
  print("Creating and inspecting DTM of Chapter", chapter_number)
  chapter_dtm <- DocumentTermMatrix(chapter_corpus)
  chapter_dtm
  str(chapter_dtm)
  inspect(chapter_dtm)
  as.matrix(chapter_dtm)
  
  # Create a TDM
  print("Creating and inspecting TDM of Chapter", chapter_number)
  chapter_tdm <- TermDocumentMatrix(chapter_corpus)
  inspect(chapter_tdm)
  
  # Find frequent terms
  print("Finding Frequent Terms", chapter_number)
  freq_terms <- findFreqTerms(chapter_tdm, lowfreq=5)
  freqs <- colSums(as.matrix(chapter_dtm))
  chapter_tf <- sort(freqs, decreasing = TRUE)
  tdm_matrix <- as.matrix(chapter_tdm)

  # Dendogram 
  print("Creating Dendogram of Chapter", chapter_number)
  tdm_df <- as.data.frame(tdm_matrix, stringsAsFactors=FALSE)
  tdm_dist <- dist(tdm_df)
  tdm_DG <- hclust(tdm_dist, method = "ward.D2")
  str(tdm_DG)
  plot_filename_dendrogram <- paste("dendrogram_chapter_", chapter_number, ".png")
  png(file = plot_filename_dendrogram, width = 800, height = 600)
  plot(tdm_DG, main = paste("Dendrogram for Chapter", chapter_number))
  dev.off()
  
  # Wordcloud
  print("Creating Wordcloud of Chapter ", chapter_number)
  words <- names(chapter_tf)
  png(file = paste("wordcloud_chapter_", chapter_number, ".png"), width = 800, height = 600)
  chapter_wc <- wordcloud(words = words, freq = chapter_tf, colors = brewer.pal(9, "BuGn")[-(1:4)], scale = c(2, 0.5))
  str(chapter_wc)
  dev.off()
  
  print("Quantitative Analysis of Chapter:", chapter_number)
  chapter_text[1:10]
  
  chapter_tokens <- tokens(chapter_text[1:10])
  str(chapter_tokens)
  
  chapter_dfm <- dfm(chapter_tokens)
  str(chapter_dfm)
  
  chapter_doc_freq <- docfreq(chapter_dfm)
  str(chapter_doc_freq)
  
  chapter_weights <- dfm_weight(chapter_dfm)
  str(chapter_weights)
  
  chapter_TFIDF <- dfm_tfidf(chapter_dfm, scheme_tf = "count", scheme_df = "inverse")
  str(chapter_TFIDF)
  
  chapter_df <- as.data.frame(chapter_text)
  chapter_df
  
  print("Performing Sentiment Analysis of Chapter:", chapter_number)
  chapter_path <- sprintf("chapters/Chapter%d.txt", i)
  chapter_string <- get_text_as_string(chapter_path)
  chapter_string

  chapter_sentences <- get_sentences(chapter_string)
  chapter_sentences
  str(chapter_sentences)

  chapter_sentiments <- get_sentiment(chapter_sentences, "syuzhet")
  chapter_sentiments
  print(str(chapter_sentiments))
  
  chapter_bing <- get_sentiment(chapter_sentences, "bing")
  chapter_bing
  
  chapter_dictionary <- get_sentiment_dictionary()
  chapter_dictionary
  
  chapter_dictionary_bing <- get_sentiment_dictionary("bing")
  chapter_dictionary_bing
  
  chapter_sum <- sum(chapter_sentiments)
  chapter_sum
  
  chapter_bing_sum <- sum(chapter_bing)
  chapter_bing_sum
  
  chapter_mean <- mean(chapter_sentiments)
  chapter_mean
  
  chapter_bing_mean <- mean(chapter_bing)
  chapter_bing_mean

  plot(chapter_sentiments, main = "Tarzan Apes of Text Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
  plot(chapter_bing, main = "Tarzan Apes of Text Plot Trajectory : Bing", xlab = "Narrative", ylab = "Emotional Valence")
  
  print("Creating Sentiment Percentage Values Plot of Chapter", i)
  
  chapter_sent_pct_val_10 <- get_percentage_values(chapter_sentiments, bins = 10)
  structure(chapter_sent_pct_val_10)
  plot_filename_10 <- sprintf("sentiment_percentage_plot_10_bins_chapter_%s.png", chapter_number)
  png(plot_filename_10, width = 800, height = 600)
  par(mar = c(2.1, 4.1, 4.1, 2.1))
  plot(chapter_sent_pct_val_10, main = paste("Tarzan Apes of Text PCT value 10 bins - Chapter", chapter_number), xlab = "Narrative", ylab = "Emotional Valence", col="red")
  dev.off()
  
  chapter_sent_pct_val_20 <- get_percentage_values(chapter_sentiments, bins = 20)
  structure(chapter_sent_pct_val_20)
  plot_filename_20 <- sprintf("sentiment_percentage_plot_20_bins_chapter_%s.png", chapter_number)
  png(plot_filename_20, width = 800, height = 600)
  par(mar = c(2.1, 4.1, 4.1, 2.1))
  plot(chapter_sent_pct_val_20, main = paste("Tarzan Apes of Text PCT value 20 bins - Chapter", chapter_number), xlab = "Narrative", ylab = "Emotional Valence", col="red")
  dev.off()
  
  print("Finished processing Chapter", chapter_number)
  
}




