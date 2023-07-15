library(dplyr)
library(stringr)

# Map function: Split text into words and count occurrences
map_func <- function(text) {
  words <- str_split(text, "\\W+")
  data.frame(word = unlist(words), count = 1, stringsAsFactors = FALSE)
}

# Reduce function: Sum the counts for each word
reduce_func <- function(data) {
  data %>% group_by(word) %>% summarise(count = sum(count))
}

# Word count function
word_count <- function(input_data) {
  mapped_data <- lapply(input_data, map_func)
  reduced_data <- do.call(rbind, lapply(mapped_data, reduce_func))
  return(reduced_data)
}

# Word frequency function
word_frequency <- function(input_data) {
  mapped_data <- lapply(input_data, map_func)
  reduced_data <- do.call(rbind, lapply(mapped_data, reduce_func))
  total_words <- sum(reduced_data$count)
  reduced_data$freq <- reduced_data$count / total_words
  return(reduced_data)
}

input_data <- c(
  "Hello world",
  "Hello again",
  "World, world, world"
)

count_result <- word_count(input_data)
print(count_result)

frequency_result <- word_frequency(input_data)
print(frequency_result)




