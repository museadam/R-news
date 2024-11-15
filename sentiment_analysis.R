# Load libraries
library(tidytext)
library(tidyverse)
library(textdata)
library(ggplot2)
library(dplyr)

# Sample dataset of tweets or reviews
text_data <- data.frame(
  id = 1:5,
  text = c(
    "I love this product! It's amazing!",
    "Terrible experience, never buying again.",
    "Decent quality, but could be better.",
    "Absolutely fantastic! Highly recommend.",
    "Not worth the money. Disappointed."
  )
)

# Tokenize text
tidy_text <- text_data %>%
  unnest_tokens(word, text)

# Remove stop words
tidy_text <- tidy_text %>%
  anti_join(stop_words, by = "word")

# View cleaned text
print(tidy_text)

# Download sentiment lexicon
sentiments <- get_sentiments("bing") # Can also use "afinn" or "nrc"

# Join with tokenized words to get sentiment
text_sentiment <- tidy_text %>%
  inner_join(sentiments, by = "word") %>%
  count(id, sentiment, sort = TRUE) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# Calculate net sentiment score
text_sentiment <- text_sentiment %>%
  mutate(sentiment_score = positive - negative)

print(text_sentiment)

# Bar plot of sentiment scores
ggplot(text_sentiment, aes(x = factor(id), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis Results", x = "Text ID", y = "Sentiment Score") +
  theme_minimal()

# Count word frequency by sentiment
word_sentiment <- tidy_text %>%
  inner_join(sentiments, by = "word") %>%
  count(word, sentiment, sort = TRUE)

# Visualize top words
word_sentiment %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(title = "Top Words by Sentiment", x = "Words", y = "Count") +
  coord_flip() +
  theme_minimal()
