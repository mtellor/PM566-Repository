# Set-up ----
library(tidyverse)
library(tidytext)
library(textdata)
library(knitr)
library(kableExtra)
library(forcats)

# Load data
pubmed <- read_csv("~/Desktop/pubmed.csv") |> 
  mutate(
    abstract_id = row_number(),
    term = str_squish(term),
    abstract = str_squish(abstract)
  ) |> 
  filter(!is.na(abstract), abstract != "")

# Question 1 ----
pubmed %>%
  count(term, name = "n_abstracts") %>%
  arrange(desc(n_abstracts)) %>%
  kable(
    col.names = c("Search Term", "Number of Abstracts"),
    caption = "Number of Abstracts per Search Term"
  ) %>%
  kable_styling(full_width = FALSE)

tokens <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  count(word, sort = TRUE)

tokens %>%
  slice_max(n, n = 20) %>%
  kable(
    col.names = c("Token", "Count"),
    caption = "Top 20 Most Frequent Tokens (Raw)"
  ) %>%
  kable_styling(full_width = FALSE)

tokens_by_term <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  count(term, word, sort = TRUE) %>%
  group_by(term) %>%
  slice_max(n, n = 5) %>%
  ungroup()

tokens_by_term %>%
  kable(
    col.names = c("Search Term", "Token", "Count"),
    caption = "Top 5 Tokens per Search Term (Raw)"
  ) %>%
  kable_styling(full_width = FALSE)

# Question 2 ----
data("stop_words")

tokens_clean <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%
  count(term, word, sort = TRUE) %>%
  group_by(term) %>%
  slice_max(n, n = 5) %>%
  ungroup()

tokens_clean %>%
  kable(
    col.names = c("Search Term", "Token", "Count"),
    caption = "Top 5 Tokens per Search Term (Stop Words Removed)"
  ) %>%
  kable_styling(full_width = FALSE)

# Question 3 ----
bigrams <- pubmed %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams %>%
  slice_max(n, n = 10) %>%
  kable(
    col.names = c("Bigram", "Count"),
    caption = "Top 10 Most Frequent Bigrams (Stop Words Removed)"
  ) %>%
  kable_styling(full_width = FALSE)

bigrams %>%
  slice_max(n, n = 10) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) +
  geom_col(fill = "dodgerblue4") +
  labs(
    title = "Top 10 Bigrams in PubMed Abstracts",
    x = "Frequency", y = "Bigram"
  ) +
  theme_bw()

# Question 4 ----
term_tokens <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%
  count(term, word, sort = TRUE)

tfidf <- term_tokens %>%
  bind_tf_idf(word, term, n)

top_tfidf <- tfidf %>%
  group_by(term) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup()

top_tfidf %>%
  kable(
    col.names = c("Search Term", "Token", "Count", "TF", "IDF", "TF-IDF"),
    caption = "Top 5 Tokens per Search Term by TF-IDF"
  ) %>%
  kable_styling(full_width = FALSE)

top_tfidf %>%
  mutate(word = reorder_within(word, tf_idf, term)) %>%
  ggplot(aes(x = word, y = tf_idf)) +
  geom_col(fill = "dodgerblue4") +
  coord_flip() +
  facet_wrap(~ term, scales = "free_y") +
  tidytext::scale_x_reordered() +
  labs(
    title = "Top 5 Tokens per Search Term by TF-IDF",
    x = "Token", y = "TF-IDF Score"
  ) +
  theme_bw()

# Question 5 ----
nrc <- get_sentiments("nrc")

nrc_full <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  inner_join(nrc, by = "word") %>%
  count(term, sentiment, sort = TRUE) %>%
  group_by(term) %>%
  slice_max(n, n = 1) %>%
  ungroup()

nrc_full %>%
  kable(
    col.names = c("Search Term", "Sentiment", "Count"),
    caption = "Most Common NRC Sentiment per Search Term"
  ) %>%
  kable_styling(full_width = FALSE)

nrc_specific <- nrc %>%
  filter(!sentiment %in% c("positive", "negative"))

nrc_filtered <- pubmed %>%
  unnest_tokens(word, abstract) %>%
  inner_join(nrc_specific, by = "word") %>%
  count(term, sentiment, sort = TRUE) %>%
  group_by(term) %>%
  slice_max(n, n = 1) %>%
  ungroup()

nrc_filtered %>%
  kable(
    col.names = c("Search Term", "Sentiment", "Count"),
    caption = "Most Common NRC Sentiment per Search Term (Excluding Positive/Negative)"
  ) %>%
  kable_styling(full_width = FALSE)


# Question 6 ----
afinn <- get_sentiments("afinn")

afinn_scores <- pubmed %>%
  mutate(abstract_id = row_number()) %>%
  unnest_tokens(word, abstract) %>%
  inner_join(afinn, by = "word") %>%
  group_by(term, abstract_id) %>%
  summarize(score = mean(value, na.rm = TRUE), .groups = "drop")

afinn_scores %>%
  ggplot(aes(x = term, y = score, fill = term)) +
  geom_boxplot() +
  labs(
    title = "AFINN Sentiment Scores by Search Term",
    x = "Search Term", y = "Average Positivity Score"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")
