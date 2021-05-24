# Loading individual Tidyverse packages
# .... YOUR CODE FOR TASK 1 ....
library(dplyr)
library(readr)
library(ggplot2)
# Reading in the McGill Billboard chord data
bb <- read_csv("datasets/bb_chords.csv")

# Taking a look at the first rows in bb
# .... YOUR CODE FOR TASK 1 ....
head(bb)

# Counting the most common chords
bb_count <- bb %>% 
  count(chord, sort = TRUE)


# Displaying the top 20 chords
# .... YOUR CODE FOR TASK 2 ....
bb_count[1:20,]

# Creating a bar plot from bb_count
bb_count %>%
  slice(1:20) %>%
  mutate(share = n/sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(chord, share, fill = chord)) +
  geom_col() +
  coord_flip() +
  xlab('Chord') +
  ylab('Share of total chords') +
  theme(legend.position = "none")

# Wrangling and counting bigrams
bb_bigram_count <- bb %>%
  # .... YOUR CODE FOR TASK 4 ....
  mutate(next_chord = lead(chord), next_title = lead(title), bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>% count(bigram)
# Displaying the first 20 rows of bb_bigram_count
head(bb_bigram_count)

# Creating a column plot from bb_bigram_count
bb_bigram_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         bigram = reorder(bigram, share)) %>%
  ggplot(aes(bigram, share, fill = bigram)) +
  geom_col() +
  coord_flip() +
  ylab('Share of total chord changes') +
  xlab('Chord change') +
  theme(legend.position="none")

# Finding 30 artists with the most songs in the corpus
bb_30_artists <- bb %>%
  select(artist, title) %>%
  unique() %>% 
  count(artist, sort = TRUE)
# Displaying 30 artists with the most songs in the corpus
bb_30_artists %>%
  slice(1:30)

tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Creating a new dataframe bb_tagged that includes a new column instrument from tags
bb_tagged <- bb %>%
  inner_join(tags)
# Displaying the new data frame
bb_tagged

# The top 20 most common chords
top_20 <- bb_count$chord[1:20]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, instrument, sort = TRUE) %>%
  ggplot(aes(chord, n, fill = chord)) +
  facet_grid(~instrument) +
  coord_flip() +
  geom_col() +
  xlab('Chords') +
  ylab('Count') 

# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, instrument, sort = TRUE) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total bigrams') +
  xlab('Bigram') +
  theme(legend.position="none")

# Set to TRUE or FALSE to reflect your answer
hypothesis_valid <- TRUE

# Set to TRUE or FALSE to reflect your answer
more_data_needed <- TRUE