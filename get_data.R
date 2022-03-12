
# get data -------------------------------------------------------------

guessable_words <-
  read_csv("data/raw_data/guessable_words_01.csv",
           col_names = "word",
           show_col_types = FALSE)

mystery_words <-
  read_csv("data/raw_data/mystery_words_01.csv",
           col_names = "word",
           show_col_types = FALSE) 


