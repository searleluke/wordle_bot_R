

# cheat at wordle ---------------------------------------------------------

# function expects a table of guesses and results
# no more than 5 rows


cheat_at_wordle <- function(live_guess){
  
  # params
  df <- mystery_words
  turn_number <- nrow(live_guess)
  guesses <- live_guess$guess
  results <- live_guess$results
  
  # create filtered words
  
  message("Turns used: ",turn_number)
  
  for (i in 1:turn_number){
    output_guess_with_colours(guesses[i],results[i])
    
    df <- 
      df %>% 
      filter_mystery_words(guesses[i],results[i])
    
  }
  
  # show remaining words
  
  remaining_words <- df$word
  if(nrow(df) == 1){
    message(nrow(df)," word remaining:")  
  } else {
    message(nrow(df)," words remaining:")  
  }
  
  print(remaining_words)
  
  # get next best guess
  get_best_next_guess(guesses[turn_number],
                      results[turn_number],
                      input_df = df) %>% 
    glimpse()
}


