
# shared functions --------------------------------------------------------


# Generate word -----------------------------------------------------------

generate_word <- function(){
  sample(mystery_words$word,1)
}

# Fast check guess (vectored) ----------------------------------------------------

# this is fast

check_guess_set_goal_vector <- function(df){
  
  # function takes an input df with vars guess and answer
  # and adds a var called results
  
  # word vectors
  guess <- df$guess
  answer <- df$answer
  
  # individual letter vectors - being very explicit because 
  # accessing nested vectors is annoying
  # someone better than me could do it without repeating every command 5 times
  
  gl1 <- str_sub(guess,1,1)
  gl2 <- str_sub(guess,2,2)
  gl3 <- str_sub(guess,3,3)
  gl4 <- str_sub(guess,4,4)
  gl5 <- str_sub(guess,5,5)
  
  al1 <- str_sub(answer,1,1)
  al2 <- str_sub(answer,2,2)
  al3 <- str_sub(answer,3,3)
  al4 <- str_sub(answer,4,4)
  al5 <- str_sub(answer,5,5)
  
  # making pass vectors
  
  p1 <- fifelse(gl1 == al1, TRUE, FALSE)
  p2 <- fifelse(gl2 == al2, TRUE, FALSE)
  p3 <- fifelse(gl3 == al3, TRUE, FALSE)
  p4 <- fifelse(gl4 == al4, TRUE, FALSE)
  p5 <- fifelse(gl5 == al5, TRUE, FALSE)
  
  # removing passed letters from the checklist
  # using string instead of NA for valid testing later
  
  al1 <- fifelse(p1, "", al1)
  al2 <- fifelse(p2, "", al2)
  al3 <- fifelse(p3, "", al3)
  al4 <- fifelse(p4, "", al4)
  al5 <- fifelse(p5, "", al5)
  
  # making results vectors
  # for each guessed letter that hasn't passed,
  # we assign a number as to where it appears in the word
  # or a 0 if it doesn't
  # we only test 4 times per letter (no point repeating the case for when
  # it's in the right place)
  # we only take the first gl that matches
  
  # Halves L1
  h1 <- fcase(
    gl1 == al2, 2,
    gl1 == al3, 3,
    gl1 == al4, 4,
    gl1 == al5, 5,
    default = 0
  )
  # then remove the half from the answer list
  al2 <- fifelse(h1 == 2, "", al2)
  al3 <- fifelse(h1 == 3, "", al3)
  al4 <- fifelse(h1 == 4, "", al4)
  al5 <- fifelse(h1 == 5, "", al5)
  
  # Halves L2
  
  h2 <- fcase(
    gl2 == al1, 1,
    gl2 == al3, 3,
    gl2 == al4, 4,
    gl2 == al5, 5,
    default = 0
  )
  
  # then remove the half from the answer list
  al1 <- fifelse(h2 == 1, "", al1)
  al3 <- fifelse(h2 == 3, "", al3)
  al4 <- fifelse(h2 == 4, "", al4)
  al5 <- fifelse(h2 == 5, "", al5)
  
  # Halves L3
  h3 <- fcase(
    gl3 == al1, 1,
    gl3 == al2, 2,
    gl3 == al4, 4,
    gl3 == al5, 5,
    default = 0
  )
  
  # then remove the half from the answer list
  al1 <- fifelse(h3 == 1, "", al1)
  al2 <- fifelse(h3 == 2, "", al2)
  al4 <- fifelse(h3 == 4, "", al4)
  al5 <- fifelse(h3 == 5, "", al5)
  
  # Halves L4
  h4 <- 
    fcase(
      gl4 == al1, 1,
      gl4 == al2, 2,
      gl4 == al3, 3,
      gl4 == al5, 5,
      default = 0
    )
  
  # then remove the half from the answer list
  al1 <- fifelse(h4 == 1, "", al1)
  al2 <- fifelse(h4 == 2, "", al2)
  al3 <- fifelse(h4 == 3, "", al3)
  al5 <- fifelse(h4 == 5, "", al5)
  
  # Halves L5
  # h5 <- test_half(gl5)
  h5 <- fcase(
    gl5 == al1, 1,
    gl5 == al2, 2,
    gl5 == al3, 3,
    gl5 == al4, 4,
    default = 0
  )
  
  # halves: 
  r1 <- fifelse(h1 > 0, "h", "f")
  r2 <- fifelse(h2 > 0, "h", "f")
  r3 <- fifelse(h3 > 0, "h", "f")
  r4 <- fifelse(h4 > 0, "h", "f")
  r5 <- fifelse(h5 > 0, "h", "f")
  
  
  # passes
  r1 <- fifelse(p1, "p", r1)
  r2 <- fifelse(p2, "p", r2)
  r3 <- fifelse(p3, "p", r3)
  r4 <- fifelse(p4, "p", r4)
  r5 <- fifelse(p5, "p", r5)
  
  
  
  results <- paste0(r1,r2,r3,r4,r5)
  
  #slam them in
  df$results <- results
  
  df
}

# check single guesses ----------------------------------------------------

get_guess_answer_results <- function(guess,
                                     answer,
                                     print = FALSE){
  test_guessable_word(guess)
  test_winnable_word(answer)
  # make results
  results <- 
    tibble(guess = guess,
           answer = answer) %>% 
    check_guess_set_goal_vector() %>% 
    select(results) %>% 
    pull()
  
  if (print) {
    output_guess_with_colours(guess, results)
  } else {
    return(results)
  }
}

report_guess_results <- function(guess,answer){
  get_guess_answer_results(guess,answer,print = TRUE)
}

play_wordle <- function(guess){
  
  answer <- generate_word()
  
  for(t in (1:6)){
    
    while(1 == 1){ # do this forever until a valid guess is chosen for the turn
      
      guess <- 
        readline(prompt = paste0("Turn ",t," - Enter guess: "))
      
      if(!guess %in% guessable_words$word){
        message("'",guess,"' is not a valid guess :(")
      } else {
        break()
      }
    }
    
    results <- get_guess_answer_results(guess,answer)
    report_guess_results(guess,answer)
    
    if(results == "ppppp"){
      turns <- t
      break()
    }
  }
  
  if(results == "ppppp"){
    
    result_string <- case_when(
      turns == 1 ~ "lmao you got it first go, wow",
      turns == 2 ~ "A double birdie! You took two turns",
      turns == 3 ~ "A birdie! Got it in three.",
      turns == 4 ~ "4 Turns. Nice! That's par.",
      turns == 5 ~ "It took five goes, but you made it and that's what counts.",
      turns == 6 ~ "Phew! Close one.",
      TRUE ~ "this game has been developed badly"
      )
    message(result_string)
  } else {
    message("You couldn't quite find '",answer,"' :(. Go again?")
  }
  
}


# filter words ------------------------------------------------------------




filter_mystery_words <- function(df, guess, filter_results){
  
  
  # filter a dataframe with 'word' var based on a guess and returned results
  # does not need a winning word
  # smart default df is mystery_words, but maybe you've already filtered this
  # this method filters twice (slow) but is robust to changes in the df
  
  # fail early and clearly
  test_valid_result_set(filter_results)
  test_guessable_word(guess)
  
  # get acceptable words
  words <- 
    df %>% 
    rename(answer = word) %>% 
    mutate(guess = guess) %>% 
    # get results for all your answers on this guess
    check_guess_set_goal_vector() %>% 
    # only want your results
    filter(results == filter_results) %>% 
    pull(answer)
  
  # filter df on acceptable words
  df %>% 
    filter(word %in% words) %>% 
    return()
}




# combine above -----------------------------------------------------------


#check, filter, report
# WIP as needed
get_result_report <- function(guess,
                              results,
                              df = mystery_words){
  
  # generate filtered
  
  filtered_mystery_words <- 
    df %>% 
    filter_mystery_words(guess,results) %>% 
    pull(word)
  
  # Output
  message(paste0("Given these results, there are ",
                   length(filtered_mystery_words),
                   " remaining possibilities: "))
  
  print(filtered_mystery_words)
}






# get_result_report("apple", "ffpfh")

