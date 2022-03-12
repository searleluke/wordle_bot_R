
# speed testing getting guess results -------------------------------------

library(microbenchmark)
# function setup ----------------------------------------------------------

get_all_results_for_guess_original <- function(guess,option_df = mystery_words, print = FALSE){
  
  # Takes a guessable word and tests it against all answers
  # returns a df with the guess, a possible result set and
  # the number of times that result set appears
  
  # a smart default is mystery_words, but maybe this is used later
  
  if(print){
    message(paste0("Testing '",guess,"'..."))
  }
  
  df <- 
    option_df$word %>% 
    map_dfr(
      ~tribble(~guess,~results,
               guess,check_guess_set_goal(guess,.x))
    ) %>% 
    
    group_by(guess,results) %>% 
    summarise(results_appear = n(),.groups = "drop") 
  
}

get_all_results_for_guess_noprint <- function(guess,option_df = mystery_words){
  
  # no appreciable difference
  df <- 
    option_df$word %>% 
    map_dfr(
      ~tribble(~guess,~results,
               guess,check_guess_set_goal(guess,.x))
    ) %>% 
    
    group_by(guess,results) %>% 
    summarise(results_appear = n(),.groups = "drop") 
  
}

get_all_results_for_guess_randomideas <- function(guess,option_df = mystery_words){
  
  # Takes a guessable word and tests it against all answers
  # returns a df with the guess, a possible result set and
  # the number of times that result set appears
  
  # a smart default is mystery_words, but maybe this is used later
  
  # rewritten for speeeeeeed

  
  wordlist <- list()
  wordlist$answer <- option_df$word
  
  # wordlist$results <- check_guess_set_goal(guess,wordlist$answer)
  
  for(i in seq_along(wordlist$answer)){
    
    wordlist$results[i] <- list(check_guess_set_goal(guess,
                                                     wordlist$answer[[i]]
    )
    )
  }
  
  df <- 
    tibble(results = wordlist$results) %>% 
    mutate(guess = guess) %>% 
    select(guess,results) %>% 
    group_by(guess,results) %>%
    summarise(results_appear = n(),.groups = "drop") %>%
    return()
}

get_all_results_for_guess_randomideas_print <- function(guess,option_df = mystery_words,print = TRUE){
  
  # Takes a guessable word and tests it against all answers
  # returns a df with the guess, a possible result set and
  # the number of times that result set appears
  
  # a smart default is mystery_words, but maybe this is used later
  
  # rewritten for speeeeeeed
  
  if(print){
    message(paste0("Testing '",guess,"'..."))
  }
 
  wordlist <- list()
  wordlist$answer <- option_df$word
  
  # wordlist$results <- check_guess_set_goal(guess,wordlist$answer)
  
  for(i in seq_along(wordlist$answer)){
    
    wordlist$results[i] <- list(check_guess_set_goal(guess,
                                                     wordlist$answer[[i]]
                                                     )
                                )
  }
  
  df <- 
    tibble(results = wordlist$results) %>% 
    mutate(guess = guess) %>% 
    select(guess,results) %>% 
    group_by(guess,results) %>%
    summarise(results_appear = n(),.groups = "drop") %>%
    return()
}

get_all_results_for_guess_listoutput <- function(guess,option_df = mystery_words, print = TRUE){
  
  # Takes a guessable word and tests it against all answers
  # returns a df with the guess, a possible result set and
  # the number of times that result set appears
  
  # a smart default is mystery_words, but maybe this is used later
  
  if(print){
    message(paste0("Trying '",guess,"'"))
  }
  
  wordlist <- list()
  wordlist$answer <- option_df$word
  
  # wordlist$results <- check_guess_set_goal(guess,wordlist$answer)
  
  for(i in seq_along(wordlist$answer)){
    
    wordlist$results[i] <- list(check_guess_set_goal(guess,
                                                     wordlist$answer[[i]]))
    
    
  }
  
  wordlist$guess <- rep(guess,length(wordlist$answer))
  
  # df <- 
  #   tibble(results = wordlist$results) %>% 
  #   mutate(guess = guess) %>% 
  #   select(guess,results) %>% 
  #   group_by(guess,results) %>%
  #   summarise(results_appear = n(),.groups = "drop") %>%
  #   return()
  
  return(wordlist)
  
}


random_ideas <- 
  get_all_results_for_guess_randomideas("soare")

random_ideas_list <- 
  get_all_results_for_guess_listoutput("soare")
  
random_ideas <- 
  get_all_results_for_guess_randomideas_print("soare")

random_ideas %>% 
  glimpse()

random_ideas_list_results <- 
  tibble(guess = random_ideas_list$guess,
       results = random_ideas_list$results) %>% 
    group_by(guess,results) %>%
    summarise(results_appear = n(),.groups = "drop")
    

random_ideas_list_results %>% 
# random_ideas %>% 
  all_equal(expected_output)
# testing -----------------------------------------------------------------

expected_output <- 
  get_all_results_for_guess("soare")
  # a df with a guess, results, and the # of times the result appeared
  # produced by playing wordle with all answers and the same guess
  # then reporting how many times each answer comes up

expected_output %>% glimpse()


mbm_original <- microbenchmark(get_all_results_for_guess("soare"), times = 20)
mbm_noprint  <- microbenchmark(get_all_results_for_guess_noprint("soare"), times = 20)
mbm_randomideas  <- microbenchmark(get_all_results_for_guess_randomideas("soare"), times = 20)
mbm_randomideas_print  <- microbenchmark(get_all_results_for_guess_randomideas_print("soare"), times = 20)
mbm_randomideas_list  <- microbenchmark(get_all_results_for_guess_listoutput("soare"), times = 20)



mbm_original
mbm_noprint # moderate improvement
mbm_randomideas # significant improvement
mbm_randomideas_print # some costs to print - maybe worth for fun tbh
mbm_randomideas_list # doing less so faster


all_equal(expected_output,random_ideas)



# all guesses -------------------------------------------------------------
# random_guesses to try
random_guesses <- 
  guessable_words %>% 
  ungroup() %>% 
  slice_sample(n = 20) %>% 
  glimpse()

get_all_results_for_all_guesses_listoutput <- function(guess_df = guessable_words,
                                                       option_df = mystery_words,
                                                       print = TRUE){
  
  
  big_list <- list()
  
  for(i in seq_along(guess_df$word)){
    # print(guess_df$word[i])
    big_list$guess <- guess_df$word[i]
    big_list$results[i] <- get_all_results_for_guess_listoutput(guess_df$word[i])
    
  }
  return(big_list)
}



test <- 
  get_all_results_for_all_guesses_listoutput(guess_df = random_guesses,
                                             option_df = option_df,
                                             print = TRUE
                                             )

test2 <- 
  get_all_results_for_all_guesses(guess_df = random_guesses,
                                             option_df = option_df,
                                             print = TRUE
  )

test %>% view()

random_guesses
option_df
