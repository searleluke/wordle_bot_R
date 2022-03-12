# Get all first results ----

# this takes about two minutes to run
# works fine on my machine (16gb ram) but
# tends to clog memory and maker later mem allocation harder
# (until you restart session)
# recommend only running once and saving output, then restarting to clear mem

get_all_onestep_results <- function(reprocess = FALSE){
  
  if(reprocess){
    
    all_results_raw <- 
      #make df of all guess/word combos
      guessable_words %>% 
      select(guess = word) %>% 
      full_join(
        mystery_words %>% 
          select(answer = word),
        by = character()
      ) %>% 
      # get results for each combo
      check_guess_set_goal_vector()
    
    
    all_results <- 
      all_results_raw %>% 
      # summarise counts remaining after each guess
      group_by(guess,results) %>% 
      summarise(results_appear = n_distinct(answer),
                .groups = "drop") %>% 
      write_rds("data/all_results.rds")
    
    rm(all_results_raw)
    
  } else {
    
    all_results <- read_rds("data/all_results.rds")
  }
  
  return(all_results)
  
}


## get first word bit scores -----------------------------------------------------



# get 2nd results -----------------------------------------------------------

get_best_next_guess <- function(guess_1,results_1,
                                input_df = mystery_words,
                                print = TRUE){
  
  # Takes 2 inputs - a first guess and a set of results
  # outputs a 1-row tibble of the next best guess and some useful stats
  
  if(print){
    # Do some messaging
    message("------------------------------------------------------")
    output_guess_with_colours(guess_1,results_1)
    message("Processing next best guess...",
            appendLF = FALSE)
  }
  
  # Parameters:
  
    # get the first round answer results based on inputs
  guess_1_results <- 
    tibble(guess = guess_1) %>% 
    full_join(input_df %>% 
                select(answer = word),
              by = character()) %>% 
    check_guess_set_goal_vector() %>% 
    filter(results == results_1)
  
    # and how often they appear (helpful)
  results_1_appear <- 
    nrow(guess_1_results)
  
  # functionalise the output method 
  test_second_guess <- function(df){
    # expecting an answerXguess df as input
    # assumes the answers are the possible answers after first guess
    # and the guesses are the next guesses you try
    # requires a results_1_appear variable
    
    # outputs a 1 row df with the best 2nd guess and some useful stats
    
    df %>% 
      # get first results
      check_guess_set_goal_vector() %>% 
      # clarify these are the 2nd round
      rename(guess_2 = guess,
             results_2 = results) %>% 
      # get some probs
      group_by(guess_2,results_2) %>% 
      # n() will be the number of diff answers for that result
      summarise(p_2 = n()/results_1_appear,
                .groups = "drop") %>% 
      mutate(bits_2 = log2(1/p_2)) %>%
      # get e_bits for each guess
      group_by(guess_2) %>% 
      summarise(e_bits_2 = sum(p_2*bits_2),
                .groups = "drop") %>% 
      # get best guess for this first word and results
      slice_max(e_bits_2, n = 1, with_ties = FALSE) %>% 
      # add back the old results
      mutate(guess_1 = guess_1,
             results_1 = results_1,
             results_1_appear = results_1_appear
      ) %>% 
      # order nicely
      select(guess_1,
             results_1,
             results_1_appear,
             guess_2,
             e_bits_2) 
  }

  # First try just the winning words
  
  guess_2_results <- 
    tibble(answer = guess_1_results$answer) %>% 
    # so check everything
    full_join(guess_1_results %>% 
                select(guess = answer),
              by = character()) %>% 
    test_second_guess()
  
  # Test if that got us to max:
  e_bits_needed <- log2(results_1_appear)
  # I not, overwrite results:
  if(guess_2_results$e_bits_2 < e_bits_needed){
    # not enough - get more info
    guess_2_results <- 
      tibble(answer = guess_1_results$answer) %>% 
      # so check everything
      full_join(guessable_words %>% 
                  rename(guess = word),
                by = character()) %>% 
      test_second_guess() 
  }
  
  if(print){
    # get expected info (for fun)
    if(e_bits_needed == 0){
      expected_info <- 1
    } else {
      expected_info <-
        guess_2_results$e_bits_2/e_bits_needed
    }
    expected_info <- 100*round(expected_info,5)
  
    message("\r", # return
            "Next best guess: '",
             guess_2_results$guess_2,
             "' | Expected information: ",
            expected_info,
            "%"
            )
  }
  
  return(guess_2_results)
}


# get all the best second guess for any first guess

get_two_step_e_bits <- function(guess_1){
  
  # just loop through all results
  one_step_results <- 
    all_results %>% 
    filter(guess == guess_1)
  
  df <- 
    one_step_results$results %>% 
    map_dfr(
    ~get_best_next_guess(guess_1,.x)
    )
  
  df %>% 
    write_rds(paste0("data/two_step_results/two_step_",guess_1,".rds"))
  
  return(df)
  
}



get_all_two_step_results <- function(top_n){
  
  # get a list of the top n words
  # get all the two-step results for n top words
  # note that this takes a couple minutes per word
  # needs word_scores to be calculated and loaded (happens up top)
  
  # check what's already done!
  existing_results <- 
    tibble(files = list.files("data/two_step_results/")) %>% 
    # slightly clumsy but whatever
    mutate(word = str_sub(files,10,14)) %>% 
    pull(word)
  
  top_words <- 
    word_scores %>% 
    slice_max(expected_bits, n = top_n, with_ties = FALSE) %>% 
    # don't repeat work
    filter(!guess %in% existing_results) %>% 
    pull(guess)
  
  # run everything (slow!)
  for(word in top_words){
    get_two_step_e_bits(word)
  }
}


 