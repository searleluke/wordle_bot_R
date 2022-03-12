# Noting values to recreate ----------------------------------------------

# should be able to recreate the values for top couple of words 
# using old data

# if so, then the weaker scores are still good and just a function of the new list

# expected information (one-step) | (two-step)
# https://youtu.be/fRed0Xmc2Wg?t=442

# salet: 5.83 | 10.02
# trace: 5.83 | 10.01
# crate: 5.83 | 10.01




# final scores:
# https://youtu.be/fRed0Xmc2Wg?t=501
# salet: 3.421 
# 1: 0 | 2: 80 | 3: 1225 | 4: 965 | 5: 45 | 6: 0
# trace: 3.424
# 1: 1 | 2: 80 | 3: 1225 | 4: 956 | 5: 52 | 6: 1
# crate: 3.424
# 1: 1 | 2: 88 | 3: 1206 | 4: 970 | 5: 49 | 6: 1



# Getting historical data -------------------------------------------------

# this is the exact dataset used (original wordle set)


guessable_words <-
  read_csv("data/raw_data/guessable_words_00.csv",
           col_names = "word",
           show_col_types = FALSE)

mystery_words <-
  read_csv("data/raw_data/mystery_words_00.csv",
           col_names = "word",
           show_col_types = FALSE) 

# make a testing folder for this script to keep all results separate

dir.create("data/test",showWarnings = FALSE)

# the missing step here is accounting for the best second word
# which gets the expected number of remaining turns for a guess
# ie if you have the right answer, it's one
# otherwise it's 1+ the remaining turns after that answer


# figuring out the method:

# python - get expected scores:
# https://github.com/3b1b/videos/blob/0dbf910cc2fca54ec0f2c023c1b9f98a7bd5f59c/_2022/wordle/simulations.py#L395

# python: get weights:
# https://github.com/3b1b/videos/blob/0dbf910cc2fca54ec0f2c023c1b9f98a7bd5f59c/_2022/wordle/simulations.py#L247

# get_weights() is a function of a list of words and priors
# priors looks like a cross join of guesses and answers
# seems like some random stuff happening but intended to return p
# not sure where results come in, might be a sneaky lookup in some code I can't read


# h0 = entropy of distributions of weights
# h1s wrap the entropy function around everything
# so what's the difference?????

# working theory: first and 2nd guess


# entropy to expected score:



# testing -----------------------------------------------------------------



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



guess_1 <- "salet"
results_1 <- "fffff"
input_df <- mystery_words

h0 <- log2(2315) # full entropy - obviously swing this around

h0

2^(-5.83) + 2*(1-2(-5.83))


entropy_to_expected_score <- function(bits){
  
  
  min_score <- 2^(-bits) + 2*(1 - (2^(-bits)))
    
  
  
}

print(entropy_to_expected_score(0))

guess_1_results <- 
  mystery_words %>% 
  filter_mystery_words(guess_1,results_1) %>% 
  rename(answer = word) %>% 
  full_join(guessable_words,
            by = character()) %>% 
  rename(guess = word) %>% 
  check_guess_set_goal_vector() %>% 
  
  glimpse()
  

guess_1_results
  tibble(guess = guess_1) %>% 
  full_join(input_df %>% 
              select(answer = word),
            by = character()) %>% 
  check_guess_set_goal_vector() %>% 
  filter(results == results_1)
