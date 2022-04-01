# packages ----------------------------------------------------------------


library(tidyverse)
library(data.table) # for faster if_else() and case_when(). Every bit helps!
library(viridis) # for charting

# get data -------------------------------------------------------------------

source("get_data.R")

# shared functions ---------------------------------------------------------------

source("functions/error_handling.R")
source("functions/general_functions.R")
source("functions/change_text_colours.R")
source("functions/find_top_words.R")
source("functions/automate_game.R")
source("functions/cheat_at_wordle.R")


# play wordle -------------------------------------------------------------

# if you've played already today, have a go here if you want more

play_wordle <- FALSE

if(play_wordle){
  play_wordle()  
}



# Processing --------------------------------------------------------------


## Guess 1 ebits ----------------------------------------------

# loads onestep_results. if needed, can reprocess.
# reprocessing takes a couple mins (millions of wordle games!)
all_results <- get_all_onestep_results(reprocess = FALSE)

# output the results:

word_scores <- 
  all_results %>% 
  group_by(guess) %>% 
  mutate(p = results_appear/sum(results_appear)) %>% 
  mutate(bits = log2(1/p)) %>% 
  mutate(expected_bits = p*bits) %>% 
  group_by(guess) %>% 
  summarise(expected_bits= sum(expected_bits))

best_one_step <- word_scores %>% 
  slice_max(expected_bits, n = 1, with_ties = FALSE)

message("Top word after one guess is '",
        best_one_step$guess,
        "', with expected bits of ",
        round(best_one_step$expected_bits, 4))

## Guess 2 ebits (selected) ----------------------------------------------

# slow to process, saves results. Uncomment to rerun, if you want.....
# get_all_two_step_results(100)

# alternatively, could run on individual words you want to know more about. 
# Will save results:
# get_two_step_e_bits("trace")
# get_two_step_e_bits("carte")

# get the current data, and report results:
all_two_steps <- 
  map_dfr(
    list.files("data/two_step_results/",full.names = TRUE),
    ~read_rds(.x)
  )

two_step_bits <- 
  all_two_steps %>% 
  group_by(guess_1) %>% 
  mutate(p_1 = results_1_appear/sum(results_1_appear)) %>% 
  mutate(bits_1 = log2(1/p_1)) %>% 
  mutate(bits = bits_1 + e_bits_2) %>% 
  
  summarise(e_bits = sum(p_1*bits)) %>% 
  arrange(desc(e_bits)) 

best_two_step <- 
  two_step_bits %>% 
  slice_max(e_bits, n = 1,with_ties = FALSE)


message("Top opening word after two guesses is '",
        best_two_step$guess_1,
        "', with expected bits of ",
        round(best_two_step$e_bits, 3))


## Simulate games ------------------------------------------------------

run_simulations <- FALSE

# Needs pre-calced data for guess_2_ebits for words you want to simulate
# runs simulations and returns average scores
# won't repeat simulations if data exists already
# have run for the top 50 e_bits_2 results

if(run_simulations){
  simulate_best_words(50)
}



# even if you dont wanna it's kinda fun to watch it go
# this is slow for when it needs to process the 'best' word to pick next, and
# nothing has been pre-processed.

# simulate_game("adieu")




## Chart simulation results ------------------------------------------------


check_simulation_results <- TRUE
if(check_simulation_results){
  chart_results()
  }



# cheat at wordle ---------------------------------------------------------

# use this to play along with the live game
# enter your guesses and results in the table
# comment out lines you haven't guessed yet
# suggest opening with 'reast' after update, but pick whatever looks good from the chart results

# returns remaining words and suggested next guess

tribble(
  ~ guess, ~ answer,
"bumph", "found"
) %>%

check_guess_set_goal_vector()

tribble(
  ~guess, ~results,
  "roast", "fhfhp",  # Turn 1 guess and results. 
  "punch", "fhhff",  # Turn 2 guess and results. Comment out if on turn 1 still.
  # "bumph", "fhfff",  # Turn 3 guess and results. Comment out if on turn 2 still.
  # "conia", "fpffh",  # Turn 4 guess and results. Comment out if on turn 3 still.
  # "conia", "fpffh",  # Turn 5 guess and results. Comment out if on turn 4 still.
  ) %>% 
  cheat_at_wordle()




# logic errors: -----------------------------------------------------------


# this highlights a logic error that needs investigating to improve the algorithm:

# bot recommends 'aggry' even though one of the three remaining words gives full info?

tribble(
  ~guess, ~results,
  "reast", "ffhff",  # Turn 1 guess and results. 
  "colin", "fhpff",  # Turn 2 guess and results. Comment out if on turn 1 still.
  # "putti", "fhfpf",  # Turn 3 guess and results. Comment out if on turn 2 still.
  # "conia", "fpffh",  # Turn 4 guess and results. Comment out if on turn 3 still.
  # "conia", "fpffh",  # Turn 5 guess and results. Comment out if on turn 4 still.
) %>% 
  cheat_at_wordle()


# bot recommends 'adaws' even though any guess of the three would give full info


tribble(
  ~guess, ~results,
  "reast", "fffff",  # Turn 1 guess and results. 
  "colin", "fpffh",  # Turn 2 guess and results. Comment out if on turn 1 still.
  "bumph", "fhfff",  # Turn 3 guess and results. Comment out if on turn 2 still.
  # "conia", "fpffh",  # Turn 4 guess and results. Comment out if on turn 3 still.
  # "conia", "fpffh",  # Turn 5 guess and results. Comment out if on turn 4 still.
) %>% 
  cheat_at_wordle()
