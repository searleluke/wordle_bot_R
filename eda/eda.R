
# eda ---------------------------------------------------------------------

all_results <- read_rds("data/all_results.rds")


word_result_summary <- 
  all_results %>%
  group_by(guess) %>% 
  summarise(diff_results = n_distinct(results)) %>% 
  arrange(desc(diff_results)) %>% 
  
  # filter(guess == "orate") %>% 
  
  view()
  glimpse()
  
  
info_theory

# Bits is how many times the probability space is halved
# we have P for each wordX result:

# b = Bits

# p = (1/2)^b
# 2^b = (1/p)
# b = log2(1/p)
# or
# b = -log2(p)

all_results %>%
  group_by(guess) %>% 
  mutate(p = results_appear/sum(results_appear)) %>% 
  mutate(bits = log2(1/p)) %>% 
  group_by(guess) %>% 
  summarise(expected_bits = sum(p*bits)) %>% 
  
  
  arrange(desc(expected_bits)) %>% 
  view()
  glimpse()
  view()


# add 2nd guess -----------------------------------------------------------


  
