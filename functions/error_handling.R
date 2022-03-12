
# error handling (boring) -------------------------------------------------

test_guessable_word <- function(word){
  
  if(!(word %in% guessable_words$word)){
    stop(paste0("'", word, "' is not a valid guess"))
  }
}


test_winnable_word <- function(word){
  
  if(!(word %in% mystery_words$word)){
    stop(paste0("'", word, "' is not an official Wordle word"))
  }
}


test_valid_result_set <- function(results){
  
  if(nchar(results) != 5){
    stop("Results set must have 5 entries, but '",results,"' has ",nchar(results),".")
  }
  
  result_entries <- c(str_split(results,"",simplify = TRUE))
  for (result in result_entries){
    
    if(!(result %in% c("p", "h", "f"))){

      stop(paste0("Single results must be 'p', 'h', or 'f'",
                  "\n\n  '",result,"' is not valid."))
    }
  }
}

