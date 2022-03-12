
# colour printing function ---------------------------------------------------------



colour_text <- function(text, col){
  paste0("\033[0;",col,"m",text,"\033[0m")
}

green_text <- function(text){
  colour_text(text, col = 32)
}

red_text <- function(text){
  colour_text(text, col = 31)
}


yellow_text <- function(text){
  colour_text(text, col = 29)
}

purple_text <- function(text){
  colour_text(text, col = 35)
}

rainbow_text <- 
  paste0(
    red_text("W"),
    purple_text("H"),
    yellow_text("O"),
    green_text("A")
    )
message(rainbow_text)

rm(rainbow_text)
  

# testing -----------------------------------------------------------------
# NOT RUN
# for(col in 29:47){message(paste0("\033[0;", col, "m","TEST","\033[0m","\n"))}



# yuck function to make the words pretty ----------------------------------

# needs a result vector and a guessed_letters vector


create_guess_with_colours <- function(guess,results){
  
  guessed_letters <- c(str_split(guess,"",simplify = TRUE))
  results <- c(str_split(results,"",simplify = TRUE))
  
  output_string <- " "
  
  for (i in (1:5)){
    
    
    if(results[i] == "p"){
      output_string <- c(output_string," ",green_text(guessed_letters[i]))
    }
    
    if(results[i] == "h"){
      output_string <- c(output_string," ",yellow_text(guessed_letters[i]))
    }
    
    if(results[i] == "f"){
      output_string <- c(output_string," ",purple_text(guessed_letters[i]))
    }
  }
  return(output_string)
}

output_guess_with_colours <- function(guessed_letters,results){
  
  output_string <- 
    create_guess_with_colours(guessed_letters,results)
  
  message(output_string)
  
}










