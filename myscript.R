
#' reading the csv christmas file and storing it under the name xmas
xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")

#' installing tidyverse
library(tidyverse)
library(purrr)

#' Step One: Plurals
#' pluralize_gift takes a gift and returns the appropriate plural
pluralize_gift <- function(gift){
  gift <- paste0(gift, "s")
  gift <- str_replace(gift, "ooses$", "eese")
  gift <- str_replace(gift, "ys$", "ies")
  gift[1] <- str_replace(gift[1], "s$", "")
  gift[1] <- str_replace(gift[1], "ies$", "")
  gift[1] <- str_replace(gift[1], "eese$", "")
  return(gift)
}


#' Step Two: Creating sentences
#' make_phrase combines phrases.  It does so by removing NA in string, removing extra spaces in between,
#' removing extra spaces on the sides, and making first item without a number.  Then it returns
#' the phrase
make_phrase <- function(num, item, verb, adjective, location){

  phrase <- paste0(num, " ", adjective, " ", item, " ", verb, " ", location)

  phrase <- str_replace_all(phrase, "NA", "")
  
  phrase <- str_replace_all(phrase, "\\s+", " ")
  
  phrase <- str_trim(phrase, side = "both")
  
  index <- which(num == 1)
  phrase[index] <- str_replace(phrase[index], "^1", "a")
  
  return(phrase)
}



#' Step Three: Iteration
#' sing_line takes the phrases created by make_phrase and puts them together in the correct order based on 
#' the inputs.  It makes phrases that are part of the lyrics to the song.  This is done by editing the phrases
#' from the make_phrase and using a for loop to use all the correct lines.
sing_line <- function(dataset, line, phrase_col){
  how_many <- c("a", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
  vec <- c(line:1)
  phrases <- dataset %>% pull({{phrase_col}})
  if(line == 1){
  }
  else{
    phrases[1] <- paste("and", phrases[1])
  }
  phrase1 <- c()
  for (x in vec) {
    word <- how_many[x]
    num_str <- as.character(x)
    phrases[x] <- str_replace(phrases[x], num_str, how_many[x])
    phrase1 <- c(phrase1, phrases[x])
    
  }
  phrase1 <- (phrase1)
  phrase1 <- paste(phrase1, collapse = ", ")
  what_day <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth")
  new_phrase <- paste("On the", what_day[line], "day of Christmas, my true love sent to me,", phrase1)
   return(new_phrase)
}



#' Step Four: Sing the song!
#' This code is used to put the phrases from sing_line into one phrase
#' and then repeat it in the same format as the song lyrics.  It will be used in step five.
# range <- c(1:12)
# phrase <- c()
# for(x in range){
#   phrase <- c(phrase, map_chr(x, ~sing_line(xmas, x, Full.Phrase)))
# }

#' Step Five: One function to rule them all.
#' sing_song takes all the previous functions and combines them into one so that the user can only call 
#' one function for the same results as many function.
sing_song <- function(xmas){
  xmas$Gift.Item <- pluralize_gift(xmas$Gift.Item)
  xmas <- xmas %>%
    mutate(
      Full.Phrase = make_phrase(Day, Gift.Item, Verb, Adjective, Location)
    )
  range <- c(1:12)
  phrase <- c()
  for(x in range){
    phrase <- c(phrase, map_chr(x, ~sing_line(xmas, x, Full.Phrase)))
  }
  return(phrase)
}



