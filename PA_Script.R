#1 Write a function called divide_and_round(), 
#which takes a number or vector of numbers, divides them all by their smallest value, 
#and rounds the results to the nearest whole number. 

divide_and_round <- function(nums){
  min_num <- min(nums, na.rm = TRUE)
  divide <- nums/min_num
  round <- round(divide)
  return(round)
}


#Write a function called no_nines_or_twelves(),
#which takes a number or vector of numbers and returns TRUE if the number is NOT divisible by 9 or 12, FALSE otherwise.

no_nines_or_twelves <- function(nums){
  divide <- nums %% 9 != 0 & nums %% 12 != 0
  return(divide)
}

#Write a function called every_other() which takes a vector and returns every other value in the vector.
#Include an optional argument called “start” which lets you choose where to start skipping; 
#that is, if start = 1, we return the 1st value, 3rd, 5th, etc. and if start = 2, we return the 2nd, 4th, 6th, etc.

every_other <- function(vec, start = 1){
  
  good_indices <- seq(start, length(vec), 2)
    
  vec[good_indices]
}


#Write a function called shorten() which takes a vector and keeps dropping the first value, 
#until the sum of the vector is less than 350 Return the remaining values.

shorten <- function(shortvec){
  total <- sum(shortvec)
  start <- 0
  while(total >= 350){
    len <- length(shortvec)
    start <- start + 1
    total<- sum(shortvec[start:len])
  }
  return(shortvec[start:len])
}
  




