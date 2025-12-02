library(stringr);

test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
password <- function(x){
    counter <- 0
    dial <- 50
    for(i in 1:length(x)){
        direction <- str_sub(x[i], 1, 1)
        rotation <- as.numeric(str_sub(x[i], start=2))
        rotation <- rotation%%100
        if(direction == "R"){
            dial <- dial + rotation
        } else{
            dial <- dial - rotation
        }
        dial <- dial%%100
        if(dial == 0){
            counter <- counter +1 
        }
    }
    counter
}

#Part 2
password <- function(x){
  counter <- 0
  dial <- 50
  for(i in 1:length(x)){
    direction <- str_sub(x[i], 1, 1)
    rotation <- as.numeric(str_sub(x[i], start=2))
    counter <- counter + rotation %/% 100
    rotation <- rotation%%100
    if(direction == "R"){
        dial <- dial + rotation
        if(dial >= 100){counter <- counter + 1}
        } else{
        if(dial == 0){dial <- 100}
        dial <- dial - rotation
        if(dial <= 0){counter <- counter + 1}
        }
        dial <- dial %% 100
    }
    counter
}