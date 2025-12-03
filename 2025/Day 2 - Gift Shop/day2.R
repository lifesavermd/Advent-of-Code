library(stringr);
library(purr);

test <- unlist(str_split(readLines("test.txt"), ","));
input <- unlist(str_split(readLines("input.txt"), ","));

#Part 1
initID <- function(x){
    ids <- as.numeric(str_split(x, "-", simplify = TRUE))
    seq(ids[1], ids[2])
}

answer <- function(x){
    ids <- as.character(unlist(map(x, initID)))
    even <- nchar(ids) %% 2 == 0
    strEven <- ids[even]
    length <- nchar(strEven)
    half <- length / 2
    firstNum <- substr(strEven, 1, half)
    secondNum <- substr(strEven, half + 1, length)
    invalidID <- as.numeric(strEven[firstNum == secondNum])
    sum(invalidID)
}

answer(input);

#Part 2
initID <- function(x){
    ids <- as.numeric(str_split(x, "-", simplify = TRUE))
    seq(ids[1], ids[2])
}

checker <- function(x){
    digits <- nchar(x)
    if(digits < 2) return(FALSE)
    for(i in seq_len(digits - 1)){
        if(digits %% i == 0){
            reps <- digits / i
            part <- substr(x, 1, i)
            if(paste(rep(part, reps), collapse = "") == x) return(TRUE)
        }
    }
    FALSE
}

answer <- function(x){
    ids <- as.character(unlist(map(x, initID)))
    invalidID <- ids[vapply(ids, checker, logical(1))]
    sum(as.numeric(invalidID))
}

answer(input);