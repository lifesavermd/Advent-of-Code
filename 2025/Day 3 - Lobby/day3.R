test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
joltage <- function(x){
    sum(sapply(x, function(bank){
       batteries <- as.numeric(strsplit(bank, "")[[1]])
       combos <- combn(batteries, 2, FUN = function(y) y[1] * 10 + y[2])
       max(combos) 
    }))
}

#Part 2
joltage <- function(x){
    sum(sapply(x, function(bank){
        batteries <- as.numeric(strsplit(bank, "")[[1]])
        size <- length(batteries)
        startPos <- 1
        selectBatt <- 12
        for(i in 1:12){
            endPos <- size - (12 - i)
            maxVal <- max(batteries[startPos:endPos])
            maxPos <- startPos - 1 + which.max(batteries[startPos:endPos])
            selectBatt[i] <- maxVal
            startPos <- maxPos + 1
        }
        as.numeric(paste(selectBatt, collapse = ""))
    }))
}