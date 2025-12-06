test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
freshIng <- function(x){
    lines <- trimws(lines)
    blankLine <- which(!nzchar(lines))[1]
    rangeLines <- lines[1:(blankLine - 1)]
    ingID <- as.numeric(lines[(blankLine + 1):length(lines)])
    IDRanges <- do.call(rbind, strsplit(rangeLines, "-"))
    IDRanges <- apply(IDRanges, 2, as.numeric)
    checkFresh <- sapply(ingID, function(y){
        any(y >= IDRanges[, 1] & y <= IDRanges[, 2])
    })
    sum(checkFresh)
}

freshIng(input);

#Part 2
freshIng <- function(x){
    lines <- trimws(x)
    blankLine <- which(!nzchar(lines))[1]
    rangeLines <- lines[1:(blankLine - 1)]
    IDRanges <- do.call(rbind, strsplit(rangeLines, "-"))
    IDRanges <- apply(IDRanges, 2, as.numeric)
    IDRanges <- IDRanges[order(IDRanges[, 1]), ]
    mergeRanges <- list()
    current <- IDRanges[1, ]
    for(i in 2:nrow(IDRanges)){
        r <- IDRanges[i, ]
        if(r[1] <= current[2] + 1){
            current[2] <- max(current[2], r[2])
        } else {
            mergeRanges[[length(mergeRanges) + 1]] <- current
            current <- r
        }
    }
    mergeRanges[[length(mergeRanges) + 1]] <- current
    sum(vapply(mergeRanges, function(y) y[2] - y[1] + 1, numeric(1)))
}

freshIng(input);