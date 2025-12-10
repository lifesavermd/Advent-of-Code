test <- strsplit(readLines("test.txt"), "");
input <- strsplit(readLines("input.txt"), "");

#Part 1
teleporter <- function(x){
    elements <- lapply(x, function(y) which(y != "."))
    beam <- elements[[1]]
    numBeams <- 1
    counter <- 0
    for(z in elements[-1]){
        counter <- counter + sum(z %in% beam)
        splitter <- beam %in% z
        numBeams <- c(numBeams[splitter], numBeams[splitter], numBeams[!splitter])
        beam <- c(beam[splitter] - 1, beam[splitter] + 1, beam[!splitter])
        numBeams <- tapply(numBeams, beam, sum)
        beam <- tapply(beam, beam, function(z) z[1])
    }
    counter
}

teleporter(input);

#Part 2
teleporter <- function(x){
    elements <- lapply(x, function(y) which(y != "."))
    beam <- elements[[1]]
    numBeams <- 1
    counter <- 0
    for(z in elements[-1]){
        counter <- counter + sum(z %in% beam)
        splitter <- beam %in% z
        numBeams <- c(numBeams[splitter], numBeams[splitter], numBeams[!splitter])
        beam <- c(beam[splitter] - 1, beam[splitter] + 1, beam[!splitter])
        numBeams <- tapply(numBeams, beam, sum)
        beam <- tapply(beam, beam, function(z) z[1])
    }
    sum(numBeams)
}

teleporter(input);