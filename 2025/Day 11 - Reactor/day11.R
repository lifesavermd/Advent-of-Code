test <- readLines("test.txt");
test2 <- readLines("test2.txt");
input <- readLines("input.txt");

#Part 1
reactor <- function(x){
    rack <- setNames(lapply(strsplit(x, ": "), function(z)
    if(length(z) == 2) strsplit(z[2], " ")[[1]] else character(0)),
    sub(":.*", "", x))
    paths <- list()
    findPaths <- function(y){
        if(y == "out") return(1L)
        if(!length(rack[[y]])) return(0L)
        if(!is.null(paths[[y]])) return(paths[[y]])
        paths[[y]] <<- sum(vapply(rack[[y]], findPaths, integer(1)))
        paths[[y]]
    }
    findPaths("you")
}

reactor(input);

#Part 2
reactor <- function(x){
    rack <- setNames(lapply(strsplit(x, ": "), function(z)
        if(length(z) == 2) strsplit(z[2], " ")[[1]] else character(0)),
        sub(":.*", "", x))
    paths <- list()
    findPaths <- function(y, dac, fft){
        dac <- dac || y == "dac"
        fft <- fft || y == "fft"
        if(y == "out") return(as.numeric(dac && fft))
        nxt <- rack[[y]]
        if(!length(nxt)) return(0L)
        key <- paste(y, dac, fft, sep = "|")
        if(!is.null(paths[[key]])) return(paths[[key]])
        paths[[key]] <<- sum(vapply(
            nxt,
            function(r) findPaths(r, dac, fft),
            numeric(1)
        ))
        paths[[key]]
    }
    findPaths("svr", FALSE, FALSE)
}

reactor(input);