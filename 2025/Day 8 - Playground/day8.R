test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
circuitCreator <- function(x, k){
    boxes <- matrix(as.numeric(unlist(strsplit(trimws(x), ","))), ncol = 3, byrow = TRUE)
    n <- nrow(boxes)
    d <- as.matrix(dist(boxes))
    idx <- which(upper.tri(d), arr.ind = TRUE)
    pairs <- idx[order(d[idx])[seq_len(min(k, nrow(idx)))], ]
    parent <- seq_len(n)
    find <- function(y) if (parent[y] == y) y else parent[y] <<- find(parent[y])
    for(i in seq_len(nrow(pairs))){
        a <- find(pairs[i, 1])
        b <- find(pairs[i, 2])
        if(a != b) parent[b] <- a
    }
    circuits <- sapply(seq_len(n), find)
    prod(as.numeric(head(sort(table(circuits), decreasing = TRUE), 3)))
}

circuitCreator(input, 1000);

#Part 2
circuitCreator <- function(x){
    boxes <- matrix(as.numeric(unlist(strsplit(trimws(x), ","))), ncol = 3, byrow = TRUE)
    n <- nrow(boxes)
    d <- as.matrix(dist(boxes))
    idx <- which(upper.tri(d), arr.ind = TRUE)
    ord <- idx[order(d[idx]), ]
    parent <- seq_len(n)
    components <- n
    find <- function(y) if (parent[y] == y) y else parent[y] <<- find(parent[y])
    for(i in seq_len(nrow(ord))){
        a <- find(ord[i, 1])
        b <- find(ord[i, 2])
        if(a != b){
            parent[b] <- a
            components <- components - 1
            if(components == 1){
                return(boxes[ord[i, 1], 1] * boxes[ord[i, 2], 1])
            }
        }
    }
}

circuitCreator(input);