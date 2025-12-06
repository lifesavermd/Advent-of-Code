test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
forklift <- function(x){
    grid <- do.call(rbind, strsplit(x, ""))
    nrow <- nrow(grid)
    ncol <- ncol(grid)
    counter <- 0
    adjacent <- expand.grid(dx = -1:1, dy = -1:1)
    adjacent <- adjacent[!(adjacent$dx == 0 & adjacent$dy == 0), ]
    for(i in 1:nrow){
        for(j in 1:ncol){
            if(grid[i, j] == "@"){
                numAdj <- 0
                for(k in 1:nrow(adjacent)){
                    y <- i + adjacent$dx[k]
                    z <- j + adjacent$dy[k]
                    if(y >= 1 && y <= nrow && z >= 1 && z <= ncol){
                        if(grid[y, z] == "@"){
                            numAdj <- numAdj + 1
                        }
                    }
                }
                if(numAdj < 4){
                    counter <- counter + 1
                }
            }
        }
    }
    counter
}

forklift(input);

#Part 2
forklift <- function(x){
    grid <- do.call(rbind, strsplit(x, ""))
    nrow <- nrow(grid)
    ncol <- ncol(grid)
    counter <- 0
    adjacent <- expand.grid(dx = -1:1, dy = -1:1)
    adjacent <- adjacent[!(adjacent$dx == 0 & adjacent$dy == 0), ]
    repeat{
        removeRolls <- list()
        for(i in 1:nrow){
            for(j in 1:ncol){
                if(grid[i, j] == "@"){
                    numAdj <- 0
                    for(k in 1:nrow(adjacent)){
                        y <- i + adjacent$dx[k]
                        z <- j + adjacent$dy[k]
                        if(y >= 1 && y <= nrow && z >= 1 && z <= ncol){
                            if(grid[y, z] == "@"){
                                numAdj <- numAdj + 1
                            }
                        }
                    }
                    if(numAdj < 4){
                        removeRolls <- append(removeRolls, list(c(i, j)))
                    }
                }
            }
        }

        if(length(removeRolls) == 0){
            break
        }
        for(coord in removeRolls){
            grid[coord[1], coord[2]] <- "."
        }
        counter <- counter + length(removeRolls)
    }
    counter
}

forklift(input);