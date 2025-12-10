library(stringr);
library(dplyr);
library(purrr);
library(collections);

test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
rectangleFinder <- function(input){
    corners <- do.call(rbind, strsplit(trimws(input), ","))
    createGrid <- function(vec){
        grid <- replicate(length(vec), vec)
        totalGrid <- t(grid)
        abs(grid - totalGrid) + 1
    }
    dx <- createGrid(as.numeric(corners[, 1]))
    dy <- createGrid(as.numeric(corners[, 2]))
    area <- dx * dy
    maxArea <- area[upper.tri(area)]
    max(maxArea)
}

#Part 2
rectangleFinder <- function(input) {
  corners <- do.call(rbind, strsplit(trimws(input), ",")) |> apply(2, as.numeric)
  
  createGrid <- function(corners) {
    pointTib <- tibble(x1 = corners[,1], y1 = corners[,2])
    pointTibS <- tibble(
      x2 = c(pointTib$x1[-1], pointTib$x1[1]),
      y2 = c(pointTib$y1[-1], pointTib$y1[1])
    )
    pointTib[,c("x2", "y2")] <- pointTibS
    pointTib
  }
  
  points <- createGrid(corners)
  
  xcols <- sort(unique(points$x1))
  yrows <- sort(unique(points$y1))
  
  xMap <- setNames(seq(1, by = 2, length.out = length(xcols)), xcols)
  yMap <- setNames(seq(1, by = 2, length.out = length(yrows)), yrows)
  
  points <- points %>%
    mutate(
      x1n = xMap[as.character(x1)],
      x2n = xMap[as.character(x2)],
      y1n = yMap[as.character(y1)],
      y2n = yMap[as.character(y2)]
    )
  
  xMax <- max(unlist(xMap))
  yMax <- max(unlist(yMap))
  pointGrid <- matrix(0L, nrow = yMax, ncol = xMax)
  
  point_fill <- function(x1n, y1n, x2n, y2n) {
    if(x1n == x2n) pointGrid[min(y1n, y2n):max(y1n, y2n), x1n] <<- 1L
    else if(y1n == y2n) pointGrid[y1n, min(x1n, x2n):max(x1n, x2n)] <<- 1L
  }
  
  pwalk(points %>% select(x1n, y1n, x2n, y2n), point_fill)
  
  for(row in 1:nrow(pointGrid)) {
    for(col in 1:ncol(pointGrid)) {
      if(pointGrid[row, col] == 1) break
      else pointGrid[row, col] <- -1L
    }
    for(col in ncol(pointGrid):1) {
      if(pointGrid[row, col] == 1) break
      else pointGrid[row, col] <- -1L
    }
  }
  
  pointGrid[pointGrid == 0L] <- 1L
  
  n <- nrow(corners)
  dx <- abs(outer(corners[,1], corners[, 1], "-")) + 1
  dy <- abs(outer(corners[,2], corners[, 2], "-")) + 1
  area <- dx * dy
  
  idx <- which(upper.tri(area), arr.ind=TRUE)
  idx <- idx[order(-area[idx]), ] # sort by descending area
  
  toNorm <- function(v, map) map[as.character(v)]
  
  validRect <- function(i, j) {
    x <- sort(toNorm(c(corners[i, 1], corners[j, 1]), xMap))
    y <- sort(toNorm(c(corners[i, 2], corners[j, 2]), yMap))
    !(-1L %in% pointGrid[y[1]:y[2], x[1]:x[2]])
  }
  
  for(k in 1:nrow(idx)) {
    i <- idx[k, 1]; j <- idx[k, 2]
    if(validRect(i, j)) return(area[i, j])
  }
  
  return(0)
}
