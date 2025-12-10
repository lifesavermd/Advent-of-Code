library(stringr);
library(combinat);
library(lpSolve);

test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
buttonPressor <- function(x){
    processManual <- function(y){
        lights <- str_extract(y, "\\[[.#]+\\]") |> str_remove_all("\\[|\\]") |> strsplit("") |> unlist()
        target <- as.integer(lights == "#")
        buttons <- str_extract_all(y, "\\([0-9,]+\\)")[[1]]
        buttons <- lapply(buttons, function(z) as.integer(strsplit(str_remove_all(z, "\\(|\\)"), ",")[[1]]))
        n <- length(target)
        k <- length(buttons)

        buttonMat <- sapply(buttons, function(w){
            vec <- integer(n)
            vec[w + 1] <- 1
            vec
        })

        for(i in 1:k){
            combos <- combn(k, i)
            for(j in 1:ncol(combos)){
                sumVec <- rowSums(buttonMat[, combos[, j], drop = FALSE]) %% 2
                if(all(sumVec == target)) return(i)
            }
        }
        NA
    }
    sum(sapply(x, processManual))
}

buttonPressor(input);

#Part 2
library(stringr)
library(lpSolve)

buttonPressor <- function(x){
    processManual <- function(y){
        target <- str_extract(y, "\\{[0-9,]+\\}") |>
        str_remove_all("\\{|\\}") |>
        strsplit(",") |>
        unlist() |>
        as.integer()
        buttons <- str_extract_all(y, "\\([0-9,]+\\)")[[1]]
        buttons <- lapply(buttons, function(z) as.integer(strsplit(str_remove_all(z, "\\(|\\)"), ",")[[1]]))
        n <- length(target)
        k <- length(buttons)

        buttonMat <- sapply(buttons, function(w){
        vec <- integer(n)
        vec[w + 1] <- 1
        vec
        })

        f.obj <- rep(1, k)
        f.con <- buttonMat
        f.dir <- rep("=", n)
        f.rhs <- target        
        sol <- lp("min", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)

        if(sol$status == 0){
        sum(sol$solution)
        } else {
        NA
        }
    }   
    sum(sapply(x, processManual))
}

buttonPressor(input);