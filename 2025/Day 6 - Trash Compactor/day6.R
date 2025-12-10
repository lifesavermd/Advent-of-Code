library(stringr);
library(purrr);
library(dplyr);

test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1
cephMath <- function(x){
    ops <- strsplit(tail(x, 1), "\\s+")[[1]]
    nums <- sapply(strsplit(head(x, -1), "\\s+"), function(y) as.numeric(y[y != ""]))
    sum(ifelse(ops == "+", rowSums(nums), exp(rowSums(log(nums)))))
}

cephMath(input);

#Part 2
cephMath <- function(x){
    ops <- strsplit(tail(x, 1), "\\s+")[[1]]
    nums <- do.call(rbind, strsplit(head(x, -1), "")) |>
    apply(2, function(z) as.numeric(paste(z, collapse = "")))
    nums2 <- split(nums, cumsum(is.na(nums)) + 1)
    sapply(seq_along(ops), function(a) ifelse(ops[a] == "+", sum, prod)(nums2[[a]], na.rm = TRUE)) |>
    sum()
}

cephMath(input);