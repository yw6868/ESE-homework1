### Questions 5 ###
## 5.1
#  First of all, store all the combinations of 123456789 for convenience.
#recursive method adds "+" or "-" or "" between 1 and 9 to make the operation result 100#
operator <- c("+", "-")
lsts <- list("1")
for (i in seq(from = 2, to = 9, by = 1)) {
  for (j in seq_len(length(lsts))) {
    for (op in operator) {
      lsts <- append(lsts, paste0(lsts[[j]], op, i))
    }
    lsts[[j]] <- paste0(lsts[[j]], i)
  }
}

Find_expression <- function(target) {
  solutions <- list()
  for (line in lsts) {
    if (eval(parse(text = line)) == target) {
      s <- paste0(line, "=", target)
      solutions <- append(solutions, s)
    }
  }
  return(solutions)
}

Find_expression(target = 100)

## 5.2
Total_solutions <- sapply(1:100, function(k) {
  return(length(Find_expression(k)))
})
plot(Total_solutions, type = "l", 
     xlab = "Integer", ylab = "Toal Number of Solutions")
max_index <- which(Total_solutions == max(Total_solutions))
min_index <- which(Total_solutions == min(Total_solutions))
cat("The Number that Yields the maximum of Total Solutions are:", max_index,
    ". In this case, the total number of solutions is",  max(Total_solutions))
cat("The Number that Yields the minimum of Total Solutions is:", min_index,
    ". In this case, the total number of solutions is",  min(Total_solutions))

# you did an excellent work, very nice!




