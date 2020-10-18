### Questions 4 ###
Least_moves <- function(n) {
  steps <- 0
  while (n > 1) {
    if (n %% 2 == 0) {
      n <- n / 2
      steps <- steps + 1
    } else {
      n <- n - 1
      steps <- steps + 1
    }
  }
  return(steps)
}
Least_moves(2)
Least_moves(5)
# good work
