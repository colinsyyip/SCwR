setwd("/Users/colinyip/Documents/Masters School Work/Statistical Computing with R/Lecture 3")
set.seed(3001)
x1 <- rpois(5, lambda = 10)
mean(x1)
var(x1)

name_output <- function(name) {
  str_format <- "Good morning %s! How are you doing?"
  return(sprintf(str_format, name))
}

calculate_values <- function(num_vec, remove_nan = T) {
  vec_mean <- mean(num_vec, na.rm = remove_nan)
  vec_median <- median(num_vec, na.rm = remove_nan)
  vec_min <- min(num_vec, na.rm = remove_nan)
  vec_max <- max(num_vec, na.rm = remove_nan)

  return(c("mean" = vec_mean, "median" = vec_median, "min" = vec_min, "max" = vec_max))
}

df1 = read.csv("polls_Germany_30072021.csv")
