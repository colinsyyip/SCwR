# Ex. 2
ex2_1 <- function(v) {
  v_mean <- mean(v)

  return(v - v_mean)
}
ex2_2 <- function(v) {
  v_var <- var(v)

  return(v / v_var)
}
ex2_3 <- function(v) {
  v_mean <- mean(v)
  v_var <- var(v)

  return((v - v_mean) / v_var)
}
calc_mean_var <- function(v) {
  return(c("mu" = mean(v), "var" = var(v)))
}

# Ex. 3
v <- c(2, 4, 17, -8, -2, 3, 6:8, -5, -2)
ex3_1_a <- mean(v)
ex3_1_b <- var(v)
ex3_2_a <- ex2_1(v)
ex3_2_b <- calc_mean_var(ex3_2_a)
ex3_3_a <- ex2_2(v)
ex3_3_b <- calc_mean_var(ex3_3_a)
ex3_4_a <- ex2_3(v)
ex3_4_b <- calc_mean_var(ex3_4_a)

# Ex. 4
summary_table <- function(v) {
  quantity <- c(
    "minimum", "first quartile", "median",
    "mean", "third quartile", "maximum"
  )
  value <- c(
    min(v), quantile(v, probs = c(0.25)), median(v),
    mean(v), quantile(v, probs = c(0.75)), max(v)
  )
  return(data.frame(quantity, value))
}
ex4_a <- summary_table(cars$speed)
ex4_b <- summary_table(cars$dist)

# Ex. 5
ex5_1 <- function(x) {
  x_bar <- mean(x)
  cube_root_avg_x <- mean((x - x_bar)^3, na.rm = T)
  variance <- mean((x - x_bar)^2, na.rm = T)
  adjusted_variance <- variance^(3 / 2)

  return(cube_root_avg_x / adjusted_variance)
}
ex5_2a <- ex5_1(rbinom(1000, size = 3, p = 0.1))
ex5_2b <- ex5_1(rbinom(1000, size = 3, p = 0.5))
ex5_2c <- ex5_1(rbinom(1000, size = 3, p = 0.8))

# Ex. 6
library(brolgar)
# Ex. 6 1
target_years <- c(1900, 1950, 2000)
ex6_1 <- list(length(target_years))
i <- 1
for (target_year in target_years) {
  year_subset <- subset(heights, year == target_year)$height_cm
  self_calc_skewness <- ex5_1(year_subset)
  package_calc_skewness <- skewness(year_subset)
  skewness_result <- c("self_calc" = self_calc_skewness, "pckg_calc" = package_calc_skewness)
  ex6_1[[i]] <- skewness_result
  i <- i + 1
}

# Ex. 6 2
filtered_wages <- exp(subset(wages, xp <= 1)$ln_wages)
ex6_2 <- ex5_1(filtered_wages) # Right

# Ex. 7
# CSV is zipped
curl::curl_download("https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel",
  destfile = "world_pop.xls"
)
population <- read.csv("world_pop.csv", skip = 3)
metadata <- read.csv("world_pop_metadata.csv")
save(population, file = "population.RData")
combined_file_data <- c(population, metadata)
save(combined_file_data, file = "joint_file.RData")
library(readxl)
excel_read_population_data <- read_excel("world_pop.xls", "Data", skip = 3)
latest_year_position <- length(names(excel_read_population_data))
latest_year <- names(excel_read_population_data)[latest_year_position]
latest_year_max <- max(excel_read_population_data[latest_year], na.rm = TRUE)
latest_year_min <- min(excel_read_population_data[latest_year], na.rm = TRUE)
ex7_8_a <- subset(
  excel_read_population_data,
  excel_read_population_data[latest_year] == latest_year_max
)$"Country Name"
ex7_8_b <- subset(
  excel_read_population_data,
  excel_read_population_data[latest_year] == latest_year_min
)$"Country Name"
