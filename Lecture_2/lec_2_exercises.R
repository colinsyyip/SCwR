# Ex. 2
A <- matrix(c(-2, 1, 5, -4, 3, 2), ncol = 3, nrow = 2)
B <- matrix(c(-1, 1, 1, -1), ncol = 2, nrow = 2)
C <- matrix(c(3, 1, 4, -4), ncol = 2, nrow = 2)

ex2_1 <- B + C
ex2_2 <- B - 2 * C
ex2_3 <- B %*% A
ex2_4 <- t(A) %*% C
ex2_5 <- solve(C)

# Ex. 3
ex3_1 <- sin(exp(5))
ex3_2 <- sqrt(c(0, 5, 17) + 49)
ex3_3 <- round(ex3_2, 2)
ex3_4 <- (c(0, 1, 4) + 7)^(1 / 3)
ex3_5 <- log(exp(7) + 12.5^2)
ex3_6 <- log(exp(7) + 12.5^2, base = 4)
ex3_7 <- abs(c(0:3) - 4)
ex3_8 <- sum(log(sqrt(c(6:20)), base = 3))
ex3_9 <- sum(log(c(6:20), base = 3) / 2)

# Ex. 4
library(brolgar)
data(heights)
heights <- as.data.frame(heights)
data(wages)
wages <- as.data.frame(wages)

# Ex. 5
ex5_1 <- head(heights, 10)
ex5_2 <- tail(heights, 15)
ex5_3_a <- nrow(heights)
ex5_3_b <- c(strsplit(names(heights), "\t"))

# Ex. 6
ex6_1 <- nrow(heights)
ex6_2 <- heights[1245, "country"]
ex6_3 <- rownames(subset(heights, country == "Portugal"))
ex6_4_a <- subset(heights, country == "Portugal")
ex6_4_b <- nrow(ex6_4_a)
ex6_4_c <- unique(ex6_4_a[["year"]])
ex6_5 <- all(ex6_4_a[2:nrow(ex6_4_a), "height_cm"] > ex6_4_a[1:nrow(ex6_4_a), "height_cm"])

# Ex. 7
ex7_1_1970 <- subset(heights, year == 1970)
ex7_1_2000 <- subset(heights, year == 2000)
ex7_2_1970 <- length(unique(ex7_1_1970[["country"]]))
ex7_2_2000 <- length(unique(ex7_1_2000[["country"]]))
ex7_3 <- intersect(ex7_1_1970[["country"]], ex7_1_2000[["country"]])
ex7_4 <- subset(ex7_1_2000, country == "Vietnam", "height_cm") - subset(ex7_1_1970, country == "Vietnam", "height_cm") # Increase

# Ex. 8
ex8_1_america <- subset(heights, continent == "Americas" & year == 1980)
ex8_1_asia <- subset(heights, continent == "Asia" & year == 1980)
ex8_2_america <- range(ex8_1_america[["height_cm"]])
ex8_2_asia <- range(ex8_1_asia[["height_cm"]])
ex8_3_america <- median(ex8_1_america[["height_cm"]])
ex8_3_asia <- median(ex8_1_america[["height_cm"]])
ex8_4 <- subset(ex8_1_america, height_cm == max(ex8_1_america[["height_cm"]]), "country")

# Ex. 9
ex9_1 <- c(nrow(wages), ncol(wages))
hourly_wage <- exp(wages[["ln_wages"]])
wages[["hourly_wage"]] <- hourly_wage
ex9_3 <- c(min(hourly_wage), max(hourly_wage))
ex9_4 <- c(mean(hourly_wage), median(hourly_wage))
lt_2yoe <- subset(wages, xp <= 2)
bt_2_5yoe <- subset(wages, xp < 5 & xp > 2)
gt_5yoe <- subset(wages, xp >= 5)
ex9_6 <- mean(lt_2yoe[["hourly_wage"]])
bt_2_5yoe_mean <- mean(bt_2_5yoe[["hourly_wage"]])
gt_5yoe_mean <- mean(gt_5yoe[["hourly_wage"]])
ex9_7_a <- bt_2_5yoe_mean > ex9_6
ex9_7_b <- bt_2_5yoe_mean > gt_5yoe_mean
