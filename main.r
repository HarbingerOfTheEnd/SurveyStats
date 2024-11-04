library(dplyr)
library(ggplot2)
library(moments)

df <- read.csv("responses.csv")
df <- df[, -which(names(df) == "Timestamp")]
sys.source("normalize.r", envir = environment())

df <- df %>%
  mutate(
    Age = sapply(Age, convert_to_midpoint),
    LectureTimePerWeek = sapply(LectureTimePerWeek, convert_to_midpoint),
    SelfStudyTimePerWeek = sapply(SelfStudyTimePerWeek, convert_to_midpoint),
    ExtraCurricularTimePerWeek = sapply(ExtraCurricularTimePerWeek, convert_to_midpoint), # nolint
    GameTimePerWeek = sapply(GameTimePerWeek, convert_to_midpoint),
    SocializingTimePerWeek = sapply(SocializingTimePerWeek, convert_to_midpoint), # nolint: line_length_linter.
    FitnessTimePerWeek = sapply(FitnessTimePerWeek, convert_to_midpoint),
    PersonalDevelopmentTimePerWeek = sapply(PersonalDevelopmentTimePerWeek, convert_to_midpoint), # nolint: line_length_linter.
    SocialMediaTimePerWeek = sapply(SocialMediaTimePerWeek, convert_to_midpoint)
  )

print(summary(df))
print(all.moments(df$Age, order.max = 4, absolute = TRUE))
print(all.moments(df$LectureTimePerWeek, order.max = 4, absolute = TRUE))
print(all.moments(df$SelfStudyTimePerWeek, order.max = 4, absolute = TRUE))
print(all.moments(df$ExtraCurricularTimePerWeek, order.max = 4, absolute = TRUE)) # nolint: line_length_linter.
print(all.moments(df$GameTimePerWeek, order.max = 4, absolute = TRUE))
print(all.moments(df$SocializingTimePerWeek, order.max = 4, absolute = TRUE))
print(all.moments(df$FitnessTimePerWeek, order.max = 4, absolute = TRUE))
print(all.moments(df$PersonalDevelopmentTimePerWeek, order.max = 4, absolute = TRUE)) # nolint: line_length_linter.
print(all.moments(df$SocialMediaTimePerWeek, order.max = 4, absolute = TRUE))

table1 <- table(df$Age, df$LectureTimePerWeek)
barplot(
  table1,
  beside = TRUE,
  col = c("green", "blue", "red", "yellow"),
  main = "Lecture Time Per Week",
  xlab = "Hours",
  ylab = "Frequency",
)
legend(
  "topright",
  legend = rownames(table1),
  fill = c("green", "blue", "red", "yellow")
)

# # Barplot for SocializingTimePerWeek
table2 <- table(df$Age, df$ExtraCurricularTimePerWeek)
barplot(
  table2,
  beside = TRUE,
  col = c("green", "blue", "red", "yellow"),
  main = "Socializing Time Per Week",
  xlab = "Hours",
  ylab = "Frequency"
)
legend(
  "topright",
  legend = rownames(table2),
  fill = c("green", "blue", "red", "yellow")
)

boxplot(
  LectureTimePerWeek ~ Age,
  data = df,
  main = "Lecture Time Per Week by Age",
  xlab = "Age",
  ylab = "Hours",
  col = "lightblue"
)

# Boxplot for SocializingTimePerWeek
boxplot(
  SocializingTimePerWeek ~ Age,
  data = df,
  main = "Socializing Time Per Week by Age",
  xlab = "Age",
  ylab = "Hours",
  col = "lightgreen"
)

# Boxplot for FitnessTimePerWeek
boxplot(
  FitnessTimePerWeek ~ Age,
  data = df,
  main = "Fitness Time Per Week by Age",
  xlab = "Age",
  ylab = "Hours",
  col = "lightcoral"
)

correlation_age_lecture <- cor(
  df$LectureTimePerWeek,
  df$ExtraCurricularTimePerWeek,
  method = "pearson"
)
cat(
  paste(
    "Pearson correlation coefficient between extra",
    "curricular activity time per week and Lecture Time Per Week:",
    sep = " "
  ),
  correlation_age_lecture,
  "\n"
)

# Perform linear regression analysis between Age and LectureTimePerWeek
regression_model <- lm(
  ExtraCurricularTimePerWeek ~ LectureTimePerWeek,
  data = df
)
print(summary(regression_model))

# Plot the regression line
plot(
  df$LectureTimePerWeek,
  df$ExtraCurricularTimePerWeek,
)
abline(regression_model, col = "red")

cov1 <- cov(df$LectureTimePerWeek, df$ExtraCurricularTimePerWeek)
print(cov1)

print(sd(df$LectureTimePerWeek))
print(sd(df$ExtraCurricularTimePerWeek))

# Assuming df is your data frame containing lecture_time and age columns
population_mean <- mean(df$LectureTimePerWeek, na.rm = TRUE)

under_20_data <- subset(df, Age < 20)

cat("Number of students under 20:", nrow(under_20_data), "\n")

if (nrow(under_20_data) > 0) {
  sample_size <- min(nrow(under_20_data), 40)
  sample_data <- under_20_data[sample(nrow(under_20_data), sample_size), ]

  sample_mean <- mean(sample_data$LectureTimePerWeek, na.rm = TRUE)
  sample_sd <- sd(sample_data$LectureTimePerWeek, na.rm = TRUE)
  n <- length(sample_data$LectureTimePerWeek)

  if (n > 1) {
    t_statistic <- (sample_mean - population_mean) / (sample_sd / sqrt(n))

    alpha <- 0.05
    df_t <- n - 1
    critical_t <- qt(1 - alpha / 2, df_t)

    cat("Calculated t-Statistic:", t_statistic, "\n")
    cat("Critical t-Value (for 95% confidence):", critical_t, "\n")

    if (abs(t_statistic) > critical_t) {
      cat("The null hypothesis is rejected: The sample mean significantly differs from the population mean.\n")
    } else {
      cat("The null hypothesis is accepted: The sample mean does not significantly differ from the population mean.\n")
    }
  } else {
    cat("Sample size is too small for t-test calculations.\n")
  }
} else {
  cat("No data available for students under 20.\n")
}

df <- df %>% mutate(GameTimePerWeek = GameTimePerWeek > 10)
k <- sum(df$GameTimePerWeek, na.rm = TRUE)
total <- nrow(df)
under_20_data <- subset(df, Age < 20)
k1 <- sum(under_20_data$GameTimePerWeek, na.rm = TRUE)
n <- nrow(under_20_data)

P <- k / total
Q <- 1 - P
p <- k1 / n

z <- (p - P) / sqrt((P * Q) / n)
alpha <- 0.05
z_alpha <- qnorm(1 - alpha)

cat("Alpha value is", alpha, "\n")
cat("zAlpha value is", z_alpha, "\n")

if (abs(z) > z_alpha) {
  cat("Null Hypothesis Rejected: The proportion of students under 20 spending more than 10 hours on games significantly differs from the overall population.\n")
} else {
  cat("Null Hypothesis Accepted: The proportion of students under 20 spending more than 10 hours on games does not significantly differ from the overall population.\n")
}
under_20_fitness <- df %>% filter(Age < 20) %>% pull(FitnessTimePerWeek)
over_20_fitness <- df %>% filter(Age >= 20) %>% pull(FitnessTimePerWeek)

x_bar <- mean(under_20_fitness, na.rm = TRUE)
y_bar <- mean(over_20_fitness, na.rm = TRUE)

sigma1 <- sd(under_20_fitness, na.rm = TRUE)
sigma2 <- sd(over_20_fitness, na.rm = TRUE)

n1 <- length(under_20_fitness)
n2 <- length(over_20_fitness)

P1 <- sigma1^2 / n1
P2 <- sigma2^2 / n2
z <- (x_bar - y_bar) / sqrt(P1 + P2)

alpha <- 0.05
z_alpha <- qnorm(1 - (alpha / 2))

cat("Sample mean for under 20 (x_bar):", x_bar, "\n")
cat("Sample mean for 20 or older (y_bar):", y_bar, "\n")
cat("Standard deviation for under 20 (sigma1):", sigma1, "\n")
cat("Standard deviation for 20 or older (sigma2):", sigma2, "\n")
cat("Sample size for under 20 (n1):", n1, "\n")
cat("Sample size for 20 or older (n2):", n2, "\n")
cat("Z statistic:", z, "\n")
cat("Critical value (z_alpha):", z_alpha, "\n")

if (abs(z) > z_alpha) {
  cat("Reject the null hypothesis: There is a significant difference in fitness time between the two age groups.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference in fitness time between the two age groups.\n")
}

df <- df %>% mutate(social_media_above_10_hours = SocialMediaTimePerWeek > 10)

X1 <- sum(df$Age < 20 & df$social_media_above_10_hours)
X2 <- sum(df$Age >= 20 & df$social_media_above_10_hours)

n1 <- sum(df$Age < 20)
n2 <- sum(df$Age >= 20)

p1 <- X1 / n1
p2 <- X2 / n2

P <- (X1 + X2) / (n1 + n2)
Q <- 1 - P

z <- (p1 - p2) / sqrt(P * Q * (1/n1 + 1/n2))

alpha <- 0.05
z_alpha <- qnorm(1 - (alpha / 2))

cat("Proportion of under 20 (p1):", p1, "\n")
cat("Proportion of 20 or older (p2):", p2, "\n")
cat("Z-statistic:", z, "\n")
cat("Critical value (z_alpha):", z_alpha, "\n")

if (abs(z) > z_alpha) {
  cat("Reject the null hypothesis: There is a significant difference in social media usage between the two age groups.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference in social media usage between the two age groups.\n")
}

# Assume we have a data frame `df` with a column `hours_attended` indicating hours attended
n <- 100  # Total number of students
p <- 20/100  # Probability of attending exactly 20 hours

# 1. Probability of attending exactly 20 hours
k_exact <- 20
prob_exact <- dbinom(k_exact, n, p)
print("The Probability that a student attends exactly 20 hours of classes is:")
print(prob_exact)

# 2. Probability of attending at least 20 hours
prob_at_least <- sum(dbinom(20:n, n, p))
print("The Probability that a student attends at least 20 hours of classes is:")
print(prob_at_least)

# 3. Probability of attending at most 20 hours
prob_at_most <- sum(dbinom(0:20, n, p))
print("The Probability that a student attends at most 20 hours of classes is:")
print(prob_at_most)

# 4. Plot the probability distribution for the number of hours spent attending classes
x <- 0:n
px <- dbinom(x, n, p)
plot(x, px, type='h', xlab='Number of Hours', ylab='Probability Distribution', main='Binomial Distribution of Hours Attended')

# Assume we have a data frame `df` with a column `extracurricular_time` indicating hours spent
m <- mean(df$ExtraCurricularTimePerWeek, na.rm = TRUE)  # Mean of extracurricular activity time
s <- sd(df$ExtraCurricularTimePerWeek, na.rm = TRUE)    # Standard deviation

# 1. Probability of less than 6 hours
prob_less_6 <- pnorm(6, m, s)
print("The Probability that a student's extracurricular activity time is less than 6 hrs is:")
print(prob_less_6)

# 2. Probability of greater than 6 hours
prob_greater_6 <- 1 - pnorm(6, m, s)
print("The Probability that a student's extracurricular activity time is greater than 6 hrs is:")
print(prob_greater_6)

# 3. Probability between 6 and 10 hours
prob_between_6_and_10 <- pnorm(10, m, s) - pnorm(6, m, s)
print("The Probability that a student's extracurricular activity time is between 6 and 10 hrs is:")
print(prob_between_6_and_10)

# 4. Plot the probability distributions
x <- seq(0, 20, length.out = 100)  # Adjust range according to the expected max time
y <- dnorm(x, m, s)
plot(x, y, type='l', xlab='Extracurricular Activity Time (hrs)', ylab='Probability Density', main='Normal Distribution of Extracurricular Activity Time')

# Highlight areas for probabilities
polygon(c(0, x[x < 6], 6), c(0, dnorm(x[x < 6], m, s), 0), col='red', border=NA)  # Less than 6
polygon(c(6, x[x >= 6 & x <= 10], 10), c(0, dnorm(x[x >= 6 & x <= 10], m, s), 0), col='green', border=NA)  # Between 6 and 10
