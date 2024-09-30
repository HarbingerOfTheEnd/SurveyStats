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
legend("topright", legend = rownames(table1))

# # Barplot for SocializingTimePerWeek
table2 <- table(df$Age, df$ExtraCurricularTimePerWeek)
barplot(
  table2,
  beside = TRUE,
  col = c("orange"),
  main = "Socializing Time Per Week",
  xlab = "Hours",
  ylab = "Frequency"
)
legend("topright", legend = rownames(table2))

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
  "Pearson correlation coefficient between Age and Lecture Time Per Week:",
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
