# Define a function to map time ranges to midpoints
convert_to_midpoint <- function(x) { # nolint: cyclocomp_linter.
  if (grepl("Less than 3 hours", x)) {
    return(2)
  }
  if (grepl("3–6 hours", x)) {
    return(4.5)
  }
  if (grepl("6–10 hours", x)) {
    return(8)
  }
  if (grepl("More than 10 hours", x)) {
    return(10)
  }
  if (grepl("Less than 5 hours", x)) {
    return(4)
  }
  if (grepl("5–10 hours", x)) {
    return(7.5)
  }
  if (grepl("10–15 hours", x)) {
    return(12.5)
  }
  if (grepl("10–20 hours", x)) {
    return(12.5)
  }
  if (grepl("20–30 hours", x)) {
    return(25)
  }
  if (grepl("More than 15 hours", x)) {
    return(20)
  }
  if (grepl("Less than 10 hours", x)) {
    return(10)
  }
  if (grepl("<18", x)) {
    return(18)
  }
  if (grepl("18-20", x)) {
    return(19)
  }
  if (grepl("21-25", x)) {
    return(23)
  }
  if (grepl(">25", x)) {
    return(26) # Adjust as per your needs
  }
  if (grepl("More than 30 hours", x)) {
    return(30)
  }
  return(NA)
}
