library(tinytest)
library(data.table)

# Example dataset
dt <- readRDS(file.path("testdata", "som.RDS"))

# Test the function with default parameters
result <- count_case(dt)
expect_true(is.data.table(result), info = "Result should be a data.table")
expect_equal(nrow(result), 19, info = "Number of rows should be 19")
expect_equal(max(sapply(result[, "hoveddiagnoser"], nchar)), 3, info = "Trim diagnose code to be only 3")

# Test the function with acute = TRUE
result_acute <- count_case(dt, acute = TRUE)
expect_equal(nrow(result_acute), 17, info = "Number of rows should be 17 when acute is TRUE")

## # Test the function with period = 1
## result_period <- count_case(dt, period = 1)
## expect_equal(nrow(result_period), 2, info = "Number of rows should be 2 for period 1")

# Test the function with days = 3
result_days <- count_case(dt, days = 3)
expect_true("dup" %in% colnames(result_days), info = "Result should contain 'dup' column when days is not 0")
expect_equal(nrow(result_days[dup == 0]), 16, info = "Delete duplicated rows ie. within 3 days")
