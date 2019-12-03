library(tidyverse)

# DAY 1 -------------------------------------------------------------------

input <- read_lines("day1/input.txt")
input %>%
  as.numeric() %>%
  divide_by_int(3) %>%
  subtract(2) %>%
  sum() %>%

  clipr::write_clip()

# Answer: 3474920

# DAY 2 -------------------------------------------------------------------
# input <- c(14, 1969, 100756)

calc_fuel <- . %>% divide_by_int(3) %>% subtract(2)

fuel_calc <- input %>% as.numeric() %>% calc_fuel
results <- list(fuel_calc)
while (TRUE) {
  fuel_calc <- calc_fuel(fuel_calc)
  results <- results %>% append(list(fuel_calc))

  if (all(fuel_calc <= 0)) {
    break
  }
}
results

results %>% do.call(what=rbind) %>%
  apply(MARGIN = 2, function(x) pmax(0,x)) %>%
  colSums() %>%

  sum()

clipr::write_last_clip()

# Answer: 5209504?
