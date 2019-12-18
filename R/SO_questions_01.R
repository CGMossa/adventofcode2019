
source("R/eda_startup.r")


data <- tibble(x = rnorm(10),
               y = rgamma(10, 10))

dist_data <- dist(data, method = "manhattan")

dist_data %>%
  broom::tidy()


# SO question -------------------------------------------------------------


a_wilcox_test <- wilcox.test(rnorm(10))
get_print_invisibly <- function(x) invisible(capture.output(print(x)))

printed_stuff <- get_print_invisibly(a_wilcox_test)

print(printed_stuff) #doesn't work
cat(printed_stuff, sep = '\n')
a_wilcox_test

