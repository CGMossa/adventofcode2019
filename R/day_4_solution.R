
source("R/eda_startup.r")


# 353096-843212
L <- 353096
R <- 843212

R - L

# lobstr::obj_size(c(L:R)) %>%
#   unclass() %>%
#   units::set_units(B) %>%
#   # with(MB)
#
#
# units::as_units(quote(B), lobstr::obj_size(c(L:R)) %>% unclass())
#
# units::units(1:2)
#
# units
#

get_digit <- function(x, d) {
  # digits from the right
  # i.e.: first digit is the ones, second is the tens, etc.
  (x %% 10 ^ d) %/% (10 ^ (d - 1))
}



# get_digit(567, 1:9)

# for one number
get_all_digit <- function(x) {
  get_digit_x <- function(d) get_digit(x,d)
  sapply(nchar(x):1, get_digit_x)
}

# for a vector of numbers
digits <- function(x) {
  out <- lapply(x, get_all_digit)
  names(out) <- x
  out
}

digits_vectorised <- function(x) {
  out <- lapply(x, function(x) get_digit(x, nchar(x):1))
  names(out) <- x
  out
}

n_digits <- function(x) if_else(x == 0, 1, floor(log10(x)) + 1)
digits_vectorised_num <- function(x) {
  out <- lapply(x, function(x) get_digit(x, n_digits(x):1))
  names(out) <- x
  out
}

random_integers <- function(n) trunc(runif(n) * 10^(ceiling(runif(n)*10)))
ri_data <- random_integers(1e3)

A <- digits(ri_data)
B <- digits_char(ri_data)
C <- digits_vectorised(ri_data)
D <- digits_vectorised_num(ri_data)

# map2(C,D, ~ .x == .y)

# A[[1]]
# D[[1]]
# A[[2]]
# D[[2]]

# all.equal(A,B)
# all.equal(A,C)

# list(A = A, D = D)  %>%
#   enframe()     %>%
#   unnest(value) %>%
#   rowid_to_column() %>%
#   pivot_wider(names_from = name)


#another approach

# ri_data[[1]] %>%
#   as.character() %>% strsplit('') %>% unlist(recursive = FALSE) %>%
#   as.integer()

digits_char <- . %>%
  as.character() %>%
  set_names(x = strsplit(., ''), .) %>%
  lapply(as.integer)

bench::mark(
  r_digits     = digits(ri_data),
  r_digits_vec = digits_vectorised(ri_data),
  r_digits_vec_num = digits_vectorised_num(ri_data),
  r_char_digits = digits_char(ri_data)
)

list(122345,
     111123,
     135679,
     111111,
     223450,
     123789) %>%
  lapply(digits_char) %>%
  unlist(recursive = FALSE) %>%
  lapply(. %>% diff %>% {all(. >= 0) & `%in%`(0, .)})

library("future.apply")
plan(multiprocess)

results <- list(L:R) %>%
  future_lapply(digits_char) %>%
  unlist(recursive = FALSE) %>%
  future_lapply(. %>% diff %>% {all(. >= 0) & `%in%`(0, .)})

results %>%
  as.numeric() %>% sum()


# PART TWO ----------------------------------------------------------------

library("future.apply")
plan(multiprocess)

list(112233,
     123444,
     111122) %>%

any(table(c(1,2,3,4,4,4)) == 2)

# TRUE FALSE TRUE
#
results <- list(L:R) %>%
  future_lapply(digits_char) %>%
  unlist(recursive = FALSE) %>%
  future_lapply(function(x) diff(x) %>% {all(. >= 0) & any(2 == table(x))})

results %>%
  as.numeric() %>% sum()

