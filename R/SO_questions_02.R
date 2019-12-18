
library(purrr)

accumulate_values <- function(time_vector,
                              input_vector,
                              list_of_parameters)
{

  number_samples <- length(time_vector)
  time_steps <- c(0, diff(time_vector))

  calculation <- (list_of_parameters$K * input_vector - list_of_parameters$M) * time_steps

  accumulated_values <- rep(0, number_samples)
  for (i in 2:number_samples) {
    accumulated_values[i] <- max(0, accumulated_values[i-1] + calculation[i])

  }

  return(accumulated_values)
}

accumulate_values_purrr <- function(time_vector,
                                    input_vector,
                                    list_of_parameters)
{
  number_samples <- length(time_vector)
  time_steps <- c(0, diff(time_vector))
  calculation <- (list_of_parameters$K * input_vector - list_of_parameters$M) * time_steps

  # accumulated_values <- rep(0, number_samples)
  # for (i in 2:number_samples) {
  #   accumulated_values[i] <- max(0, accumulated_values[i-1] + calculation[i])
  #
  # }
  accumulated_values <- calculation %>% purrr::accumulate(function(x, y) max(0, x + y))

  return(accumulated_values)
}

# Data
set.seed(1987)
Nums <- 1000000
# Nums <- 1000
time_vector <- seq(1, Nums, by = 1)
input_vector <- rnorm(Nums)
list_of_parameters <- list(K = 5, M = 0.01)

number_samples <- length(time_vector)
time_steps <- c(0, diff(time_vector))
calculation <- (list_of_parameters$K * input_vector - list_of_parameters$M) * time_steps

answer <- accumulate_values_purrr(time_vector,
                                  input_vector,
                                  list_of_parameters)

Rcpp::cppFunction(
  plugins = "cpp11",
  "std::vector<double> process(NumericVector calculation) {
  // NumericVector result (calculation.length(), 0.0);
  std::vector<double> result;
  // result.capacity(calculation.length());

  std::accumulate(calculation.begin(),
                  calculation.end(),
                  0.0,
                  [&result](double x, double y){
                    auto new_entry = std::max(0.0, x + y);
                    result.push_back(new_entry);
                    return new_entry;
                  });

  return result;
}")

Rcpp::cppFunction(
  plugins = "cpp11",
  "std::vector<double> process_with_cap(NumericVector calculation) {
  std::vector<double> result;
  result.reserve(calculation.length());

  std::accumulate(calculation.cbegin(),
                  calculation.cend(),
                  0.0,
                  [&result](double x, double y){
                    auto new_entry = std::max(0.0, x + y);
                    result.push_back(new_entry);
                    return new_entry;
                  });

  return result;
}")



bench::mark(for_loopa = accumulate_values(time_vector,
                                          input_vector,
                                          list_of_parameters),
            rcpp_process = process(calculation),
            rcpp_process_with_cap = process_with_cap(calculation),
            purrr_accumulate = accumulate_values_purrr(time_vector,
                                                       input_vector,
                                                       list_of_parameters)) %>%
  bench:::summary.bench_mark(relative = TRUE)

