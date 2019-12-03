library(tidyverse)

input <-
  c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0)


execute_program <- function(input, noun, verb) {

  if (noun < 0 || noun>100 || verb < 0 || noun > 100) {
    return(NA)
    # return(Inf)
  }

  #TASK SPECIFIC
  # input[1 + 1] <- 12 # noun
  # input[2 + 2] <- 2  # verb
  input[1 + 1] <- noun
  input[2 + 1] <- verb

  LIMIT <- input %>% length()
  opcode_indices <- seq.int(1, LIMIT, by = 4)

  # stopifnot(all(input[opcode_indices] %in% c(1, 2, 99)))
  # maybe this is because that while program executes, this
  # becomes true?

  for (opcode_index in opcode_indices) {
    opcode <- input[opcode_index]

    if (!opcode %in% c(1, 2, 99)) {
      stop("error occurred")
    }

    left_index  <- input[opcode_index + 1] + 1
    right_index <- input[opcode_index + 2] + 1
    store_index <- input[opcode_index + 3] + 1

    # stopifnot(all(1 <= c(left_index, right_index, store_index) &
    #                 c(left_index, right_index, store_index) <= LIMIT))

    if (opcode == 1) {

      input[store_index] <-
        input[left_index] + input[right_index]
    }

    if (opcode == 2) {
      input[store_index] <-
        input[left_index] * input[right_index]
    }

    if (opcode == 99) {
      break
    }
  }

  input[1]
  # output <- input
  # output[1]
}


execute_program(input, 12, 2)
# Answer: 3101844


error_program <- function(noun, verb) sqrt(abs(19690720 - execute_program(input, noun, verb)))

# lpSolve::lp("min", 2, )

# combn(c(-1, 1), 2)

noun_df <-
  # tibble(noun = seq.int(0, 99, 2), verb = 24) %>%
  # tibble(verb = seq.int(0, 99, 2), noun = 24) %>%
  # tibble(verb = seq.int(0, 99, 1)) %>%
  expand_grid(verb = seq.int(0, 99, 1),
              noun = seq.int(0, 99, 1)) %>%
  {print(.);.} %>%
  mutate(error = map2_dbl(noun, verb, error_program %>% auto_browse)) %>%
  arrange(error) %>%
  {print(.);.} %>%
  identity()

noun <-  84
verb <-  78

100 * noun + verb
