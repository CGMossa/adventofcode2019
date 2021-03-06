
source("R/eda_startup.r")

day_2_input <- list(
  problem =
    c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,
      31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,
      13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,
      10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,
      1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0))

intcode_day_2 <- function(input, noun, verb) {

  input[1 + 1] <- noun
  input[2 + 1] <- verb

  cat(glue("noun: {noun}, verb: {verb}"),"\n")

  opcode_index <- 1
  repeat {

    opcode <- input[opcode_index]
    A <- input[input[opcode_index + 1] + 1]
    B <- input[input[opcode_index + 2] + 1]
    result_index <- input[opcode_index + 3] + 1

    # cat(glue("(opcode; noun, verb) = ({opcode}; {A}, {B})"), "\n")

    switch(opcode,
           `1` = {
             input[result_index] <- A + B
           },
           `2` = {
             input[result_index] <- A * B
           },
           `99` = {
             print("Finished.")
             break
           }
    )

    #next opcode index
    opcode_index <- opcode_index + 4

    if (opcode_index > length(input)) {
      print("Loop broke")
      break
    }
  }
  # input
  input %>% head(1)
}

# intcode_day_2(day_2_input$problem, 12, 2)

day_5_input <- list(
  input = c(3,225,1,225,6,6,1100,1,238,225,104,0,1101,90,64,225,1101,15,56,225,1,14,153,224,101,-147,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,2,162,188,224,101,-2014,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1001,18,81,224,1001,224,-137,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,16,16,224,101,-256,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,101,48,217,224,1001,224,-125,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1002,158,22,224,1001,224,-1540,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,83,31,225,1101,56,70,225,1101,13,38,225,102,36,192,224,1001,224,-3312,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1102,75,53,225,1101,14,92,225,1101,7,66,224,101,-73,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,77,60,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,374,101,1,223,223,8,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,434,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,1107,226,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,479,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,494,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,524,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,569,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,614,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226)
)

intcode_day_5 <- function(input, opcode_input) {

  # 0 => position mode (as address)
  # 1 => immediate mode (as value)

  opcode_index <- 1
  repeat {
    opcode <- input[opcode_index]

    # ABCDE
    # A <- ( opcode      %% 1e5) %/% 1e4
    # B <- ((opcode - A) %% 1e4) %/% 1e3
    # C <- ((opcode - B) %% 1e3) %/% 1e2
    # # D <- ((some_digit - C) %% 1e2) %/% 1e1
    # # E <- ((some_digit - D) %% 1e1)
    # DE <- (opcode - C) %% 1e2

    c(A, B, C, DE) %<-% {
      formatC(opcode, width = 5, flag = 0) %>%
        strsplit(character(0)) %>% unlist(recursive = FALSE) %>%
        tapply(seq_along(.) %>% {.[length(.) - 1] <- length(.); .},
               . %>% paste0(collapse = "")) %>%
        as.numeric()
    }

    # stopifnot(opcode ==  DE)
    # stopifnot(A == 0, B == 0, C == 0)

    opcode <- DE

    switch(as.character(opcode),
           `1`  = {
             left         <- input[opcode_index + 1]
             right        <- input[opcode_index + 2]

             if (C == 0) {
               left <- input[left + 1]
             }
             if (B == 0) {
               right <- input[right + 1]
             }
             stopifnot(A == 0)
             result_index <- input[opcode_index + 3] + 1
             input[result_index] <- left + right
             opcode_index <- opcode_index + 4
           },
           `2`  = {
             left         <- input[opcode_index + 1]
             right        <- input[opcode_index + 2]

             if (C == 0) {
               left <- input[left + 1]
             }
             if (B == 0) {
               right <- input[right + 1]
             }
             stopifnot(A == 0)
             result_index <- input[opcode_index + 3] + 1
             input[result_index] <- left * right
             opcode_index <- opcode_index + 4
           },
           `3`  = {
             # take input
             left <- input[opcode_index + 1]
             if (C == 0) {
               input[left + 1] <- opcode_input
             }
             opcode_index <- opcode_index + 2
           },
           `4`  = {
             # print output at position
             left <- input[opcode_index + 1]
             if (C == 0) {
               left <- input[left + 1]
             }

             opcode_output <- left

             cat(opcode_output, '\n')
             opcode_index <- opcode_index + 2
           },
           `5` = {
             opcode_index <- opcode_index + 3
             # jump-if-true
             left <- input[opcode_index + 1]
             if (C == 0) {
               left <- input[left + 1]
             }
             if (left != 0) {
               right <- input[opcode_index + 2]
               if (B == 0) {
                 right <- input[right + 1]
               }
               opcode_index <- right + 1
             }
           },
           `6` = {
             opcode_index <- opcode_index + 3
             # jump-if-false
             left <- input[opcode_index + 1]
             if (C == 0) {
               left <- input[left + 1]
             }
             if (left == 0) {
               right <- input[opcode_index + 2]
               if (B == 0) {
                 right <- input[right + 1]
               }
               opcode_index <- right + 1
             }
           },
           `7` = {
             left  <- input[opcode_index + 1]
             right <- input[opcode_index + 2]
             if (C == 0) {
               left <- input[left + 1]
             }
             if (B == 0) {
               right <- input[right + 1]
             }
             input[input[opcode + 3] + 1] <- left < right
             opcode_index <- opcode_index + 4
           },
           `8` = {
             left  <- input[opcode_index + 1]
             right <- input[opcode_index + 2]
             if (C == 0) {
               left <- input[left + 1]
             }
             if (B == 0) {
               right <- input[right + 1]
             }
             input[input[opcode + 3] + 1] <- left == right
             opcode_index <- opcode_index + 4
           },
           `99` = {
             print("Finished.")
             break
           },
           {
             stop("unknown opcode")
           }
    )

    if (opcode_index > length(input)) {
      stop("Loop broke")
      # print("Loop broke")
      break
    }
  }
  # input
  input %>% head(1)
}

# 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
# 3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
# 3,3,1108,-1,8,3,4,3,99   - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
# 3,3,1107,-1,8,3,4,3,99   - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).

part2_eg <- list(c(3,9,8,9,10,9,4,9,99,-1,8), #- Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
                 c(3,9,7,9,10,9,4,9,99,-1,8), #- Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
                 c(3,3,1108,-1,8,3,4,3,99),   #- Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
                 c(3,1107,-1,8,3,4,3,99)) %>% #- Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
  set_names(seq_along(.))

intcode_day_5(part2_eg$`1`, opcode_input = 8)  # 1
intcode_day_5(part2_eg$`1`, opcode_input = 4)  # 0
intcode_day_5(part2_eg$`1`, opcode_input = 12) # 0
intcode_day_5(part2_eg$`2`, opcode_input = 8)  # ?
intcode_day_5(part2_eg$`2`, opcode_input = 4)  # 1
intcode_day_5(part2_eg$`2`, opcode_input = 10) # 0
intcode_day_5(part2_eg$`3`, opcode_input = 8)  # 1
intcode_day_5(part2_eg$`3`, opcode_input = 4)  # 0
intcode_day_5(part2_eg$`3`, opcode_input = 12) # 0
intcode_day_5(part2_eg$`4`, opcode_input = 8)  # ?
intcode_day_5(part2_eg$`4`, opcode_input = 4)  # 1

intcode_day_5(c(3,0,4,0,99), opcode_input = 15555)

day_2_input_ <- day_2_input$problem
day_2_input_[2] <- 12
day_2_input_[3] <- 2

# intcode_day_2(day_2_input$problem, 12, 2)
intcode_day_5(day_2_input_, NA)

intcode_day_5(day_5_input$input, opcode_input = 1)
# Answer: 7988899
