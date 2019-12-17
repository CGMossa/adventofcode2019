

source("R/eda_startup.r")

example_1 <- ".#..#
.....
#####
....#
...##"

{
  examples_list <- list("......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####", "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.", ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..",".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")
}

main_problem <- "#..#.#.###.#...##.##....
.#.#####.#.#.##.....##.#
##..#.###..###..#####..#
####.#.#..#....#..##.##.
.#######.#####...#.###..
.##...#.#.###..###.#.#.#
.######.....#.###..#....
.##..##.#..#####...###.#
#######.#..#####..#.#.#.
.###.###...##.##....##.#
##.###.##.#.#..####.....
#.#..##..#..#.#..#####.#
#####.##.#.#.#.#.#.#..##
#...##.##.###.##.#.###..
####.##.#.#.####.#####.#
.#..##...##..##..#.#.##.
###...####.###.#.###.#.#
..####.#####..#####.#.##
..###..###..#..##...#.#.
##.####...##....####.##.
####..#..##.#.#....#..#.
.#..........#..#.#.####.
###..###.###.#.#.#....##
########.#######.#.##.##"

#' Uff, the base_r version is faster.

asteroid_grid_to_matrix <- . %>%
  strsplit("\n") %>%
  unlist(recursive = FALSE) %>%
  str_split(pattern = character(1)) %>%
  stringi::stri_list2matrix(byrow = TRUE)

asteroid_matrix <- example_1 %>%
  asteroid_grid_to_matrix

locations_to_matrix <- . %>%
  {. == "#"} %>%
  which(arr.ind = TRUE) %>%
  as_tibble() %>%
  # rename(x = row, y = col) %>%
  rename(y = row, x = col) %>%
  mutate(x = x - 1, y = y - 1) %>%
  arrange(x, y)



asteroid_grid_to_matrix <- . %>%
  strsplit("\n") %>%
  unlist(recursive = FALSE) %>%
  str_split(pattern = character(1)) %>%
  stringi::stri_list2matrix(byrow = TRUE)

locations_to_matrix <- . %>%
  {. == "#"} %>%
  which(arr.ind = TRUE) %>%
  as_tibble() %>%
  # rename(x = row, y = col) %>%
  rename(y = row, x = col) %>%
  mutate(x = x - 1, y = y - 1) %>%
  arrange(x, y)


get_monitor_station_matrix <- function(problem_string) {

  asteroid_matrix <- problem_string %>%
    asteroid_grid_to_matrix()

  ast_loc_mat <- asteroid_matrix %>%
    locations_to_matrix() %>%
    as.matrix()

  results <- lapply(seq_len(nrow(ast_loc_mat)), function(k)  {
    D_mat <- t(t(ast_loc_mat[-k,]) - ast_loc_mat[k,])
    D_mat_norm <- D_mat / sqrt(rowSums(D_mat^2))
    J_mat <- tcrossprod(D_mat_norm)
    J_counts <- (abs(J_mat - 1) <= 1e-7)
    list(
      k = k,
      x = ast_loc_mat[k, 'x'],
      y = ast_loc_mat[k, 'y'],
      blocked = J_counts %>% {.[upper.tri(., diag = TRUE)] <- FALSE;.} %>% apply(1, any) %>% sum,
      J_mat = list(J_mat),
      J_counts = list(J_counts),
      D_mat = list(D_mat),
      D_mat_norm = list(D_mat_norm)
    )
  }) %>%
    bind_rows() %>%
    mutate(result = nrow(ast_loc_mat) - 1 - blocked)

  monitor_station_matrix <- asteroid_matrix
  monitor_station_matrix[ast_loc_mat + 1] <- results$result

  list(monitor_station_matrix = monitor_station_matrix,
       answer = results %>% arrange(desc(result)) %>% slice(1),
       ast_loc_mat = ast_loc_mat,
       results = results)
}

results <- get_monitor_station_matrix(example_1)$results

results %>%
  select(k, x, y, D_mat) %>%
  mutate(D_mat = D_mat %>% lapply(FUN = as_tibble)) %>%
  unnest(D_mat, names_repair = janitor::make_clean_names) %>%
  rename(xend = x_2, yend = y_2) %>%
  mutate(xend = x + xend, yend = y + yend) %>%
  mutate(y = max(y) - y) %>%
  mutate(yend = max(yend) - yend) %>%
  # mutate_at(vars(contains('x'), contains("y")), ~ .x + 1) %>%
  rowid_to_column("rowid") %>% {
    ggplot(.) +
      geom_point(aes(x, y)) +
      ggrepel::geom_label_repel(
        data = . %>% distinct(k, .keep_all = TRUE),
        aes(x, y, label = glue("({x},{y})"))) +
      geom_segment(aes(x, y, xend = xend, yend = yend,
                       color = factor(k)),
                   data = . %>% filter(k == 1),
                   size = 2,
                   alpha = .3,
                   # position = "jitter",
                   position = position_jitterdodge(0,.1),
                   # position = position_dodge(.2),
                   arrow = arrow(type = "closed", angle = 15)) +
      coord_equal() +
      NULL
  }

get_monitor_station_matrix(example_1)$answer %>%
  glue_data("Best is {x},{y} with {result} other asteroids detected.")

get_monitor_station_matrix(examples_list[[1]])$answer %>%
  glue_data("Best is {x},{y} with {result} other asteroids detected.")

get_monitor_station_matrix(examples_list[[2]])$answer %>%
  glue_data("Best is {x},{y} with {result} other asteroids detected.")

get_monitor_station_matrix(examples_list[[3]])$answer %>%
  glue_data("Best is {x},{y} with {result} other asteroids detected.")


get_monitor_station_matrix(main_problem)$answer %>%
  glue_data("Best is {x},{y} with {result} other asteroids detected.")


# PART TWO ----------------------------------------------------------------

ex4 <- get_monitor_station_matrix(examples_list[[4]])
asteroids_hit <- tribble(
  ~no, ~x_coord, ~y_coord,
  1, 11,12,
  2, 12,1,
  3, 12,2,
  10, 12,8,
  20, 16,0,
  50, 16,9,
  100, 10,16,
  199, 9,6,
  200, 8,2,
  201, 10,9,
  299, 11,1,
)

# DIAGNOSTIC
# ex4$ast_loc_mat %>%
#   head()

ex4$answer %>%
  mutate(D_mat = D_mat %>% lapply(as_tibble)) %>%
  select(k,x,y, blocked, result, D_mat) %>%
  unnest(D_mat, names_repair = janitor::make_clean_names) %>%
  mutate(x_1 = x_2 + x, y_1 = y_2 + y) %>%
  rowid_to_column() %>%
  # mutate(...) # adjust rowid so it becomes point indices
  mutate(distance = sqrt(x_2^2 + y_2^2)) %>%

  mutate(angle = pi - atan2(y_2, x_2)) %>%
  # mutate(angle = atan2(y_2 - 1, x_2)) %>%
  mutate(angle = pracma::rad2deg(angle)) %>%

  arrange(distance, angle) %>%
  # arrange(angle, distance) %>%

  # slice(asteroids_hit$no)
  identity()

asteroids_hit %>%
  rename(x_target = x_coord, y_target = y_coord) %>%
  mutate(x_center = 11, y_center = 13) %>%
  # mutate(x_target = x_coord - x, y_target = y_coord - y) %>%
  # mutate(
  #   distance = sqrt(xdir^2 + ydir^2),
  #   angle = pracma::atan2d(ydir, xdir),
  #   angle = 90 - angle)
  mutate(
    # x_center = ex4$answer$x,
    # y_center = ex4$answer$y,
    # x_target = x + ex4$answer$x,
    # y_target = y + ex4$answer$y,
    x_up = x_center,
    y_up = y_center + 1,
    angle_1 = atan2(y_up - y_center, x_up - x_center),
    angle_2 = atan2(y_target - y_center, x_target - x_center),
    angle = angle_1 - angle_2,
    angle = case_when(
      angle >  pi ~ angle - 2*pi,
      angle < -pi ~ angle + 2*pi,
      TRUE ~ angle
    ),
    angle = pracma::rad2deg(angle)) %>%
  select(-angle_1, -angle_2, -x_up, -y_up)


ex4$answer$D_mat[[1]] %>%
  as_tibble() %>%
  mutate(
    distance = sqrt(x^2 + y^2),
    # angle = pracma::atan2d(y, x),
    # angle = atan2(y - 1, x),
    x_center = ex4$answer$x,
    y_center = ex4$answer$y,
    x_target = x + ex4$answer$x,
    y_target = y + ex4$answer$y,
    x_up = x_center,
    y_up = y_center + 1,
    angle_1 = atan2(y_up - y_center, x_up - x_center),
    angle_2 = atan2(y_target - y_center, x_target - x_center),

    angle = angle_1 - angle_2,

    angle = case_when(
      angle >  pi ~ angle - 2*pi,
      angle < -pi ~ angle + 2*pi,
      TRUE ~ angle
    ),
    angle = pracma::rad2deg(angle),
  ) %>%
  arrange(angle, distance) %>%
  # arrange(distance, angle) %>%

  slice(asteroids_hit$no) %>%

  bind_cols(asteroids_hit) %>%
  # mutate(distance_coord = sqrt(x_coord^2 + y_coord^2)) %>%
  mutate(distance_coord = sqrt((x_coord - x_center)^2 + (y_coord - y_center)^2)) %>%
  select(no, x_target, x_coord, y_target, y_coord, distance, distance_coord, angle)
  # select(contains("x"), contains("y"), everything())



# mutate(another_angle = pracma::rad2deg(atan2(y-1,x))) %>%
# mutate(angle = pracma::rad2deg(pi - atan2(y,x)))

clockwise_direction <- function(a,c,b) {
  # // The atan2 functions return arctan y/x in the interval [−π , +π] radians
  # double Dir_C_to_A = atan2(Ay - Cy, Ax - Cx);
  # double Dir_C_to_B = atan2(By - Cy, Bx - Cx);
  # double Angle_ACB = Dir_C_to_A - Dir_C_to_B;
  #
  # // Handle wrap around
  # const double Pi = acos(-1);  // or use some π constant
  # if (Angle_ACB > Pi) Angle_ACB -= 2*Pi;
  # else if (Angle_ACB < -Pi) Angle_ACB += 2*Pi;
  #
  # // Answer is in the range of [-pi...pi]
  # return Angle_ACB;
}
