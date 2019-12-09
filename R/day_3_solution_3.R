source("R/eda_startup.r")

# PART ONE ----------------------------------------------------------------


example_1_input <- "R8,U5,L5,D3
U7,R6,D4,L4"
example_2_input <- "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"
example_3_input <- "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
problem_input <- "R1003,D138,L341,U798,L922,U153,R721,D177,L297,D559,L414,U470,L589,D179,L267,D954,R739,D414,L865,U688,R541,U242,R32,D607,L480,D401,L521,U727,L295,D154,R905,D54,L353,U840,L187,U942,R313,D143,R927,D962,R739,U152,R6,D9,L807,D67,R425,D235,L598,D107,L838,D522,L882,U780,L942,D29,R933,U129,L556,D11,L859,D455,L156,U673,L54,D141,R862,U88,R362,U742,L511,D408,R825,U622,R650,D393,L882,D969,R866,D232,L423,U371,L744,U35,L196,D189,R803,U663,R41,U741,R742,U929,L311,U30,R357,D776,L929,U85,R415,U540,R921,U599,R651,U79,R608,D620,L978,D92,L491,D310,L830,U656,R244,U72,L35,U768,R666,U356,R82,U596,L798,D455,L280,D626,R586,U668,R331,D245,L140,U3,R283,U813,R620,U975,L795,U477,L100,D94,R353,D732,R694,U702,L305,U497,R900,U810,L412,D954,R584,D444,L531,D875,R49,D328,L955,U227,L370,D548,L351,U571,R373,U743,R105,D226,L755,U325,R496,D960,L415,U262,R197,D508,R725,U930,L722,D162,L996,D610,R346,U680,L75,U211,R953,U147,R114,D48,L305,D284,L630,U575,R142,D518,R704,D820,L617,D118,R67,D674,L90,D916,L483,D598,L424,U92,R188,U413,L702,D262,R720,D995,L759,D732,L259,D814,L342,U642,L875,U726,R265,D143,R754,D235,L535,U1,R211,D720,R943,D726,L398,U636,R994,U653,L401,U877,R577,D460,L730,U889,R166,D641,L693,U490,L78,D80,R535,U551,L866,U283,L336,U586,L913,U474,R158,D220,R278,U11,R421,D661,R719,D696,R188,D735,L799,U391,R331,U581,R689,D82,R375,D125,R613,D705,L927,U18,R399,D352,L411,D777,L733,D884,R791,U973,R772,D878,R327,U215,L298,D360,R426,D872,L99,U78,L745,U59,L641,U73,L294,D247,R944,U512,L396
L1004,D252,L909,D935,R918,D981,L251,U486,R266,U613,L546,D815,L789,D692,L550,U633,R485,U955,R693,D784,R974,U529,R926,U550,L742,U88,R647,D572,R832,D345,R98,D122,R634,U943,L956,U551,R295,U122,L575,U378,R652,U97,R129,D872,R275,D492,L530,D328,R761,U738,R836,U884,R636,U776,L951,D977,R980,U526,L824,U125,R778,D818,R281,U929,R907,U234,L359,D521,R294,U137,L607,U421,L7,U582,R194,U979,L941,D999,R442,D330,L656,U410,R753,U704,R834,D61,R775,U750,R891,D989,R856,D944,R526,D44,R227,U938,R130,D280,L721,D171,R763,D677,L643,U931,L489,U250,L779,U552,R796,U220,R465,U700,L459,U766,R501,D16,R555,U257,R122,U452,L197,U905,L486,D726,L551,U487,R785,U470,L879,U149,L978,D708,R18,U211,L652,D141,L99,D190,L982,U556,R861,U745,L786,U674,R706,U986,R554,D39,R881,D626,R885,U907,R196,U532,L297,U232,L508,U283,L236,U613,L551,U647,R679,U760,L435,D475,R916,U669,R788,U922,R107,D503,R687,D282,L940,U835,L226,U421,L64,U245,R977,D958,L866,D328,R215,D532,R350,D199,R872,U373,R415,U463,L132,U225,L144,U786,R658,D535,R263,U263,R48,D420,L407,D177,L496,U521,R47,D356,L503,D557,R220,D879,L12,U853,R265,U983,L221,U235,R46,D906,L271,U448,L464,U258,R952,D976,L949,D526,L458,D753,L408,U222,R256,U885,R986,U622,R503,D5,L659,D553,R311,U783,L541,U17,R267,U767,L423,D501,R357,D160,L316,D912,R303,U648,L182,U342,L185,U743,L559,U816,R24,D203,R608,D370,R25,U883,L72,D816,L877,U990,R49,U331,L482,U37,L585,D327,R268,D106,L224,U401,L203,D734,L695,U910,L417,U105,R135,U876,L194,U723,L282,D966,R246,U447,R966,U346,L636,D9,L480,D35,R96"

#' Idea source [](https://math.stackexchange.com/questions/3176543/intersection-point-of-2-lines-defined-by-2-points-each)

input_string_to_list_of_wires <- . %>%
  str_split("\n", simplify = FALSE) %>%
  flatten() %>%
  set_names(c("wire_1","wire_2")) %>%
  map(. %>% str_split(",")) %>%
  flatten()

example_1_wires <- input_string_to_list_of_wires(example_1_input)
example_2_wires <- input_string_to_list_of_wires(example_2_input)
example_3_wires <- input_string_to_list_of_wires(example_3_input)
problem_input_wires <- input_string_to_list_of_wires(problem_input)
example_1_wires
example_2_wires
example_3_wires

# EXPLORATION: using `geom_spoke`...
# example_1_wires %>%
#   enframe() %>%
#   unnest(value) %>%
#   separate(name, c(NA, "wire_id")) %>%
#   separate(value, c("angle", "distance"))


#' Note that the wires are of unequal length.
# central_point <- c(0,0)

follow_the_wire <- function(wire) {
  # results <- vector("list", length(wire) + 1)
  results <- list()
  x <- 0
  y <- 0
  x_sign <- list(R = +1, L = -1, U =  0, D =  0)
  y_sign <- list(R =  0, L =  0, U = +1, D = -1)
  direction <- substr(wire[1], 1, 1)
  distance  <- as.integer(substring(wire[1], 2, nchar(wire[1])))
  for (dest in wire) {
    direction <- substr(dest, 1, 1)
    distance  <- as.integer(substring(dest, 2, nchar(dest)))
    results <- append(results, list(list(x = x, y = y, direction = direction, distance = distance)))

    # switch(direction,
    #        R = {},
    #        L = {},
    #        U = {},
    #        D = {})

    x <- x + x_sign[[direction]] * distance
    y <- y + y_sign[[direction]] * distance
  }
  results <- append(results, list(list(x = x, y = y, direction = NA_character_, distance = NA_integer_)))
  results
}
example_1 <- list(); example_1$wire_1 <- example_1_wires$wire_1 %>% follow_the_wire()
example_2 <- list(); example_2$wire_1 <- example_2_wires$wire_1 %>% follow_the_wire()
example_3 <- list(); example_3$wire_1 <- example_3_wires$wire_1 %>% follow_the_wire()
example_1$wire_2 <- example_1_wires$wire_2 %>% follow_the_wire()
example_2$wire_2 <- example_2_wires$wire_2 %>% follow_the_wire()
example_3$wire_2 <- example_3_wires$wire_2 %>% follow_the_wire()
example_1
example_2
example_3
problem <- list();
problem$wires_1 <- problem_input_wires$wire_1 %>% follow_the_wire()
problem$wires_2 <- problem_input_wires$wire_2 %>% follow_the_wire()

wire_list_to_df <- . %>%
  enframe() %>%
  separate(name, c(NA,"wire_id")) %>%
  unnest_auto(value) %>%
  unnest_auto(value)

example_1_df <- example_1 %>% wire_list_to_df()
example_2_df <- example_2 %>% wire_list_to_df()
example_3_df <- example_3 %>% wire_list_to_df()
problem_df <- problem %>% wire_list_to_df()

example_1_df
example_2_df
example_3_df
problem_df

plot_wire_df <- . %>% {
  ggplot(., aes(x, y, color = wire_id, group = wire_id)) +
    geom_path(size = 2) +
    geom_path(size = 2) +
    geom_point(size = 1, color = "balck") +
    ggrepel::geom_label_repel(data = . %>%
                                group_by(wire_id) %>%
                                mutate(id = 1:n()) %>%
                                sample_n(min(20, n())) %>%
                                ungroup() %>%
                                mutate(label = glue("{id}:{direction}({distance})")),
                              mapping = aes(label = label),
                              color = "black",
                              size = 2) +
    scale_color_manual(values = c("1" = "green",
                                  "2" = "blue",
                                  "cross_point" = "violet")) +
    coord_fixed()
}

example_1_plot <- example_1_df %>% plot_wire_df()
example_2_plot <- example_2_df %>% plot_wire_df()
example_3_plot <- example_3_df %>% plot_wire_df()
example_1_plot
example_2_plot
example_3_plot

problem_plot <- problem_df %>% plot_wire_df()
problem_plot

library(patchwork)
example_plot <- example_1_plot +
  example_2_plot +
  example_3_plot + plot_layout(ncol = 1)
example_plot

combined_wires_df_to_segments <- . %>%
  group_by(wire_id) %>%
  mutate(wire_point_id = 1:n()) %>%
  select(contains("id"), everything()) %>%  #unnecessary
  slice(1, rep(2:(n() - 1), each = 2), n()) %>%
  ungroup() %>%
  mutate(segment_id = seq.int(1, n() / 2) %>% rep(each = 2)) %>%
  select(contains("id"), everything()) %>%  #unnecessary
  # group_by(segment_id) %>% select(x, y) %>% nest() %>% ungroup() %>%
  mutate(label = c("from","to") %>% rep(n() / 2)) %>%
  # pivot_wider(names_from = label, values_from = c(x,y, wire_id, wire_point_id),
  pivot_wider(names_from = label, values_from = c(x,y, wire_id),
              id_cols = segment_id) %>%
  assertr::success_continue(wire_id_from == wire_id_to) %>%
  select(everything(),
         wire_id = wire_id_from,
         # -wire_id_from,
         -wire_id_to) %>%
  identity()

example_1_segment_df <- example_1_df %>% combined_wires_df_to_segments()
example_2_segment_df <- example_2_df %>% combined_wires_df_to_segments()
example_3_segment_df <- example_3_df %>% combined_wires_df_to_segments()
example_1_segment_df
example_2_segment_df
example_3_segment_df

problem_segment_df <- problem_df %>% combined_wires_df_to_segments()

find_cross_wires_intersecton <- . %>%
  group_by(wire_id) %>% nest() %>% {
    crossing(Red = .$data[[1]],
             Blue = .$data[[2]], .name_repair = "unique")
  } %$% {

    # a = Red/from
    # b = Red/to
    # c = Blue/from
    # d = Blue/to

    #n  =     b    -     a
    nx <- Red$x_to - Red$x_from
    ny <- Red$y_to - Red$y_from

    #m  =     c    -     d
    mx <- Blue$x_from - Blue$x_to
    my <- Blue$y_from - Blue$y_to

    #p  =     c    -     a
    px <- Blue$x_from - Red$x_from
    py <- Blue$y_from - Red$y_from

    D <- nx * my - ny * mx
    Qx <- my * px - mx * py
    Qy <- nx * py - ny * px

    # browser()

    boolean_remove <-
      (D == 0) |
      sign(D * Qx) == c(0,-1) | sign(D * Qy) == c(0, -1) |
      abs(D) <= abs(Qx) | abs(D) <= abs(Qy)
    # abs(D) < abs(Qx) | abs(D) < abs(Qy)

    Qx <- Qx#[!boolean_remove]
    Qy <- Qy#[!boolean_remove]
    D <-   D#[!boolean_remove]

    # browser()

    t <-  Qx / D
    s <-  Qy / D

    list(x = Red$x_from + t * nx,
         y = Red$y_from + t * ny,
         D = D,
         Qx = Qx,
         Qy = Qy,
         t = t,
         s = s,
         boolean_remove = boolean_remove)
    # cbind(x = Red$x_from[!boolean_remove] + t * nx[!boolean_remove],
    #       y = Red$y_from[!boolean_remove] + t * ny[!boolean_remove],
    #       t = t,
    #       s = s)
  } %>%
  as_tibble() %>%
  rowid_to_column("cross_point_id") %>%     #debugging
  # mutate(label = glue("{boolean_remove}"))
  identity()

#' The only inconsistency remaining here is that the origin is sometimes included.
#' It is as if <some point> --> <origin> compared to <origin> --> <origin> yields
#' an "intersection" point, even if it shouldn't.

example_1_cross_points <- example_1_segment_df %>%
  find_cross_wires_intersecton()
example_2_cross_points <- example_2_segment_df %>%
  find_cross_wires_intersecton()
example_3_cross_points <- example_3_segment_df %>%
  find_cross_wires_intersecton()
problem_cross_points <- problem_segment_df %>%
  find_cross_wires_intersecton()

example_1_cross_points
example_2_cross_points
example_3_cross_points

problem_cross_points

example_1_plot + geom_point(data = example_1_cross_points,
                            mapping = aes(x, y),
                            size = 10,
                            shape = 4, inherit.aes = FALSE) +
  ggrepel::geom_label_repel(aes(x, y, label = glue("{cross_point_id}:{!boolean_remove}")),
                            data = example_1_cross_points %>% filter(!boolean_remove),
                            inherit.aes = FALSE)

example_2_plot + geom_point(data = example_2_cross_points,
                            mapping = aes(x, y),
                            size = 10,
                            shape = 4, inherit.aes = FALSE) +
  ggrepel::geom_label_repel(aes(x, y, label = glue("{cross_point_id}:{!boolean_remove}")),
                            data = example_2_cross_points %>% filter(!boolean_remove),
                            inherit.aes = FALSE)

example_3_plot + geom_point(data = example_3_cross_points,
                            mapping = aes(x, y),
                            size = 10,
                            shape = 4, inherit.aes = FALSE) +
  ggrepel::geom_label_repel(aes(x, y, label = glue("{cross_point_id}:{!boolean_remove}")),
                            data = example_3_cross_points %>% filter(!boolean_remove),
                            inherit.aes = FALSE)

distance_from_origin <- . %>% filter(!boolean_remove) %>%
  add_row(x = 0, y = 0, boolean_remove = FALSE, .before = 1) %>%
  {dist(.[,c("x","y")], method = "manhattan")} %>%
  as.matrix() %>% `[`( , 1) %>% sort()

# example 1 => 6
example_1_cross_points %>% distance_from_origin
# example 2 => distance 159
example_2_cross_points %>% distance_from_origin
# example 3 => distance 135
example_3_cross_points %>% distance_from_origin

problem_cross_points %>% distance_from_origin

#' Answer: 446



# PART TWO ----------------------------------------------------------------

# ANALYSIS of situation
# bind_rows(example_1_cross_points,
#       example_2_cross_points,
#       example_3_cross_points,
#       problem_cross_points,
#       .id = "example") %>%
#   # filter(x==0, y==0) %>%
#   # filter(Qx == 0 | Qy == 0) %>%
#   View("Cross Points")

example_1_cross_points %<>% filter(!boolean_remove) %>% select(cross_point_id, x, y)
example_2_cross_points %<>% filter(!boolean_remove) %>% select(cross_point_id, x, y)
example_3_cross_points %<>% filter(!boolean_remove) %>% select(cross_point_id, x, y)
problem_cross_points   %<>% filter(!boolean_remove) %>% select(cross_point_id, x, y)

# explorative..
example_1_cross_points %>% nest(data = c(x, y))
example_2_cross_points %>% nest(data = c(x, y))
example_3_cross_points %>% nest(data = c(x, y))
problem_cross_points

#' Note that this function is eerily similar to the `segment`-function. Thus
#' it is really unnecessary.
finding_location_of_cross_point <- . %>%
  rename(x_prev = x, y_prev = y) %>%
  group_by(wire_id) %>%
  mutate(x_next = lead(x_prev), y_next = lead(y_prev)) %>%
  ungroup()

ex_1_wire_df <- example_1_df %>% finding_location_of_cross_point()
ex_2_wire_df <- example_2_df %>% finding_location_of_cross_point()
ex_3_wire_df <- example_3_df %>% finding_location_of_cross_point()

problem_wire_df <- problem_df %>% finding_location_of_cross_point()

steps_to_cross_points <- function(cross_points_df, wire_df) {

  cross_points_df %>%
    crossing(wire_df = wire_df %>%
               group_by(wire_id) %>% nest(wire_df = c(
                 x_prev, x_next, y_prev, y_next, direction, distance
               ))) %>%
    unpack(wire_df) %>%
    # group_by(wire_id) %>%
    mutate(traveled_distance = pmap(., function(x, y, wire_df, ...) {
      traveled_distance <- 0
      # foreach::foreach(
      for (o in seq_len(nrow(wire_df))) {

        x_prev    = wire_df$x_prev[o]
        y_prev    = wire_df$y_prev[o]
        x_next    = wire_df$x_next[o]
        y_next    = wire_df$y_next[o]
        distance  = wire_df$distance[o]
        direction = wire_df$direction[o]
        switch(
          direction,
          U = {
            if (x == x_prev & y_prev <= y & y <= y_next) {
              # print(glue("step {o}"))
              return(traveled_distance + y - y_prev)
            } else {
              traveled_distance <- traveled_distance + distance
            }
          },
          R = {
            if (y == y_prev & x_prev <= x & x <= x_next) {
              # print(glue("step {o}"))
              return(traveled_distance + x - x_prev)
            } else {
              traveled_distance <- traveled_distance + distance
            }
          },
          L = {
            if (y == y_prev & x_next <= x & x <= x_prev) {
              # print(glue("step {o}"))
              return(traveled_distance + x_prev - x)
            } else {
              traveled_distance <- traveled_distance + distance
            }
          },
          D = {
            if (x == x_prev & y_next <= y & y <= y_prev) {
              # print(glue("step {o}"))
              return(traveled_distance + y_prev - y)
            } else {
              traveled_distance <- traveled_distance + distance
            }
          })
      }
      traveled_distance
    })) %>%
    unnest(traveled_distance) %>%
    group_by(cross_point_id) %>%
    summarise(total_distance = sum(traveled_distance)) %>%
    arrange(total_distance) %>%
    identity()
}

#' Example 1 answer: 30
steps_to_cross_points(cross_points_df = example_1_cross_points, wire_df = ex_1_wire_df)
#' Example 2 answer: 610
steps_to_cross_points(cross_points_df = example_2_cross_points, wire_df = ex_2_wire_df)
#' Example 3 answer: 410
steps_to_cross_points(cross_points_df = example_3_cross_points, wire_df = ex_3_wire_df)

#' Problem answer..???
steps_to_cross_points(cross_points_df = problem_cross_points, wire_df = problem_wire_df)
#' Answer: 9006.
#'


