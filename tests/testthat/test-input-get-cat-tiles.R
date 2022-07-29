context("Tests input rules of get_cat_tiles")
cat("Tests input rules of get_cat_tiles \n")



test_that("missing argument", {


  # build bubbletree
  btd <- get_bubbletree(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        seed = NULL,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)


  expect_error(get_cat_tiles(
    #btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "bubbletree \\(btd\\) is missing")

  expect_error(get_cat_tiles(
    btd = btd,
    #f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f is missing")

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    #integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is missing")

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    #round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    #show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    #disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    #tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    #tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    #x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature"),
    #rotate_x_axis_labels = T),
    NA)
})





test_that("null/na argument", {


  # build bubbletree
  btd <- get_bubbletree(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        seed = NULL,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)


  expect_error(get_cat_tiles(
    btd = NA,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "problem with the input bubbletree")
  expect_error(get_cat_tiles(
    btd = NULL,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "problem with the input bubbletree")



  expect_error(get_cat_tiles(
    btd = btd,
    f = NA,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")
  expect_error(get_cat_tiles(
    btd = btd,
    f = NULL,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = NA,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = NULL,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = NA,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = NULL,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = NULL,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "show_hclust is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = NA,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "show_hclust is a logical parameter \\(TRUE or FALSE\\)")




  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = NULL,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "disable_hclust is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = NA,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "disable_hclust is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = NULL,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = NA,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = NULL,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = NA,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = NULL,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = NA,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = NULL),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    show_hclust = F,
    disable_hclust = F,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = NA),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
})



