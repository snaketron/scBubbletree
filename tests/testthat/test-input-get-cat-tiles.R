context("Tests input rules of get_cat_tiles")
cat("Tests input rules of get_cat_tiles \n")



test_that("missing argument", {


  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)


  expect_error(get_cat_tiles(
    #btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "bubbletree \\(btd\\) input not found")

  expect_error(get_cat_tiles(
    btd = btd,
    #f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f input not found")

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    #integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical input not found")

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    #round_digits = 2,
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
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature"),
    #rotate_x_axis_labels = T),
    NA)
})





test_that("null/na argument", {


  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)


  expect_error(get_cat_tiles(
    btd = NA,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  expect_error(get_cat_tiles(
    btd = NULL,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")



  expect_error(get_cat_tiles(
    btd = btd,
    f = NA,
    integrate_vertical = T,
    round_digits = 2,
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
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = NA),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
})





test_that("btd argument", {

  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)



  btd_test <- btd
  btd_test$A <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$A <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")
  btd_test <- btd
  btd_test$A <- 1
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd\\$A must be numeric matrix")



  btd_test <- btd
  btd_test$k <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$k <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")
  btd_test <- btd
  btd_test$k <- 1
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "problem in btd\\: btd\\$k must be a positive integer \\(k>=2\\)")
  btd_test <- btd
  btd_test$k <- c(T, F)
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "problem in btd\\: btd\\$k must be a positive integer \\(k>=2\\)")




  btd_test <- btd
  btd_test$km <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$km <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")



  btd_test <- btd
  btd_test$ph <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$ph <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")



  btd_test <- btd
  btd_test$pair_dist <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$pair_dist <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")



  btd_test <- btd
  btd_test$cluster <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$cluster <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")



  btd_test <- btd
  btd_test$tree_meta <- NULL
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  btd_test <- btd
  btd_test$tree_meta <- NA
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")
  btd_test <- btd
  btd_test$tree_meta <- 1
  expect_error(get_cat_tiles(
    btd = btd_test,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "problem in btd: btd\\$tree_meta is not a data.frame")

})






test_that("f argument", {

  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)



  f <- sample(x = LETTERS[1:5], size = 99, replace = T)
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "length of f is not equal to number of cells in btd")



  f <- NA
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  f <- NULL
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  f <- 1:100
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  f <- as.factor(1:100)
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  f <- sample(x = LETTERS[1:5], size = 100, replace = T)
  f[1] <- NA
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "f must be a character vector")



  f <- sample(x = LETTERS[1:5], size = 100, replace = T)
  f[1] <- Inf
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)



  f <- sample(x = LETTERS[1:5], size = 110, replace = T)
  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "length of f is not equal to number of cells in btd")

})






test_that("integrate_vertical argument", {

  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = T,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = 1,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = "1",
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = Inf,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = c(T, F),
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "integrate_vertical is a logical parameter \\(TRUE or FALSE\\)")


})





test_that("tile_bw argument", {

  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = 1,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = "1",
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = Inf,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = c(T, F),
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")


})





test_that("rotate_x_axis_labels argument", {

  # build bubbletree
  btd <- get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                        k = 3,
                        B = 20,
                        N_eff = 50,
                        n_start = 100,
                        iter_max = 100,
                        kmeans_algorithm = "MacQueen",
                        cores = 1,
                        round_digits = 2,
                        show_simple_count = F)

  f <- sample(x = LETTERS[1:5], size = 100, replace = T)

  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = T),
    NA)



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = 1),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = "1"),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = Inf),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = F,
    round_digits = 2,
    tile_text_size = 3,
    tile_bw = F,
    x_axis_name = "Feature",
    rotate_x_axis_labels = c(T, F)),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")


})


