context("Tests input rules of get_num_violins")
cat("Tests input rules of get_num_violins \n")


test_that("missing argument", {
  # build bubbletree
  btd <- get_bubbletree_kmeans(
    x = matrix(data = rnorm(n = 100*10), ncol = 10),
    k = 3,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)

  fs <- matrix(data = rnorm(n = 100*5), ncol = 5)

  
  expect_error(get_num_violins(btd = btd,
                  fs = fs,
                  x_axis_name = "Feature distribution",
                  rotate_x_axis_labels = TRUE),
               NA)
  
  expect_error(get_num_violins(#btd = btd,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "bubbletree \\(btd\\) input not found")
  
  expect_error(get_num_violins(btd = btd,
                               # fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "fs input not found")
  
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               # x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
  
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = "Feature distribution"),
                               #rotate_x_axis_labels = TRUE),
               NA)
})



test_that("na/null argument", {
  # build bubbletree
  btd <- get_bubbletree_kmeans(
    x = matrix(data = rnorm(n = 100*10), ncol = 10),
    k = 3,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  fs <- matrix(data = rnorm(n = 100*5), ncol = 5)
  
  
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
  
  
  expect_error(get_num_violins(btd = NA,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "btd should be a list with 9 elements")
  expect_error(get_num_violins(btd = NULL,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "btd should be a list with 9 elements")
  
  
  
  expect_error(get_num_violins(btd = btd,
                               fs = NA,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "fs must be a numeric vector or matrix")
  expect_error(get_num_violins(btd = btd,
                               fs = NULL,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "fs must be a numeric vector or matrix")
  
  
  
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = NA,
                               rotate_x_axis_labels = TRUE),
               "x_axis_name must be a character string")
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = NULL,
                               rotate_x_axis_labels = TRUE),
               "x_axis_name must be a character string")
  
  
  
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = NA),
              "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_num_violins(btd = btd,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = NULL),
              "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  
  
})



test_that("btd argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  fs <- d_500$fs
  
  btd_k <- get_bubbletree_kmeans(
    x = x,
    k = 5,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  btd_r <- get_bubbletree_louvain(
    x = x,
    r = 0.1,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    louvain_algorithm = "original",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  btd_r_0 <- expect_warning(get_bubbletree_louvain(
    x = x,
    r = 10^(-5),
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    louvain_algorithm = "original",
    cores = 1,
    round_digits = 2,
    show_simple_count = F),
    "Only one cluster at specified r, bubbletree can't be constructed")
  
  btd_d <- get_bubbletree_dummy(
    x = x,
    cs = sample(x = base::LETTERS[1:5], size = nrow(x), replace = T),
    B = 20,
    N_eff = 50,
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  
  expect_error(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
  expect_error(get_num_violins(btd = btd_r,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
  expect_error(get_num_violins(btd = btd_r_0,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "NA/NULL elements or wrong class detected in the bubbletree")
  expect_error(get_num_violins(btd = btd_d,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
})



test_that("fs argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  fs <- d_500$fs
  
  
  btd_k <- get_bubbletree_kmeans(
    x = x,
    k = 5,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  fs <- d_500$fs
  expect_error(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
    NA)
  
  
  fs <- d_500$fs
  fs <- fs[, 1]
  expect_error(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               NA)
  
  
  fs <- d_500$fs
  fs <- fs[1, ]
  expect_error(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "length\\(fs\\) != length\\(btd\\$cluster\\)")
  
  
  fs <- d_500$fs
  fs[, 1] <- NA
  expect_condition(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
          paste0("complete feature column is composed of NA or infinite values",
                 "|some features are NAs, they will be omitted"))
  
  
  f <- d_500$fs
  f[, 1] <- Inf
  expect_condition(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
        paste0("complete feature column is composed of NA or infinite values",
               "|some features are NAs, they will be omitted"))
  
  
  fs <- d_500$fs
  fs[1:20, 1] <- Inf
  expect_warning(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "ome feature values in fs are infinite, they will be omitted")
  
  
  fs <- d_500$fs
  fs[1:20, 1] <- "A"
  expect_error(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "fs must be a numeric vector or matrix")
  
  
  
  fs <- d_500$fs[, 1]
  fs[1:20] <- NA
  expect_warning(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "some features are NAs, they will be omitted")
  
  
  fs <- d_500$fs[, 1]
  fs[1:20] <- Inf
  expect_warning(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "some feature values in fs are infinite, they will be omitted")
  
  
  fs <- d_500$fs[, 1]
  fs[1:(length(fs)-1)] <- Inf
  expect_warning(get_num_violins(btd = btd_k,
                               fs = fs,
                               x_axis_name = "Feature distribution",
                               rotate_x_axis_labels = TRUE),
               "some feature values in fs are infinite, they will be omitted")
  
  
  fs <- d_500$fs[, 1]
  fs[1:(length(fs)-1)] <- NA
  expect_warning(get_num_violins(btd = btd_k,
                                 fs = fs,
                                 x_axis_name = "Feature distribution",
                                 rotate_x_axis_labels = TRUE),
                 "some features are NAs, they will be omitted")
  
})
