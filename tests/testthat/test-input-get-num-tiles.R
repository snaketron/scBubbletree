context("Tests input rules of get_num_tiles")
cat("Tests input rules of get_num_tiles \n")


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

  f <- rnorm(n = 100)

  
  expect_error(get_num_tiles(
    #btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "bubbletree \\(btd\\) input not found")

  expect_error(get_num_tiles(
    btd = btd,
    # fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "fs input not found")
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    # summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function input not found")
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    # round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    # tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    # tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    # x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = ""),
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
  f <- rnorm(n = 100)
  
  
  expect_error(get_num_tiles(
    btd = NA,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  expect_error(get_num_tiles(
    btd = NULL,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "btd should be a list with 9 elements")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = NA,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "fs must be a numeric vector or matrix")
  expect_error(get_num_tiles(
    btd = btd,
    fs = NULL,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "fs must be a numeric vector or matrix")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = NA,
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = NULL,
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = NA,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = NULL,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = NA,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = NULL,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = NA,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = NULL,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = NA,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = NULL,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  
  
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = NA),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_num_tiles(
    btd = btd,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = NULL),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
})


test_that("btd argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
  
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
  
  btd_r <- get_bubbletree_graph(
    x = x,
    r = 0.1,
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    algorithm = "original",
    cores = 1,
    round_digits = 2,
    show_simple_count = F)
  
  btd_r_0 <- expect_warning(get_bubbletree_graph(
    x = x,
    r = 10^(-5),
    B = 20,
    N_eff = 50,
    n_start = 100,
    iter_max = 100,
    algorithm = "original",
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
  
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_r,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_d,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_r_0,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "NA/NULL elements or wrong class detected in the bubbletree")
})


test_that("f argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  
  
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
  
  f <- d_500$fs
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  
  f <- d_500$fs
  f <- f[, 1]
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  
  f <- d_500$fs
  f <- f[1, ]
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "length\\(fs\\) \\!\\= length\\(btd\\$cluster\\)")
  
  
  f <- d_500$fs
  f[, 1] <- NA
  expect_condition(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs
  f[, 1] <- Inf
  expect_condition(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some feature values in fs are infinite, they will be omitted")
  
  
  f <- d_500$fs
  f[1:20, 1] <- Inf
  expect_condition(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some feature values in fs are infinite, they will be omitted")
  
  
  f <- d_500$fs
  f[1:20, 1] <- "A"
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "fs must be a numeric vector or matrix")
  
  
  f <- d_500$fs
  f[1:20, 1] <- "A"
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "fs must be a numeric vector or matrix")
  
  
  f <- d_500$fs[, 1]
  f[1:20] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:20] <- Inf
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some feature values in fs are infinite, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- Inf
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some feature values in fs are infinite, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "sum",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "median",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "pct zero",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
  
  f <- d_500$fs[, 1]
  f[1:(length(f)-1)] <- NA
  expect_warning(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "pct nonzero",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "some features are NAs, they will be omitted")
  
})


test_that("summary_function argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "median",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "sum",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "pct nonzero",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "pct zero",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "pct",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "means",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = c("mean", "median"),
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = c("mean", "mean"),
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
})


test_that("round_digits argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = -1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 0,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 0.5,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = "1",
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = c(1, 2),
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = as.factor(1),
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "round_digits must be a positive integer")
})


test_that("tile_text_size argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
  
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = -1,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 0,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = Inf,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_text_size must be a number >0")
})


test_that("tile_bw argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
  
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = T,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = 1,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = "a",
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = c(T, F),
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = c(T, T),
    x_axis_name = "",
    rotate_x_axis_labels = T),
    "tile_bw is a logical parameter \\(TRUE or FALSE\\)")
})


test_that("x_axis_name argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
  
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = 1,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = T,
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = as.factor("A"),
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = c('a', 'b'),
    rotate_x_axis_labels = T),
    "x_axis_name must be a character string")
})


test_that("x_axis_name argument", {
  
  data("d_500", package = "scBubbletree")
  x <- d_500$A
  f <- d_500$fs
  
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
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = T),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name = "",
    rotate_x_axis_labels = F),
    NA)
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name="",
    rotate_x_axis_labels = 1),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name="",
    rotate_x_axis_labels = "a"),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  
  expect_error(get_num_tiles(
    btd = btd_k,
    fs = f,
    summary_function = "mean",
    round_digits = 1,
    tile_text_size = 2,
    tile_bw = F,
    x_axis_name="",
    rotate_x_axis_labels = c(T, F)),
    "rotate_x_axis_labels is a logical parameter \\(TRUE or FALSE\\)")
  
})
