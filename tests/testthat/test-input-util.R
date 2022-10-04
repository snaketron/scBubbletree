context("Tests input rules of util")
cat("Tests input rules of util \n")


test_that("get_dendrogram: missing argument", {
  # source("R/util.R")
  # source("R/input_checks.R")
  ph <- ape::rtree(n = 10, rooted = FALSE, tip.label = 1:10)
  cluster <- sample(x = 1:10, size = 100, replace = TRUE)
  
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    NA)
  
  expect_error(get_dendrogram(
    # ph = ph,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    "ph input not found")
  
  expect_error(get_dendrogram(
    ph = ph,
    # cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    "cluster input not found")
  
  expect_error(get_dendrogram(
    ph = ph,
    # cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    "cluster input not found")
  
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    # round_digits = 1,
    show_simple_count = T),
    "round_digits input not found")
  
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = 1),
    # show_simple_count = T),
    "show_simple_count input not found")
})

test_that("get_dendrogram: ph and cluster arguments", {
  # source("R/util.R")
  # source("R/input_checks.R")
  
  ph <- ape::rtree(n = 10, rooted = FALSE, tip.label = 1:10)
  cluster <- sample(x = 1:20, size = 100, replace = TRUE)
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    "mismatch between tip\\.labels of ph and cluster IDs")
  
  ph <- ape::rtree(n = 10, rooted = FALSE, tip.label = 1:10)
  cluster <- sample(x = 1:10, size = 100, replace = TRUE)
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    NA)
  
  ph <- ape::rtree(n = 10, rooted = TRUE, tip.label = 1:10)
  cluster <- sample(x = 1:10, size = 100, replace = TRUE)
  expect_error(get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    NA)
  
  hc <- hclust(d = as.dist(m = matrix(rnorm(n = 9), nrow = 3)))
  cluster <- sample(x = 1:10, size = 100, replace = TRUE)
  expect_error(get_dendrogram(
    ph = hc,
    cluster = cluster,
    round_digits = 1,
    show_simple_count = T),
    "ph must be a phylo object")
})
