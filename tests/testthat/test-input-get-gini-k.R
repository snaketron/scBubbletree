context("Tests input rules of get_gini_k")
cat("Tests input rules of get_gini_k \n")



test_that("missing argument", {
  # input data
  data("d_500", package = "scBubbletree")
  A <- d_500$A
  labels <- d_500$f
  
  b_k <- get_k(x = A,
               ks = 1:5,
               B_gap = 5,
               n_start = 100,
               iter_max = 200,
               kmeans_algorithm = "MacQueen",
               cores = 1)
  
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  
  expect_error(get_gini_k(obj = b_k), 
               "labels input not found")
  
  expect_error(get_gini_k(labels = labels), 
               "obj input not found")
  
  expect_error(get_gini_k(), 
               "labels input not found")

})




test_that("na/null argument", {
  # input data
  data("d_500", package = "scBubbletree")
  A <- d_500$A
  labels <- d_500$f
  
  b_k <- get_k(x = A,
               ks = 1:5,
               B_gap = 5,
               n_start = 100,
               iter_max = 200,
               kmeans_algorithm = "MacQueen",
               cores = 1)
  
  
  expect_error(get_gini_k(labels = NA,
                          obj = b_k), 
               "labels must be a vector with more than one element")
  expect_error(get_gini_k(labels = NULL,
                          obj = b_k), 
               "labels must be a vector with more than one element")
  
  
  expect_error(get_gini_k(labels = NA,
                          obj = NA), 
               "labels must be a vector with more than one element")
  expect_error(get_gini_k(labels = NULL,
                          obj = NULL), 
               "labels must be a vector with more than one element")
  
  
  expect_error(get_gini_k(labels = labels,
                          obj = NA), 
               "problem with input obj")
  expect_error(get_gini_k(labels = labels,
                          obj = NULL), 
               "problem with input obj")
  
})






test_that("labels input", {
  # input data
  data("d_500", package = "scBubbletree")
  A <- d_500$A
  
  
  b_k <- get_k(x = A,
               ks = 1:5,
               B_gap = 5,
               n_start = 100,
               iter_max = 200,
               kmeans_algorithm = "MacQueen",
               cores = 1)
  
  labels <- d_500$f
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  
  
  labels <- d_500$f
  labels[1] <- NA
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               "labels cannot have INF/NA/NULL values")
  
  
  labels <- d_500$f
  labels[1] <- Inf # inf converted to character
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  
  
  labels <- d_500$f
  labels[1] <- 1 # inf converted to character
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  
  
  labels <- d_500$f
  labels <- as.factor(labels)
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               "labels can only contain characters or numbers")
  
  
  labels <- d_500$f
  labels <- as.numeric(as.factor(labels))
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  
  
  labels <- d_500$f
  labels <- numeric(length = length(labels))
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
})






test_that("obj input", {
  # input data
  data("d_500", package = "scBubbletree")
  A <- d_500$A
  
  b_k <- get_k(x = A,
               ks = 1:5,
               B_gap = 5,
               n_start = 100,
               iter_max = 200,
               kmeans_algorithm = "MacQueen",
               cores = 1)
  
  b_r <- get_r(x = A,
               rs = c(0.1, 0.5, 1),
               B_gap = 5,
               n_start = 10,
               iter_max = 50,
               algorithm = "original")
  
  
  labels <- d_500$f
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  expect_error(get_gini_k(labels = labels,
                          obj = b_r), 
               NA)
  
  
  
  
  b_k <- get_k(x = A,
               ks = 1,
               B_gap = 5,
               n_start = 100,
               iter_max = 200,
               kmeans_algorithm = "MacQueen",
               cores = 1)
  
  b_r <- get_r(x = A,
               rs = 0.1,
               B_gap = 5,
               n_start = 10,
               iter_max = 50,
               algorithm = "original")
  
  
  labels <- d_500$f
  expect_error(get_gini_k(labels = labels,
                          obj = b_k), 
               NA)
  expect_error(get_gini_k(labels = labels,
                          obj = b_r), 
               NA)
  
  
  
})
