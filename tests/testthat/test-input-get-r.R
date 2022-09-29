context("Tests input rules of get_r")
cat("Tests input rules of get_r \n")



test_that("missing argument", {


  expect_error(get_r(#x = matrix(data = rnorm(n = 300*10), ncol = 10),
    rs = c(0.1, 0.5, 1),
    B_gap = 5,
    n_start = 20,
    iter_max = 100,
    louvain_algorithm = "original",
    cores = 1,
    knn_k = 50),
    "x input not found")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     #rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs input not found")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     # B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     # n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     # iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     # louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     # cores = 1,
                     knn_k = 50),
               NA)


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1),
               #knn_k = 50),
               NA)

})




test_that("null/na argument", {


  expect_error(get_r(x = NULL,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix")
  expect_error(get_r(x = NA,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = NULL,
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = NA,
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = NULL,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = NA,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = NULL,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = NA,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = NULL,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = NA,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = NULL,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = NA,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = NULL,
                     knn_k = 50),
               "cores must be a positive integer")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = NA,
                     knn_k = 50),
               "cores must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = NULL),
               "knn_k must be a positive integer")
  expect_error(get_r(x = matrix(data = rnorm(n = 300*10), ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = NA),
               "knn_k must be a positive integer")
})




test_that("x argument", {


  expect_error(get_r(x = 1:100,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix")


  expect_error(get_r(x = data.frame(matrix(data = rnorm(n = 300*10),
                                           nrow = 300, ncol = 10)),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix")



  x <- matrix(data = NA, nrow = 300, ncol = 10)
  expect_error(get_r(x = x,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix")


  x <- matrix(data = Inf, nrow = 300, ncol = 10)
  expect_error(get_r(x = x,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix, infinite values not allowed")


  x <- matrix(data = rnorm(n = 300*10), nrow = 300, ncol = 10)
  x[10, 1] <- NA
  expect_error(get_r(x = x,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix, NAs not allowed")


  x <- matrix(data = rnorm(n = 300*10), nrow = 300, ncol = 10)
  x[10, 1] <- Inf
  expect_error(get_r(x = x,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "x must be numeric matrix, infinite values not allowed")


  # identical elements in x
  x <- matrix(data = 0, nrow = 1000, ncol = 10)
  expect_error(get_r(x = x,
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "all elements in x are identical")


  # more cols than rows
  x <- matrix(data = rnorm(n = 50*100), nrow = 50, ncol = 100)
  expect_warning(get_r(x = x,
                       rs = c(0.1, 0.5, 1),
                       B_gap = 5,
                       n_start = 20,
                       iter_max = 100,
                       louvain_algorithm = "original",
                       cores = 1,
                       knn_k = 50),
                 "more columns \\(features\\) than rows \\(cells\\) in x")


})




test_that("rs argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = matrix(data = 1:10, ncol = 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = matrix(data = 1:10, nrow = 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = data.frame(x = 1:10),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = numeric(length = 0),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.5, 0.8, NA, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers,
           no NAs are allowed")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.5, 0.8, Inf, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers,
           no infinite values are allowed")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.5, 0.8, -5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.5, 0.8, NULL, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.5, 0.8, 0.8, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers,
           duplicate r values are not allowed")





  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = -10,
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = 0,
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "rs must be a positive number or vector of positive numbers")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(1, 0.5, 0.1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)
})




test_that("B_gap argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = -1,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = -1.5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 0.2,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = Inf,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 1.5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = "100",
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "B_gap must be a positive integer > 0")

})






test_that("n_start argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = -1,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = Inf,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = -Inf,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 0.5,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 1:10,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = "100",
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 1.5,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "n_start must be a positive integer")

})




test_that("iter_max argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = -1,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = Inf,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = -Inf,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max  = 0.5,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 1:10,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = "100",
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 1.5,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               "iter_max must be a positive integer")

})






test_that("cores argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = -1,
                     knn_k = 50),
               "cores must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = Inf,
                     knn_k = 50),
               "cores must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 0.5,
                     knn_k = 50),
               "cores must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1:5,
                     knn_k = 50),
               "cores must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = "2",
                     knn_k = 50),
               "cores must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1.5,
                     knn_k = 50),
               "cores must be a positive integer")

})




test_that("louvain_algorithm argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "o",
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = c("o", "a"),
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "A",
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "L",
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = Inf,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = NA,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = 1,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = FALSE,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 50),
               NA)




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "LMR",
                     cores = 1,
                     knn_k = 50),
               NA)




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "SLM",
                     cores = 1,
                     knn_k = 50),
               NA)



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "Leiden",
                     cores = 1,
                     knn_k = 50),
               NA)




  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = base::unique,
                     cores = 1,
                     knn_k = 50),
               "see \\?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")


})








test_that("knn_k argument", {


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = -1),
               "knn_k must be a positive integer")



  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = Inf),
               "knn_k must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 0.5),
               "knn_k must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 1:5),
               "knn_k must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = "50"),
               "knn_k must be a positive integer")


  expect_error(get_r(x = matrix(data = rnorm(n = 300*10),
                                nrow = 300, ncol = 10),
                     rs = c(0.1, 0.5, 1),
                     B_gap = 5,
                     n_start = 20,
                     iter_max = 100,
                     louvain_algorithm = "original",
                     cores = 1,
                     knn_k = 1.5),
               "knn_k must be a positive integer")

})
