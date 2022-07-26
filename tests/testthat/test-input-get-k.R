context("Tests input rules of get_k")
cat("Tests input rules of get_k \n")



test_that("missing argument", {


  expect_error(get_k(#x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x input not found")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     #ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks input not found")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     #B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     # B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     # cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     # n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     # iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     # kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     # mini_output = F,
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F#,
                     # cores = 1
                     ),
               NA)





})




test_that("null/na argument", {


  expect_error(get_k(x = NULL,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix")
  expect_error(get_k(x = NA,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = NULL,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = NA,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = NULL,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = NA,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = NULL,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = NA,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = NULL,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = NA,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = NULL,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = NA,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = NULL,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = NA,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NULL,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans\\: kmeans_algorithm must be one of\\: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NA,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans\\: kmeans_algorithm must be one of\\: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = NULL,
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = NA,
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = NULL),
               "cores must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = NA),
               "cores must be a positive integer")
})




test_that("x argument", {


  expect_error(get_k(x = 1:100,
    ks = 1:5,
    B = 20,
    B_gap = 10,
    cv_prop = 0.1,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    mini_output = F,
    cores = 1),
    "x must be numeric matrix")


  expect_error(get_k(x = data.frame(matrix(data = rnorm(n = 100*10),
                                           nrow = 100, ncol = 10)),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix")



  x <- matrix(data = NA, nrow = 100, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix")


  x <- matrix(data = Inf, nrow = 100, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix, infinite values not allowed")


  x <- matrix(data = rnorm(n = 100*10), nrow = 100, ncol = 10)
  x[10, 1] <- NA
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix, NAs not allowed")


  x <- matrix(data = rnorm(n = 100*10), nrow = 100, ncol = 10)
  x[10, 1] <- Inf
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "x must be numeric matrix, infinite values not allowed")


  # identical elements in x
  x <- matrix(data = 0, nrow = 1000, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "all elements in x are identical")


  # more cols than rows
  x <- matrix(data = rnorm(n = 50*100), nrow = 50, ncol = 100)
  expect_warning(get_k(x = x,
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "more columns \\(features\\) than rows \\(cells\\) in x")


})




test_that("ks argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = matrix(data = 1:10, ncol = 1),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = matrix(data = 1:10, nrow = 1),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = data.frame(x = 1:10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = numeric(length = 0),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, NA, 10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           no NAs are allowed")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, Inf, 10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           no infinite values are allowed")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, -5, 10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, NULL, 10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, 3, 10),
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           duplicate k values are not allowed")





  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = -10,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 0,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:200,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "max\\(ks\\) should be smaller than nrow\\(x\\)")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 5:1,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)




})




test_that("B_gap argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = -1,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = -1.5,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 0.2,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = Inf,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 10,
                     B_gap = 1.5,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 10,
                     B_gap = "100",
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B_gap must be a positive integer > 0")

})




test_that("B arguments", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = -1,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = -1.5,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 0.2,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = Inf,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = "100",
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 1.5,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "B must be a positive integer > 0")

})




test_that("n_start argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = -1,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = Inf,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = -Inf,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 0.5,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 1:10,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = "100",
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 1.5,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "n_start must be a positive integer")

})




test_that("iter_max argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = -1,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = Inf,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = -Inf,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max  = 0.5,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 1:10,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = "100",
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 1.5,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "iter_max must be a positive integer")

})




test_that("cv_prop argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = -1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = Inf,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.25,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 3,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = FALSE,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = "0.5",
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               "cv_prop is a number between 0 \\(excluding\\) and 1")


})




test_that("cores argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = -1),
               "cores must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = Inf),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 0.5),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1:5),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = "2"),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1.5),
               "cores must be a positive integer")

})




test_that("kmeans_algorithm argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = F,
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Mac",
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = c("MacQueen", "Lloyd"),
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "A",
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "L",
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = Inf,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NA,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = 1,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = FALSE,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Hartigan-Wong",
                     mini_output = F,
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Forgy",
                     mini_output = F,
                     cores = 1),
              NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Lloyd",
                     mini_output = F,
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = base::unique,
                     mini_output = F,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


})




test_that("mini_output argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = 1,
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = 2,
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = "T",
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = c(T, F),
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B = 20,
                     B_gap = 10,
                     cv_prop = 0.1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     mini_output = base::unique,
                     cores = 1),
               "mini_output is a logical parameter \\(TRUE or FALSE\\)")

})



