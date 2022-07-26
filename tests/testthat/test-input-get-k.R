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





test_that("B_gap arguments", {


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
               NA)


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
               NA)

})




#
# test_that("Null input", {
#
#
#   expect_error(checkInput(usage.data = NULL,
#                           mcmc.chains = NULL,
#                           mcmc.cores = NULL,
#                           mcmc.steps = NULL,
#                           mcmc.warmup = NULL,
#                           hdi.level = NULL),
#                "arguments must be specified")
#
#
#   expect_error(checkInput(),
#                "arguments must be specified")
#
#
# })
#
#
#
# test_that("usage.data check", {
#
#   expect_error(checkUsageData(usage.data = NA),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = NULL),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = Inf),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = character()),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = numeric()),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = logical()),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = array()),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = matrix()),
#                "usage.data must be data.frame")
#
#   expect_error(checkUsageData(usage.data = data.frame()),
#                "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(
#     usage.data = data.frame(a = NA, b = NA, c = NA, d = NA)),
#     "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2), condition = numeric(length = 2),
#     gene_name = numeric(length = 2), gene_usage_count = numeric(length = 2))),
#     "column sample_id must be of character type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2),
#     condition = numeric(length = 2),
#     gene_name = numeric(length = 2),
#     gene_usage_count = numeric(length = 2),
#     temp = numeric(length = 2))),
#     "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2),
#     condition = numeric(length = 2),
#     gene_name = numeric(length = 2))),
#     "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2),
#     gene_name = numeric(length = 2))),
#     "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2))),
#     "usage.data must contain the following columns: 'sample_id',
#          'condition', 'gene_name' and 'gene_usage_count'")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = numeric(length = 2),
#     condition = numeric(length = 2),
#     gene_name = character(length = 2),
#     gene_usage_count = character(length = 2),
#     stringsAsFactors = FALSE)),
#     "column sample_id must be of character type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 2),
#     condition = numeric(length = 2),
#     gene_name = character(length = 2),
#     gene_usage_count = character(length = 2),
#     stringsAsFactors = FALSE)),
#     "column condition must be of character type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 2),
#     condition = character(length = 2),
#     gene_name = character(length = 2),
#     gene_usage_count = character(length = 2),
#     stringsAsFactors = FALSE)),
#     "column gene_usage_count must be of numeric type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 2),
#     condition = character(length = 2),
#     gene_name = character(length = 2),
#     gene_usage_count = NA,
#     stringsAsFactors = FALSE)),
#     "column gene_usage_count must be of numeric type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 2),
#     condition = character(length = 2),
#     gene_name = character(length = 2),
#     gene_usage_count = logical(length = 2),
#     stringsAsFactors = FALSE)),
#     "column gene_usage_count must be of numeric type.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 2),
#     condition = c("A", "A"),
#     gene_name = character(length = 2),
#     gene_usage_count = numeric(length = 2),
#     stringsAsFactors = FALSE)),
#     "exactly 2 biological conditions must be provided.")
#
#   expect_error(checkUsageData(usage.data = data.frame(
#     sample_id = character(length = 3),
#     condition = c("A", "B", "C"),
#     gene_name = character(length = 3),
#     gene_usage_count = numeric(length = 3),
#     stringsAsFactors = FALSE)),
#     "exactly 2 biological conditions must be provided.")
# })
#
#
#
# test_that("hdi.level check", {
#
#   expect_error(object = checkHdi(hdi.level = NA),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(object = checkHdi(hdi.level = NULL),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(object = checkHdi(hdi.level = Inf),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(object = checkHdi(hdi.level = numeric(length = 1)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = double(length = 1)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = integer(length = 1)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = character(length = 1)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = logical(length = 1)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#
#
#   expect_error(checkHdi(hdi.level = numeric(length = 3)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = double(length = 3)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = integer(length = 3)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = character(length = 3)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
#   expect_error(checkHdi(hdi.level = logical(length = 3)),
#                regexp = "hdi\\.level must be a number in range \\(0, 1\\)")
#
# })
#
#
#
# test_that("mcmc.chains check", {
#
#   expect_error(object = checkMcmcChains(mcmc.chains = NA),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = NULL),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = Inf),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = numeric(length = 1)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = double(length = 1)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = integer(length = 1)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = character(length = 1)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = logical(length = 1)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_silent(object = checkMcmcChains(mcmc.chains = as.integer(x = 2)))
#
#   # len > 1
#   expect_error(object = checkMcmcChains(mcmc.chains = numeric(length = 3)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = double(length = 3)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = integer(length = 3)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = character(length = 3)),
#                regexp = "mcmc.chains must be a positive integer > 0")
#
#   expect_error(object = checkMcmcChains(mcmc.chains = logical(length = 3)),
#                regexp = "mcmc.chains must be a positive integer > 0")
# })
#
#
#
# test_that("mcmc.cores check", {
#
#   expect_error(object = checkMcmcCores(mcmc.cores = NA),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = NULL),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = Inf),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = numeric(length = 1)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = double(length = 1)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = integer(length = 1)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = character(length = 1)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = logical(length = 1)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_silent(object = checkMcmcCores(mcmc.cores = as.integer(x = 2)))
#
#   # len > 1
#   expect_error(object = checkMcmcCores(mcmc.cores = numeric(length = 3)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = double(length = 3)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = integer(length = 3)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = character(length = 3)),
#                regexp = "mcmc.cores must be a positive integer > 0")
#
#   expect_error(object = checkMcmcCores(mcmc.cores = logical(length = 3)),
#                regexp = "mcmc.cores must be a positive integer > 0")
# })
#
#
#
# test_that("mcmc.steps and mcmc.warmup check", {
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = NA, mcmc.warmup = NA),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = NULL, mcmc.warmup = NULL),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = Inf, mcmc.warmup = Inf),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = numeric(length = 1),
#                                        mcmc.warmup = numeric(length = 1)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = double(length = 1),
#                                        mcmc.warmup = double(length = 1)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = integer(length = 1),
#                                        mcmc.warmup = integer(length = 1)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = character(length = 1),
#                                        mcmc.warmup = character(length = 1)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = logical(length = 1),
#                                        mcmc.warmup = logical(length = 1)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_silent(object = checkMcmcSteps(mcmc.steps = as.integer(x = 500),
#                                         mcmc.warmup = as.integer(x = 100)))
#
#   # len > 1
#   expect_error(object = checkMcmcSteps(mcmc.steps = numeric(length = 2),
#                                        mcmc.warmup = numeric(length = 2)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = double(length = 2),
#                                        mcmc.warmup = double(length = 2)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = integer(length = 2),
#                                        mcmc.warmup = integer(length = 2)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = character(length = 2),
#                                        mcmc.warmup = character(length = 2)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
#
#   expect_error(object = checkMcmcSteps(mcmc.steps = logical(length = 2),
#                                        mcmc.warmup = logical(length = 2)),
#                regexp = "mcmc.steps >= 500 & mcmc.warmup >= 100.")
# })
#
#
