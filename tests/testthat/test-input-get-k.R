context("Tests input rules of get_k")
cat("Tests input rules of get_k \n")


test_that("missing argument", {


  expect_error(get_k(#x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x input not found")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     #ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks input not found")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     # B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     # n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     # iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     # kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     # cores = 1
                     ),
               NA)





})



test_that("null/na argument", {


  expect_error(get_k(x = NULL,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix")
  expect_error(get_k(x = NA,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = NULL,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = NA,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = NULL,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = NA,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = NULL,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = NA,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = NULL,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = NA,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NULL,
                     cores = 1),
               "see \\?kmeans\\: kmeans_algorithm must be one of\\: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NA,
                     cores = 1),
               "see \\?kmeans\\: kmeans_algorithm must be one of\\: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = NULL),
               "cores must be a positive integer")
  expect_error(get_k(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = NA),
               "cores must be a positive integer")
})



test_that("x argument", {


  expect_error(get_k(x = 1:100,
    ks = 1:5,
    B_gap = 10,
    n_start = 100,
    iter_max = 100,
    kmeans_algorithm = "MacQueen",
    cores = 1),
    "x must be numeric matrix")


  expect_error(get_k(x = data.frame(matrix(data = rnorm(n = 100*10),
                                           nrow = 100, ncol = 10)),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix")



  x <- matrix(data = NA, nrow = 100, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix")


  x <- matrix(data = Inf, nrow = 100, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix, infinite values not allowed")


  x <- matrix(data = rnorm(n = 100*10), nrow = 100, ncol = 10)
  x[10, 1] <- NA
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix, NAs not allowed")


  x <- matrix(data = rnorm(n = 100*10), nrow = 100, ncol = 10)
  x[10, 1] <- Inf
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "x must be numeric matrix, infinite values not allowed")


  # identical elements in x
  x <- matrix(data = 0, nrow = 1000, ncol = 10)
  expect_error(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "all elements in x are identical")


  # more cols than rows
  x <- matrix(data = rnorm(n = 50*100), nrow = 50, ncol = 100)
  expect_warning(get_k(x = x,
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "more columns \\(features\\) than rows \\(cells\\) in x")


})



test_that("ks argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = matrix(data = 1:10, ncol = 1),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = matrix(data = 1:10, nrow = 1),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = data.frame(x = 1:10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = numeric(length = 0),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, NA, 10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           no NAs are allowed")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, Inf, 10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           no infinite values are allowed")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, -5, 10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, NULL, 10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = c(1, 3, 3, 10),
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers,
           duplicate k values are not allowed")





  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = -10,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 0,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "ks must be a positive integer or vector of positive integers")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:200,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "max\\(ks\\) should be smaller than nrow\\(x\\)")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 5:1,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)




})



test_that("B_gap argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = -1,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = -1.5,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 0.2,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = Inf,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 1.5,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = "100",
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "B_gap must be a positive integer > 0")

})



test_that("n_start argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = -1,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = Inf,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = -Inf,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 0.5,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 1:10,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = "100",
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 1.5,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "n_start must be a positive integer")

})



test_that("iter_max argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = -1,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = Inf,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = -Inf,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max  = 0.5,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 1:10,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = "100",
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 1.5,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               "iter_max must be a positive integer")

})



test_that("cores argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = -1),
               "cores must be a positive integer")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = Inf),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 0.5),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1:5),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = "2"),
               "cores must be a positive integer")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1.5),
               "cores must be a positive integer")

})



test_that("kmeans_algorithm argument", {


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "MacQueen",
                     cores = 1),
               NA)



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Mac",
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = c("MacQueen", "Lloyd"),
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "A",
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "L",
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = Inf,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = NA,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")



  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = 1,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = FALSE,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Hartigan-Wong",
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Forgy",
                     cores = 1),
              NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = "Lloyd",
                     cores = 1),
               NA)




  expect_error(get_k(x = matrix(data = rnorm(n = 100*10),
                                nrow = 100, ncol = 10),
                     ks = 1:5,
                     B_gap = 10,
                     n_start = 100,
                     iter_max = 100,
                     kmeans_algorithm = base::unique,
                     cores = 1),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


})

