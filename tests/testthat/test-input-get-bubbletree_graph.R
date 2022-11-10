context("Tests input rules of get_bubbletree_graph")
cat("Tests input rules of get_bubbletree_graph \n")


test_that("missing argument", {


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)

  expect_error(get_bubbletree_graph(
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x input not found")

  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              #r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "r input not found")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              #B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              #N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              #n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              #iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              #algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                                      r = 1,
                                      B = 20,
                                      N_eff = 50,
                                      n_start = 100,
                                      iter_max = 100,
                                      algorithm = "original",
                                      # knn_k = 50,
                                      cores = 1,
                                      round_digits = 2,
                                      show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              #cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              #round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10),
                                                 ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2),
                              #show_simple_count = F),
               NA)


})



test_that("null/na argument", {



  expect_error(get_bubbletree_graph(x = NULL,
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x must be numeric matrix")
  expect_error(get_bubbletree_graph(x = NA,
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x must be numeric matrix")

  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = NULL,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "r must be a positive number \\(r>0\\) to build a bubbletree")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = NA,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "r must be a positive number \\(r>0\\) to build a bubbletree")



  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                                     r = 1,
                                     B = NULL,
                                     N_eff = 50,
                                     n_start = 100,
                                     iter_max = 100,
                                     algorithm = "original",
                                     knn_k = 50,
                                     cores = 1,
                                     round_digits = 2,
                                     show_simple_count = F),
               "B must be a positive integer >= 0")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                                     r = 1,
                                     B = NA,
                                     N_eff = 50,
                                     n_start = 100,
                                     iter_max = 100,
                                     algorithm = "original",
                                     knn_k = 50,
                                     cores = 1,
                                     round_digits = 2,
                                     show_simple_count = F),
               "B must be a positive integer >= 0")



  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = NULL,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "N_eff must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = NA,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "N_eff must be a positive integer")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = NULL,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "n_start must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = NA,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "n_start must be a positive integer")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = NULL,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "iter_max must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = NA,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "iter_max must be a positive integer")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = NULL,
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
"see \\?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = NA,
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
          "see \\?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")



  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                                      r = 1,
                                      B = 20,
                                      N_eff = 50,
                                      n_start = 100,
                                      iter_max = 100,
                                      algorithm = "original",
                                      knn_k = NULL,
                                      cores = 1,
                                      round_digits = 2,
                                      show_simple_count = F),
               "knn_k must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                                      r = 1,
                                      B = 20,
                                      N_eff = 50,
                                      n_start = 100,
                                      iter_max = 100,
                                      algorithm = "original",
                                      knn_k = NA,
                                      cores = 1,
                                      round_digits = 2,
                                      show_simple_count = F),
               "knn_k must be a positive integer")



  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = NULL,
                              round_digits = 2,
                              show_simple_count = F),
               "cores must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = NA,
                              round_digits = 2,
                              show_simple_count = F),
               "cores must be a positive integer")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = NULL,
                              show_simple_count = F),
               "round_digits must be a positive integer")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = NA,
                              show_simple_count = F),
               "round_digits must be a positive integer")


  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = NULL),
               "show_simple_count is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_bubbletree_graph(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              r = 1,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = NA),
               "show_simple_count is a logical parameter \\(TRUE or FALSE\\)")


})

