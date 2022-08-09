context("Tests input rules of get_gini")
cat("Tests input rules of get_gini \n")



test_that("missing argument", {

  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- base::sample(x = base::LETTERS[1:3],
                         size = 500, replace = T)

  expect_error(get_gini(clusters = clusters), "labels is missing")

  expect_error(get_gini(labels = labels), "clusters is missing")

  expect_error(get_gini(), "labels is missing")

})




test_that("null/na argument", {

  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- base::sample(x = base::LETTERS[1:3],
                         size = 500, replace = T)

  expect_error(get_gini(labels = NA,
                        clusters = clusters),
               "labels must be a vector with more than one element")
  expect_error(get_gini(labels = NULL,
                        clusters = clusters),
               "labels must be a vector with more than one element")

  expect_error(get_gini(labels = labels,
                        clusters = NA),
               "clusters must be a vector with more than one element")
  expect_error(get_gini(labels = labels,
                        clusters = NULL),
               "clusters must be a vector with more than one element")

  expect_error(get_gini(labels = NA,
                        clusters = NA),
               "labels must be a vector with more than one element")
  expect_error(get_gini(labels = NULL,
                        clusters = NULL),
               "labels must be a vector with more than one element")

})




test_that("labels argument", {


  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- base::sample(x = base::LETTERS[1:3],
                         size = 500, replace = T)

  # NA element
  labels_test <- labels
  labels_test[1] <- NA
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # INF element but as character
  labels_test <- labels
  labels_test[1] <- Inf
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               NA)


  labels <- base::sample(x = 1:3,
                         size = 500, replace = T)
  # NA element
  labels_test <- labels
  labels_test[1] <- NA
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # INF element
  labels_test <- labels
  labels_test[1] <- Inf
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # negative element
  labels_test <- labels
  labels_test[1] <- -5
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               NA)


  expect_error(get_gini(labels = 1,
                        clusters = clusters),
               "labels must be a vector with more than one element")

  expect_error(get_gini(labels = c(1, 1),
                        clusters = clusters),
               "labels and clusters must be equal-length vectors")

  expect_error(get_gini(labels = rep(1, times=length(clusters)),
                        clusters = clusters),
               NA)

  expect_error(get_gini(labels = rep(1, times=length(clusters)),
                        clusters = clusters)$wgi==0,
               NA)

  expect_error(all(get_gini(labels = rep(1, times=length(clusters)),
                        clusters = clusters)$gi==0),
               NA)


  # labels as factors
  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- as.factors(base::sample(x = base::LETTERS[1:3],
                                    size = 500, replace = T))
  expect_error(get_gini(labels = labels,
                        clusters = clusters),
               NA)


  # labels as logicals
  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- base::sample(x = c(T, F), size = 500, replace = T)
  expect_error(get_gini(labels = labels, clusters = clusters),
               "labels can only contain characters, factors or numbers")

})




test_that("clusters argument", {


  clusters <- base::sample(x = base::LETTERS[1:3],
                           size = 500, replace = T)
  labels <- base::sample(x = base::LETTERS[1:3],
                         size = 500, replace = T)

  # NA element
  labels_test <- labels
  labels_test[1] <- NA
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # INF element but as character
  labels_test <- labels
  labels_test[1] <- Inf
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               NA)


  labels <- base::sample(x = 1:3,
                         size = 500, replace = T)
  # NA element
  labels_test <- labels
  labels_test[1] <- NA
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # INF element
  labels_test <- labels
  labels_test[1] <- Inf
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               "labels cannot have INF\\/NA\\/NULL values")

  # negative element
  labels_test <- labels
  labels_test[1] <- -5
  expect_error(get_gini(labels = labels_test,
                        clusters = clusters),
               NA)




  expect_error(get_gini(labels = 1,
                        clusters = clusters),
               "labels must be a vector with more than one element")

  expect_error(get_gini(labels = c(1, 1),
                        clusters = clusters),
               "labels and clusters must be equal-length vectors")

  expect_error(get_gini(labels = rep(1, times=length(clusters)),
                        clusters = clusters),
               NA)

  expect_error(get_gini(labels = rep(1, times=length(clusters)),
                        clusters = clusters)$wgi==0,
               NA)

  expect_error(all(get_gini(labels = rep(1, times=length(clusters)),
                            clusters = clusters)$gi==0),
               NA)

})
