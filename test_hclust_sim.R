a <- matrix(data = rnorm(n = 200*10, mean = 0, sd = 0.01), ncol = 10)
b <- a + matrix(data = rnorm(n = 200*10, mean = 2, sd = 0.01), ncol = 10)
c <- b+matrix(data = rnorm(n = 200*10, mean = 4, sd = 0.01), ncol = 10)
d <- c+matrix(data = rnorm(n = 200*10, mean = 4, sd = 0.01), ncol = 10)
x <- rbind(a, b, c, d)
rm(a,b,c)
c <- rep(x = c(1,2,3,4), each = 200)
hc <- hclust(dist(x), method = "average")
plot(hc)
hc$height


get_hdi <- function(vec, hdi_level) {
  sortedPts <- sort(vec)
  ciIdxInc <- floor(hdi_level * length(sortedPts))
  nCIs = length(sortedPts) - ciIdxInc
  ciWidth = rep(0 , nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i + ciIdxInc] - sortedPts[i]
  }
  HDImin = sortedPts[which.min(ciWidth)]
  HDImax = sortedPts[which.min(ciWidth) + ciIdxInc]
  HDIlim = c(HDImin, HDImax)
  return(HDIlim)
}



get_pair_dist <- function(m, c) {

  get_euc <- function(x, y) {
    for(i in 1:ncol(y)) {
      y[,i] <- (x[i]-y[,i])^2
    }
    y <- apply(X = y, MARGIN = 1, FUN = sum)
    y <- sqrt(y)
    return(y)
  }

  cs <- unique(c)
  stats <- c()
  for(i in 1:(length(cs)-1)) {
    x_i <- m[which(c == cs[i]), ]

    for(j in (i+1):length(cs)) {
      x_j <- m[which(c == cs[j]), ]


      w <- matrix(data = 0, nrow = nrow(x_i), ncol = nrow(x_j))
      for(k in 1:nrow(x_i)) {
        w[k, ] <- get_euc(x = x_i[k, ], y = x_j)
      }
      w <- as.vector(w)

      # symmetric distances
      stats <- rbind(stats, data.frame(c_i = cs[i],
                                       c_j = cs[j],
                                       M = mean(w)))
      stats <- rbind(stats, data.frame(c_i = cs[j],
                                       c_j = cs[i],
                                       M = mean(w)))

    }
  }

  return(stats)
}

w <- get_pair_dist(m = x, c = c)

u_hc <- hclust(d = as.dist(acast(data = w,formula = c_i~c_j, value.var = "M")),
               method = "average")
plot(u_hc)

u_hc$height
hc$height
