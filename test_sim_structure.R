a <- t(replicate(n = 500, expr = rnorm(n = 100, mean = 0, sd = 1)))
b <- t(replicate(n = 100, expr = rnorm(n = 100, mean = 1, sd = 1)))

c <- t(replicate(n = 100, expr = rnorm(n = 100, mean = 5, sd = 1)))
d <- t(replicate(n = 100, expr = rnorm(n = 100, mean = 2, sd = 1)))
d <- c*d

e <- t(replicate(n = 1000, expr = rnorm(n = 100, mean = -15, sd = 1)))
f <- t(replicate(n = 20, expr = rnorm(n = 100, mean = 15, sd = 1)))

x <- rbind(a,b,c,d,e,f)

