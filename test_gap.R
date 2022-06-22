require(scBubbletree)

a <- matrix(data = rnorm(n = 500*8, mean = 0, sd = 1), ncol = 8)
b <- rbind(matrix(data = rnorm(n = 250*1, mean = 0, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 250*1, mean = 4, sd = 1), ncol = 1))
c <- rbind(matrix(data = rnorm(n = 250*1, mean = 6, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 250*1, mean = 10, sd = 1), ncol = 1))
a <- cbind(b, c, a)
rm(b,c)




require(cluster)
gaps <- cluster::clusGap(x = a,
                         FUNcluster = kmeans,
                         K.max = 10,
                         iter.max = 100,
                         B = 50,
                         d.power = 1,
                         spaceH0 = "original")
plot(gaps)
gap_cluster <- data.frame(gaps$Tab)
gap_cluster$k <- as.numeric(rownames(gap_cluster))


ggplot()+
  geom_point(data = gap_cluster,
             aes(x = k, y = gap))+
  geom_errorbar(data = gap_cluster,
                aes(x = k, y = gap, ymin = gap-SE.sim,
                    ymax = gap+SE.sim), width = 0.3)


b_test_0 <- get_k(B = 5,
                  cv_prop = 1,
                  ks = 1:10,
                  x = a,
                  cores = 4,
                  mini_output = F,
                  B_gap = 100)



ggplot()+
  geom_point(data = b_test_0$gap_stats_summary,
             aes(x = k, y = gap_mean))+
  geom_errorbar(data = b_test_0$gap_stats_summary,
                aes(x = k, y = gap_mean, ymin = L95, ymax = H95), width = 0.3)+
  geom_point(data = b_test_0$gap_stats,
             aes(x = k, y = gap), size = 0.25, col = "red",
             position = position_jitter(width = 0.25, height = 0))+
  theme_bw()+
  ylab(label = "Average Gap")|
ggplot(data = b_test_0$wcss_stats_summary)+
  geom_point(aes(x = k, y = wcss_mean))+
  geom_errorbar(aes(x = k, y = wcss_mean, ymin = L95, ymax = H95), width = 0.1)+
  theme_bw()+
  scale_y_log10()+
  ylab(label = "Average WCSS")




plot(b_test_0$gap_stats_summary$gap_mean,
     gap_cluster$gap);abline(0,1)


