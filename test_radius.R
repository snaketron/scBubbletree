c <- c(10000, 3000, rnbinom(n = 4, size = 100, prob = 0.1))
c <- data.frame(c = c, id = as.character(1:6))
ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c))

require(patchwork)

ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c))+
  scale_radius(range = c(1, 7))+
  guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
         size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))+
  theme(legend.position = "top")|
  ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c))+
  scale_size(range = c(1, 7))+
  guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
         size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))+
  theme(legend.position = "top")



# r <- 1:10
# a = 2*pi*r^2
# plot(r, a)
# with r -> accentuate large vs. small



c <- c(10000, 3000, rnbinom(n = 4, size = 100, prob = 0.1))
c <- data.frame(c = c, id = as.character(1:6))
ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c))


c <- c(7:1)
c <- data.frame(c = c, id = as.character(1:7))

require(patchwork)

ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c), shape = 22)+
  scale_radius(range = c(1, 7))+
  guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
         size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))+
  theme(legend.position = "top")|
  ggplot(data = c)+
  geom_point(aes(x = id, y = c, size = c), shape = 22)+
  scale_size(range = c(1, 7))+
  guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
         size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))+
  theme(legend.position = "top")

