# R packages
library(hexSticker)
library(tidyverse)
library(MASS)

set.seed(1234)
# data for cloud
cl <- mvrnorm(n = 1400,c(-2,0.12), matrix(c(4.5,0,0,0.000025),2,2))
cl2 <- data.frame(x=cl[,1], y=cl[,2])

# plot mountains and cloud
p <- ggplot(data.frame(x=c(-12,12)), aes(x)) +
  stat_function(fun=dnorm, geom='area', fill='seagreen', colour='black', args=list(mean=-4, sd=2.5)) +
  stat_function(fun=dnorm, geom='area', fill='seagreen', colour='black', args=list(mean=-2, sd=2)) +
  stat_function(fun=dnorm, geom='area', fill='seagreen', colour='black', args=list(mean=0, sd=2)) +
  stat_function(fun=dnorm, geom='area', fill='seagreen', colour='black', args=list(mean=2, sd=2)) +
  stat_function(fun=dnorm, geom='area', fill='seagreen', colour='black', args=list(mean=4, sd=2.5)) +
  geom_point(aes(x, y), data=cl2, size=1, colour="gray96") +
  ylim(0,0.2) +
  theme_void() + 
  theme_transparent()

# make sticker
sticker(p, package="jpaRmd", 
        p_size = 60, p_family = "sans",
        s_x = 1, s_y = 0.85, s_width = 1.5, s_height = 0.9,
        h_fill = "gray45",h_color = "violetred4",
        filename="jpaRmd.png",dpi = 600)
