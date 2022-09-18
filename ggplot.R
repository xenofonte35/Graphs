library(ggplot2)
df <- data.frame(x = 1:3, y = c(9, 1, 9))
base <- ggplot(df, aes(x, y)) + ylim(0, 10)
base + geom_path(size = 10) + geom_path(size = 1, colour = "red")
base + geom_path(size = 10, linejoin = "mitre") + geom_path(size = 1, colour = "red")
#poligonos
shapes <- data.frame(shape = c(0:19, 22, 21, 24, 23, 20),
x = 0:24 %/% 5,y = -(0:24 %% 5))
ggplot(shapes, aes(x, y)) + 
geom_point(aes(shape = shape), size = 5, fill = "red") +
geom_text(aes(label = shape), hjust = 0, nudge_x = 0.15) +
scale_shape_identity() +expand_limits(x = 4.1) +theme_void()



  

