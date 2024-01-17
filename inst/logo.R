library(ggplot2)
library(hexSticker)

gg <- ggplot() +
  geom_line(aes(x, y), data = data.frame(x = c(0, 1.5, 3), y = c(2, 3, 2))) +
  geom_hline(yintercept = 0:2) +
  geom_vline(xintercept = 0:3) + 
  theme_void()

sticker(gg,
        package = "rsmatrix",
        filename = "man/figures/logo.png",
        s_x = 1,
        s_width = 1,
        p_size = 18,
        p_family = "mono",
        p_color = "#c42300",
        h_fill = "#80d280",
        h_color = "#c42300")
