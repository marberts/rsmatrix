library(ggplot2)
library(hexSticker)

gg <- ggplot() +
  geom_hline(yintercept = 1:3) +
  geom_vline(xintercept = 1:4) + 
  theme_void()

sticker(gg,
        package = "rsmatrix",
        filename = "man/figures/logo.png",
        s_x = 1,
        s_width = 1,
        p_size = 18,
        p_family = "mono",
        p_color = "#FF5733",
        h_fill = "#80d280",
        h_color = "#FF5733")
