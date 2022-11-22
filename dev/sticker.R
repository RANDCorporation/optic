library(hexSticker)

# Glass color:
# #4E1594

sticker("./dev/sunglass.png", package = "optic",
        s_x = 1, s_y = 1.2, 
        s_width = 0.8, s_height = 0.8,
        p_y = 0.7,
        p_size = 36,
        p_color = "#482475",dpi = 300, 
        #p_family = "Noto Sans",  
        h_fill = "white",h_color = "#482475",
        filename = file.path("inst/figures/optic.png"))
