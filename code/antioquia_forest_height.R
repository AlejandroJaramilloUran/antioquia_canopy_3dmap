library(dplyr)
library(tidyverse)
library(sf)
library(classInt)
library(rayshader)
library(geodata)

#### DATA MANIPULATION ---------------------------------------------------------
# Load the Antioquia canopy height data
antioquia_fh <- terra::rast("Canopy_Antioquia.tif")

# I had problems with NA 
# So I converted it into -9999 in GEE (code in the .js file)
antioquia_fh[antioquia_fh == -9999] <- NA

# range of the data
antioquia_fh <- setMinMax(antioquia_fh)
minmax(antioquia_fh)

# decrease the resolution and leave the average pixel value 
antioquia_fh_lr <- antioquia_fh |> terra::aggregate(
  fact = 10,
  fun = "mean", 
  na.rm = T
  )

# df: 
antioquia_fh_df <- antioquia_fh_lr |> as.data.frame(xy = TRUE) |>
  rename(height = b1)
  
#### 2D PLOT -------------------------------------------------------------------
# breaks
breaks <- classIntervals(antioquia_fh_df$height, 
                         n = 6, style = "fisher")$brks

# colors:
cols <- c("black", "#ffd3af", "#fbe06e", "#6daa55", "#205544")

# Distribution of data
ggplot(antioquia_fh_df, aes(x = height)) +
  geom_histogram(
    bins = 30, fill = "grey40", color = "white") +
  labs(
    x        = "Canopy height (m)",
    y        = "Frequency"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    axis.title   = element_text(face = "bold"),
    panel.grid  = element_blank()
  )

# there is a little bias to the left, so it's better if the first colors are 
# more compressed

gradient <- colorRampPalette(
  cols, 
  bias = 1.8
  )(6)

# plot 

library(ggspatial)
p <- ggplot(antioquia_fh_df) +
  geom_raster(aes(x, y, fill = height)) +
  scale_fill_gradientn(name     = "Canopy height (m)",
                       colours  = gradient,
                       breaks   = round(breaks, 0), 
                       guide   = guide_colorbar(
                         barheight = unit(5, "cm"),  
                         title.hjust = 0.5)) +            
  coord_sf(crs = 4326, default = FALSE) +               
  theme_minimal() +
  theme(
    axis.text   = element_blank(),
    axis.title  = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank(), 
    legend.text = element_text(size = 10)
  ) +
  annotation_north_arrow(
    location = "tl",                
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  labs(title = "Canopy height variation in Antioquia")

p


#### 3D ------------------------------------------------------------------------

# dimensions
h <- nrow(antioquia_fh_lr)
w <- ncol(antioquia_fh_lr)

# 2D plot without north arrow
p2 <- ggplot(antioquia_fh_df) +
  geom_raster(aes(x, y, fill = height)) +
  scale_fill_gradientn(name     = "Canopy height (m)",
                       colours  = gradient,
                       breaks   = round(breaks, 0)) +
  coord_sf(crs = 4326, default = FALSE) +  
  theme_minimal() +
  theme(
    axis.text   = element_blank(),
    axis.title  = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank(), 
    legend.text = element_text(size = 7), 
    plot.background = element_rect(
      fill = "white", color = NA
    )
  )


# 3D
rayshader::plot_gg(
  ggobj     = p2 + theme(legend.position = "none"),
  width     = w / 500,
  height    = h / 500,
  pointcontract = 1,
  scale     = 300,
  solid     = FALSE,
  soliddepth= 0,
  shadow    = FALSE,     
  raytrace  = FALSE,     
  sunangle  = 45,
  zoom      = 0.6,
  phi       = 45,
  theta     = 0,
  multicore = FALSE, 
  background = "white"
)

# snapshot to the final result before the high quality image
rayshader::render_snapshot(
  filename     = "preview_antioquia.png",
  title_text   = "Preview Antioquia",
  title_size   = 20,
  title_offset = c(50, 50),
  vignette     = FALSE
)


# render high quality

rayshader:: render_highquality(
  filename = "antioquia_canopy_height.png", 
  preview = T, 
  interactive = F, 
  light = T, 
  lightdirection = c(
    45, 40, 45, 40
  ), 
  lightintensity = c(
    1000, 1500, 150, 100
  ), 
  lightaltitude = c(
    15, 15, 80, 80
  ), 
  ground_material = 
    rayrender::microfacet(
      roughness = .6
    ), 
  width = 4000, 
  height = 4000
)


# Finally, add the 2D legend:
library(cowplot)
legend <- get_legend(p)

legend_plot <- plot_grid(legend)
ggsave("legend_antioquia.png", legend_plot,
       width = 2, height = 6, bg = "transparent")

library(magick)
img_3d     <- image_read("antioquia_canopy_height.png")
img_legend <- image_read("legend_antioquia.png")

h_3d <- image_info(img_3d)$height
legend_height <- round(h_3d * 0.6)
img_legend_resized <- image_resize(img_legend, geometry = paste0("x", legend_height))

final <- image_composite(
  img_3d,
  img_legend_resized,
  operator = "over",
  gravity  = "northeast",     
  offset = "+10+100"
)
final


image_write(final, path = "ch_antioquia_3d.png", format = "png")

