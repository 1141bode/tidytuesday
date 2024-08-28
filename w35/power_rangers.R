### 2024-08-27 | W35 The Power Rangers Franchise

## 1. Load packages and setup --------------------------------------------------

pacman::p_load(
  tidyverse,    
  prismatic,      
  Cairo,      
  skimr,        
  scales,       
  stringr,
  extrafont
)  

### 1.1 Figure size and resolution 

camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  7.77,
  height =  8,
  units  = "in",
  dpi    = 320
)

## 2. Reading the data ---------------------------------------------------------

power_rangers_seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')

## 3. Examining the data -------------------------------------------------------

skim(power_rangers_seasons)
glimpse(power_rangers_seasons)

## 4. Manipulating the data ----------------------------------------------------

df <- power_rangers_seasons %>% arrange(desc(IMDB_rating)) # sort by IMDB rating, descending

## 5. Visualization ------------------------------------------------------------

### 5.1 Aesthetics 

bkg_colour <- "#fffff2"
title_colour <- "#800020"
subtitle_colour <- "gray40"
caption_colour <- "gray40"
text_colourour <- "gray20"

### 5.2 Texts 

font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

tt <- str_glue("#TidyTuesday: { 2024 } Week { 35 } &bull; Source: Power Rangers: Seasons and episodes data<br>")
title_text <- str_glue("It's Morphing Time! Power Rangers Seasons by Rating")
subtitle_text <- str_glue(
  "Final season ratings across 27 Power Rangers seasons, according to IMDB.<br>")
caption_text <- str_glue("{tt} &bull; Ana Luisa Bodevan & 1141bode.github.io")

### 5.3 Theme

theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  plot.background       = element_rect(fill = bkg_colour, color = bkg_colour),
  panel.background      = element_rect(fill = bkg_colour, color = bkg_colour),
  plot.margin           = margin(t = 20, r = 20, b = 20, l = 20),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.8), color = text_colour, family = "text"),
  axis.line.x           = element_line(color = "gray40", linewidth = .15),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
  
  strip.text            = element_textbox(size     = rel(1),
                                          face     = 'bold',
                                          color    = text_colour,
                                          hjust    = 0.5,
                                          halign   = 0.5,
                                          r        = unit(5, "pt"),
                                          width    = unit(5.5, "npc"),
                                          padding  = margin(3, 0, 3, 0),
                                          margin   = margin(3, 3, 3, 3),
                                          fill     = "transparent"),
  panel.spacing       = unit(1, 'lines')
)  

### 5.4 Plot

p <- ggplot(df, aes(x = reorder(season_title, IMDB_rating), y = IMDB_rating)) +
  geom_segment(aes(xend = season_title, yend = 0), color = "grey40", linewidth = 1) + # Lollipop sticks
  geom_point(color = "#ee4266", size = 3) + # Lollipop tips
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Season Title",
    y = "IMDB Rating"
  ) +
  coord_flip() + # Flip coordinates for better readability
  theme(
    axis.title.x = element_text(size = rel(1.1), color = text_colour, family = "text", face = "bold"),
    axis.title.y = element_text(size = rel(1.1), color = text_colour, family = "text", face = "bold"),
    axis.text = element_text(size = rel(0.8), color = text_colour, family = "text"),
    plot.title = element_text(size = rel(2.7), family = "title", color = title_colour, face = "bold"),
    plot.subtitle = element_text(size = rel(1.5), family = "subtitle", color = subtitle_colour),
    plot.caption = element_text(size = rel(1.2), family = "caption", color = caption_colour),
    plot.background = element_rect(fill = bkg_colour, color = bkg_colour),
    panel.background = element_rect(fill = bkg_colour, color = bkg_colour),
    strip.background = element_rect(fill = bkg_colour, color = bkg_colour),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray')
  )

print(p)

ggsave(
  filename = here::here("temp_plots", "power_rangers_seasons_rating.png"), # Path to save the plot
  plot = p,  # Plot object to save
  device = "png",  # File format
  width = 7.77,  # Width in inches
  height = 8,  # Height in inches
  units = "in",  # Units for width and height
  dpi = 320  # Resolution in DPI
)
