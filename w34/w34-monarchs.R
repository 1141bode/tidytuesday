# 2024-08-20 week 34: English Monarchs and Marriages


# load libraries -----------------------------------

library(pacman)
pacman :: p_load('data.table', 'tidyverse', 'dplyr', 'glue', 'ggplot2',
                 'ggrepel', 'extrafont', 'ggtext')


# load data -----------------------------------------

english_monarchs_marriages_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')


# Data wrangling ------------------------------------

df <-  english_monarchs_marriages_df
df <- df |> as.data.table()

df <- df |>
  filter(
    str_detect(year_of_marriage, c("[–?]"), negate = TRUE),
    str_detect(consort_age, c("[–?]"), negate = TRUE),
    str_detect(king_age, c("[–?]"), negate = TRUE) # excludes unknown dates and ages 
  ) |>
  mutate(across(contains("age"), as.numeric)) # convert age to a numeric variable

df$difference <- abs(df$king_age - df$consort_age) # create column for the age gap

df <- df %>% arrange(desc(difference)) # reorder from largest to smallest age gap 

df_graph <- df[df$year_of_marriage < 1600,] # so we only have marriages until the end of Elizabethan era

df_graph$king_name <- reorder(df_graph$king_name, df_graph$difference)


# Styling -----------------------------------------------

font_add_google("Fraunces")
font_add_google("Candara")
showtext.auto()  

body_font <- "Candara"
title_font <- "Fraunces"

tg_col <- '#fafafa'
text_col <- 'black'


# Plot ------------------------------------------------------------

legend_df <- data.frame(
  person_type = c("Monarch", "Consort"),
  x = c(1, 1),
  y = c(1, 2)
)



p <- ggplot(df_graph, aes(y = reorder(king_name, difference))) + 
  # King point with specified color
  geom_point(aes(x = king_age), color = '#6818a5', size = 3, shape = 19) +
  # Consort point with specified color
  geom_point(aes(x = consort_age), color = '#ffb703', size = 3, shape = 19) + 
  # Connecting line
  geom_segment(aes(x = king_age, xend = consort_age, 
                   y = king_name, yend = king_name), 
               color = "grey50", size = 0.5, linetype = "solid") +
  geom_label_repel(
    aes(x = consort_age, y = king_name, label = NA),
    nudge_x = 0.25,  # Adjust the horizontal position
    nudge_y = 0.25,
    max.overlaps = Inf, 
    label.size = NA, 
    fill = alpha("#e4e4e3", .65),
    size = 3.75, 
    family = title_font
  ) +
  labs(
    title = "Age Gaps Between Monarchs and Consorts",
    x = "Age (Years)",
    y = "",
    subtitle = "Age Gaps in Marriages Until the end of Elizebethean Era",
    caption = "Source: <b>Tidy Tuesday W34 2024 | ianvisits.co.uk</b> | Graphic: <b>Ana Luisa Bodevan</b>"
  ) +
  geom_point(data = legend_df, aes(x = x, y = y, color = person_type), size = 3, shape = 19, show.legend = TRUE) +
  scale_color_manual(
    values = c("Monarch" = "#6818a5", "Consort" = "#ffb703"),
    breaks = c("Monarch", "Consort"),
    labels = c("Monarch", "Consort")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12), 
    axis.text.x = element_text(size = 14, face = "bold", family = title_font), 
    axis.text.y = element_text(size = 14, face = "bold", family = title_font),
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = title_font),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = title_font, color = "grey30"),
    plot.caption = element_markdown(margin = margin(t = 25), size = 8, family = body_font, hjust = 1),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
 p

 # Save the plot p as a PNG file
ggsave("w34", plot = p, width = 10, height = 8, dpi = 300)
 