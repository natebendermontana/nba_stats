# Dynamic player report
library(ggthemes)
library(tidyverse)
library(ggforce)
library(tidytext)
library(gt)
library(here)
library(cowplot)

# load data
nba_pbp_raw <- read.csv(here("data", "nba_pbp_raw.csv"))

# nba_pbp_raw %>% 
#   distinct(type_id, type_text)

nba_pbp_raw %>%
  filter(type_id %in% c(92:153)) %>% 
  summarise(
    min_x = min(coordinate_x_raw, na.rm = TRUE),
    max_x = max(coordinate_x_raw, na.rm = TRUE),
    min_y = min(coordinate_y_raw, na.rm = TRUE),
    max_y = max(coordinate_y_raw, na.rm = TRUE)
  )

pbp_norm <- nba_pbp_raw %>%
  mutate(
    norm_x = (coordinate_x_raw - 25) * 10,  # Center x around 0 (-250 to 250)
    norm_y = coordinate_y_raw * 10          # Scale y to fit 0-470 range
  )

# Creating Court 

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

# Court Dimenons & lines
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

# Court themes
court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray20',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray20"
  )
)

# Function to create court based on given dimensions
plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  court_points <- court_points
  
  # Final plot creation
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray20', color = 'gray20'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

final_player_data <- data.frame(
  x = runif(100, -25, 25),  # Simulated x-coordinates
  y = runif(100, 0, 47),    # Simulated y-coordinates
  isShotMade = sample(c("TRUE", "FALSE"), 100, replace = TRUE)
)

p1 <- plot_court(court_themes$ppt, use_short_three = FALSE) +
  geom_point(data = final_player_data, 
             aes(x = x, y = y, color = isShotMade, fill = isShotMade), 
             size = 3, shape = 21, stroke = 0.5) +  
  #scale_color_manual(values = c("green4", "red3"), 
   #                  breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
  scale_fill_manual(values = c("green2", "gray20"), 
                    breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
  scale_x_continuous(limits = c(-27.5, 27.5)) +
  scale_y_continuous(limits = c(0, 45)) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", vjust = -4),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", vjust = -8),
    #legend.position.inside = c(0.5, 0.85),
    #legend.direction = "horizontal",
    #legend.title = element_blank(),
    #legend.text = element_text(hjust = 0.5, size = 10, face = "bold", colour = "white"),
    plot.caption = element_text(hjust = 0.5, size = 6, face = "bold", colour = "lightgrey", vjust = 8),
    panel.background = element_rect(fill = "gray20", color = NA),  
    plot.background = element_rect(fill = "gray20", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

# Final seamless plot with background
ggdraw(p1) + 
  theme(plot.background = element_rect(fill = "gray20", color = NA))
