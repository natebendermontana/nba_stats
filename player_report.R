# Dynamic player report
require(ggplot2)
library(ggthemes)
library(tidyverse)
library(ggforce)
library(tidytext)
library(gt)
library(here)
library(plyr)

# load data
nba_pbp_raw <- read.csv(here::here("data", "nba_pbp_raw.csv"))
nba_pbp_lastseason <- read.csv(here::here("data", "nba_pbp_lastseason.csv"))

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

######## Functions ####
require(ggplot2)
require(plyr)

#' Bin data into hexagons (2d).
#'
#' @param x a numeric vector of x positions
#' @param y a numeric vector of y positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param height height of each hexagon, if \code{NULL} computed from ybins
#' @param width width of each hexagon, if \code{NULL} computed from ybins
#' @param xbins number of horizontal bins, if \code{width} unspecified
#' @param ybins number of vertical bins, if \code{height} unspecified
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @export
#' @seealso \code{\link{hex_pos}} for algorithm that finds hexagon center
#'   closest to each point and \code{\link{hex_coord}} that generates
#'   coordinates of each hexagon.
#' @return A data frame with columns \code{x}, \code{y} and \code{freq},
#'   and attributes \code{width} and \code{height}.
#' @S3method plot bin_hex
#' @examples
#' plot(hex_bin(runif(1e4), runif(1e4)))
#' plot(hex_bin(rnorm(1e4), rnorm(1e4)))
#' 
#' data(baseball, package = "plyr")
#' bin <- hex_bin(baseball$g, baseball$ab)


hex_bin <- function(x, y, weight = NULL, var4 = NULL, width = NULL, height = NULL, 
                    xbins = 20, ybins = 20, frequency.to.area = FALSE, na.rm = FALSE, 
                    hmin = 0, hmax = 1, xlim = NULL, ylim = NULL, ...) {
  if(hmax > 1) warning("hmax > 1 is likely to result in some hexagon overplotting")
  
  cleaned <- clean_xy(x, y, weight, var4, xlim=xlim, ylim=ylim)
  
  if (is.null(xlim)) xlim <- range(cleaned$x)
  if (is.null(ylim)) ylim <- range(cleaned$y)
  if (is.null(width))  width  <- diff(xlim) / xbins
  if (is.null(height)) height <- diff(ylim) / ybins
  height <- height * sqrt(3)
  
  pos <- hex_pos(cleaned$x, cleaned$y, width, height)
  cleaned$x <- pos[,1]; cleaned$y <- pos[,2]
  
  # bin values by hexagon
  binned <- count(cleaned, c("x", "y"), "weight")
  
  var4_sum <- aggregate(cleaned$var4, by=list(cleaned$x, cleaned$y), FUN=mean)
  names(var4_sum) = c('x','y','var4')
  # cols must match order of binned
  binned$var4 <- var4_sum$var4[match(paste(binned$x, binned$y), paste(var4_sum$x, var4_sum$y))]
  
  # swap size/frequency variables to map respectively to colour/size
  names(binned) <- c('x','y','col','size')
  if(frequency.to.area) binned <- transform(binned, size=col, col=size)
  
  # 'freq' field now definitely maps to hex area
  if(!is.null(var4) & min(binned$size)<0) warning("size vector cannot include negative values")
  
  # scale size variable and min-max parameters from hexagon area to side
  binned$size = hex_side(binned$size)
  hmax_a = hex_side(hmax)/hex_side(1)
  hmin_a = hex_side(hmin)/hex_side(1)
  hrange = hmax_a - hmin_a
  
  # normalise and rescale to custom min-max parameters
  binned$size <- binned$size * hrange / max(binned$size) + hmin_a
  
  structure(
    binned,
    width = width,
    height = height,
    class = c("bin_hex", "data.frame")
  )
}


clean_xy <- function(x, y, weight=NULL, var4=NULL, na.rm=TRUE, xlim=NULL, ylim=NULL) {
  # If !na.rm, remove missing values with a warning.  
  # Otherwise just remove them
  missing <- !is.finite(x) | !is.finite(y)
  nmissing <- sum(missing)
  
  if (na.rm && nmissing > 0) {
    warning("Removing ", nmissing, " missing values")
  }
  
  # Check weights, and throw out missing values and zero-weight observations
  if (is.null(weight)) {
    weight <- rep.int(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }
  
  # Check sizes, and throw out missing values and zero-weight observations
  if (is.null(var4)) {
    var4 <- rep.int(1, length(x))
  } else {
    var4[is.na(var4)] <- 0
  }
  
  ok <- !missing & weight > 0 & var4 > 0
  if (all(ok)) { data.frame(x = x, y = y, weight = weight, var4 = var4)
  } else {
    x <- x[ok]
    y <- y[ok]
    var4 <- var4[ok]
    weight <- weight[ok]
  }
  out <- data.frame(x = x, y = y, weight = weight, var4 = var4)
  if (is.null(xlim)) xlim <- range(out$x); if (is.null(ylim)) ylim <- range(out$y)
  out[out$x >= min(xlim) & out$x <= max(xlim) & out$y >= min(ylim) & out$y <= max(ylim),]
}


plot.bin_hex <- function(x, ...) {
  if (!require("scales")) {
    message("Scales package required for plotting 2d densities")
    return()
  }
  
  if(all(x$col == x$col[1])) { # no variation in colour variable
    col <- rep("black", nrow(x))
  } else col <- cscale(x$col, seq_gradient_pal(low = "grey70", high = "red"))
  
  hexes <- hex_coord(x=x$x, y=x$y, width=attr(x, "width"), height=attr(x, "height"), 
                     size=x$size, ...)
  
  plot(hexes[,1], hexes[,2], type = "n", ...)
  polygon(hexes, col = col, border = NA)
}


# Binning algorithms are available for various lattices in dimensions 2-8
# (Conway and Sloane 1982). The following subroutine is a fast FORTRAN
# implementation of hexagonal binning. The key observation is that hexagon
# centers fall on two staggered lattices whose cells are rectangles. Presuming
# the long side of the rectangle is in the y direction, dividing the y
# coordinates by square root (3) [SQRT(3)] makes the cells square. Thus the
# algorithm uses two lattices with square cells. The first lattice has points
# at the integers with [0, 0] as the lower left value. The second lattice is
# shifted so that the lower left value is at [.5 , .5]. The x and y vectors
# are scaled into [0, SIZE] and [0, SIZE / SQRT(3)], respectively. SIZE
# determines the portions of the lattices that are used. For each data point,
# binning consists of finding one candidate lattice point from each lattice
# and then selecting the nearest of the two candidates.

#' Find centre of closest hexagon.
#'
#' @param x numeric x position
#' @param y numeric y position
#' @param width of hexagon
#' @param height of hexagon
#' @return matrix giving position of closest hexagon center
#' @keywords internal
#' @export
#' @examples
#' x <- runif(1e4)
#' y <- runif(1e4)
#' res <- hex_pos(x, y, 0.5, 0.5)
#' plot(x, y, type = "n")
#' segments(x, y, res[, 1], res[, 2], col = "grey80")
#' points(unique(res), pch = 20, cex = 2)


hex_pos <- function(x, y, width, height) {
  height <- height / sqrt(3)
  
  minx <- min(x, na.rm = TRUE)
  miny <- min(y, na.rm = TRUE)
  
  # Scale to [0, nrows/ncols]
  sx <- (x - minx) / width
  sy <- (y - miny) / height
  
  # Find closest center: [0, 0] or [0.5, 0.5]?
  fx <- round(sx)
  fy <- round(sy)
  
  dist_0 <- 3 * (sx - fx)^2 + (sy - fy)^2
  dist_1 <- 3 * (sx - fx + 0.5)^2 + (sy - fy + 0.5)^2
  dist_2 <- 3 * (sx - fx + 0.5)^2 + (sy - fy - 0.5)^2
  dist_3 <- 3 * (sx - fx - 0.5)^2 + (sy - fy + 0.5)^2
  dist_4 <- 3 * (sx - fx - 0.5)^2 + (sy - fy - 0.5)^2
  dist_smallest <- pmin(dist_0, dist_1, dist_2, dist_3, dist_4)
  
  x_offset <- rep(0, length(x))
  x_offset[dist_smallest == dist_1] <- +0.5
  x_offset[dist_smallest == dist_2] <- +0.5
  x_offset[dist_smallest == dist_3] <- -0.5
  x_offset[dist_smallest == dist_4] <- -0.5
  
  y_offset <- rep(0, length(y))
  y_offset[dist_smallest == dist_1] <- +0.5
  y_offset[dist_smallest == dist_2] <- -0.5
  y_offset[dist_smallest == dist_3] <- +0.5
  y_offset[dist_smallest == dist_4] <- -0.5
  
  # Transform back to original coordinates
  cbind(x = (fx - x_offset) * width + minx, y = (fy - y_offset) * height + miny)  
}

#' Generate hexagon coordinates.
#'
#' Long axis is horizontal. Edges clock-wise from far-left, separated by
#' row of missing values.
#'
#' @param x horizontal position of center
#' @param y vertical position of center
#' @param width hex width
#' @param height hex height
#' @export
#' @keywords internal
#' @return A two column matrix with 7 times as many rows as input.
#' @examples
#' x <- runif(1000)
#' y <- runif(1000)
#' res <- unique(hex_pos(x, y, 0.5, 0.5))
#' hexes <- hex_coord(res[, 1], res[, 2], 0.6, 0.5)
#' 
#' hexes <- hex_coord(res[, 1], res[, 2], rnorm(1000,.5,.3), rnorm(1000,.5,.3))
#' 
#' plot(hexes, type = "n")
#' polygon(hexes)
#' points(res)


hex_coord <- function(x, y, width, height, size = 1) {
  dx <- size * width / 6
  dy <- size * height / 2 / sqrt(3)
  
  hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx, NA)
  hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy, NA)
  
  cbind(as.vector(hex_x), as.vector(hex_y))
}


hex_coord_df <- function(x, y, width, height, size = 1) {
  # like hex_coord but returns a dataframe of vertices grouped by an id variable
  dx <- size * width / 6
  dy <- size * height / 2 / sqrt(3)
  
  hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx)
  hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy)
  id    <- rep(1:length(x), each=6)
  
  data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
}


## Functions for calculating hexagon geometries

# hexagon side from area
hex_side = function(area) sqrt(2 * area / (sqrt(3)*3))

# hexagon area from side (not used)
hex_area = function(side) side^2 * sqrt(3) * 3/2

# quick function for gg-plotting with 1 line (limited functionality)
qplothex = function(x, y, var4 = NULL, f.to.a = FALSE, ...){
  bin = hex_bin(x=x, y=y, var4=var4, frequency.to.area=f.to.a, ...)
  hexes = hex_coord_df(x=bin$x, y=bin$y, width=attr(bin, "width"), height=attr(bin, "height"), size=bin$size)
  hexes$col = rep(bin$col, each=6)
  ggplot(hexes, aes(x=x, y=y)) + geom_polygon(aes(fill=col, group=id))
}

# Creating Court ####
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles),
                y = center[2] + radius * sin(angles)))
}

# Court dimensions & lines
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
    )
}

p1 <- plot_court(court_themes$ppt, use_short_three = FALSE) +
  # geom_point(data = final_player_data, 
  #            aes(x = x, y = y, color = isShotMade, fill = isShotMade), 
  #            size = 3, shape = 21, stroke = 0.5) +  
  #scale_color_manual(values = c("green4", "red3"), 
  #                  breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
  #scale_fill_manual(values = c("green2", "gray20"), 
  #                  breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
  #scale_fill_discrete(guide = "none") +  # Ensure no fill legend is shown for geom_hex
  
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
    panel.background = element_rect(fill = "gray10", color = NA),  
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
p1

#-------------------------------------------------------------------------------
### Hexagons #####
library(hexbin)
detach("package:plyr", unload = TRUE)
library(dplyr)

stephcurry_2324 <- nba_pbp_lastseason %>% 
  filter(type_id %in% c(92:96, 104:153), 
         athlete_id_1 == "3975") %>% 
  select(athlete_id_1, type_id, text, scoring_play, coordinate_x_raw, coordinate_y_raw)

leaguewide_2324 <- nba_pbp_lastseason %>% 
  filter(type_id %in% c(92:96, 104:153)) %>%   
  mutate(is_make = as.numeric(scoring_play)) %>%
  select(athlete_id_1, type_id, text, scoring_play, is_make, coordinate_x_raw, coordinate_y_raw)

hexbin_obj <- hexbin(
  x = leaguewide_2324$coordinate_x_raw,
  y = leaguewide_2324$coordinate_y_raw,
  xbins = 70,
  IDs = TRUE
)

league_grouped <- leaguewide_2324 %>%
  mutate(hex_id = hexbin_obj@cID)%>%
  group_by(hex_id)

league_hex_summary <- league_grouped %>%
  summarise(
    makes = sum(as.numeric(scoring_play), na.rm = TRUE),
    attempts = n(),                                       
    fg_pct_league = makes / attempts                      
  )

hexbin_obj <- hexbin(
  x = stephcurry_2324$coordinate_x_raw,
  y = stephcurry_2324$coordinate_y_raw,
  xbins = 70,
  IDs = TRUE
)

hex_centers <- tibble(
  hex_id = hexbin_obj@cell,   # Hexagon IDs
  x = hexbin_obj@xcm,         # X-coordinates of hexagon centers
  y = hexbin_obj@ycm          # Y-coordinates of hexagon centers
)

player_grouped <- stephcurry_2324 %>%
  mutate(hex_id = hexbin_obj@cID)%>%
  group_by(hex_id)

player_hex_summary <- player_grouped %>%
  summarise(
    makes = sum(as.numeric(scoring_play), na.rm = TRUE),
    attempts = n(),
    fg_pct_player = makes / attempts
  ) %>%
  left_join(hex_centers, by = "hex_id") 

comparison_data <- player_hex_summary %>%
  left_join(league_hex_summary, by = "hex_id") %>%
  mutate(fg_diff = fg_pct_player - fg_pct_league)
















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

custom_colors <- c("#3878b8", "#f5f7f9", "#fffbce", "#f7ae5d", "#db1e16")

library(plyr)
my_bin <- hex_bin(comparison_data$x, comparison_data$y, var4=comparison_data$fg_pct_player, frequency.to.area=T)
my_hexes = hex_coord_df(x=my_bin$x, y=my_bin$y, width=attr(my_bin, "width"), height=attr(my_bin, "height"), size=my_bin$size)
my_hexes$fg_pct_player = rep(my_bin$col, each=6)
names(my_hexes)[1:2] = c('x','y')

my_hexes <- my_hexes %>%
  mutate(x = x - 25)  # Shift x by 25 to center around 0

ggplot(my_hexes, aes(x=x, y=y)) + geom_polygon(aes(fill=fg_pct_player, group=id)) +
  geom_path(
    data = court_points,  # This should be the court line data from plot_court()
    aes(x = x, y = y, group = desc),  # Ensure grouping for lines
    color = "floralwhite", linewidth = 0.7
  ) +
  scale_fill_gradientn(
    colors = custom_colors,    # Apply custom color palette
   # name = "FG%",              # Legend title
    limits = c(0, 1),          # FG% should range between 0 and 1
    labels = scales::percent   # Display percentages
  ) +
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
    panel.background = element_rect(fill = "gray10", color = NA),  
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
  

