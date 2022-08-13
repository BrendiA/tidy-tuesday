library(tidyverse)
library(lubridate)
library(ggforce)
library(ggflags)

tuesdata <- tidytuesdayR::tt_load(2022, week = 32)
wheels <- tuesdata$wheels %>% select(-`...1`)

# Tallest wheel for a given year
tall_wheels <- wheels %>% 
  mutate(open_year = lubridate::year(opened)) %>% 
  group_by(open_year) %>% 
  slice_max(height, n = 1) %>% 
  ungroup()

# Find local maxima of each height 
tall_wheels <- tall_wheels %>% 
  filter(cummax(height) == height) %>% 
  filter(name != "Space Eye",
         name != "Golden Gate Flyer")

# Update values for missing diameters and number of cabins
tall_wheels <- tall_wheels %>% 
  mutate(
    diameter = case_when(
      name == "The Dubai Eye" ~ 250 * 3.28, # https://www1.hdec.kr/en/newsroom/news_view.aspx?NewsSeq=424&NewsType=FUTURE&NewsListType=news_list#.YvOKrOxBy3I
      name == "Grande Roue de Paris" ~ 328.1, # https://pfranzme.wordpress.com/2009/10/30/la-grande-roue-de-paris/
      name == "Chicago Wheel" ~ 250, # https://chicagology.com/columbiaexpo/fair007/
      TRUE ~ diameter)
  ) %>% 
  mutate(
    height = if_else(
      name == "The Dubai Eye", 
      820, # https://www.thetravel.com/what-is-the-largest-ferris-wheel-in-the-world/
      height)
  ) %>% 
  # number of cabins
  mutate(number_of_cabins = case_when(
    name == "The Dubai Eye" ~ 48, # https://en.wikipedia.org/wiki/Ain_Dubai
    name == "Grande Roue de Paris" ~ 42, # https://www.observationwheeldirectory.com/wheels/roue-de-paris/
    TRUE ~ number_of_cabins 
  )) %>% 
  mutate(closing_year = year(closed)) %>% 
  mutate(closing_year = case_when(
    name == "Great Wheel" ~ 1907, # http://www.skyscrapernews.com/buildings.php?id=4731
    name == "Grande Roue de Paris" ~ 1920, # https://en.wikipedia.org/wiki/Grande_Roue_de_Paris#cite_note-GRdP1920-3
    TRUE ~ closing_year
  )) %>% 
  mutate(status = if_else(name %in% c("High Roller", "The Dubai Eye"), "Operating", status)) %>%
  mutate(open_year = if_else(name == "The Dubai Eye", 2021, open_year)) %>% 
  arrange(height)

# obtain iso2c country code for plotting with ggflags
tall_wheels <- tall_wheels %>% 
  mutate(country_code = countrycode::countrycode(tall_wheels$country, "country.name", "iso2c"), 
         country_code = if_else(country == "Dubai", "ae", country_code),
         country_code = str_to_lower(country_code), # compatible with ggflags syntax
         ) %>% 
  # compute radius of circle
  mutate(radius = diameter / 2,
         name = fct_reorder(name, height))

name_appender <- function(name) {
  paste0(name, 
         "\n", 
         tall_wheels$open_year,
         "\n(",
         ifelse(!is.na(tall_wheels$closing_year),
                paste("disassembled in", tall_wheels$closing_year),
                tall_wheels$status
         ),
         ")")
}

library(showtext)
sysfonts::font_add_google(name = "Playfair Display", family = "Bold 700 Italic")
sysfonts::font_add_google(name = "Roboto Slab", family = "Regular 400")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Carts for Ferris wheel
carts <- tall_wheels %>%
  group_by(name) %>%
  summarise(
    cart = seq_len(number_of_cabins / 2), # one cabin seats 2 passengers
    cart_x = cos(cart/number_of_cabins * pi * 4) * radius ,
    cart_y = (sin(cart/number_of_cabins * pi * 4) * radius) + height - radius,
    start = pi / 2,
    end = start + pi,
    group = factor(cart %% 4), # four colours for the cabins
    scale = radius / max(cart), # scale cart size based on different radii
    height = height,
    .groups = "drop"
  )

tall_wheels %>% 
  ggplot() +
  # Ferris wheel circle
  geom_circle(aes(x0 = 0,
                  y0 = height - radius, # center of circle
                  r = radius),
              size = 0.7) +
  # Bottom of cart
  geom_arc_bar(data = carts,
               aes(x0 = cart_x,
                   y0 = cart_y - scale,
                   r0 = 0,
                   r = scale * 1.2,
                   start = start,
                   end = end,
                   fill = group)) +
  # top of cart
  geom_arc_bar(data = carts,
               aes(x0 = cart_x,
                   y0 = cart_y + scale,
                   r0 = 0,
                   r = - scale * 0.8,
                   start = start,
                   end = end,
                   fill = group
               )) +
  # left leg
  geom_segment(aes(x = - (height - radius) / 2, xend = 0,
                   yend = height - radius, y = -50),
               size = 1) +
  # right leg
  geom_segment(aes(x = (height - radius) / 2, xend = 0,
                   yend = height - radius, y = -50),
               size = 1) +
  # platform
  geom_hline(yintercept = -50, 
             size = 1,
             colour = "black") +
  # height label
  ggtext::geom_richtext(aes(x = radius * 1.3 + 70,
                            y = height - radius,
                            label = paste(round(height), "ft")),
                        angle = 90,
                        size = 3.5,
                        family = "Regular 400",
                        fill = "#a2b7a4") +
  # height arrows
  geom_segment(aes(x = radius * 1.3,
                   xend = radius * 1.3,
                   y = 0,
                   yend = height),
               arrow = arrow(ends = 'both', 
                             type = "closed",
                             length = unit(0.1, "inches"))) +
  ggflags::geom_flag(aes(country = country_code,
                         x = 0,
                         y = height - radius),
                     size = 8) +
  facet_wrap(~name, 
             strip.position = "bottom",
             labeller = as_labeller(name_appender),
             nrow = 2) +
  colorspace::scale_fill_discrete_qualitative(palette = "Dynamic") +
  labs(x = "",
       y = "",
       title = "The battle for height since the first Ferris wheel in 1893",
       subtitle = "each cabin carries two passengers",
       caption = "Source: {ferriswheel} R package | #tidytuesday 2022 week 32"
  ) +
  scale_y_continuous(breaks = seq(0, 800, 200)) +
  theme(
    text = element_text(family = "Regular 400"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing.x = unit(-1, "lines"),
    plot.title = element_text(hjust = 0.5, size = 24, vjust = -5, family = "Bold 700 Italic", face = "bold.italic"),
    plot.margin = margin(-15, 30, -10, 0),
    plot.subtitle = element_text(hjust = 0.5, size = 16, vjust = -8, family = "Bold 700 Italic", face = "italic"),
    plot.caption = element_text(vjust = 8),
    plot.background = element_rect("#d2e6d4"),
    panel.background = element_rect("#d2e6d4"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", colour = "white"),
    strip.background = element_rect(fill = "#313a43", colour = "white"),
    strip.text = element_text(colour = "white", face = "bold", hjust = 0.4)
  ) 

ggsave("plot.png", width = 14, height = 8, units = "in")

