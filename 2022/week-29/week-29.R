library(tidyverse)
library(ggforce)
library(rnaturalearth)
library(sf)

select <- dplyr::select

# Read-in data
tuesdata <- tidytuesdayR::tt_load(2022, week = 29)
technology <- tuesdata$technology

# European Union
eu <-
  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
    "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# EU map
eu_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name %in% eu) %>% 
  # Add ISO3 for France
  mutate(iso_a3 = if_else(name == "France", "FRA", iso_a3))

# Create bounding box for EU
bbox_eu <- st_bbox(c(xmin = -10, xmax = 50,
                     ymin = 20, ymax = 80),
                   crs = st_crs(eu_map))

eu_map_cropped <- st_crop(eu_map, bbox_eu)

# EU in 2020
eu_energy <- technology %>% 
  filter(category == "Energy",
         str_detect(label, "(TWH)"),
         year == 2020,
         iso3c %in% eu_map_cropped$iso_a3) %>% 
  # extract electricity source
  mutate(source = if_else(str_detect(label, "^Electricity"),
                          str_extract(label, "(?<=from )(.+)(?= ())"),
                          label))

# Compute electricity production of renewable, nuclear & fossil fuel for each country
eu_energy_wide <- eu_energy %>% 
  select(-variable, -label) %>% 
  pivot_wider(names_from = source,
              values_from = value) %>% 
  group_by(iso3c) %>% 
  summarise(renewable = hydro + `other renewables` + solar + wind,
            nuclear = nuclear,
            fossil_fuel = coal + gas + oil,
            frac = (renewable + nuclear) / `Gross output of electric energy (TWH)`) %>% 
  ungroup()

# Long format for plotting
eu_energy_long <- eu_energy_wide %>% 
  pivot_longer(cols = renewable:fossil_fuel,
               names_to = "source",
               values_to = "value") %>% 
  mutate(
    source = factor(source),
    .after = iso3c
  )

# Find point coordinates for each country
eu_centroids <- eu_map_cropped %>% 
  st_centroid() %>% 
  select(iso_a3, geometry) %>% 
  mutate(long = map_dbl(geometry, ~ .x[1]),
         lat = map_dbl(geometry, ~ .x[2])) 

# Add point coordinates to EU countries
eu_energy_long <- eu_energy_long %>% 
  left_join(eu_centroids, by = c("iso3c" = "iso_a3"))

# Fix problematic centroids
eu_energy_long <- eu_energy_long %>% 
  # Centroid not centered well
  mutate(
    # move point coordinate left/right
    long = case_when(iso3c == "GRC" ~ long - 1,
                     iso3c == "SWE" ~ long - 1,
                     iso3c == "FIN" ~ long + 1,
                     TRUE ~ long),
    # move point coordinate up/down
    lat = case_when(iso3c == "MLT" ~ lat + 1.5,
                    iso3c == "HRV" ~ lat + 0.5,
                    TRUE ~ lat)
  ) %>% 
  # Overlap between countries
  mutate(
    # extend line segment left/right
    new_long = case_when(iso3c == "LVA" ~ long + 6,
                         iso3c == "BEL" ~ long - 6,
                         iso3c == "SVK" ~ long + 5,
                         TRUE ~ long),
    # extend line segment up/down
    new_lat = case_when(iso3c == "CYP" ~ lat + 2,
                        iso3c == "SVN" ~ lat - 3,
                        iso3c == "LUX" ~ lat - 8.5,
                        TRUE ~ lat)
  ) 

# Create new data frame for geom_segment
df_segment <-
  eu_energy_long %>% 
  mutate(
    # Shorten line segment by 1 unit
    xend = case_when(
      new_long > long ~ new_long - 1,
      new_long < long ~ new_long + 1,
      TRUE ~ new_long),
    # Shorten line segment by one unit
    yend = case_when(
      new_lat > lat ~ new_lat - 1,
      new_lat < lat ~ new_lat + 1,
      TRUE ~ new_lat
    )
  )

library(showtext)
sysfonts::font_add_google(name = "Oswald", family = "Bold 700")
sysfonts::font_add_google(name = "Roboto", family = "Regular 400")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

context <- 
  str_wrap(
    "The EU achieved its 2020 target to increase the share of renewable energy by 20% (1990 as baseline).
    In reducing the dependence on fossil fuels, there is greater reliance on the affordable nuclear energy,
    alongside renewables such as wind and solar power. However, some EU members are opposed to nuclear power 
    as they plan for nuclear phase-outs.", 
    width = 60)

eu_energy_long %>% 
  ggplot() +
  # Map
  geom_sf(data = eu_map_cropped,
          colour = "#f8f8ff",
          fill = "#e1e1e1") +
  # Donut charts
  geom_arc_bar(
    aes(x0 = new_long,
        y0 = new_lat,
        r0 = ifelse(source %in% c("renewable", "nuclear"), 0.9, 1),
        r = ifelse(source %in% c("renewable", "nuclear"), 1.1, 1),
        fill = source,
        amount = value),
    stat = "pie"
  ) +
  scale_fill_manual(name = "Source of electricity",
                    values = c("#5ea675", "#a65e8f", "gold"),
                    breaks = c("renewable", "nuclear"),
                    labels = c( "% of renewable energy", "% of nuclear energy")) +
  
  theme_void() +
  # Line segment to connecting adjusted donut charts
  geom_segment(
    data = df_segment,
    aes(
      x = long,
      y = lat,
      xend = xend,
      yend = yend
    )) +
  # Original lat/long of adjusted donut charts
  geom_point(data = filter(.data = eu_energy_long, new_lat != lat | new_long != long),
             aes(x = long,
                 y = lat),
             size = 1) +
  # Labels in donut charts
  geom_text(aes(x = new_long,
                y = new_lat,
                label = paste0(iso3c, 
                               "\n",
                               scales::label_percent(scale = 100, accuracy = 1)(frac))),
            size = 2.5, 
            family = "Regular 400",
            fontface = "bold") +
  # Paragraph 
  geom_text(aes(x = -11.5, y = 65, label = context),
            size = 4, 
            hjust = 0,
            stat = "unique", 
            family = "Regular 400",
            colour = "#262a77") +
  labs(title = "Share of renewable and nuclear energy in the European Union (EU) in 2020",
       subtitle = "A 'donut' represents the gross output of electric energy") +
  guides(fill = guide_legend(byrow = TRUE)) + 
  theme(
    aspect.ratio = 1,
    legend.position = c(0.225, 0.67),
    legend.title = element_text(family = "Regular 400", face = "bold.italic", size = 13, hjust = 0),
    legend.text = element_text(family = "Regular 400", size = 12, face = "bold"),
    legend.key.width = unit(4, "cm"),
    legend.key.height = unit(0.4, "cm"),
    text = element_text(colour = "#262a77"),
    plot.title = element_text(hjust = 0.5, vjust = -3, size = 20, family = "Bold 700", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16, vjust = -3, family = "Bold 700"),
    plot.background = element_rect(fill = "#d4ebf2")) 

# Save as png file
file <- here::here("2022/week-29/plot.png")
ggsave(file, width = 10, height = 10)
knitr::plot_crop(file) # remove borders 
