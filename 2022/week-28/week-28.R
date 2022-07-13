library(tidyverse)
library(colorspace)
library(lubridate)
library(patchwork)
library(showtext)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

# Read-in data and clean names
flights <- tuesdata$flights %>% 
  janitor::clean_names() 

# Generate fonts for plot
sysfonts::font_add_google(name = "Montserrat", family = "Black 900")
sysfonts::font_add_google(name = "Lato", family = "Black 900 Italic")
sysfonts::font_add_google(name = "Lato", family = "Regular 400")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Compute average flight activities across EU
total_flights <- flights %>%
  group_by(flt_date) %>%
  summarise(total_dep = sum(flt_dep_1, na.rm = TRUE),
            .groups = "drop")

# Add missing dates (e.g. dates in the future) from 2022-01-06
total_flights <- total_flights %>% 
  complete(flt_date = seq.Date(from = as_date("2022-01-01"),
                               to = as_date("2022-12-31"),
                               by = "day",
                               fill = list(value = NA)))

# Add date components
total_flights <- total_flights %>%
  mutate(year = year(flt_date), 
         week_of_year = week(flt_date),
         month = month(flt_date, label = TRUE, abbr = TRUE),
         day = day(flt_date)
  )

# Function prepares data for plotting based on a given year
create_df <- function(given_year) {
  
  # Extract first day of the week for the year
  first_day_of_year <-
    wday(as.Date(paste(given_year, "01", "01", sep = "-")),
         week_start = 1)
  
  total_flights %>%
    filter(year == given_year) %>%
    mutate(day_of_week = wday(flt_date,
                              label = TRUE,
                              week_start = first_day_of_year)) %>%
    # Reverse order so 1 starts first in the calendar
    mutate(day_of_week = forcats::fct_rev(day_of_week)) %>% 
    group_by(month) %>%
    mutate(week_of_month = dense_rank(week_of_year)) %>%
    ungroup()
}

# Function plots heatmap based on the data generated
plot_heatmap <- function(df){
  
  # Year of data frame
  given_year <- unique(df$year)
  
  ggplot(data = df,
         aes(x = week_of_month, y = day_of_week)) +
    # Heat map
    geom_tile(aes(fill = total_dep)) +
    # Calendar days
    geom_text(aes(label = day),
              size = 2,
              colour = "white") +
    colorspace::scale_fill_continuous_sequential(name = "Number of departures",
                                                 palette = "Viridis",
                                                 na.value = "grey60", # Fill missing dates
                                                 limits = c(0, 30000),
                                                 breaks = seq(0, 30000, 10000),
                                                 oob = scales::squish,
                                                 label = scales::label_comma(),
                                                 c2 = 60, # increase
                                                 l1 = 10) + # decrease
    facet_wrap(~ month,
               nrow = 1) +
    theme_minimal() +
    # coord_fixed(ratio = 1) +
    labs(title = given_year) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.width = unit(1.5, "cm"),
      legend.text = element_text(family = "Regular 400"),
      legend.title = element_text(vjust = 0.8, family = "Regular 400"),
      panel.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, family = "Black 900 Italic", size = 12, face = "bold"),
      strip.text = element_text(family = "Regular 400", size = 8)
    ) 
}

# Create heat map for each year
plot_list <- 
  purrr::map(unique(sort(total_flights$year)),
             ~ create_df(.x) %>% 
               plot_heatmap())

# Subtitle 
sub <-
  str_wrap(
    "The aviation industry took a toll on the Covid-19 pandemic. 
    There are signs of recovery, with commercial flights in the EU increasing from mid-June 2021. 
    Will the rest of 2022 see a total number of departures close to pre-pandemic levels?",
    width = 130
  )

plot_list[[3]] + plot_list[[4]] + 
  plot_list[[5]] + plot_list[[6]] +
  plot_list[[7]] +
  plot_layout(guides = "collect",
              ncol = 1,
              height = 0.5) &
  plot_annotation(
    title = "Commercial flight departures in the EU",
    subtitle = sub,
    theme = theme(
      legend.position = "bottom",
      plot.title = element_text(family = "Black 900", hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(family = "Regular 400", hjust = 0.5, size = 14),
      plot.background = element_rect(fill = "#f0f8ff")) 
  )

# ggsave("week-28-plot.png", width = 13, height = 11, units = "in")


