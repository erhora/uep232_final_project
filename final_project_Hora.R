# Loading Packages --------------------------------------------------------
library(tidyverse)
library(sf)
library(gganimate)
library(units)
library(patchwork)
library(png)
library(readxl)
library(lubridate)

theme_set(theme_minimal())


# Parallel Processing ----
# This only makes the code go faster if possible.
# Not having this will not change the results.
library(parallel)
library(foreach)
library(doParallel)
# Careful with this- if there's not enough memory,
# your computer can crash.
all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores - 1)




# Loading Data / General Processing ---------------------------------------
# Removing States / Territories not in the Continental US ----
remove_regions <- c("AS", "GU", "PR", "VI", "MP", "AK", "HI")
fp_codes <- readxl::read_excel("data/statefp/codes_conversion.xlsx")




# Tornado Paths ----
# https://www.spc.noaa.gov/gis/svrgis/
# Description of variables:
# https://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf
tornado_paths <- st_read("data/1950-2021-torn-aspath/1950-2021-torn-aspath.shp") %>% 
  janitor::clean_names() %>% 
  filter(
    !(st %in% remove_regions)
  ) %>% 
  # Albers Conic Projection
  st_transform(crs = 5070)


tornado_paths_grey <- tornado_paths %>% 
  select(-yr)


tornado_median_width <- tornado_paths %>% 
  as_tibble() %>%
  group_by(yr) %>% 
  summarize(
    median_wid = median(wid)
  ) %>% 
  arrange(desc(median_wid))


tornado_paths <- left_join(tornado_paths, tornado_median_width, by = "yr") %>% 
  # Albers Conic Projection
  st_transform(crs = 5070)




# USA Map ----
# Statewide
usa_map_state <- st_read("data/cb_2018_us_state/cb_2018_us_state_500k.shp") %>% 
  janitor::clean_names() %>% 
  left_join(fp_codes) %>% 
  filter(
    !is.na(postal_code),
    !(postal_code %in% remove_regions)
  ) %>% 
  # Albers Conic projection
  st_transform(crs = 5070)


# Countywide
usa_map_counties <- st_read("data/cb_2018_us_county/cb_2018_us_county_500k.shp") %>% 
  janitor::clean_names() %>% 
  mutate(
    statefp = as.numeric(statefp)
  ) %>% 
  rename(
    fips = statefp,
    county_name = name
  ) %>% 
  left_join(fp_codes) %>% 
  filter(
    !is.na(postal_code),
    !(postal_code %in% remove_regions)
  ) %>% 
  # Albers Conic Projection
  st_transform(crs = 5070)




# Figures 1, 2, 3, and A1 -------------------------------------------------
# I first needed to divvy up my data into decades.
# I theoretically could have picked a different time frame,
# but 10 year blocks are easy to understand.

tornado_paths_decades <- tornado_paths %>% 
  mutate(
    # Help from Stack
    # https://stackoverflow.com/questions/35352914/floor-a-year-to-the-decade-in-r
    decade = yr - yr %% 10 
  )


decade_shp <- function(i_year){
  # This writes a shapefile for each decade
  # without me doing this manually
  file_name <- paste0("data/decades/tornadoes_", i_year, "s.shp")
  
  tornado_paths_decades %>% 
    filter(decade == i_year) %>% 
    write_sf(file_name)
}

decades_list <- tornado_paths_decades %>% 
  distinct(decade) %>% 
  pull()

for (decade in decades_list){
  decade_shp(decade)
}



#### Figure A1 Specifically
county_count_norm_plot <- function(i_decade){
  # This makes a normalized plot for each decade.
  # I use the same scale and dimensions.
  
  tornado_paths_decade_specific <- tornado_paths_decades %>% 
    filter(
      decade == i_decade
    )
  
  
  # Equivalent of a spatial join in ArcGIS Pro
  # Instead of Join Count, I have sum_int
  county_count_specific <- usa_map_counties %>% 
    st_intersection(tornado_paths_decade_specific %>% select(c(geometry))) %>% 
    mutate(
      int_place = 1
    ) %>% 
    group_by(county_name, name) %>% 
    summarize(
      sum_int = sum(int_place)
    ) %>%
    arrange(desc(sum_int)) %>% 
    as_tibble() %>% 
    select(-geometry)
  
  
  # Joining the intersections with the data again to get
  # this new column
  usa_map_counties_count_specific <- usa_map_counties %>% 
    left_join(county_count_specific) 
  
  
  # Calculating the Counts, Normalized area
  usa_map_counties_count_norm_specific <- usa_map_counties_count_specific %>% 
    mutate(
      county_area_calc = st_area(geometry),
      county_area = set_units(aland + awater, "m^2"),
      sum_per_area = drop_units((sum_int / county_area) * 1000 * 1000),
    ) 
  
  
  # Every decade except the 2020s is complete.
  # I want the title to reflect that 
  complete_title <- bquote(
    "Normalized Tornadoes per County per"~ km^2 ~"in the"~ .(i_decade)*"s"
    )
  partial_title <- bquote(
    "Normalized Tornadoes per County per"~ km^2 ~"in the"~ .(i_decade)*"s (so far)"
    )
  
  plot_title <- case_when(
    i_decade != 2020 ~ complete_title,
    i_decade == 2020 ~ partial_title
  )
  
  
  # I want the title to be as big as possible without 
  # going over the edge.
  title_size <- case_when(
    i_decade != 2020 ~ 13.5,
    i_decade == 2020 ~ 12
  )
  
  
  # Automating Making the plot 
  normalized_decade <- ggplot() +
    geom_sf(
      data = usa_map_counties_count_norm_specific,
      aes(fill = sum_per_area),
      color = NA
    ) +
    scale_fill_gradient2(
      name = "Normalized\nTornado Count",
      high = "#FFFF33",  
      mid = "#FF3333",  
      midpoint = 0.01,
      low = "#000066", 
      limits = c(0, 0.02),
      breaks = c(0, 0.01, 0.02),
      na.value = "#898989"
    ) +
    geom_sf(
      data = usa_map_state %>% 
        st_transform(crs = 5070),
      aes(geometry = geometry),
      size = 0.65,
      color = "grey30",
      fill = NA
    ) +
    labs(
      title = plot_title
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = title_size)
    )
  
  plot_name <- paste0("data/saved_plots/normalized", i_decade, "s.png")
  
  ggsave(
    normalized_decade,
    filename = plot_name,
    dpi = 600
  )
  
}


# Running this function for each decade
for (decade in decades_list){
  decade_shp(decade)
  county_count_norm_plot(decade)
}




# Figure 4 ----------------------------------------------------------------
# When looking at this tornado, it has the same
# amount as the unadjusted inflation tally, shown in 
# https://www.spc.noaa.gov/faq/tornado/damage$.htm
# I will need to adjust for inflation if I look at cost.
tornado_paths %>% 
  filter(
    yr == 2011,
    mo == 5,
    dy == 22,
    st == "MO"
  )

# From: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
# These data are not tidy... yet.
months <- seq(1, 12)

inflation <- read_excel(
  "data/inflation/inflation.xlsx",
  col_names = c("year", months),
  skip = 1
) %>% 
  pivot_longer(
    cols = -year,
    values_to = "cpi",
    names_to = "month"
  ) %>% 
  rename(
    yr = year,
    mo = month
  ) %>% 
  mutate(
    mo = as.numeric(mo)
  )


# Tornadoes 2016 and afterwards are in units of dollars.
# Once adjusted for inflation, the damages are binned 
# according to the codebook: 
# https://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf
tornado_paths_a2016 <- tornado_paths %>% 
  filter(
    yr >= 2016,
    loss != 0
  ) %>% 
  merge(inflation) %>% 
  mutate(
    loss = loss / cpi * 281.148,
    binned_loss = case_when(
      loss < 50 ~ 1,
      loss < 500 ~ 2,
      loss < 5000 ~ 3,
      loss < 50000 ~ 4,
      loss < 500000 ~ 5,
      loss < 5000000 ~ 6,
      loss < 50000000 ~ 7,
      TRUE ~ 8
    )
  ) %>% 
  select(-loss)


# Tornadoes after 1996 but before 2016
# are recorded in millions of dollars.
# Once adjusted for inflation, the damages are binned 
# according to the codebook: 
# https://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf
tornado_paths_a1996 <- tornado_paths %>% 
  filter(
    yr >= 1996 & yr < 2016, 
    loss != 0
  ) %>% 
  merge(inflation) %>% 
  mutate(
    loss = loss * 1000000 / cpi * 281.148,
    binned_loss = case_when(
      loss < 50 ~ 1,
      loss < 500 ~ 2,
      loss < 5000 ~ 3,
      loss < 50000 ~ 4,
      loss < 500000 ~ 5,
      loss < 5000000 ~ 6,
      loss < 50000000 ~ 7,
      TRUE ~ 8
    )
  ) %>% 
  select(-loss)


# Tornadoes between 1950 and 1996 are
# recorded in bins- according to the NOAA codebook:
# https://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf.
# I found the center of each bin and then adjusted the center 
# (The idea is that about half the tornadoes will be above the center
# of the bin and half below, so taking the midpoint is analogous to
# using an average. This is because there is no way of knowing what
# the actual damages are. This is an approximation.)
tornado_paths_b1996 <- tornado_paths %>% 
  filter(
    yr < 1996,
    loss != 0
  ) %>% 
  merge(inflation) %>% 
  mutate(
    approx_loss = case_when(
      loss == 1 ~ (0 + 50)/2,
      loss == 2 ~ (50 + 500)/2,
      loss == 3 ~ (500 + 5000)/2,
      loss == 4 ~ (5000 + 50000)/2,
      loss == 5 ~ (50000 + 500000)/2,
      loss == 6 ~ (500000 + 5000000)/2,
      loss == 7 ~ (5000000 + 50000000)/2,
      TRUE ~ (50000000 + 500000000)/2,
    ),
    loss = approx_loss / cpi * 281.148,
    binned_loss = case_when(
      loss < 50 ~ 1,
      loss < 500 ~ 2,
      loss < 5000 ~ 3,
      loss < 50000 ~ 4,
      loss < 500000 ~ 5,
      loss < 5000000 ~ 6,
      loss < 50000000 ~ 7,
      TRUE ~ 8
    )
  ) %>% 
  select(-c(loss, approx_loss))


# Combining the separately-processed damages.
# Ready to be adjusted for inflation.
tornado_binned_loss <- rbind(
  tornado_paths_b1996, 
  tornado_paths_a1996) %>% 
  rbind(
    tornado_paths_a2016
  )


# Adjusting for inflation
binned_loss <- tornado_binned_loss %>% 
  as_tibble() %>%
  mutate(
    greater_500k = case_when(
      binned_loss >= 6 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(yr) %>% 
  summarize(
    mean_loss = mean(binned_loss),
    median_loss = median(binned_loss),
    fraction_greater_500k = sum(greater_500k) / n(),
    number_greater_500k = sum(greater_500k)
  ) %>% 
  pivot_longer(
    cols = c(mean_loss, median_loss),
    names_to = "center_type",
    values_to = "loss"
  )

# Before and after 1996 are not great points of comparison
# because of how the data are recorded. I will separate them 
# to make a plot.

# Damages Exceeding $500,000 before 1996
before_96_frac <- binned_loss %>% 
  filter(yr < 1996) %>% 
  ggplot(aes(x = yr, y = fraction_greater_500k)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Fraction of Tornadoes causing\nan Excess of $500k in Damages"
  ) +
  theme(
    axis.title.y = element_text(size = 10)
  )

before_96_num <- binned_loss %>% 
  filter(yr < 1996) %>% 
  ggplot(aes(x = yr, y = number_greater_500k)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Number of Tornadoes causing\nan Excess of $500k in Damages",
    caption = "Bins were Centered and Adjusted for Inflation"
  ) +
  theme(
    axis.title.y = element_text(size = 10)
  )

# Side-by-side plot
before_96_frac / before_96_num

ggsave(
  before_96_frac / before_96_num,
  filename = "data/saved_plots/before96_cost.png",
  dpi = 1200
)


# Tornadoes in 1996 and beyond (until 2021)
after_96_frac <- binned_loss %>% 
  filter(yr >= 1996) %>% 
  ggplot(aes(x = yr, y = fraction_greater_500k)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Fraction of Tornadoes causing\nan Excess of $500k in Damages"
  ) +
  theme(
    axis.title.y = element_text(size = 10)
  )

after_96_num <- binned_loss %>% 
  filter(yr >= 1996) %>% 
  ggplot(aes(x = yr, y = number_greater_500k)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Number of Tornadoes causing\nan Excess of $500k in Damages",
    caption = "Dollar Amounts were Adjusted for Inflation"
  ) +
  theme(
    axis.title.y = element_text(size = 10)
  )

after_96_frac / after_96_num

ggsave(
  after_96_frac / after_96_num,
  filename = "data/saved_plots/after96_cost.png",
  dpi = 1200
)


# Making a combined plot with all the plots
all_together <- (before_96_frac + after_96_frac) / (before_96_num + after_96_num)
all_together

ggsave(
  all_together,
  filename = "data/saved_plots/all_costs_num_frac_together.png",
  height = 6,
  width = 12,
  dpi = 1200
)




# Figure A2 ---------------------------------------------------------------
number_tornadoes_ef_3 <- tornado_paths %>% 
  as_tibble() %>% 
  group_by(yr, mag) %>% 
  summarize(
    tornado_count = n()
  ) %>%
  filter(mag >= 0 & mag < 3) %>% 
  mutate(
    mag = as.factor(case_when(
      mag == 0 ~ "F0/EF0",
      mag == 1 ~ "F1/EF1", 
      mag == 2 ~ "F2/EF2"
    ))
  ) %>% 
  ggplot(aes(x = yr, y = tornado_count, fill = mag)) +
  geom_col() +
  facet_wrap(~mag) +
  labs(
    x = "Year",
    y = "Number of Tornadoes",
    fill = "Scale"
  ) +
  geom_vline(xintercept = 2007)

ggsave(
  number_tornadoes_ef_3,
  filename = "data/saved_plots/number_tornadoes_ef_3.png",
  width = 12,
  height = 5,
  dpi = 1200
)




# Figure A3 ---------------------------------------------------------------
number_tornadoes_ef_5 <- tornado_paths %>% 
  as_tibble() %>% 
  group_by(yr, mag) %>% 
  summarize(
    tornado_count = n()
  ) %>%
  filter(mag >= 3) %>% 
  mutate(
    mag = as.factor(case_when(
      mag == 3 ~ "F3/EF3",
      mag == 4 ~ "F4/EF4",
      mag == 5 ~ "F5/EF5"
    ))
  ) %>%
  ggplot(aes(x = yr, y = tornado_count, fill = mag)) +
  geom_col() +
  facet_wrap(~mag) +
  labs(
    x = "Year",
    y = "Number of Tornadoes",
    fill = "Scale"
  ) +
  geom_vline(xintercept = 2007)

ggsave(
  number_tornadoes_ef_5,
  filename = "data/saved_plots/number_tornadoes_ef_5.png",
  width = 12,
  height = 5,
  dpi = 1200
)




# GIF_1_COUNTIES ----------------------------------------------------------
tornado_paths_plot <- ggplot() +
  geom_sf(
    data = usa_map_state,
    aes(geometry = geometry),
    size = 1
  ) +
  theme_void() +
  geom_sf(
    data = tornado_paths_grey,
    aes(geometry = geometry),
    size = 0.3,
    color = "Grey"
  ) +
  geom_sf(
    data = tornado_paths %>% 
      mutate(
        `Median Tornado\nWidth (yards)` = wid
      ),
    aes(
      geometry = geometry,
      size = `Median Tornado\nWidth (yards)`
    ),
    # size = 0.3,
    color = "Red"
  ) +
  # transition_manual() if transition_time() doesn't work
  transition_manual(
    yr
  ) +
  labs(
    caption = "Year: {current_frame}"
  ) +
  theme(
    plot.caption = element_text(size = 36)
  )


# Animating the plot (by year)
animate(
  tornado_paths_plot,
  fps = 2,
  res = 600,
  height = 6,
  width = 10,
  units = "in", 
  end_pause = 8
)

# Saving the animation
anim_save(
  "data/saved_plots/gif_1_counties.gif", 
  animation = last_animation()
)




# GIF_2_FATALITIES --------------------------------------------------------
tornado_paths %>% 
  as_tibble() %>% 
  select(fat) %>% 
  pull() %>% 
  sum()
# There have been a little over 6 thousand people who have died
# from tornadoes.


# Finding the 10 states with the greatest number of fatalities
# (10 is an arbitrary number).
# The goal will be to find the cumulative sum of fatalities per year.
top_10_fatal <- tornado_paths %>% 
  as_tibble() %>% 
  group_by(st) %>%
  summarize(
    total_fatalities = sum(fat)
  ) %>% 
  arrange(desc(total_fatalities)) %>% 
  head(10) %>% 
  select(st) %>% 
  pull()


# Help from here
# https://stackoverflow.com/questions/46954165/rename-a-set-of-columns-in-r-by-position
# There could be years where a fatal tornado didn't occur in a particular state.
# I still need the number of years to be the same length.
# I am assuming that the most recent data have been downloaded: year(now()) - 1
all_years <- data.frame(seq(1950, year(now()) - 1))
colnames(all_years)[1] <- "yr"
all_years


# Preparing the tibble to add iteratively
all_fatalities_sum <- tribble(
  ~"yr", ~"st", ~"cum_sum"
)

# Looping through each state, adding iteratively
# to the all_fatalities_sum
for (i_st in top_10_fatal){
  # First ensuring that each state has the correct number of years.
  all_years_state <- left_join(
    all_years,
    tornado_paths %>% 
      as_tibble() %>% 
      filter(st %in% top_10_fatal) %>% 
      filter(st == i_st) %>% 
      group_by(yr, st) %>% 
      summarize(
        total_fatalities = sum(fat)
      ) 
  ) %>% 
    # For states without fatalities, 
    # the missing values produced by the left join
    # are converted to 0s since the missing values
    # were introduced via my coercion.
    mutate(
      total_fatalities = ifelse(
        !is.na(total_fatalities),
        total_fatalities,
        0
      ),
      st = ifelse(
        !is.na(st),
        st,
        i_st
      )
    )
  
  # Binding the cumulative sum with itself
  all_years_state_c <- cbind(all_years_state,
                             all_years_state %>% 
                               ungroup() %>% 
                               select(total_fatalities) %>% 
                               cumsum())
  
  # Selecting the appropriate columns
  all_years_state_c <- cbind(
    all_years_state_c[,1:2],
    all_years_state_c[,ncol(all_years_state_c)]
  ) 
  
  # Giving the cumulative sum a better name
  colnames(all_years_state_c)[ncol(all_years_state_c)] <- "cum_sum"
  
  # Once the data are processed (above in the loop), I can add them iteratively
  all_fatalities_sum <- all_fatalities_sum %>% 
    rbind(all_years_state_c)
}


# Ordering the states from most deadly to least deadly
st_order <- all_fatalities_sum %>% 
  filter(yr == year(now()) - 1) %>% 
  arrange(desc(cum_sum)) %>% 
  select(st) %>% 
  pull() %>% 
  rev()


# Constructing the animated bar plot
fatalities_plot <- all_fatalities_sum %>% 
  mutate(
    st = factor(st, levels = st_order)
  ) %>% 
  filter(st %in% st_order) %>% 
  ggplot(aes(y = st, x = cum_sum)) + 
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = cum_sum),
    hjust = 1.5,
    color = "white"
  ) +
  # layer needed to animate the plot
  transition_manual(
    yr
  ) + 
  labs(
    title = "Cumulative Sum of Tornado Fatalities per State (Not Normalized)",
    x = "Cumulative Sum of Tornado Fatalities",
    y = "States with the Greatest Number of Tornado Fatalities",
    caption = "Year: {current_frame}"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 13)
  )

fatalities_plot

animate(
  fatalities_plot,
  fps = 4,
  res = 600,
  height = 6,
  width = 10,
  units = "in", 
  end_pause = 12
)

# Saving the animation
anim_save(
  "data/saved_plots/gif_2_fatalities.gif", 
  animation = last_animation()
)




# Checking a particularly deadly time for Alabama- year 2011 ----
all_fatalities_sum %>% 
  filter(st == "AL" &
           yr > 2009 &
           yr < 2013) %>% 
  glimpse()


tornado_paths %>% 
  filter(st == "AL" &
           yr > 2009 &
           yr < 2013&
           fat > 0) %>% 
  glimpse()
# Alabama had a particularly challenging year in 2011. 
# This is sad.
# and are taking steps to keep people safe. 
# People shouldn't be penalized for living in Alabama.
# People are people and matter.




# Concluding Remarks ------------------------------------------------------
# I learned so much in this project- about tornadoes, R, and ArcGIS Pro.
# This was a great opportunity to explore this interest. 
# I won't be studying tornadoes in the future, but I enjoyed the process.