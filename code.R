# Loading packages -------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(gganimate)
library(ggrepel)


# Setting theme ----------------------------------------------------------------
options(repr.plot.width = 10, repr.plot.height = 6, repr.plot.res = 150 )

my_theme <- function(text_color) {
  theme_bw() +
    theme(text = element_text(size = 10, family = "Manjari", face = "bold", color = text_color),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5)
    )
}

# Reading data -----------------------------------------------------------------
temp <- read_csv("global_temp.csv", col_names = TRUE, col_types = cols(
  dt = col_date(),
  LandMaxTemperature = col_number(),
  LandMinTemperature = col_number(),
  LandMaxTemperatureUncertainty = col_number(),
  LandMinTemperatureUncertainty = col_number(),
  LandAndOceanAverageTemperature = col_number(),
  LandAndOceanAverageTemperatureUncertainty = col_number()
))

temp_country  <- read_csv("global_temp_country.csv", col_names = TRUE)


# EDA --------------------------------------------------------------------------

# Adding features 
temp$year <- as.numeric(format(temp$dt, "%Y"))
temp_country$year <- as.numeric(format(temp_country$dt, "%Y"))
breaks <- seq(1750, 2100, by = 50)
temp$half_century <- cut(temp$year, breaks, labels = paste(breaks[-length(breaks)], breaks[-1], sep = "-"), include.lowest = TRUE)


## Global Temperatures ---------------------------------------------------------

# Over the past three decades,we observe that minimum temperatures consistently 
# surpass the 2.5°C threshold, indicating a warming trend. 
# Additionally, temperatures exhibited more variability prior to 1800, 
# though there's a degree of skepticism regarding the accuracy of those ancient 
# records when compared to the recent data.
temp %>% 
  ggplot(aes(dt, LandAverageTemperature, color = LandAverageTemperature)) +
  geom_line() + 
  geom_smooth(color = "maroon") +
  labs(x = NULL, y = NULL) +
  ggtitle("Land Average Temperature Uncertainty") +
  my_theme(text_color = "black") +
  scale_color_gradient(name = "ºC", low = "blue", high = "orange")
  

# There is a spreaded uncertainty until 1850. Therefore, will rely mostly on data
# from that point on.
# Let's look now at the temperature changes for each 50 years.

temp %>% 
  ggplot(aes(dt, LandAverageTemperatureUncertainty, color = LandAverageTemperatureUncertainty)) +
  geom_line() +
  labs(x = 'Date', y = NULL) +
  ggtitle("Land Average Temperature Uncertainty") +
  my_theme(text_color = "black") +
  scale_color_gradient(name = "ºC", low = "blue", high = "orange")

# The upward shift from 2000 on in the entire boxplot suggests a 
# systematic increase in the distribution of the data.

temp %>% 
  ggplot(aes(half_century, LandMinTemperature)) +
  geom_boxplot(fill = "steelblue1") +
  labs(x = NULL, y = NULL) +
  ggtitle("Land Minimum Temperature by each Half-Century") +
  my_theme(text_color = "black")

temp %>% 
  ggplot(aes(half_century, LandMaxTemperature)) +
  geom_boxplot(fill = "lightyellow") +
  labs(x = NULL, y = NULL) +
  ggtitle("Land Maximum Temperature by each Half-Century") +
  my_theme(text_color = "black")


# There is an upward move throughout each half-century.
# Still, there were times when the average temperatures would drop as in the late
# 40s - early 50s. But since then we do not see a drop at all. There was a stagnation
# between 2005- 2010 and then the trend is still upwards.
temp %>% 
  filter(year > 1850) %>% 
  ggplot(aes(dt, LandAndOceanAverageTemperature, color = LandAndOceanAverageTemperature)) +
  geom_line() +
  geom_smooth(color = "maroon") +
  facet_wrap(~ half_century, ncol = 5, scales = "free_x") +
  labs(x = 'Year', y = '') +
  ggtitle("Land and Ocean Average Temperature by each Half-Century") +
  my_theme(text_color = "black") +
  scale_color_gradient(low = "blue", high = "orange") +
  guides(color = FALSE)


# We can observe 2 things:
# 1. The distribution has less variance across centuries. Accordingly,
# we are experiencing certain temperatures more often, seemingly higher temperatures
# and less often minimum temperatures. If you look closely, it seems like
# there are more separate houses instead of a connecting city.

# 2. The changes seem insignificant to the human eye, for example from 2000 on
# we did not have anymore temperatures less than 2.5 degrees. Of course, that
# could be due the way the temperatures were recorded, but since 1900, we did not
# have even 0 average degrees.

temp %>% 
  ggplot(aes(LandAverageTemperature)) +
  geom_histogram(bins = 50, fill = 'tan2') +
  facet_wrap(~ half_century, nrow = 6, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  ggtitle("Land and Ocean Average Temperature by each Half-Century") +  
  my_theme(text_color = "black")


# Looking at min and max temperatures, we can see the distribution is moving
# to the right.

temp %>% 
  ggplot(aes(LandMinTemperature)) +
  geom_histogram(bins = 50, fill = 'steelblue1') +
  facet_wrap(~ half_century, nrow = 6, scales = "free_y")

temp %>% 
  ggplot(aes(LandMaxTemperature)) +
  geom_histogram(bins = 50, fill = 'tan2') +
  facet_wrap(~ half_century, nrow = 6, scales = "free_y")

# Let's zoom in now by country.

## Temperatures by country -----------------------------------------------------


# Distinguishing between major economies categorized as developed and 
# those categorized as developing.
# Please note that this classification is subjective and has been arbitrarily 
# defined by the author.

temp_country <- temp_country %>%
  mutate(country_type = case_when(
    Country %in% c("United States", 'United Kingdom (Europe)', "Germany", 
                   'France (Europe)', 'Belgium', 'Netherlands', 
                   'Australia', 'Japan', 'Canada') ~ "Developed",
    Country %in% c("China", "Brazil", "Russia", "India", "South Africa", "Mexico") ~ "Developing",
    TRUE ~ "Other"),
         month = month(dt)
  )

# The average temperatures were pretty stable until around 1890s. 
# From that on, probably due to the Industrial Revolution, the temperatures started
# increasing. The increase is almost not noticeable compared to the increases from 1960s on.
# From 1960 on, the temperatures have increased by 1 degree.
# The most striking example is the US. The last years, it increased by 2 degrees.
# Also, there are no records for the US for before 1820 or so, meaning
# we shall take the periods before that with the grain of salt in the analysis.

temp_country %>%
  filter(country_type == 'Developed') %>% 
  group_by(Country, year) %>% 
  summarise(avg_temp_year = mean(AverageTemperature)) %>% 
  ggplot(aes(year, avg_temp_year, color = Country)) +
  geom_line() +
  geom_smooth(color = "maroon") +
  facet_wrap(~ Country, scales = "free") +
  labs(x = NULL, y = NULL) +
  ggtitle("Average Temperature by Year in Developed Countries") +
  my_theme(text_color = "black") +
  guides(color = FALSE)

# Similar here.
temp_country %>%
  filter(country_type == 'Developing') %>% 
  group_by(Country, year) %>% 
  summarise(avg_temp_year = mean(AverageTemperature)) %>% 
  ggplot(aes(year, avg_temp_year, color = Country)) +
  geom_line() +
  geom_smooth(color = "maroon") +
  facet_wrap(~ Country, scales = "free") +
  labs(x = NULL, y = NULL) +
  ggtitle("Average Temperature by Year in Developing Countries") +
  my_theme(text_color = "black") +
  guides(color = FALSE)

# Now, I'm doing this for myself. I want to see how things have changed
# in my homeland. Similar pattern.
  
# Across seasons, the winter season has experienced almost 2 degree increase
# since the 1900s!

temp_country %>% 
  filter(Country == 'Moldova') %>% 
  mutate(season = case_when(month %in% c(12, 1, 2) ~ 'Winter',
                            month %in% c(3, 4, 5) ~ 'Spring',
                            month %in% c(6, 7, 8) ~ 'Summer',
                            month %in% c(9, 10, 11) ~ 'Autumn')) %>% 
  group_by(year, season, Country) %>% 
  summarise(avg_temp_year_season = mean(AverageTemperature)) %>% 
  ggplot(aes(year, avg_temp_year_season, color = avg_temp_year_season)) +
  geom_point() +
  facet_wrap(~ season, scales = "free") +
  geom_smooth(color = "maroon") +
  scale_color_gradient(name = "ºC", low = "blue", high = "orange") +
  labs(x = NULL, y = NULL) +
  ggtitle("Average Temperature by Year and by Season in Moldova") +
  my_theme(text_color = "black")


# A frequently used metric in climatology is anomalies. 
# Although not originally present in the dataset, I'll be creating my own. 
# I know, this here is not pure science, but this project is more of a playground.
# Since the 30-year reference period is widely used when analyzing
# climate data, I'd be applying it here as well.

temp <- temp %>% 
  mutate(y30 = ceiling(year / 30) * 30)

# No more blues are heard.
temp %>%
  group_by(y30) %>%
  summarise(avg_temp = mean(LandAverageTemperature, na.rm = TRUE)) %>% 
  mutate(anomaly = avg_temp - lag(avg_temp)) %>% 
  ggplot(aes(x = y30, y = anomaly, fill = factor(sign(anomaly)))) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("steelblue1", "orange"), guide = FALSE) +
  labs(x = "", y = "Anomaly") +
  ggtitle("Temperature Difference from 30 Years Prior") +
  my_theme(text_color = "black")

# Animations --------------------------------------------------------------------

# See how it keeps going up?
temp_country %>% 
  mutate(y30 = ceiling(year / 30) * 30) %>% 
  group_by(y30, Country, country_type) %>% 
  summarise(avg_temp = mean(AverageTemperature, na.rm = TRUE)) %>% 
  group_by(Country, country_type) %>% 
  mutate(anomaly = avg_temp - dplyr::lag(avg_temp, order_by = y30),
  anomaly_cum = cumsum(replace(anomaly, is.na(anomaly), 0))) %>% 
  filter(country_type == 'Developed') %>% 
  ggplot(aes(y30, anomaly_cum, color = Country)) +
  geom_point(size = 4) +
  my_theme(text_color = "black") +
  labs(x = NULL, y = NULL) +
  ggtitle("Cumulative Anomaly in Developed Countries") +
  # gganimate:
  labs(title = 'Year: {frame_time}', x = '', y = 'Cumulative Anomaly') +
  transition_time(y30) +
  ease_aes('cubic-in-out')

# Save the animation as a gif: 
anim_save("cumulative_anomaly_developed_countries.gif")

