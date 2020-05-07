library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

# load county-level confirmed cases data.
dat_dir <- file.path("JHU_CSSE_COVID-19", "csse_covid_19_data",
                     "csse_covid_19_time_series")
confirmed_path <- file.path(dat_dir, "time_series_covid19_confirmed_US.csv")
deaths_path <- file.path(dat_dir, "time_series_covid19_deaths_US.csv")
confirmed_df_orig <- read_csv(confirmed_path)
stopifnot(length(unique(confirmed_df_orig$UID)) == nrow(confirmed_df_orig))

confirmed_df <- confirmed_df_orig %>%
  gather(key = "date_char", value = "confirmed", -(UID:Combined_Key)) %>%
  mutate(date = mdy(date_char))

# load county-level confirmed data.
death_df_orig <- read_csv(deaths_path)
stopifnot(length(unique(death_df_orig$UID)) == nrow(death_df_orig))
death_df <- death_df_orig %>%
  gather(key = "date_char", value = "deaths", -(UID:Population)) %>%
  mutate(date = mdy(date_char))

jhu_df <- death_df %>% select(UID, date, Population, deaths) %>%
  inner_join(confirmed_df, ., by = c("UID", "date")) %>%
  select(-confirmed, everything()) %>%
  arrange(date) %>%
  # get number of deaths / confirmed per day
  group_by(Combined_Key) %>%
  mutate(deaths_per_day = deaths - lag(deaths, n = 1,
                                       default = 0, order_by = date),
         confirmed_per_day = confirmed - lag(confirmed, n = 1,
                                             default = 0, order_by = date)) %>%
  ungroup() %>%
  select(-date_char)
head(jhu_df)

# error checking
stopifnot(nrow(jhu_df) == nrow(death_df))
stopifnot(nrow(jhu_df) == nrow(confirmed_df))
x <- jhu_df %>% filter(date == mdy("03-31-2020"))
stopifnot(length(unique(x$Combined_Key)) == nrow(x))
tmp <- jhu_df %>%
  filter(Admin2 == "New York", Province_State == "New York") %>%
  arrange(date) %>%
  mutate(confirmed2 = cumsum(confirmed_per_day),
         deaths2 = cumsum(deaths_per_day))
stopifnot(all.equal(tmp$confirmed, tmp$confirmed2))
stopifnot(all.equal(tmp$deaths, tmp$deaths2))

plt_counties_over_time <- jhu_df %>%
  group_by(Province_State, date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n, color = Province_State)) +
  geom_line() +
  ggtitle("Number of Counties / State Over Time") +
  xlab("Date") + ylab("Number of Counties") +
  guides(color = FALSE)
ggsave("figures/counties.png", plt_counties_over_time)


jhu_df %>%
  filter(date == mdy("03-31-2020")) %>%
  group_by(Province_State) %>%
  summarize(n = n())

jhu_df %>%
  filter(startsWith(Admin2, "Out of ")) %>%
  group_by(Province_State) %>%
  summarize_at(vars(deaths, confirmed), sum) %>%
  filter(deaths > 0 | confirmed > 0)

jhu_state <- jhu_df %>%
  group_by(Province_State, date) %>%
  summarize_at(vars(Population, deaths, confirmed), sum) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(Province_State) %>%
  mutate(deaths_per_day = deaths - lag(deaths, n = 1,
                                       default = 0, order_by = date),
         confirmed_per_day = confirmed - lag(confirmed, n = 1,
                                             default = 0, order_by = date)) %>%
  ungroup()

# checking
tmp <- jhu_state %>%
  filter(Province_State == "New York") %>%
  arrange(date) %>%
  mutate(confirmed2 = cumsum(confirmed_per_day),
         deaths2 = cumsum(deaths_per_day))
stopifnot(all.equal(tmp$confirmed, tmp$confirmed2))
stopifnot(all.equal(tmp$deaths, tmp$deaths2))

usa_bystate <- jhu_state %>%
  group_by(date) %>%
  summarize_at(vars(Population, deaths, confirmed), sum) %>%
  ungroup() %>%
  arrange(date)

confirmed_path_global <- file.path(dat_dir, "time_series_covid19_confirmed_global.csv")
usa_global_confirmed <- read_csv(confirmed_path_global) %>%
  filter(`Country/Region` == "US") %>%
  select(-`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  gather(key = "date_str", value = "confirmed_global") %>%
  mutate(date = mdy(date_str)) %>%
  select(-date_str) %>%
  arrange(date)
death_path_global <- file.path(dat_dir, "time_series_covid19_deaths_global.csv")
usa_global_deaths <- read_csv(death_path_global) %>%
  filter(`Country/Region` == "US") %>%
  select(-`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  gather(key = "date_str", value = "deaths_global") %>%
  mutate(date = mdy(date_str)) %>%
  select(-date_str) %>%
  arrange(date)
stopifnot(all(usa_global_confirmed$date == usa_bystate$date))
stopifnot(all(usa_global_deaths$date == usa_bystate$date))


usa_merged <- usa_bystate %>%
  inner_join(usa_global_confirmed, by = "date") %>%
  inner_join(usa_global_deaths, by = "date")

# some plots
plt_confirmed <- usa_merged %>%
  ggplot(aes(x = confirmed_global, y = confirmed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10() + scale_y_log10() +
  xlab("Data from Global CSV") + ylab("Aggregated State Data") +
  ggtitle("Confirmed Cases Comparison")
ggsave("figures/comparison-cases.png", plt_confirmed)

plt_deaths <- usa_merged %>%
  ggplot(aes(x = deaths_global, y = deaths)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10() + scale_y_log10() +
  xlab("Data from Global CSV") + ylab("Aggregated State Data") +
  ggtitle("Deaths Comparison")
ggsave("figures/comparison-deaths.png", plt_deaths)

plt_confirmed_time <- usa_merged %>%
  select(date, By_State = confirmed, Global = confirmed_global) %>%
  gather(key = "data_source", value = "Confirmed", -date) %>%
  ggplot(aes(x = date, y = Confirmed, color = data_source)) +
  geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Confirmed Cases") +
  scale_color_discrete(name = "Data Source") +
  ggtitle("US Confirmed Cases")
ggsave("figures/time-cases.png", plt_confirmed_time)

plt_deaths_time <- usa_merged %>%
  select(date, By_State = deaths, Global = deaths_global) %>%
  gather(key = "data_source", value = "Deaths", -date) %>%
  filter(date >= mdy("03-01-2020")) %>%
  ggplot(aes(x = date, y = Deaths, color = data_source)) +
  geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Deaths") +
  scale_color_discrete(name = "Data Source") +
  ggtitle("US Deaths")
ggsave("figures/time-deaths.png", plt_deaths_time)

plt_combined <- plot_grid(plt_confirmed, plt_deaths,
                          plt_confirmed_time, plt_deaths_time)
ggsave("figures/combined_plt.png", plt_combined, width = 12, height = 8)


# final data frames
jhu_county_final <- jhu_df %>%
  select(county = Admin2, stateName = Province_State, date,
         positiveIncrease = confirmed_per_day,
         deathIncrease = deaths_per_day,
         positive = confirmed,
         death = deaths) %>%
  arrange(date)


jhu_state_final <- jhu_state %>%
  select(stateName = Province_State, date,
         positiveIncrease = confirmed_per_day,
         deathIncrease = deaths_per_day,
         positive = confirmed,
         death = deaths,
         population = Population) %>%
  arrange(date)

# some counties and states have negative for the cases / deaths per day
jhu_county_final %>%
  filter(!startsWith(county, "Out of "), !startsWith(county, "Unassigned")) %>%
  mutate_if(is.numeric, function(x) { x >= 0 }) %>%
  filter(!positiveIncrease)

jhu_state_final %>%
  mutate_if(is.numeric, function(x) { x >= 0 }) %>%
  filter(!positiveIncrease | !deathIncrease)

jhu_usa_final <- usa_merged %>%
  select(date, positive = confirmed_global,
         death = deaths_global) %>%
  mutate(deathIncrease = death - lag(death, n = 1, default = 0),
         positiveIncrease = positive - lag(positive, n = 1, default = 0))

stopifnot(all.equal(cumsum(jhu_usa_final$positiveIncrease),
                    jhu_usa_final$positive))
jhu_usa_final %>%
  summarize_if(is.numeric, function(x) { all(x >= 0) }) %>%
  as.logical() %>%
  all() %>%
  stopifnot()

cur_date <- format(Sys.Date(), "%Y-%m-%d")
write_csv(jhu_county_final,
          sprintf("cleaned_data/JHU_COVID-19_County_%s.csv", cur_date))
write_csv(jhu_state_final,
          sprintf("cleaned_data/JHU_COVID-19_State_%s.csv", cur_date))
write_csv(jhu_usa_final,
          sprintf("cleaned_data/JHU_COVID-19_USA_%s.csv", cur_date))
