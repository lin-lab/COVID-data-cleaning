#######################################################################
## Load libraries
#######################################################################
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

#######################################################################
## Process county-level data
#######################################################################

# load county-level confirmed cases data.
dat_dir <- file.path("JHU_CSSE_COVID-19", "csse_covid_19_data",
                     "csse_covid_19_time_series")
confirmed_path <- file.path(dat_dir, "time_series_covid19_confirmed_US.csv")
confirmed_df_orig <- read_csv(confirmed_path)
stopifnot(length(unique(confirmed_df_orig$UID)) == nrow(confirmed_df_orig))

kc1 <- confirmed_df_orig %>% filter(Admin2 == "Kansas City")
stopifnot(nrow(kc1) == 1)


confirmed_df <- confirmed_df_orig %>%
  gather(key = "date_char", value = "confirmed", -(UID:Combined_Key)) %>%
  mutate(date = mdy(date_char))

kc2 <- confirmed_df %>% filter(Admin2 == "Kansas City")
num_dates <- sum(endsWith(colnames(confirmed_df_orig), "20"))
stopifnot(nrow(kc2) == num_dates)

# load county-level deaths data.
deaths_path <- file.path(dat_dir, "time_series_covid19_deaths_US.csv")
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

kc3 <- jhu_df %>% filter(Admin2 == "Kansas City")
stopifnot(nrow(kc3) == num_dates)

# error checking
stopifnot(nrow(jhu_df) == nrow(death_df))
stopifnot(nrow(jhu_df) == nrow(confirmed_df))
x <- jhu_df %>% filter(date == mdy("03-31-2020"))
stopifnot(length(unique(x$Combined_Key)) == nrow(x))
# check if we calculated deaths per day / cases per day correctly
tmp <- jhu_df %>%
  filter(Admin2 == "New York", Province_State == "New York") %>%
  arrange(date) %>%
  mutate(confirmed2 = cumsum(confirmed_per_day),
         deaths2 = cumsum(deaths_per_day))
stopifnot(all.equal(tmp$confirmed, tmp$confirmed2))
stopifnot(all.equal(tmp$deaths, tmp$deaths2))

# check that number of counties in each state didn't change over time
counties_over_time <- jhu_df %>%
  group_by(Province_State, date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(Province_State) %>%
  summarize(sd_n = sd(n))
stopifnot(all.equal(counties_over_time$sd_n, rep(0, nrow(counties_over_time))))

jhu_df %>%
  filter(date == mdy("03-31-2020")) %>%
  group_by(Province_State) %>%
  summarize(n = n())

jhu_df %>%
  filter(startsWith(Admin2, "Out of ")) %>%
  group_by(Province_State) %>%
  summarize_at(vars(deaths, confirmed), sum) %>%
  filter(deaths > 0 | confirmed > 0)

#######################################################################
## Aggregate county-level data into state-level data
#######################################################################

jhu_state_orig <- jhu_df %>%
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

# MA has a reporting issue on 2020-06-01, so we replace the number.
jhu_state_orig %>%
  filter(Province_State == "Massachusetts", date >= ymd("2020-05-30")) %>%
  select(date, confirmed, confirmed_per_day)

jhu_state <- jhu_state_orig %>%
  mutate(confirmed_per_day = case_when(
          Province_State == "Massachusetts" & date == ymd("2020-06-01") ~ 326,
          TRUE ~ confirmed_per_day))
jhu_state %>%
  filter(Province_State == "Massachusetts", date >= ymd("2020-05-30")) %>%
  select(date, confirmed, confirmed_per_day)

# checking
tmp <- jhu_state %>%
  filter(Province_State == "New York") %>%
  arrange(date) %>%
  mutate(confirmed2 = cumsum(confirmed_per_day),
         deaths2 = cumsum(deaths_per_day))
stopifnot(all.equal(tmp$confirmed, tmp$confirmed2))
stopifnot(all.equal(tmp$deaths, tmp$deaths2))

#######################################################################
## Aggregate state-level data into country-level data
#######################################################################

usa_bystate <- jhu_state %>%
  group_by(date) %>%
  summarize_at(vars(Population, deaths, confirmed), sum) %>%
  ungroup() %>%
  arrange(date)


#######################################################################
## Process country-level data
#######################################################################

# read data for other countries
confirmed_path_global <- file.path(dat_dir, "time_series_covid19_confirmed_global.csv")
confirmed_global <- read_csv(confirmed_path_global) %>%
  select(-Lat, -Long) %>%
  gather(key = "date_str", value = "confirmed_global",
         -`Province/State`, -`Country/Region`) %>%
  mutate(date = mdy(date_str)) %>%
  select(-date_str) %>%
  arrange(date)

death_path_global <- file.path(dat_dir, "time_series_covid19_deaths_global.csv")
deaths_global <- read_csv(death_path_global) %>%
  select(-Lat, -Long) %>%
  gather(key = "date_str", value = "deaths_global",
         -`Province/State`, -`Country/Region`) %>%
  mutate(date = mdy(date_str)) %>%
  select(-date_str) %>%
  arrange(date)

# merge confirmed cases + deaths and calculate per-day stats
stopifnot(all.equal(deaths_global$`Province/State`,
                    confirmed_global$`Province/State`))

jhu_global <- inner_join(confirmed_global, deaths_global,
                         by = c("Province/State", "Country/Region",
                                "date")) %>%
  rename(positive = confirmed_global, death = deaths_global)
# error checking
stopifnot(nrow(jhu_global) == nrow(deaths_global))
stopifnot(nrow(jhu_global) == nrow(confirmed_global))

# calculate new cases/deaths per day
jhu_global_incr <- jhu_global %>%
  group_by(`Province/State`, `Country/Region`) %>%
  mutate(positiveIncrease = positive - lag(positive, n = 1, default = 0),
         deathIncrease = death - lag(death, n = 1, default = 0)) %>%
  ungroup()

#######################################################################
## Aggregate province level data for China, Canada, and Australia
#######################################################################

countries_to_agg <- c("China", "Canada", "Australia")
jhu_global %>%
  filter(`Country/Region` %in% countries_to_agg) %>%
  select(`Province/State`, `Country/Region`) %>%
  distinct() %>%
  print(n = Inf)

# need to remove these provinces in Canada
remove_provinces <- c("Diamond Princess", "Grand Princess", "Recovered")

jhu_global_agg <- jhu_global %>%
  filter(`Country/Region` %in% countries_to_agg) %>%
  filter(!(`Province/State` %in% remove_provinces)) %>%
  group_by(`Country/Region`, date) %>%
  summarize_at(vars(positive, death), sum) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(`Country/Region`) %>%
  mutate(deathIncrease = death - lag(death, n = 1,
                                     default = 0, order_by = date),
         positiveIncrease = positive - lag(positive, n = 1,
                                           default = 0, order_by = date)) %>%
  ungroup() %>%
  mutate(`Province/State` = NA)


# Hong Kong and Macau are written differently in the UID lookup table.
jhu_global_w_agg <- rbind(jhu_global_incr, jhu_global_agg) %>%
  rename(Province_State = `Province/State`,
         Country_Region = `Country/Region`) %>%
  mutate(Province_State = case_when(
          Province_State == "Hong Kong" & Country_Region == "China" ~ "Hong Kong SAR",
          Province_State == "Macau" & Country_Region == "China" ~ "Macau SAR",
          TRUE ~ Province_State))


# Merge UID lookup table
uid_path <- file.path("JHU_CSSE_COVID-19", "csse_covid_19_data",
                      "UID_ISO_FIPS_LookUp_Table.csv")
uid_lookup <- read_csv(uid_path)

jhu_global_final <- jhu_global_w_agg %>%
  left_join(uid_lookup, by = c("Province_State", "Country_Region"))
stopifnot(nrow(jhu_global_final) == nrow(jhu_global_w_agg))
stopifnot(!anyNA(jhu_global_final$UID))


# error checking
aus <- jhu_global_final %>%
  filter(Country_Region == "Australia", date == ymd("2020-04-22"))
stopifnot(sum(aus$positive[1:8]) == aus$positive[9])


# checking if we calculated the positive/death increase correctly.
tmp <- jhu_global_final %>%
  filter(Country_Region == "US")
stopifnot(all.equal(cumsum(tmp$positiveIncrease), tmp$positive))
tmp <- jhu_global_final %>%
  filter(Province_State == "Alberta", Country_Region == "Canada")
stopifnot(all.equal(cumsum(tmp$positiveIncrease), tmp$positive))
tmp <- jhu_global_final %>%
  filter(is.na(Province_State), Country_Region == "Canada")
stopifnot(all.equal(cumsum(tmp$positiveIncrease), tmp$positive))

#######################################################################
## Subset USA and compare to the aggregating county data
#######################################################################

usa_global_confirmed <- confirmed_global %>%
  filter(`Country/Region` == "US") %>%
  select(date, confirmed_global) %>%
  arrange(date)
usa_global_deaths <- deaths_global %>%
  filter(`Country/Region` == "US") %>%
  select(date, deaths_global) %>%
  arrange(date)

stopifnot(all(usa_global_confirmed$date == usa_bystate$date))
stopifnot(all(usa_global_deaths$date == usa_bystate$date))

usa_merged <- usa_bystate %>%
  inner_join(usa_global_confirmed, by = "date") %>%
  inner_join(usa_global_deaths, by = "date")

# some plots to compare
plt_confirmed <- usa_merged %>%
  ggplot(aes(x = confirmed_global, y = confirmed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10() + scale_y_log10() +
  xlab("Data from Global CSV") + ylab("Aggregated State Data") +
  ggtitle("Confirmed Cases Comparison")

plt_deaths <- usa_merged %>%
  ggplot(aes(x = deaths_global, y = deaths)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10() + scale_y_log10() +
  xlab("Data from Global CSV") + ylab("Aggregated State Data") +
  ggtitle("Deaths Comparison")

plt_confirmed_time <- usa_merged %>%
  select(date, By_State = confirmed, Global = confirmed_global) %>%
  gather(key = "data_source", value = "Confirmed", -date) %>%
  ggplot(aes(x = date, y = Confirmed, color = data_source)) +
  geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Confirmed Cases") +
  scale_color_discrete(name = "Data Source") +
  ggtitle("US Confirmed Cases")

plt_deaths_time <- usa_merged %>%
  select(date, By_State = deaths, Global = deaths_global) %>%
  gather(key = "data_source", value = "Deaths", -date) %>%
  filter(date >= mdy("03-01-2020")) %>%
  ggplot(aes(x = date, y = Deaths, color = data_source)) +
  geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Deaths") +
  scale_color_discrete(name = "Data Source") +
  ggtitle("US Deaths")

plt_combined <- plot_grid(plt_confirmed, plt_deaths,
                          plt_confirmed_time, plt_deaths_time)
ggsave("figures/combined_plt.png", plt_combined, width = 12, height = 8)

## Overall we recommend using the USA data from the global csv, not from
## aggregating the county-level data.


#######################################################################
## Construct final data frames for export
#######################################################################

jhu_county_final <- jhu_df %>%
  mutate(FIPS_str = sprintf("%05d", FIPS)) %>%
  select(county = Admin2, stateName = Province_State, date,
         FIPS = FIPS_str, UID, positiveIncrease = confirmed_per_day,
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

#######################################################################
## Some rows of the data have negative new cases/deaths on certain days
#######################################################################

# some rows have negative new for the cases / deaths per day
jhu_county_final %>%
  filter(!startsWith(county, "Out of "), !startsWith(county, "Unassigned")) %>%
  mutate_if(is.numeric, function(x) { x >= 0 }) %>%
  filter(!positiveIncrease)

jhu_state_final %>%
  mutate_if(is.numeric, function(x) { x >= 0 }) %>%
  filter(!positiveIncrease | !deathIncrease)

jhu_global_final %>%
  mutate_if(is.numeric, function(x) { x >= 0 }) %>%
  filter(!positiveIncrease | !deathIncrease)

kc4 <- jhu_df %>% filter(Admin2 == "Kansas City")
stopifnot(nrow(kc4) == num_dates)

#######################################################################
## Write out data
#######################################################################

cur_date <- format(Sys.Date(), "%Y-%m-%d")
write_csv(jhu_county_final,
          sprintf("cleaned_data/JHU_COVID-19_County.csv", cur_date))
write_csv(jhu_state_final,
          sprintf("cleaned_data/JHU_COVID-19_State.csv", cur_date))
write_csv(jhu_global_final,
          sprintf("cleaned_data/JHU_COVID-19_Global.csv", cur_date))
