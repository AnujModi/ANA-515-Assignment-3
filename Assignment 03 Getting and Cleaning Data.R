getwd()

#Question 1:
df <- read.csv("C:/Users/amgsm/Downloads/McDaniel/515/StormEvents_details-ftp_v1.0_d1998_c20220425.csv")
#BTW i got no parsing error that you got as shown in your tutorial ppt...

#Question 2:
df <- df[, c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")]

#Question 3:
if (!requireNamespace("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)
df <- arrange(df, STATE)

#Question 4:
if (!requireNamespace("tidyverse")) {
  install.packages("tidyverse")
}
library(tidyverse)
df <- df %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))
#i googled here since str_to_title is only possible for strings... didnt know how to apply it to an entire row... i got the answer as mutate... after a couple of tries, this is what i came up with... let me know if there is a better method to convert these columns to titlecase

#Question 5:
df <- df %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

#Question 6:
df <- df %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 3, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, side = "left", pad = "0")) %>%
  unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "-")
#kindly note that i wasnt getting the pad here at 001 (3 units)... but rather 01... since my state data was only upto 99... so i added width 3 and padded single digits with two 0's... to get the same result as your tutorial...

#Question 7:
df <- df %>%
  rename_all(tolower)

#Question 8:
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)

#Question 9:
df <- df %>%
  filter(substr(begin_yearmonth, 1, 4) == "1998") %>%
  group_by(state) %>%
  summarise(num_events = n())

df <- merge(df, us_state_info, by = "state", all.x = TRUE)

df <- df[!is.na(df$area), ]
#just a small issue here... my birth year is 1995... and i got no data for the same since the dataset is for 1998... so i have changed the same to 1998 above... that one yields results...

#Question 10:
if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)
storm_plot <- ggplot(df, aes(x = area, y = num_events)) + 
  geom_point(aes(color = region)) + 
  labs(x = "Land area(square miles)",
       y = "# of storm events in 1998")
storm_plot