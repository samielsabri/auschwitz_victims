
library(tidyverse)
library(ggplot2)
library(dplyr)

auschwitz_victims <- read_csv("Auschwitz_Death_Certificates.csv")
city_country <- read.csv("city_country.csv")

city_country <- city_country %>% filter(str_detect(Timezone, "Europe"))

city_country_expanded <- city_country %>%
  mutate(Alternate.Names = str_to_lower(Alternate.Names)) %>%
  separate_rows(Alternate.Names, sep = ";") %>%
  mutate(Alternate.Names = if_else(Alternate.Names == "", NA_character_, Alternate.Names)) %>%
  unite("All_Names", c(Alternate.Names), sep = ",", remove = FALSE, na.rm = TRUE) %>%
  separate_rows(All_Names, sep = ",")

city_country_unique <- city_country_expanded %>%
  distinct(All_Names, Country, .keep_all = TRUE)

auschwitz_victims_joined <- auschwitz_victims %>%
  left_join(city_country_unique, by = c("Birthplace" = "All_Names"), relationship="many-to-many")

auschwitz_victims_joined <- auschwitz_victims_joined %>% unique("Country")

auschwitz_victims_final <- auschwitz_victims_joined %>% select(`Last Name`, 
                                                               `First Name(s)`,
                                                               `Date of Birth`,
                                                                `Date of Death`,
                                                               Birthplace,
                                                               Residence,
                                                               Religion,
                                                               Country)

write.csv(auschwitz_victims_final, "auschwitz_victims_final.csv", row.names = FALSE)


