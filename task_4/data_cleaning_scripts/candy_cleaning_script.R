library(tidyverse)
library(assertr)
library(readxl)
library(here)
library(janitor)

y2015_candy_data <- read_xlsx(here::here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))

y2016_candy_data <- read_xlsx(here::here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))

y2017_candy_data <- read_xlsx(here::here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

cc_y2015_candy_data <-
  clean_names(y2015_candy_data)

cc_y2016_candy_data <-
  clean_names(y2016_candy_data)

cc_y2017_candy_data <-
  clean_names(y2017_candy_data)

chosen_cols_2015 <-
  cc_y2015_candy_data %>% 
  subset(
    select = -c(
      timestamp,
      please_leave_any_remarks_or_comments_regarding_your_choices:
        please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
      which_day_do_you_prefer_friday_or_sunday:please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_beyonce_knowles)
  )

chosen_cols_2016 <-
  cc_y2016_candy_data %>%
  subset(
    select = -c(
      timestamp,
      which_state_province_county_do_you_live_in,
      please_list_any_items_not_included_above_that_give_you_joy:york_peppermint_patties_ignore)
  )

chosen_cols_2017 <-
  cc_y2017_candy_data %>%
  subset(
    select = -c(
      internal_id,
      q5_state_province_county_etc,
      q7_joy_other:click_coordinates_x_y)
  )


cc_id_2015 <-
  chosen_cols_2015 %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(year = 2015) %>%
  tibble::rowid_to_column("id")

cc_id_2016 <-
  chosen_cols_2016 %>%
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(year = 2016) %>%
  tibble::rowid_to_column("id")

cc_id_2017 <-
  chosen_cols_2017 %>% 
  filter_all(any_vars(!is.na(.))) %>%
  mutate(year = 2017) %>%
  tibble::rowid_to_column("id")


cc_pivot_2015 <-
  cc_id_2015 %>%
  pivot_longer(
    cols = c(butterfinger:necco_wafers), 
    names_to = "candy_type", 
    values_to = "rating"
  )

cc_pivot_2016 <-
  cc_id_2016 %>%
  pivot_longer(
    cols = c(x100_grand_bar: york_peppermint_patties),
    names_to = "candy_type",
    values_to = "rating"  
  )

cc_pivot_2017 <-
  cc_id_2017 %>%
  pivot_longer(
    cols = c(q6_100_grand_bar: q6_york_peppermint_patties),
    names_to = "candy_type",
    values_to = "rating"
  ) %>%
  mutate(candy_type = str_remove(candy_type, fixed("q6_")))


cc_pivot_2015_rename <-
  cc_pivot_2015 %>% 
  rename(
    age = how_old_are_you,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
  ) 

cc_pivot_2016_rename <-
  cc_pivot_2016 %>% 
  rename(
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in
  )

cc_pivot_2017_rename <-
  cc_pivot_2017 %>% 
  rename(
    going_out = q1_going_out,
    gender = q2_gender,
    age = q3_age,
    country = q4_country
  )


joined_candy <-
  bind_rows(cc_pivot_2015_rename, cc_pivot_2016_rename, cc_pivot_2017_rename)


jc_age_done <-
  joined_candy %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(age = replace(age, age < 4 | age > 120, NA))


jc_age_extras <-
  jc_age_done %>% 
  mutate(
    age = replace(age, id == 117 & year == 2016, 51),
    age = replace(age, id == 303 & year == 2016, 47),
    age = replace(age, id == 622 & year == 2016, 54),
    age = replace(age, id == 623 & year == 2016, 54),
    age = replace(age, id == 692 & year == 2016, 44),
    age = replace(age, id == 829 & year == 2016, 45),
    age = replace(age, id == 1101 & year == 2016, 30),
    age = replace(age, id == 186 & year == 2017, 35),
    age = replace(age, id == 558 & year == 2017, 46),
    age = replace(age, id == 728 & year == 2017, 45),
  )


clean_candy_complete <-
  jc_age_extras %>%
  mutate(
    country = replace(country, country %in% c(
      "usa", "US", "United States of America", "uSA", "united states", "United States", "us", "USSA", "U.S.A", "Murica", "USA!", "Usa", "U.S.", "Us", "Units States", "United states", "USA USA USA", "the best one - usa", "USA! USA! USA!",
      "Cascadia", "u.s.", "The Yoo Ess of Aaayyyyyy", "united states of america", "USA!!!!!!", "USA! USA!", "United Sates", "Sub-Canadian North America... 'Merica", "Trumpistan", "U.s.", 'Merica', "UNited States", "United Stetes", "america", "The republic of Cascadia", "USA USA USA USA", "United States of America", "United State", "United staes", "u.s.a.", "USAUSAUSA","US of A", "Unites States", "The United States", "Unied States", "U S", "The United States of America", "unite states", "cascadia", "USA? Hard to tell anymore..", "‘merica", "usas", "Pittsburgh", "New York", "California", "USa", "I pretend to be from Canada, but I am really from the United States.", "United Stated", "Ahem….Amerca", "New Jersey", "United ststes", "United Statss", "murrika", "USAA", "Alaska", "united States", "u s a", "United Statea", "united ststes", "USA USA USA!!!!", "U.S.A.", "USA (I think but it's an election year so who can really tell)", "America", "United States of America", "'merica", "Ahem....Amerca", "North Carolina", "United States of America ")
      ,"USA")
  )%>%
  mutate(
    country = replace(country, country %in% c(
      "canada", "Canada`", "Can", "CANADA")
      ,"Canada"
    )
  )%>% 
  mutate(
    country = replace(country, country %in% c(
      "uk", "United Kingdom", "England", "england", "United Kindom", "U.K.", "Uk", "Scotland", "United kingdom")
      ,"UK"
    )
  ) %>%
  mutate(
    country = replace(country, country %in% c(
      "Japan", "france", "A tropical island south of the equator", "Switzerland", "Korea", "belgium", "croatia", "Portugal", "españa", "Panama", "France", "Australia", "hungary", "Austria", "New Zealand", "Germany", "Mexico", "Brasil", "South Korea", "Philippines", "sweden", "The Netherlands", "Finland", "China", "germany", "kenya", "Netherlands", "netherlands", "UAE", "finland", "Europe", "Costa Rica", "Greece", "australia", "Canae", "Ireland", "South africa", "Iceland", "Denmark", "Indonesia", "Singapore", "Taiwan", "hong kong", "spain", "Sweden", "Hong Kong")
    ,"Other country") 
  ) %>% 
  mutate(
    country = replace(country, !country %in% c("Canada", "USA", "UK", "Other country")
    , NA
    )
  )

clean_candy_complete <-
  clean_candy_complete %>%
  select(id, year, country, age, gender, going_out, candy_type, rating) %>%
  arrange(year, id)

write_csv(clean_candy_complete, (here::here("clean_data/clean_candy_script.csv")))
