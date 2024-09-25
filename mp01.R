if(!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)
if(!file.exists("2022_fare_revenue.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_fare_revenue.xlsx" in your project
  # directory.
  download.file("http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx", 
                destfile="2022_fare_revenue.xlsx", 
                quiet=FALSE, 
                method="wget")
}
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`,       # Sum over different `TOS` for the same `Mode`
           `Agency Name`,  # These are direct operated and sub-contracted 
           `Mode`) |>      # of the same transit modality
  # Not a big effect in most munis (significant DO
  # tends to get rid of sub-contractors), but we'll sum
  # to unify different passenger experiences
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()

# Next, expenses
if(!file.exists("2022_expenses.csv")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_expenses.csv" in your project
  # directory.
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE, 
                method="wget")
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

# Monthly Transit Numbers
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "ridership.xlsx" in your project
  # directory.
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE, 
                method="wget")
}
TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

#Task 1

USAGE <- rename(USAGE, metro_area = `UZA Name`)

distinct(USAGE, Mode)

# Task 2
USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "HR" ~ "Heavy Rail", 
    Mode == "LR" ~ "Light Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail/Automated Guideway",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "CC" ~ "Cable Car",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "PB" ~ "Publico",
    TRUE ~ "Unknown"))

USAGE <- USAGE |> 
  select(-`3 Mode`) |> 
  rename(`Unlinked Passenger Trips` = UPT,
         `Vehicle Revenue Miles` = VRM)

#Task 3
#1
USAGE |> 
  group_by(Agency) |> 
  summarize(Total_Miles = sum(`Vehicle Revenue Miles`)) |> 
  arrange(desc(Total_Miles))
#MTA
#2
USAGE |> 
  group_by(Mode) |> 
  summarize(Total_Miles = sum(`Vehicle Revenue Miles`)) |> 
  arrange(desc(Total_Miles))
#Mode
#3
USAGE |> 
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month == as.Date("2024-05-01")) |> 
  select(`Unlinked Passenger Trips`)
# 180458819
#5
April2019 <- USAGE |> 
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month == as.Date("2019-04-01"))
April2020 <- USAGE |> 
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month == as.Date("2020-04-01"))
USAGE |> 
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month >= as.Date("2019-04-01"), 
         month <= as.Date("2020-04-01")) |> 
  ggplot(aes(month, `Unlinked Passenger Trips`/1000000, label = round(`Unlinked Passenger Trips`/1000000))) +
  geom_line(color = "red") +
  geom_text(data = April2019, nudge_y = 5) +
  geom_text(data = April2020, nudge_x = 5, nudge_y = 5) +
  labs(title = "MTA Ridership April 2019 - April 2020", x = "Month", y = "Trips in Millions")
#Task 4
#Find Three Interesting Facts
USAGE |> 
  group_by(metro_area) |> 
  summarise(Trips = sum(`Unlinked Passenger Trips`)/1000000) |> 
  arrange(desc(Trips)) |> 
  slice_head(n = 10) |> 
  ggplot(aes(Trips, reorder(factor(metro_area, levels = metro_area), Trips))) +
  geom_bar(stat = "identity") +
  labs(x = "Trips in Millions", y = "Metro Area") +
  theme_minimal()
# Top ridership doesn't necessarily follow the biggest cities in the country
USAGE |> 
  filter(metro_area == "Chicago, IL--IN" | metro_area == "Washington--Arlington, DC--VA--MD") |> 
  filter(Mode == "Bus" | Mode == "Heavy Rail") |> 
  mutate(year = year(month)) |> 
  group_by(Mode, year, metro_area) |> 
  summarise(Trips = sum(`Unlinked Passenger Trips`)/1000000) |> 
  arrange(desc(Trips)) |> 
  ggplot(aes(year, Trips)) +
  geom_line(aes(color = Mode, linetype = metro_area)) +
  labs(y = "Trips in Millions") +
  theme_minimal() 
# Interesting that after pandemic in DC the Bus and heavy rail ridership is equal wheras pre pandemic heavy rail
# far exceeded bus

USAGE |> 
  filter(Mode == "Ferryboat") |> 
  group_by(metro_area) |> 
  summarise(Trips = sum(`Unlinked Passenger Trips`)) |> 
  arrange(desc(Trips))
#Interesting which cities have the most ferry trips, small cities like Portland Maine make it really high on the list.

#Task 5
USAGE_2022_ANNUAL <- USAGE |> 
  mutate(year = year(month)) |> 
  group_by(year, `NTD ID`, Agency, metro_area, Mode) |> 
  summarize(UPT = sum(UPT), VRM = sum(VRM)) |> 
  ungroup() |> 
  filter(year == 2022) |> 
  select(-year)
USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, 
                                  FINANCIALS, 
                                  join_by(`NTD ID`, Mode)) |>
  drop_na()

#Task 6
#1
USAGE_AND_FINANCIALS <- filter(USAGE_AND_FINANCIALS, UPT >= 400000)
arrange(USAGE_AND_FINANCIALS, desc(UPT))
#MTA Heavy Rail
#2
USAGE_AND_FINANCIALS |> 
  mutate(Farebox_recovery = `Total Fares`/Expenses) |> 
  arrange(desc(Farebox_recovery))
#Ferryboat Port Imperial Ferry Corporation
#3
USAGE_AND_FINANCIALS |> 
  mutate(Expenses_PerUPT = Expenses/UPT) |> 
  arrange(Expenses_PerUPT)
#Bus North Carolina State University
#4
USAGE_AND_FINANCIALS |> 
  mutate(Fares_PerUPT = `Total Fares`/UPT) |> 
  arrange(desc(Fares_PerUPT))
#CommuterBus Altoona Hampton Jitney, Inc.
#5
USAGE_AND_FINANCIALS |> 
  mutate(Expenses_PerVRM = Expenses/VRM) |> 
  arrange(Expenses_PerVRM)
#VanPool Metropolitan Transportation Commission
#6
USAGE_AND_FINANCIALS |> 
  mutate(Fares_PerVRM = `Total Fares`/VRM) |> 
  arrange(desc(Fares_PerVRM))
#FerryBoat Jacksonville Transportation Authority

#I would probably say the most effiecent is the one with the best farebox recovery so the
# Port Imperial Ferry Corporation