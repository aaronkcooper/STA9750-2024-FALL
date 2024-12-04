library(httr2)
library(tidyverse)
library(scales)
#Task 1-2
api <- read_lines("api.txt")
#Task 3
alphavantage_req <- request("https://www.alphavantage.co/query") |>
  req_url_query(`function` = "TIME_SERIES_MONTHLY_ADJUSTED",
                symbol = "VTI",
                apikey = api[1],
                datatype = "csv")
resp <- req_perform(alphavantage_req)
csv <- resp |> resp_body_string()
VTI <- read.csv(text = csv)
fred_req <- request("https://api.stlouisfed.org/fred/series/observations") |>
  req_url_query(series_id = "FRBATLWGT12MMUMHGO",
                api_key = api[2],
                file_type = "json")
resp2 <- req_perform(fred_req)
json <- resp2 |> resp_body_json()
wages <- do.call(rbind, lapply(json$observations, as.data.frame))
wages <- select(wages, c("date", "value"))
alphavantage_req2 <- request("https://www.alphavantage.co/query") |>
  req_url_query(`function` = "TIME_SERIES_MONTHLY_ADJUSTED",
                symbol = "VBTLX",
                apikey = api[1],
                datatype = "csv")
resp3 <- req_perform(alphavantage_req2)
csv2 <- resp3 |> resp_body_string()
VBTLX <- read.csv(text = csv2)
alphavantage_req3 <- request("https://www.alphavantage.co/query") |>
  req_url_query(`function` = "TIME_SERIES_MONTHLY_ADJUSTED",
                symbol = "VEU",
                apikey = api[1],
                datatype = "csv")
resp4 <- req_perform(alphavantage_req3)
csv3 <- resp4 |> resp_body_string()
VEU <- read.csv(text = csv3)
alphavantage_req4 <- request("https://www.alphavantage.co/query") |>
  req_url_query(`function` = "TIME_SERIES_MONTHLY_ADJUSTED",
                symbol = "VBIRX",
                apikey = api[1],
                datatype = "csv")
resp5 <- req_perform(alphavantage_req4)
csv4 <- resp5 |> resp_body_string()
VBIRX <- read.csv(text = csv4)
fred_req2 <- request("https://api.stlouisfed.org/fred/series/observations") |>
  req_url_query(series_id = "CPIAUCSL",
                api_key = api[2],
                file_type = "json")
resp6 <- req_perform(fred_req2)
json2 <- resp6 |> resp_body_json()
inflation <- do.call(rbind, lapply(json2$observations, as.data.frame))
inflation <- select(inflation, c("date", "value"))

wages <- wages |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))

inflation <- inflation |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))

VTI <- VTI |> 
  rename(date = timestamp) |> 
  select(date, close) |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))
VEU <- VEU |> 
  rename(date = timestamp) |> 
  select(date, close) |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))
VBIRX <- VBIRX |> 
  rename(date = timestamp) |> 
  select(date, close) |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))
VBTLX <- VBTLX |> 
  rename(date = timestamp) |> 
  select(date, close) |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = ceiling_date(date, unit = "month") - days(1))

fred_data <- inner_join(wages, inflation, by = "date")
fred_data <- fred_data |> 
  rename(wages_pct_change = value.x,
         CPI = value.y)

indices <- inner_join(VTI, VEU, by = "date")
indices <- indices |> 
  rename(VTI = close.x,
         VEU = close.y)
indices <- inner_join(indices, VBIRX, by = "date")
indices <- indices |> 
  rename(VBIRX = close)
indices <- inner_join(indices, VBTLX, by = "date")
indices <- indices |> 
  rename(VBTLX = close)

indices <- indices |> 
  pivot_longer(cols = c(VTI, VEU, VBIRX, VBTLX), names_to = "ETF", values_to = "Return")
#Task 4
indices |> 
  filter(ETF == "VEU" | ETF == "VTI") |> 
  ggplot(aes(date, Return, color = ETF)) +
    geom_line(size = 1.2, alpha = .8) +
    labs(
      x = "Date",
      y = "Cumulative Returns",
      title = "US Returns vs International Returns"
    ) +
    scale_y_continuous(labels = dollar_format()) +
    theme_minimal() +
    theme(legend.position = "bottom")
indices |> 
  filter(ETF == "VBIRX" | ETF == "VBTLX") |> 
  ggplot(aes(date, Return, color = ETF)) +
  geom_line(size = 1.2, alpha = .8) +
  labs(
    x = "Date",
    y = "Cumulative Returns",
    title = "Short Debt vs Bond Market Returns"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(legend.position = "bottom")

fred_data <- fred_data |> 
  mutate(wages_pct_change = as.numeric(wages_pct_change),
         CPI = as.numeric(CPI),
         CPI_pct_change = (CPI - lag(CPI))/lag(CPI)*100)
fred_data <- fred_data |> 
  mutate(CPI_pct_change = ifelse(is.na(CPI_pct_change), 0, CPI_pct_change),
         wages_monthly_change = ((1 + wages_pct_change / 100)^(1/12) - 1) * 100)
cor(fred_data$wages_monthly_change, fred_data$CPI_pct_change)

salary <- fred_data |> 
  mutate(new_salary = 161 * cumprod(1 + wages_monthly_change / 100))
cor(salary$new_salary, salary$CPI)

#Task 5
starting_salary <- 50000

orp <- function(start, wages, returns, asset_allocation = c(US_equities = 0.54, Intl_equities = 0.36, Bonds = 0.1)) {
  months <- length(wages$date)
  salary <- numeric(months)
  salary[1] <- start
  for (i in 2:months) {
    salary[i] <- salary[i - 1] * (1 + wage_growth[i - 1])
  }
  employee_rate <- sapply(salary, function(s) {
    if (s <= 45000) return(0.03)
    if (s <= 55000) return(0.035)
    if (s <= 75000) return(0.045)
    if (s <= 100000) return(0.0575)
    return(0.06)
  })
  
}
