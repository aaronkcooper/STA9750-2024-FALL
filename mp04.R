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

indices_longer <- indices |> 
  pivot_longer(cols = c(VTI, VEU, VBIRX, VBTLX), names_to = "ETF", values_to = "Return")
#Task 4
indices_longer |> 
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
indices_longer |> 
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

indices <- arrange(indices, date)

VTI_returns <- c(0, diff(indices$VTI) / head(indices$VTI, -1))
mean(VTI_returns)*1200
VEU_returns <- c(0, diff(indices$VEU) / head(indices$VEU, -1))
mean(VEU_returns)*1200
VBTLX_returns <- c(0, diff(indices$VBTLX) / head(indices$VBTLX, -1))
mean(VBTLX_returns)*1200
#Task 5
starting_salary <- 50000

orp <- function(start, wages, returns, asset_allocation = c(US_equities = 0.54, Intl_equities = 0.36, Bonds = 0.1, Short_term_debt = 0)) {
  returns$VTI_return <- c(0, diff(returns$VTI) / head(returns$VTI, -1))
  returns$VEU_return <- c(0, diff(returns$VEU) / head(returns$VEU, -1))
  returns$VBTLX_return <- c(0, diff(returns$VBTLX) / head(returns$VBTLX, -1))
  returns$VBIRX_return <- c(0, diff(returns$VBIRX) / head(returns$VBIRX, -1))
  months <- length(wages$date)
  salary <- numeric(months)
  salary[1] <- start
  for (i in 2:months) {
    salary[i] <- salary[i - 1] * (1 + (as.numeric(wages$value[i - 1]))/1200)
  }
  monthly <- salary / 12
  employee_rate <- sapply(salary, function(s) {
    if (s <= 45000) return(0.03)
    if (s <= 55000) return(0.035)
    if (s <= 75000) return(0.045)
    if (s <= 100000) return(0.0575)
    return(0.06)
  })
  employer_rate <- c(rep(.08, 7 * 12), rep(.1, (months - 7 * 12)))
  employee_contribution <- monthly * employee_rate
  employer_contribution <- monthly * employer_rate
  total_contribution <- employee_contribution + employer_contribution
  orp_balance <- numeric(months)
  for (i in 1:months) {
    growth_rate <- sum(
      returns$VTI_return[i] * asset_allocation["US_equities"],
      returns$VEU_return[i] * asset_allocation["Intl_equities"],
      returns$VBTLX_return[i] * asset_allocation["Bonds"],
      returns$VBIRX_return[i] * asset_allocation["Short_term_debt"]
    )
    if (i == 1) {
      orp_balance[i] <- total_contribution[i]
    } else {
      orp_balance[i] <- (orp_balance[i - 1]*(1+ growth_rate) + total_contribution[i]) 
    }
  }
  return(orp_balance[months])
}
orp_returns <- orp(50000, filter(wages, date >= "2007-04-30"), indices)

trs <- function(start, wages, inflation, retirement_years) {
  months <- length(wages$date)
  service_years <- months %/% 12
  salary <- numeric(months)
  salary[1] <- start
  for (i in 2:months) {
    salary[i] <- salary[i - 1] * (1 + (as.numeric(wages$value[i - 1]))/1200)
  }
  cpi <- as.numeric(inflation$value)
  final_3_years <- salary[(months - 35):months]  
  FAS <- mean(final_3_years)
  
  if (service_years < 20) {
    benefit_rate <- 0.0167 * service_years
  } else if (service_years == 20) {
    benefit_rate <- 0.0175 * service_years
  } else {
    benefit_rate <- 0.35 + 0.02 * service_years
  }
  annual_benefit <- FAS * benefit_rate 
  monthly_benefit <- annual_benefit / 12
  
  inflation_rate <- numeric(months)
  for (i in 13:months) {
    inflation_rate[i] <- (cpi[i] - cpi[i - 12]) / cpi[i - 12]
  }
  
  trs_balance <- 0
  inflation_adjusted_benefit <- monthly_benefit
  for (year in 1:retirement_years) {
    if (retirement_years == 0) {
      return(monthly_benefit*12)
    }
    if (year == 1) {
      inflation_rate <- max(0.01, min(0.03, mean(cpi[1:12]) / 100))
    } else {
      inflation_rate <- max(0.01, min(0.03, mean(cpi[((year - 1) * 12 + 1):(year * 12)]) / 100))
    }
    
    # Adjust benefit for inflation
    inflation_adjusted_benefit <- inflation_adjusted_benefit * (1 + inflation_rate)
    
    # Add 12 months of benefit payments
    trs_balance <- trs_balance + inflation_adjusted_benefit * 12
  }
  
  return(trs_balance)
}
trs(50000, filter(wages, date >= "2007-04-30"), inflation, 0)/12

#Task 6
# retirement_years - 15
trs15 <- trs(50000, filter(wages, date >= "2007-04-30"), inflation, 15)

trs15/12
simulate_orp_withdrawals <- function(
    initial_balance, 
    annual_withdrawal, 
    average_returns = c(US_equities = 0.07, Intl_equities = 0.04, Bonds = 0.02, Short_term_debt = .01),
    asset_allocation = c(US_equities = 0.54, Intl_equities = 0.36, Bonds = 0.1, Short_term_debt = 0),
    inflation_rate = 0.025,
    retirement_years = 15
) {
  # Number of months in retirement
  months <- retirement_years * 12
  
  # Convert annual growth and inflation rates to monthly rates
  monthly_growth_rate <- sum(
    average_returns["US_equities"] * asset_allocation["US_equities"],
    average_returns["Intl_equities"] * asset_allocation["Intl_equities"],
    average_returns["Bonds"] * asset_allocation["Bonds"]
  )
  monthly_growth_rate <- (1 + monthly_growth_rate)^(1 / 12) - 1
  monthly_inflation_rate <- (1 + inflation_rate)^(1 / 12) - 1
  
  # Initialize vectors to track balances and withdrawals
  orp_balance <- numeric(months)
  withdrawals <- numeric(months)
  
  # Set initial values
  orp_balance[1] <- initial_balance
  withdrawals[1] <- annual_withdrawal / 12
  
  # Simulate withdrawals and returns over retirement
  for (i in 2:months) {
    # Adjust withdrawal for inflation annually
    if (i %% 12 == 1) {
      withdrawals[i] <- withdrawals[i - 1] * (1 + inflation_rate)
    } else {
      withdrawals[i] <- withdrawals[i - 1]
    }
    
    # Update ORP balance: apply growth, then subtract withdrawal
    orp_balance[i] <- orp_balance[i - 1] * (1 + monthly_growth_rate) - withdrawals[i]
    
    # Prevent negative balance
    if (orp_balance[i] < 0) {
      orp_balance[i] <- 0
      withdrawals[i] <- 0 # No more withdrawals if the balance is exhausted
      break
    }
  }
  
  # Return a list of results
  return(list(
    final_balance = orp_balance[months],
    total_withdrawn = sum(withdrawals),
    funds_exhausted = any(orp_balance == 0),
    withdrawals = withdrawals,
    balances = orp_balance
  ))
}
orp_simulation <- simulate_orp_withdrawals(290000, .04*290000)
orp_simulation
#Task 7
adjust_allocation <- function(age) {
  if (age >= 25 & age <= 49) {
    asset_allocation <- c(US_equities = 0.54, Intl_equities = 0.36, Bonds = 0.10, Short_term_debt = 0)
  } else if (age >= 50 & age <= 59) {
    asset_allocation <- c(US_equities = 0.47, Intl_equities = 0.32, Bonds = 0.21, Short_term_debt = 0)
  } else if (age >= 60 & age <= 74) {
    asset_allocation <- c(US_equities = 0.34, Intl_equities = 0.23, Bonds = 0.43, Short_term_debt = 0)
  } else {
    asset_allocation <- c(US_equities = 0.19, Intl_equities = 0.13, Bonds = 0.62, Short_term_debt = 0.06)
  }
  return(asset_allocation)
}
orp_random <- function(start, wages, asset_allocation) {
  months <- length(wages$date)
  salary <- numeric(months)
  salary[1] <- start
  for (i in 2:months) {
    salary[i] <- salary[i - 1] * (1 + (as.numeric(wages$value[i - 1]))/1200)
  }
  monthly <- salary / 12
  employee_rate <- sapply(salary, function(s) {
    if (s <= 45000) return(0.03)
    if (s <= 55000) return(0.035)
    if (s <= 75000) return(0.045)
    if (s <= 100000) return(0.0575)
    return(0.06)
  })
  employer_rate <- c(rep(.08, 7 * 12), rep(.1, (months - 7 * 12)))
  employee_contribution <- monthly * employee_rate
  employer_contribution <- monthly * employer_rate
  total_contribution <- employee_contribution + employer_contribution
  orp_balance <- numeric(months)
  for (i in 1:months) {
    growth_rate <- sum(
      runif(1, -.15, .15) * asset_allocation["US_equities"],
      runif(1, -.1, .12) * asset_allocation["Intl_equities"],
      runif(1, -.01, .02) * asset_allocation["Bonds"],
      runif(1, -.005, .015) * asset_allocation["Short_term_debt"]
    )
    if (i == 1) {
      orp_balance[i] <- total_contribution[i]
    } else {
      orp_balance[i] <- (orp_balance[i - 1]*(1+ growth_rate) + total_contribution[i]) 
    }
  }
  return(orp_balance[months])
}
simulate_orp_withdrawals_random <- function(
    initial_balance, 
    annual_withdrawal, 
    asset_allocation = c(US_equities = 0.54, Intl_equities = 0.36, Bonds = 0.1, Short_term_debt = 0),
    inflation_rate = 0.025,
    retirement_years = 15
) {
  # Number of months in retirement
  months <- retirement_years * 12
  
  # Convert annual growth and inflation rates to monthly rates
  monthly_growth_rate <- sum(
    runif(1, -.15, .15) * asset_allocation["US_equities"],
    runif(1, -.1, .12) * asset_allocation["Intl_equities"],
    runif(1, -.01, .02) * asset_allocation["Bonds"],
    runif(1, -.005, .015) * asset_allocation["Short_term_debt"]
  )
  monthly_growth_rate <- (1 + monthly_growth_rate)^(1 / 12) - 1
  monthly_inflation_rate <- (1 + inflation_rate)^(1 / 12) - 1
  
  # Initialize vectors to track balances and withdrawals
  orp_balance <- numeric(months)
  withdrawals <- numeric(months)
  
  # Set initial values
  orp_balance[1] <- initial_balance
  withdrawals[1] <- annual_withdrawal / 12
  
  # Simulate withdrawals and returns over retirement
  for (i in 2:months) {
    # Adjust withdrawal for inflation annually
    if (i %% 12 == 1) {
      withdrawals[i] <- withdrawals[i - 1] * (1 + inflation_rate)
    } else {
      withdrawals[i] <- withdrawals[i - 1]
    }
    
    # Update ORP balance: apply growth, then subtract withdrawal
    orp_balance[i] <- orp_balance[i - 1] * (1 + monthly_growth_rate) - withdrawals[i]
    
    # Prevent negative balance
    if (orp_balance[i] < 0) {
      orp_balance[i] <- 0
      withdrawals[i] <- 0 # No more withdrawals if the balance is exhausted
      break
    }
  }
  
  # Return a list of results
  return(list(
    final_balance = orp_balance[months],
    total_withdrawn = sum(withdrawals),
    funds_exhausted = any(orp_balance == 0),
    withdrawals = withdrawals,
    balances = orp_balance
  ))
}
bootstrap_orp <- function(wages, n_bootstrap, asset_allocation_func) {
  set.seed(123)  # for reproducibility
  bootstrap_results <- numeric(n_bootstrap)
  avg_monthly_income <- numeric(n_bootstrap)
  exhaustion_count <- 0
  for (i in 1:n_bootstrap) {
    # Randomly sample a starting salary between $40,000 and $120,000
    start_salary <- runif(1, min = 40000, max = 120000)
    # Randomly sample a starting age between 22 and 63
    age_at_start <- sample(22:63, 1)
    # Assume death age is 80
    death_age <- 80
    # Calculate the ORP balance for each resample
    orp_balance_working <- orp_random(start_salary, sampled_wages,
                                 asset_allocation_func(age_at_start))
    retirement_years <- death_age - age_at_start - 17
    if (retirement_years > 0) {
      orp_simulation <- simulate_orp_withdrawals_random(orp_balance_working, .04*orp_balance_working, retirement_years = retirement_years)
      bootstrap_results[i] <- orp_simulation$final_balance
      avg_monthly_income[i] <- bootstrap_results[i]/retirement_years/12
      if (orp_simulation$final_balance == 0) {
        exhaustion_count <- exhaustion_count + 1
      }
    }
    else {
      bootstrap_results[i] <- orp_balance_working
      avg_monthly_income[i] <- 0
    }
  }
  
  # Return a summary of the bootstrap results
  return(list(
    mean = mean(bootstrap_results),
    lower_ci = quantile(bootstrap_results, 0.025),  # 95% CI lower bound
    upper_ci = quantile(bootstrap_results, 0.975),  # 95% CI upper bound
    exhaustion_percentage = exhaustion_count / n_bootstrap,
    results = bootstrap_results,
    monthly_income = avg_monthly_income
  ))
}


bootstrap_orp_results <- bootstrap_orp(filter(wages, date >= "2007-04-30"), 200, adjust_allocation)
print(bootstrap_orp_results)

bootstrap_trs <- function(wages, inflation, n_bootstrap) {
  set.seed(123)  # for reproducibility
  bootstrap_results <- numeric(n_bootstrap)
  avg_monthly_income <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    # Resample wages and inflation with replacement
    start_salary <- runif(1, min = 40000, max = 120000)
    sampled_wages <- wages[sample(1:nrow(wages), nrow(wages), replace = TRUE), ]
    sampled_inflation <- inflation[sample(1:nrow(inflation), nrow(inflation), replace = TRUE), ]
    # Randomly sample a starting age between 22 and 60
    age_at_start <- sample(22:63, 1)
    # Assume death age is 80
    death_age <- 80
    # Calculate the TRS balance for each resample
    retirement_years <- death_age - age_at_start - 17
    bootstrap_results[i] <- trs(start_salary, sampled_wages, sampled_inflation, retirement_years)
    avg_monthly_income[i] <- bootstrap_results[i]/retirement_years/12
  }
  
  # Return a summary of the bootstrap results
  return(list(
    mean = mean(bootstrap_results),
    lower_ci = quantile(bootstrap_results, 0.025),  # 95% CI lower bound
    upper_ci = quantile(bootstrap_results, 0.975),  # 95% CI upper bound
    results = bootstrap_results,
    monthly_income = avg_monthly_income
  ))
}

# Example Usage:
bootstrap_trs_results <- bootstrap_trs(filter(wages, date >= "2007-04-30"), inflation, 200)
print(bootstrap_trs_results)

