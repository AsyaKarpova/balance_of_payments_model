library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)
library(GenSA)
library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(zoo)
library(fable)
library(lubridate)


all_vars = import('data/vars_for_model.csv') %>% mutate(date = yearmonth(date))
View(all_vars)

exog = all_vars %>% select(date, brent, gas_lng, gas_europe, usd_eur,
                           v_prod_oil, v_prod_op, v_prod_gas, 
                           n_y, n_c, n_j, n_g, n_ds,
                           rate_repo, rate_10tr,
                           r_dum_cur_purch, em_index)
                           



long_exog = exog %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()
exog_tsb = long_exog %>% as_tsibble(index = date, key = series)

long_exog %>% group_by(series) %>% top_n(date, n=1)
# models

series_models = exog_tsb %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )


exog_forecasts = series_models %>% 
  forecast(h = "2 year")

exog_forecasts %>%
  filter(series %in% c('brent', 'gas_europe', 'gas_lng')) %>%
  autoplot(exog_tsb, level = NULL) +
  xlab("Year") + ylab("price")

exog_forecasts %>%
  filter(series %in% c('n_j', 'n_c', 'n_ds', 'n_g', 'n_y')) %>%
  autoplot(exog_tsb, level = NULL) +
  xlab("Year") + ylab("price")

exog_forecasts_naive = exog_forecasts %>% filter(.model == 'snaive') %>% as_tsibble() %>%
         select(-`.distribution`, -`.model`)

exog_full = rbind(exog_tsb, exog_forecasts_naive) %>% filter(year(date) < 2021)

