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

prices = import('data/data_month.xlsx')
prices_quater = import('data/data_quarter.xlsx')

# gas, op, oil monthly data until 2013Q12 (end_2013 obs.)
# export, import, n_' until 2018Q12 (end_2018 obs.)
# r_exp_serv, r_exp_all,r_imp_goods,r_imp_serv,r_imp_all and some r_bal_' from 2012Q1 (start from start_2012 index)

vars = prices %>% select(-'dum01':-'dum12', -'dum_2012', -'dum_1114', -'n_j':-'n_ds', -'n_y',-'n_c', -'n_g', -'vcor')


# prepare tsibble 

vars = mutate(vars, date = yearmonth(date))              
long_table = vars %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

series_tsb = long_table %>% as_tsibble(index = date, key = series)
series_tsb1 = series_tsb %>% fill_gaps()

series_models = series_tsb1 %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )

series_forecasts = series_models %>% 
  forecast(h = "3 month")
series_forecasts
series_forecasts$series%>%unique()

series_forecasts %>%
  filter(series %in% c('brent', 'gas_europe', 'gas_lng')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")

series_forecasts %>%
  filter(series %in% c('p_exp_gas', 'p_exp_op', 'p_exp_oil')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")



test = series_tsb1 %>% filter(date > yearmonth('2014-01-01') & date < yearmonth('2018-01-01'))

mape = series_models %>%
  accuracy() %>%
  arrange(MAPE)


snaive_fit = series_models %>% filter(.model == 'snaive')
arima_fit = series_models %>% filter(.model == 'arima')
ets_fit = series_models %>% filter(.model == 'ets')

snaive = series_forecasts %>% filter(.model == 'snaive')
arima = series_forecasts %>% filter(.model == 'arima')
ets = series_forecasts %>% filter(.model == 'ets')


export(mape, 'mape.csv')


long_table %>% filter(series == 'r_errors') %>% tail()
