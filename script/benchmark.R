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

vars = prices %>%
    select(-'dum01':-'dum12', -'dum_2012', -'dum_1114', -'n_j':-'n_ds', -'n_y',-'n_c', -'n_g', -'vcor')


# prepare tsibble 

vars = mutate(vars, date = yearmonth(date))  

long_table = vars %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

series_tsb = long_table %>% as_tsibble(index = date, key = series)
series_tsb1 = series_tsb %>% fill_gaps()

test_data = series_tsb1 %>% group_by(series) %>% filter(date >= as.Date(max(date)) - months(12))
train_data = series_tsb1 %>% group_by(series) %>% filter(date < as.Date(max(date)) - months(12))

# models

series_models = train_data %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )

series_forecasts = series_models %>% 
  forecast(h = "1 year")

series_forecasts %>%
  filter(series %in% c('brent', 'gas_europe', 'gas_lng')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")

series_forecasts %>%
  filter(series %in% c('p_exp_gas', 'p_exp_op', 'p_exp_oil')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")

series_forecasts %>%
  filter(series %in% c('r_bal_fin', 'r_errors', 'r_dif_reserves')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")


mape = series_forecasts %>%
  accuracy(test_data) %>%
  arrange(MAPE)



snaive_fit = series_models %>% filter(.model == 'snaive')
arima_fit = series_models %>% filter(.model == 'arima')
ets_fit = series_models %>% filter(.model == 'ets')

snaive = series_forecasts %>% filter(.model == 'snaive')
arima = series_forecasts %>% filter(.model == 'arima')
ets = series_forecasts %>% filter(.model == 'ets')

View(mape)
export(mape, 'mape.csv')








# these series are from 2006 to the end of 2013

fuels = series_tsb1 %>% filter(series %in% c('p_exp_oil', 'p_exp_gas', 'p_exp_op',
                                            'v_exp_oil', 'v_exp_gas', 'v_exp_op',
                                            'r_exp_oil', 'r_exp_gas', 'r_exp_op',
                                            'r_exp_othg'))


fuels_train = fuels %>% filter(year(date) < 2012)

fuels_models = fuels_train %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )

fuels_forecasts = fuels_models %>% 
  forecast(h = "1 year")

fuels_forecasts %>%
  autoplot(fuels, level = NULL)

acc_fuels = accuracy(fuels_forecasts, fuels) %>% arrange(MAPE)


# models 2 (data from 2006 to may 2019)
data2 = series_tsb1 %>% filter(series %in%  c("brent", "em_index", "gas_europe", 
                                              "gas_lng", "rub_usd", "usd_eur"))


data2_train = data2 %>% filter(year(date) < 2019)

data2_models = data2_train %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )

data2_forecasts = data2_models %>% 
  forecast(h = "1 year")

data2_forecasts %>%
  autoplot(data2, level = NULL)

acc_data2 = accuracy(data2_forecasts, data2) %>% arrange(MAPE)

# models 3 (data from 2012 until december 2018)
data3 = series_tsb1 %>% filter(series %in%  c("r_bal_fin", "r_bal_inv", "r_bal_rent", 
                                              "r_bal_serv", "r_bal_sinc", "r_bal_wage", "r_cap_account", "r_cur_account", 
                                              "r_dif_reserves", "r_errors", "r_exp_all", "r_exp_serv", "r_imp_all", 
                                              "r_imp_serv"))


data3_train = data3 %>% filter(year(date) < 2018)

data3_models = data3_train %>% 
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value), 
    arima = ARIMA(value)
  )

data3_forecasts = data3_models %>% 
  forecast(h = "1 year")

data3_forecasts %>%
  autoplot(data3, level = NULL)

acc_data3 = accuracy(data3_forecasts, data3) %>% arrange(MAPE)

long_table %>% group_by(series) %>% filter(date >= max(date)-365)

a = long_table %>% ungroup() %>% count(series) %>% filter(n==84) %>% select(series)
