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

data = import('data/data_month1.xlsx')
prices_quater = import('data/data_quarter_new.xlsx')

# gas, op, oil monthly data until 2013Q12 (end_2013 obs.)
# export, import, n_' until 2018Q12 (end_2018 obs.)
# r_exp_serv, r_exp_all,r_imp_goods,r_imp_serv,r_imp_all and some r_bal_' from 2012Q1 (start from start_2012 index)

# prepare tsibble

endog = data %>% select(-c(brent, gas_lng, gas_europe, usd_eur,
                           v_prod_oil, v_prod_op, v_prod_gas,
                           n_y, n_c, n_j, n_g, n_ds, n_ex, n_im, dum_2012, dum_1114,
                           rate_repo, rate_10tr, r_price_cur_purch,
                           r_dum_cur_purch, em_index, dum01:dum12, vcor)) %>% mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc)

endog = mutate(endog, date = yearmonth(date))  %>%filter(year(date) <=2019)

long_table = endog %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

series_tsb = long_table %>% as_tsibble(index = date, key = series)
series_tsb1 = series_tsb %>% fill_gaps()

test_data = series_tsb1 %>% group_by(series) %>% filter(date >= as.Date(max(date)) - months(12))
train_data = series_tsb1 %>% group_by(series) %>% filter(date < as.Date(max(date)) - months(12))


#test_data = series_tsb1 %>% group_by(series) %>% filter(year(date) == 2019)
#train_data = series_tsb1 %>% group_by(series) %>% filter(year(date) < 2019)

# models
train_data %>% filter(series=='r_cur_purch') %>% tail(15)
series_models = train_data %>%
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value),
    arima = ARIMA(value),
    rw = RW(value)
  )
#series_models %>% filter(series=='r_cur_purch') %>% pull(snaive) %>% .[[1]] %>% report()

series_forecasts = series_models %>%
  forecast(h = "1 year")
strange_fcst = series_forecasts %>% filter(.model=="arima", series == "r_cur_purch")
strange_fcst

series_forecasts %>%
  filter(series %in% c('r_bal_inv', 'r_imp_serv', 'p_exp_gas')) %>%
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
  accuracy(series_tsb1) %>%
  arrange(MAPE)


test_data %>% filter(series == "r_cur_purch")
mase = mape %>% select(.model, series, MAE)
mase
mase_l = mase %>% pivot_longer(cols = MAE)
mase_rw = mase_l %>%filter(.model == 'rw') %>% rename(value_rw = value)  %>% select(series, value_rw)

mase_final = left_join(mase, mase_rw) %>% mutate(mase = MAE / value_rw) %>%filter(.model != 'rw')
mase_final
series_models


snaive = series_forecasts %>% filter(.model == 'snaive')
arima = series_forecasts %>% filter(.model == 'arima')
ets = series_forecasts %>% filter(.model == 'ets')
snaive%>%filter(series == 'r_bal_rent') %>%View()
View(mape)
export(mase_final, 'mase_bench1.Rds')

snaive







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


mase_b%>%View()
mase_our = import('mase_table_pse.Rds')
mase_our$.model = 'bp_model'
mase_b = import('mase_bench.Rds')

all_metrics = bind_rows(mase_our, mase_b) %>% select(series, mase, `.model`)
all_metr = all_metrics %>% spread( key = '.model', value = 'mase') %>% filter(series != 'aq_assets', series != 'aq_obl', series!= 'r_bal_sinc', series != 'r_bal_rent', series != 'r_cap_account')

library(xtable)
a = xtable(all_metr, caption = 'MASE')
b = printbold(a, each = 'row')
