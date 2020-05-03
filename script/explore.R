library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)
library(GenSA)
library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(rlang)
library(feasts)
library(fable)
library(lubridate)


data = import('data/data_month.xlsx') %>%
  mutate(date = yearmonth(date))
all_vars_quater = import('data/data_quarter.xlsx')
par_model = import('script/gensa_par.Rds')
source('script/functions.R')


### exogenous vars for scenarios

exog = data %>% select(date, brent, gas_lng, gas_europe, usd_eur,
                       v_prod_oil, v_prod_op, v_prod_gas,
                       n_y, n_c, n_j, n_g, n_ds,
                       rate_repo, rate_10tr, r_price_cur_purch,
                       r_dum_cur_purch, em_index, dum01:dum12, vcor)

endog = data %>% select(-c(brent, gas_lng, gas_europe, usd_eur,
                           v_prod_oil, v_prod_op, v_prod_gas,
                           n_y, n_c, n_j, n_g, n_ds,
                           rate_repo, rate_10tr, r_price_cur_purch,
                           r_dum_cur_purch, em_index, dum01:dum12, vcor))

long_exog = exog %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

long_endog = endog %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

exog_tsb = long_exog %>% as_tsibble(index = date, key = series)
endog_tsb = long_endog %>% as_tsibble(index = date, key = series)

endog_tsb %>% features(value, unitroot_kpss)
#long_exog %>% group_by(series) %>% top_n(date, n=1)

# models
