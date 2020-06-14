library(forecast)
library(rio)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(xtable)
library(patchwork)


data = import('data/data_month1.xlsx') %>%
  mutate(date = yearmonth(date))

# все колонки разделим на 1000, так они входят в модель
all_vars_quater = import('data/data_quarter_new.xlsx') %>%
  mutate_at(vars(-date), ~ . / 1000) %>%
  mutate(date = yearquarter(ymd(date)))

par_model = import('script/gensa_par.Rds') # параметры GenSa
par_model2 = import('script/par_model.Rds') # параметры из excel

# подсоединить все функции из скрипта functions.R
source('script/functions.R')


### exogenous vars for scenarios

exog = data %>% select(date, brent, gas_lng, gas_europe, usd_eur,
                       v_prod_oil, v_prod_op, v_prod_gas,
                       n_y, n_c, n_j, n_g, n_ds,
                       rate_repo, rate_10tr, r_price_cur_purch,
                       r_dum_cur_purch, em_index)

long_exog = exog %>% filter(year(date)==2019) %>%  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()
mean_val_2019 = long_exog %>% as_tibble()%>% ungroup() %>% group_by(series) %>%summarize(mean_val = mean(value))
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


#long_exog %>% group_by(series) %>% top_n(date, n=1)

# models

series_models = exog_tsb %>%
  model(
    snaive = SNAIVE(value ~ lag('year'))
    #ets = ETS(value),
    #arima = ARIMA(value)
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


# history and prediction
exog_full = full_join(exog_forecasts_naive, exog_tsb) #%>% filter(year(date) < 2021) %>% spread(series, value)

# all variables in long format
all_vars_tsb = data %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>%
  drop_na() %>%
  as_tsibble(index = date, key = series)
all_vars_tsb %>% filter(year(date) == 2019) %>% as_tibble() %>% group_by(series)  %>% summarise(value_2019 = sum(value)) %>% View()

all_vars = full_join(all_vars_tsb, exog_full) %>%
  filter(year(date) < 2021) %>% spread(series, value)

all_vars[is.na(all_vars$dum_1114),]['dum_1114'] = 1
all_vars[is.na(all_vars$dum_2012),]['dum_2012'] = 0


all_vars = filter(all_vars, year(date) <= 2019)
restore_all_ts = predict_bp(all_vars, par_model)
restored_data = restore_all_ts[[1]]
predictions= restore_all_ts[[2]]

autoplot(ts.union(real_data = ts(all_vars$r_cur_purch, start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_cur_purch, start = c(2006, 1), freq = 12)))+
                    ylab('value') + xlab('') + ggtitle('Currency purchase')
all_vars %>% select(brent) %>%tail(15)
predictions$r_hat_cur_purch

autoplot(ts.union(real_data = ts(all_vars$r_cur_account, start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_cur_acc, start = c(2006, 1), freq = 12)))+
  ylab('value') + xlab('') + ggtitle('Счет текущих операций')

autoplot(ts.union(real_data = ts(all_vars$rub_usd, start = c(2006, 1), freq = 12),
                  model = ts(predictions$hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')

# rub_usd


autoplot(ts.union(real_data = ts(all_vars$p_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(predictions$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('Средняя цена экспорта нефти')
autoplot(ts.union(real_data = ts(all_vars$r_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('Средняя выручка от экспорта нефти')


autoplot(ts.union(real_data = ts(all_vars$v_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(predictions$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil exported')

autoplot(ts.union(real_data = ts(all_vars$r_errors, start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Чистые ошибки и пропуски')


autoplot(ts.union(real_data = ts(all_vars$r_bal_fin, start = c(2006, 1), freq = 12),
                  model = ts(predictions$hat_fin_bal, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Чистые ошибки и пропуски')

autoplot(ts.union(real_data = ts(all_vars$r_dif_reserves, start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Чистые ошибки и пропуски')
