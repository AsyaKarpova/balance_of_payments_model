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
data_quater = import('data/data_quarter_new.xlsx')
all_vars_quater = mutate_at(data_quater, vars(-date), ~ . / 1000)



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

test_data = series_tsb1 %>% group_by(series) %>% filter(year(date) == 2018)#filter(date >= as.Date(max(date)) - months(11))
train_data = series_tsb1 %>% group_by(series) %>% filter(year(date) <= 2017)


#test_data = series_tsb1 %>% group_by(series) %>% filter(year(date) == 2019)
#train_data = series_tsb1 %>% group_by(series) %>% filter(year(date) < 2019)

# models

series_models = train_data %>%
  model(
    snaive = SNAIVE(value ~ lag('year')),
    ets = ETS(value),
    arima = ARIMA(value),
    rw = RW(value)
  )
library(xtable)
series_models%>%select(series, arima)%>%as_tibble() %>%xtable()
#series_models %>% filter(series=='r_cur_purch') %>% pull(snaive) %>% .[[1]] %>% report()
train_data%>% filter(year(date) == 2018) %>%filter(value == 0) %>%View()

series_forecasts = series_models %>%
  forecast(h = "1 year")
series_forecasts %>%filter(series == 'r_bal_rent_sinc')
#### block for mase calculation from package metrics
get_mase = function(the_model, the_series) {
  .yhat = filter(series_forecasts, .model == the_model,
                 series == the_series) %>% pull(value)
  .ytrue = filter(series_tsb1, series == the_series) %>% tail(12)%>%
    pull(value)
  return(mase(.ytrue, .yhat))
}
get_mase("ets", "aq_assets")
get_mase("snaive", "p_exp_gas")
mase(c(0,0,0), c(0,0,0))

.mase_table = cross_df(list(model = unique(series_forecasts$.model),
                            series = unique(series_forecasts$series)))
.mase_table
.mase_table = mutate(.mase_table,
                     mase = map2_dbl(model, series, ~ get_mase(.x, .y)))
.mase_table


series_forecasts %>%
  filter(series %in% c('r_bal_rent_sinc')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")

series_forecasts %>%
  filter(series %in% c('p_exp_gas', 'p_exp_op', 'p_exp_oil')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")


series_forecasts %>%
  filter(series %in% c('r_bal_fin', 'r_errors')) %>%
  autoplot(series_tsb1, level = NULL) +
  xlab("Year") + ylab("price")


mape = series_forecasts %>%
  accuracy(series_tsb1) %>%
  arrange(MAPE)
mape
mape = mape %>% select(.model, series, MAPE)
mape %>% gather(-series, key = 'model', value = MAPE)
mape = rename(mape, 'model' = '.model')
.mase_table
mape_mase_bench = left_join(.mase_table, mape)
mape_mase_bench %>%View()

export(mape_mase_bench, 'mape_mase_bench_2018.Rds')

snaive



forecast()




mase_b%>%View()
mase_our = import('true_mape_our_2018.Rds')
mase_our$model = 'bp_model'

mase_bench = import('mape_mase_bench_2018.Rds') %>%rename('mape' = 'MAPE') %>% mutate(mape = mape/100)
mase_bench %>%View()
all_metrics = bind_rows(mase_our, mase_bench) %>% select(series, mape, `model`)

all_metr = all_metrics %>% spread(key = 'model', value = mape) %>% filter(series != 'aq_assets', series != 'aq_obl', series!= 'r_bal_sinc', series != 'r_bal_rent', series != 'r_cap_account')
all_metr = all_metr %>% select(series, bp_model, arima, ets, snaive)
all_metr %>%View()

all_metrics2 = bind_rows(mase_our, mase_bench) %>% select(series, mase, `model`)
all_metrics2 %>%View()
?spread
all_metr2 = all_metrics2 %>% spread(key = 'model', value = mase) %>% filter(series != 'aq_assets', series != 'aq_obl', series!= 'r_bal_sinc', series != 'r_bal_rent', series != 'r_cap_account')
all_metr2 = all_metr2 %>% select(series, bp_model, arima, ets, snaive) %>% mutate(bp_model2 = bp_model, arima2 = arima, ets2 = ets, snaive2 = snaive) %>%
  select(series, bp_model2, arima2, ets2, snaive2)
all_metr2 %>%View()
all_mase_mape = full_join(all_metr, all_metr2) %>%# 2 for mase
  select(series, bp_model, bp_model2, arima, arima2, ets, ets2, snaive, snaive2)
library(xtable)
a = xtable(all_mase_mape[1:9])
a

print(a, include.rownames=FALSE)
all_metr %>% View()
