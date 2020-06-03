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
library(fable)
library(lubridate)


data_base = import('scenarios/all_vars_2020_base.xlsx') %>%
  mutate(date = yearmonth(date))
data_best = import('data/all_vars_2020_best.xlsx') %>%
  mutate(date = yearmonth(date))
data_bad= import('scenarios/all_vars_2020_bad.xlsx') %>%
  mutate(date = yearmonth(date))
all_vars_quater = import('data/data_quarter_new.xlsx')
all_vars_quater = mutate_at(all_vars_quater, vars(-date), ~ . / 1000)
#par_model = import('script/gensa_par.Rds')
source('script/functions.R')


par_oil = import('par_oil_2019.Rds')
par_gas = import('par_gas_2019.Rds')
par_op = import('par_op_2019.Rds')
par_othg = import('par_othg_2019.Rds')
par_imp = import('par_imp_2019.Rds')
par_exp_serv = import('par_exp_serv_2019.Rds')
par_bal_wage = import('par_bal_wage_2019.Rds')
par_rent_sinc = import('par_rent_sinc.Rds')
par_inv = import('par_inv_2019.Rds')
par_errors = import('par_errors_2019.Rds')
par_difr_res = import('par_dif_res_2019.Rds')
par_cur_purch = import('par_cur_purch_2019.Rds')
par_rub_usd = import('par_rub_usd_2019.Rds')
par_model = list(par_oil = par_oil, par_gas = par_gas, par_op = par_op,
                 par_othg = par_othg, par_imp = par_imp, par_exp_serv = par_exp_serv,
                 par_bal_wage = par_bal_wage, par_rent_sinc = par_rent_sinc, par_inv = par_inv,
                 par_errors = par_errors, par_difr_res = par_difr_res, par_cur_purch = par_cur_purch,
                 par_rub_usd = par_rub_usd)

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
all_vars_tsb_base = data_base %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>%
  drop_na() %>%
  as_tsibble(index = date, key = series)

all_vars_tsb_best= data_best %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>%
  drop_na() %>%
  as_tsibble(index = date, key = series)

all_vars_tsb_bad= data_bad %>%
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>%
  drop_na() %>%
  as_tsibble(index = date, key = series)



all_vars_base = all_vars_tsb_base %>% spread(series, value)
all_vars_good = all_vars_tsb_best %>% spread(series, value)
all_vars_bad = all_vars_tsb_bad %>% spread(series, value)

#### сценарий!!!!
#https://cbr.ru/Collection/Collection/File/27833/forecast_200424.pdf
# BRENT - 27, 35, 45


#all_vars = all_vars %>% mutate(r_price_cur_purch = ifelse(r_dum_cur_purch == 0, 0, r_price_cur_purch))



#export(all_vars, 'data/all_vars_pred.csv')
### parameters from gensa

par_model2 = par_model
pred_2020_base = predict_bp(all_vars_base, par_model)
restored_data_2020_base = pred_2020_base[[1]]
predictions_2020_base = pred_2020_base[[2]]

pred_2020_bad = predict_bp(all_vars_bad, par_model)
restored_data_2020_bad = pred_2020_bad[[1]]
predictions_2020_bad = pred_2020_bad[[2]]

pred_2020_good = predict_bp(all_vars_good, par_model)
restored_data_2020_good = pred_2020_good[[1]]
predictions_2020_best= pred_2020_good[[2]]



cp = autoplot(ts.union(real_data = window(ts(all_vars_base$r_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_bal_trade, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('млрд.долл.') + xlab('') + ggtitle('Торговый баланс') + theme(legend.position = 'none')

cp

ru = autoplot(ts.union(real_data = window(ts(all_vars_base$rub_usd, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       base = window(ts(predictions_2020_best$hat_rub_usd_final, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_base$hat_rub_usd_final, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_bad$hat_rub_usd_final, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('рублей за доллар') + xlab('') + ggtitle('RUB/USD')

ru
ro = autoplot(ts.union(real_data = window(ts(all_vars_base$r_exp_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_oil, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('млрд.долл.') + xlab('') + ggtitle('Выручка от продажи нефти')
ro
rg = autoplot(ts.union(real_data = window(ts(all_vars_base$r_exp_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_gas, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('млрд.долл.') + xlab('') + ggtitle('Выручка от продажи газа') + theme(legend.position = 'none')
rg
rop = autoplot(ts.union(real_data = window(ts(all_vars_base$_exp_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_op, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Выручка от продажи нефтепродуктов') + theme(legend.position = 'none')


vo = autoplot(ts.union(real_data = window(ts(all_vars_base$v_exp_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       base = window(ts(predictions_2020_base$v_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       bad = window(ts(predictions_2020_bad$v_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       good = window(ts(predictions_2020_best$v_hat_oil, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Объем продажи нефти')
vo
vg = autoplot(ts.union(real_data = window(ts(all_vars_base$v_exp_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       base = window(ts(predictions_2020_base$v_hat_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       bad = window(ts(predictions_2020_bad$v_hat_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                       good = window(ts(predictions_2020_best$v_hat_gas, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Объем продажи газа')

vg
vop = autoplot(ts.union(real_data = window(ts(all_vars_base$v_exp_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                        base = window(ts(predictions_2020_base$v_hat_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                        bad = window(ts(predictions_2020_bad$v_hat_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                        good = window(ts(predictions_2020_best$v_hat_op, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Объем продажи нефтепродуктов')

vop
err = autoplot(ts.union(real_data = window(ts(all_vars_base$r_errors, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_errors, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_errors, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_errors, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Чистые пропуски и ошибки') + theme(legend.position = 'none')
err
dif_res = autoplot(ts.union(real_data = window(ts(all_vars_base$r_dif_reserves, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_dif_res_short, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_dif_res_short, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_dif_res_short, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('млрд.долл.') + xlab('') + ggtitle('Изменение резеров')

bal_fin = autoplot(ts.union(real_data = window(ts(all_vars_base$r_bal_fin, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$hat_fin_bal, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$hat_fin_bal, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$hat_fin_bal, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('млрд.долл.') + xlab('') + ggtitle('Финансовый баланс')+   theme(legend.position = 'none')
trade_bal = autoplot(ts.union(real_data = window(ts(all_vars_base$r_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                            base = window(ts(predictions_2020_base$r_hat_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                            bad = window(ts(predictions_2020_bad$r_hat_bal_trade, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                            good = window(ts(predictions_2020_best$r_hat_bal_trade, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('') + xlab('') + ggtitle('Торговый баланс')
trade_bal
cur_acc = autoplot(ts.union(real_data = window(ts(all_vars_base$r_cur_account, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2020_base$r_hat_cur_acc, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  bad = window(ts(predictions_2020_bad$r_hat_cur_acc, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2020_best$r_hat_cur_acc, start = c(2006, 1), freq = 12),start = c(2015, 1))), size=0.8) + ylab('млрд.долл.') + xlab('') + ggtitle('Счет текущих операций')

library(patchwork)
cur_acc/cp

ru/cp
ro/rgб
# rub_usd


autoplot(ts.union(real_data = window(ts(all_vars$rub_usd, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$hat_rub_usd_final, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')




autoplot(ts.union(real_data = window(ts(all_vars$p_exp_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$p_hat_oil, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('p_exp_oil') + xlab('') +  ggtitle('Average price of oil exported')
#ggsave('oil_p.png')

autoplot(ts.union(real_data = window(ts(all_vars$v_exp_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$v_hat_oil, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil exported')
#ggsave('oil_v.png')
autoplot(ts.union(real_data = window(ts(all_vars$r_exp_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2021_base$r_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  neg = window(ts(predictions_2021_neg$r_hat_oil, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2021_good$r_hat_oil, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')


#ggsave('oil_r.png')


autoplot(ts.union(real_data = window(ts(all_vars$p_exp_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$p_hat_op, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('p_exp_op')  + xlab('') + ggtitle('Average price of oil products exported')
##ggsave('op_p.png')

autoplot(ts.union(real_data = window(ts(all_vars$v_exp_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$v_hat_op, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('v_exp_op') + xlab('') + ggtitle('Average volume of oil products exported')

#ggsave('op_v.png')
autoplot(ts.union(real_data = window(ts(all_vars$r_exp_op, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_op, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('r_exp_op') + xlab('') + ggtitle('Average revenue from export of oil products')

#ggsave('op_r.png')




autoplot(ts.union(real_data = window(ts(all_vars$p_exp_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$p_hat_gas, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('p_exp_gas') + xlab('') + ggtitle('Average price of gas exported')
#ggsave('gas_p.png')

autoplot(ts.union(real_data = window(ts(all_vars$v_exp_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$v_hat_gas, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('v_exp_gas') + xlab('') + ggtitle('Average volume of gas exported')
#ggsave('gas_v.png')

autoplot(ts.union(real_data = window(ts(all_vars$r_exp_gas, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_gas, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from export of gas')
#ggsave('gas_r.png')


### optimisation for export other goods model


autoplot(ts.union(real_data = window(ts(all_vars$r_exp_othg, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_othg, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of other goods')
#ggsave('exp_othg.png')


autoplot(ts.union(real_data = window(ts(all_vars$r_exp_goods, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_gds, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  base = window(ts(predictions_2021_base$r_hat_gds, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  neg = window(ts(predictions_2021_neg$r_hat_gds, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  good = window(ts(predictions_2021_good$r_hat_gds, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of all goods')

#ggsave('exp_all.png')



autoplot(ts.union(real_data = window(ts(all_vars$r_imp_goods, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_imp_gds, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import of goods')

#ggsave('imp_goods.png')

autoplot(ts.union(real_data = window(ts(all_vars$r_imp_serv, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_imp_serv, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ggtitle('Average revenue from import of services') + ylab('revenue') + xlab('')

#ggsave('imp_services.png')

autoplot(ts.union(real_data = window(ts(all_vars$r_imp_all, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_imp_all , start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import')

#ggsave('imp_all.png')



autoplot(ts.union(real_data = window(ts(all_vars$r_exp_serv, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_exp_serv, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of services')

#ggsave('exp_serv.png')


autoplot(ts.union(real_data = window(ts(all_vars$r_exp_all, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_exp_all, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of goods and services')

#ggsave('exp_gs.png')




autoplot(ts.union(real_data = window(ts(all_vars$r_bal_rent + all_vars$r_bal_sinc, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_rent_sinc, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('value') + xlab('') + ggtitle('Balance of rent and secondary income')

#ggsave('rent_sink.png')


autoplot(ts.union(real_data = window(ts(all_vars$r_bal_inv, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_inv, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('value') + xlab('') + ggtitle('Balance of investment income')


autoplot(ts.union(real_data = window(ts(all_vars$r_bal_wage, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_wage, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('value') + xlab('') + ggtitle('Balance of wages')


autoplot(ts.union(real_data = window(ts(all_vars$r_errors, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_errors, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('value') + xlab('') + ggtitle('net errors and omissions')


autoplot(ts.union(real_data = window(ts(all_vars$r_dif_reserves, start = c(2006, 1), freq = 12), start = c(2015, 1)),
                  model = window(ts(predictions$r_hat_dif_res_short, start = c(2006, 1), freq = 12),start = c(2015, 1)))) + ylab('change') + xlab('') + ggtitle('Difference of reserves')

