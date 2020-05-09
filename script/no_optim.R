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
library(patchwork)

all_vars = import('data/data_month1.xlsx') %>% mutate(date = yearmonth(date)) %>% as_tsibble()
all_vars_quater = import('data/data_quarter_new.xlsx')
all_vars_quater = mutate_at(all_vars_quater, vars(-date), ~ . / 1000)

all_vars = all_vars %>% filter(year(date) <= 2019)

par_model = import('script/gensa_par.Rds')
par_model2 = import('script/par_model.Rds')

source('script/functions.R')

all_vars = create_tibble(all_vars)
# gas, op, oil monthly data until 2013Q12 (end_2013 obs.)
# export, import, n_' until 2018Q12 (end_2019 obs.)
# r_exp_serv, r_exp_all,r_imp_goods,r_imp_serv,r_imp_all and some r_bal_' from 2012Q1 (start from start_2012 index)

### parameters from gensa
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
export(par_model, 'par_model_2019.Rds')
# dates
end_2018 = 156
end_2019 = 168
end_2013 = 96
end_2019q4 = 52
start_2012 = 73
all_vars

restored_all = predict_bp(all_vars, par_model)
predictions = restored_all[[2]]
predictions$r_hat_cap_acc = rep(0, nrow(all_vars))
a = autoplot(ts.union(real_data = ts(all_vars$r_cur_account[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_cur_acc, start = c(2006, 1), freq = 12))) + ylab('млрд.долл') + xlab('') +  ggtitle('Счет текущих операций') + theme(legend.position = 'none')

b = autoplot(ts.union(real_data = ts(all_vars$r_bal_fin[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$hat_fin_bal, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') +  ggtitle('Финансовый баланс')

c = autoplot(ts.union(real_data = ts(all_vars$r_errors[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('млрд. долл') + xlab('') +  ggtitle('Чистые ошибки и пропуски')  +  theme(legend.position = 'none')

d = autoplot(ts.union(real_data = ts(all_vars$r_cap_account[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_cap_acc, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') +  ggtitle('Счет операций с капиталом')

er_plot = autoplot(ts.union(real_data = ts(all_vars$rub_usd[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$hat_rub_usd_final, start = c(2006, 1), freq = 12))) + xlab('') + ylab('рублей за доллар') +  ggtitle('RUB/USD')

cur_p_plot = autoplot(ts.union(real_data = ts(all_vars$r_cur_purch[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$r_hat_cur_purch, start = c(2006, 1), freq = 12))) + xlab('') + ylab('млрд.долл') +  ggtitle('Покупка валюты Минфином') + theme(legend.position = 'none')


bp_plot = (a+d)/(c+b)
er_plot/cur_p_plot

p_oil = autoplot(ts.union(real_data = ts(all_vars$p_exp_oil[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(predictions$p_hat_oil, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"долл./"',  '~ м^3'))) +  ggtitle('Средняя цена экспорта нефти') + theme(legend.position = 'none')
v_oil = autoplot(ts.union(real_data = ts(all_vars$v_exp_oil[1:end_2019], start = c(2006, 1), freq = 12),
                          model = ts(predictions$v_hat_oil, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"млрд."',  '~ м^3'))) +  ggtitle('Объем экспорта нефти') + theme(legend.position = 'none')
r_oil = autoplot(ts.union(real_data = ts(all_vars$r_exp_oil[1:end_2019], start = c(2006, 1), freq = 12),
                          model = ts(predictions$r_hat_oil, start = c(2006, 1), freq = 12))) + xlab('') + ylab('млрд. долл.') +  ggtitle('Выручка от экспорта нефти')
r_oil/(v_oil + p_oil)

p_op = autoplot(ts.union(real_data = ts(all_vars$p_exp_op[1:end_2019], start = c(2006, 1), freq = 12),
                          model = ts(predictions$p_hat_op, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"долл./"',  '~ м^3'))) +  ggtitle('Средняя цена экспорта нефтепродуктов') + theme(legend.position = 'none')
v_op = autoplot(ts.union(real_data = ts(all_vars$v_exp_op[1:end_2019], start = c(2006, 1), freq = 12),
                          model = ts(predictions$v_hat_op, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"млрд."',  '~ м^3'))) +  ggtitle('Объем экспорта нефтепродуктов') + theme(legend.position = 'none')
r_op = autoplot(ts.union(real_data = ts(all_vars$r_exp_op[1:end_2019], start = c(2006, 1), freq = 12),
                          model = ts(predictions$r_hat_op, start = c(2006, 1), freq = 12))) + xlab('') + ylab('млрд. долл.') +  ggtitle('Выручка от экспорта нефтепродуктов')
(r_op /(v_op + p_op)) + ggtitle('j')

p_gas = autoplot(ts.union(real_data = ts(all_vars$p_exp_gas[1:end_2019], start = c(2006, 1), freq = 12),
                         model = ts(predictions$p_hat_gas, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"долл./"',  '~ м^3'))) +  ggtitle('Средняя цена экспорта газа') + theme(legend.position = 'none')
v_gas = autoplot(ts.union(real_data = ts(all_vars$v_exp_gas[1:end_2019], start = c(2006, 1), freq = 12),
                         model = ts(predictions$v_hat_gas, start = c(2006, 1), freq = 12))) + xlab('') + ylab(parse(text = paste0('"млрд."',  '~ м^3'))) +  ggtitle('Объем экспорта газа') + theme(legend.position = 'none')
r_gas = autoplot(ts.union(real_data = ts(all_vars$r_exp_gas[1:end_2019], start = c(2006, 1), freq = 12),
                         model = ts(predictions$r_hat_gas, start = c(2006, 1), freq = 12))) + xlab('') + ylab('млрд. долл.') +  ggtitle('Выручка от экспорта газа')
r_gas /(v_gas + p_gas)


X_oil = all_vars %>%
  select(const, brent, brent_1, rub_usd_1)

R_oil = all_vars %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil)

R_oil_quarter = all_vars_quater %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil) %>% na.omit()

pred_oil = make_pred_oil(par_oil, X_oil, R_oil, end = end_2019)

autoplot(ts.union(real_data = ts(all_vars$p_exp_oil[1:end_2019], start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil') + xlab('') +  ggtitle('Average price of oil exported')
#ggsave('oil_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_oil[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil exported')
#ggsave('oil_v.png')
b = autoplot(ts.union(real_data = ts(all_vars$r_exp_oil[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')

a = autoplot(ts.union(real_data = ts(all_vars_quater$r_exp_oil, start = c(2006, 1), freq = 4),
                  model = ts(pred_oil$r_hat_oil_quarter, start = c(2006, 1), freq = 4))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')


#ggsave('oil_r.png')


mape_v_hat_oil = mase(pred_oil$v_hat_oil[(end_2013-12):end_2013], all_vars$v_exp_oil[(end_2013-12):end_2013])
mape_r_hat_oil = mase(pred_oil$r_hat_oil[(end_2013-12):end_2013], all_vars$r_exp_oil[(end_2013-12):end_2013])
mape_p_hat_oil = mase(pred_oil$p_hat_oil[(end_2013-12):end_2013], all_vars$p_exp_oil[(end_2013-12):end_2013])
mape_p_hat_oil
r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter



# oil product model

X_op = all_vars %>%
  select(const,
         brent, brent_1, brent_2, brent_3,
         rub_usd_eur_1, v_prod_op_1)

R_op = all_vars %>%
  select(p_exp_op, v_exp_op, r_exp_op)

R_op_quarter = all_vars_quater %>%
  select(p_exp_op, v_exp_op, r_exp_op) %>% na.omit()

pred_op = make_pred_op(par_op, X_op, R_op, end = end_2019)
X_op %>%tail()
autoplot(ts.union(real_data = ts(all_vars$p_exp_op[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12))) + ylab('p_exp_op')  + xlab('') + ggtitle('Average price of oil products exported')
##ggsave('op_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_op[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12))) + ylab('v_exp_op') + xlab('') + ggtitle('Average volume of oil products exported')

#ggsave('op_v.png')
autoplot(ts.union(real_data = ts(all_vars$r_exp_op[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12))) + ylab('r_exp_op') + xlab('') + ggtitle('Average revenue from export of oil products')

autoplot(ts.union(real_data = ts(all_vars_quater$v_exp_op, start = c(2006, 1), freq = 4),
                  model = ts(pred_op$v_hat_op_quarter, start = c(2006, 1), freq = 4))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')
#ggsave('op_r.png')
resid = all_vars$v_exp_op[(end_2013-12):end_2013] - pred_op$v_hat_op[(end_2013-12):end_2013]
train = pred_op$v_hat_op[1:(end_2013-13)]
a = MASE(.train = train, .resid = resid, .period=12)
mape_v_hat_op = mase(pred_op$v_hat_op[(end_2013-12):end_2013], all_vars$v_exp_op[(end_2013-12):end_2013])
mape_r_hat_op = mase(pred_op$r_hat_op[(end_2013-12):end_2013], all_vars$r_exp_op[(end_2013-12):end_2013])
mape_p_hat_op = mase(pred_op$p_hat_op[(end_2013-12):end_2013], all_vars$p_exp_op[(end_2013-12):end_2013])
mape_v_hat_op
r_hat_op = pred_op$r_hat_op
r_hat_op_quarter = pred_op$r_hat_op_quarter

### optimization for gas model


X_gas = all_vars %>%
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6,
         v_prod_gas, usd_eur, dif_usd_rub_ratio_1)

R_gas = all_vars %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_gas_quarter = all_vars_quater %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas) %>% na.omit()


pred_gas = make_pred_gas(par_gas, X_gas, R_gas, end = end_2019)

r_hat_gas = pred_gas$r_hat_gas
r_hat_gas_quarter = pred_gas$r_hat_gas_quarter


autoplot(ts.union(real_data = ts(all_vars$p_exp_gas[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12))) + ylab('p_exp_gas') + xlab('') + ggtitle('Average price of gas exported')
#ggsave('gas_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_gas[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12))) + ylab('v_exp_gas') + xlab('') + ggtitle('Average volume of gas exported')
#ggsave('gas_v.png')

autoplot(ts.union(real_data = ts(all_vars$r_exp_gas[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12))) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from export of gas')
#ggsave('gas_r.png')

mape_v_hat_gas = mase(pred_gas$v_hat_gas[(end_2013-12):end_2013], all_vars$v_exp_gas[(end_2013-12):end_2013])
mape_r_hat_gas = mase(pred_gas$r_hat_gas[(end_2013-12):end_2013], all_vars$r_exp_gas[(end_2013-12):end_2013])
mape_p_hat_gas = mase(pred_gas$p_hat_gas[(end_2013-12):end_2013], all_vars$p_exp_gas[(end_2013-12):end_2013])

### optimisation for export other goods model

X_othg = tibble(const = 1,
                r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op,
                r_hat_oog_dum = r_hat_oog * all_vars$dum_1114[1:end_2019],
                gpd_defl = lag(all_vars$n_y, 1)[1:end_2019] / all_vars$rub_usd_1[1:end_2019])

X_othg %>%tail()

R_othg = all_vars %>% select(r_exp_othg, r_exp_goods)

R_othg_quarter = all_vars_quater %>%
  select(r_exp_othg, r_exp_goods) %>% na.omit()

pred_exp = make_pred_exp(par_othg, X_othg, R_othg, end = end_2019)

autoplot(ts.union(real_data = ts(all_vars$r_exp_othg[1:end_2013], start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of other goods')
#ggsave('exp_othg.png')


autoplot(ts.union(real_data = ts(all_vars$r_exp_goods[1:end_2019-1], start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of all goods')

#ggsave('exp_all.png')

r_hat_othg = pred_exp$r_hat_othg
r_hat_gds = pred_exp$r_hat_gds

mape_r_hat_othg = mase(r_hat_othg[(end_2013-12):end_2013], all_vars$r_exp_othg[(end_2013-12):end_2013])
mape_r_hat_gds = mase(r_hat_gds[(end_2013-12):end_2013], all_vars$r_exp_goods[(end_2013-12):end_2013])
mape_r_hat_gds



# Model for goods import (restore monthly data!!!!) (r_imp_goods^ + r_imp_serv^ + r_imp_all^)

X_imp = tibble(consump_defl = lag(all_vars$n_c, 1)/all_vars$rub_usd_1,
               j_defl = lag(all_vars$n_j, 1)/all_vars$rub_usd_1,
               ds_defl = (all_vars$n_ds_1)/all_vars$rub_usd_1)



X_imp%>% tail()
X_imp = X_imp[1:end_2019,] %>%
  mutate(r_hat_gds = r_hat_gds)

R_imp = all_vars %>%
  select(r_imp_goods, r_imp_serv, r_imp_all)

R_imp_quarter = all_vars_quater %>%
  select(r_imp_goods, r_imp_serv, r_imp_all) %>% na.omit()


par_imp = c(par_imp_gds, par_imp_serv)

pred_imp = make_pred_imp(par_imp, X_imp, R_imp, end = end_2019)

autoplot(ts.union(real_data = ts(all_vars$r_imp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import of goods')


#ggsave('imp_goods.png')

autoplot(ts.union(real_data = ts(all_vars$r_imp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_serv, start = c(2006, 1), freq = 12))) + ggtitle('Average revenue from import of services') + ylab('revenue') + xlab('')

#ggsave('imp_services.png')

autoplot(ts.union(real_data = ts(all_vars$r_imp_all, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_imp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import')

#ggsave('imp_all.png')

r_hat_imp_gds = pred_imp$r_hat_imp_gds[1:end_2019]
r_hat_imp_serv = pred_imp$r_hat_imp_serv[1:end_2019]
r_hat_imp_all = pred_imp$r_imp_all[1:end_2019]


mape_r_hat_imp_gds = mase(r_hat_imp_gds[(end_2019-12):end_2019-1], all_vars$r_imp_goods[(end_2019-12):end_2019-1])
mape_r_hat_imp_serv = mase(r_hat_imp_serv[(end_2019-12):end_2019-1], all_vars$r_imp_serv[(end_2019-12):end_2019-1])
mape_r_hat_imp_all
mape_r_hat_imp_all = mase(r_hat_imp_all[(end_2019-12):end_2019-1], all_vars$r_imp_all[(end_2019-12):end_2019-1])

### Model for export of services

par = par_exp_serv

X_exp_serv = tibble(const = 1,
                    r_hat_gds = r_hat_gds,
                    r_hat_imp_serv = r_hat_imp_serv)

R_exp_serv = all_vars %>%
  select(r_exp_serv)

R_exp_serv_quarter = all_vars_quater %>%
  select(r_exp_serv) %>% na.omit()


pred_exp_serv = make_pred_exp_serv(par_exp_serv, X_exp_serv, R_exp_serv)

autoplot(ts.union(real_data = ts(all_vars$r_exp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp_serv$r_hat_exp_serv, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of services')

#ggsave('exp_serv.png')

r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

mape_r_hat_exp_serv = mase(r_hat_exp_serv[(end_2019-12):end_2019-1], all_vars$r_exp_serv[(end_2019-12):end_2019-1])



r_hat_exp_all = r_hat_exp_serv + r_hat_gds
mape_r_hat_exp_all = mase(r_hat_exp_all[(end_2019-12):end_2019-1], all_vars$r_exp_all[(end_2019-12):end_2019-1])

r_hat_exp_all_quarter = c(NA, roll_sum(r_hat_exp_all[4:end_2019-1], n = 3, by = 3))

autoplot(ts.union(real_data = ts(all_vars$r_exp_all, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_exp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of goods and services')

#ggsave('exp_gs.png')

r_hat_bal_trade = r_hat_gds - r_hat_imp_gds
r_hat_bal_trade_quarter = roll_sum(r_hat_bal_trade, n = 3, by = 3)
mape_r_hat_bal_trade = mase(r_hat_bal_trade[(end_2019-12):end_2019-1], all_vars$r_bal_trade[(end_2019-12):end_2019-1])

r_hat_bal_serv = r_hat_exp_serv - r_hat_imp_serv
r_hat_bal_serv_quarter = roll_sum(r_hat_bal_serv, n = 3, by = 3)
mape_r_hat_bal_serv = mase(r_hat_bal_serv[(end_2019-12):end_2019-1], all_vars$r_bal_serv[(end_2019-12):end_2019-1])



#### model for (1) balance of rent and secondary income; (2) investement; (3) wages

X_rent_sinc = tibble(const = 1,
                     r_hat_exp_serv = r_hat_exp_serv,
                     r_hat_gds = r_hat_gds)

R_rent_sinc = all_vars %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>% rename('r_real' = 'r_bal_rent_sinc')

R_rent_sinc_quarter = all_vars_quater %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>%
  rename('r_real' = 'r_bal_rent_sinc')%>%
  na.omit()


pred_rent_sinc = make_pred_balances(par_rent_sinc, X_rent_sinc, R_rent_sinc)

autoplot(ts.union(real_data = ts(all_vars$r_bal_rent + all_vars$r_bal_sinc, start = c(2006, 1), freq = 12),
                  model = ts(pred_rent_sinc$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of rent and secondary income')

#ggsave('rent_sink.png')

r_hat_rent_sinc = pred_rent_sinc$r_hat

mape_r_hat_rent_sinc = mase(r_hat_rent_sinc[(end_2019-12):end_2019-1], (all_vars$r_bal_rent + all_vars$r_bal_sinc)[(end_2019-12):end_2019-1])

mape_r_hat_rent_sinc


# (2) model for the balance of investment

X_inv = tibble(const = 1,
               r_hat_bal_trade = r_hat_bal_trade,
               r_hat_bal_serv = r_hat_bal_serv)

R_inv = all_vars %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')

R_inv_quarter = all_vars_quater %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')

pred_inv = make_pred_balances(par_inv, X_inv, R_inv)

autoplot(ts.union(real_data = ts(all_vars$r_bal_inv, start = c(2006, 1), freq = 12),
                  model = ts(pred_inv$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of investment income')

#ggsave('inv_income.png')
r_hat_inv = pred_inv$r_hat

mape_r_hat_inv = mase(r_hat_inv[(end_2019-12):end_2019-1], all_vars$r_bal_inv[(end_2019-12):end_2019-1])





# (3) balance of wages
X_wage = tibble(const = 1,
                r_hat_oil = r_hat_oil,
                r_hat_othg = r_hat_othg,
                r_hat_exp_serv = r_hat_exp_serv,
                r_hat_imp_serv = r_hat_imp_serv)




R_wage = all_vars %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage')

R_wage_quarter = all_vars_quater %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage') %>%
  na.omit()

pred_wage = make_pred_balances(par_bal_wage, X_wage, R_wage)
autoplot(ts.union(real_data = ts(all_vars$r_bal_wage, start = c(2006, 1), freq = 12),
                  model = ts(pred_wage$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of wages')

#ggsave('bwages.png')
r_hat_wage = pred_wage$r_hat

mape_r_hat_wage = mase(r_hat_wage[(end_2019-12):end_2019], all_vars$r_bal_wage[(end_2019-12):end_2019-1])
mase(r_hat_wage[(end_2019-12):end_2019], all_vars$r_bal_wage[(end_2019-12):end_2019-1])


### model for erros

X_errors = select(all_vars, const, dif_brent, dum01:dum12)

R_errors = all_vars %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

R_errors_quarter = all_vars_quater %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors') %>%
  na.omit()

pred_erros = make_pred_errors(par_errors, X_errors)

r_hat_errors = pred_erros$r_hat_errors

errors = autoplot(ts.union(real_data = ts(all_vars$r_errors, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Чистые пропуски и ошибки, млрд. долл.') + theme(legend.position = "none")
errors
#ggsave('errors.png')

mape_r_hat_errors = mase(r_hat_errors[(end_2019-12):end_2019-1], all_vars$r_errors[(end_2019-12):end_2019-1])
mase(r_hat_errors[(end_2019-12):end_2019-1], all_vars$r_errors[(end_2019-12):end_2019-1])

cur_acc = autoplot(ts.union(real_data = ts(all_vars$r_cur_account, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_cur_acc, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Счет текущих операций, млрд. долл') + theme(legend.position = 'none')
mape_r_hat_errors
### models for difference of reserves
r_hat_cur_acc = r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage
mape_r_hat_cur_acc = mase(r_hat_cur_acc[(end_2019-12):end_2019-1], all_vars$r_cur_account[(end_2019-12):end_2019-1])
r_hat_cap_acc = rep(0, nrow(all_vars))
cap_acc = autoplot(ts.union(факт = ts(all_vars$r_cap_account, start = c(2006, 1), freq = 12),
                  модель = ts(r_hat_cap_acc, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Счет операций с капиталом, млрд. долл.')
cap_acc
X_difr = tibble(const = 1,
                dif_brent = all_vars$dif_brent,
                dif_brent_cor = (all_vars$dif_brent * all_vars$vcor)/(all_vars$rub_usd_1),
                dif_brent_1 = all_vars$brent_1 - all_vars$brent_2,
                dif_usd_rub = all_vars$dif_usd_rub,
                dif_usd_rub_1 = all_vars$rub_usd_1 - all_vars$rub_usd_2,
                dif_usd_eur = all_vars$dif_usd_eur,
                quarter = 0,
                r_cur_purch = all_vars$r_cur_purch,
                r_cur_purch_1 = lag(all_vars$r_cur_purch,1))
X_difr = X_difr[1:end_2019, ] %>% mutate(r_hat_cur_acc = r_hat_cur_acc,
                                     r_hat_cur_acc_1 = lag(r_hat_cur_acc, 1),)

X_difr_dummy = bind_cols(X_difr, select(all_vars[1:end_2019, ], dum01:dum12))

R_dif_reserves = all_vars %>%
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')
pred_res = make_pred_dif_res(par_difr_res, X_difr_dummy)

r_hat_dif_res = pred_res$r_hat_dif_res
r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
r_hat_dif_res_short = pred_res$r_hat_dif_res_short
r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter

dif_res = autoplot(ts.union(real_data = ts(all_vars$r_dif_reserves, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('change') + xlab('') + ggtitle('Difference of reserves')
dif_res
#ggsave('dif_res.png')

mape_r_hat_dif_res = mase(r_hat_dif_res_short[(end_2019-12):(end_2019-1)], all_vars$r_dif_reserves[(end_2019-12):(end_2019-1)])
mape_r_hat_dif_res

r_hat_bal_fin = r_hat_errors[1:end_2019] - r_hat_dif_res_short[1:end_2019] + r_hat_cur_acc[1:end_2019]


bal_fin = autoplot(ts.union(real_data = ts(all_vars$r_bal_fin, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_bal_fin, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Финансовый баланс, млрд. долл.') + theme(legend.position = "none")

bal_fin

#ggsave('balfin.png')

### currency purchase model


X_cur = tibble(r_price_cur_purch = all_vars$r_price_cur_purch,
               brent = all_vars$brent,
               brent_1 = all_vars$brent_1,
               brent_2 = all_vars$brent_2,
               r_dum_cur_purch = all_vars$r_dum_cur_purch)


R_cur = all_vars %>%
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch')

R_cur_quarter = all_vars_quater %>%
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch') %>%
  na.omit()


pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur, end = end_2019)
r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch
r_hat_cur_purch_quarter = pred_cur_purch$r_hat_cur_purch_quarter


autoplot(ts.union(real_data = ts(all_vars$r_cur_purch, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_cur_purch, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Currency purchase')

# - 1 AS THERE IS NO CURRENCY PURCHASE AT THE END OF 2018
mape_cur_purch = mase(r_hat_cur_purch[(end_2019-11):(end_2019)], all_vars$r_cur_purch[(end_2019-11):(end_2019)])
mape_cur_purch

#ggsave('cur_purch.png')

### model for exchange rate
X_rub_usd = tibble(const = 1,
                   dif_brent_ratio = all_vars$dif_brent_ratio,
                   dif_brent_ratio_1114 = all_vars$dum_1114 * dif_brent_ratio,
                   dif_brent_ratio_cor = (all_vars$vcor*dif_brent_ratio)/mean(all_vars$rub_usd_1, na.rm = TRUE),
                   dif_r_1114 = all_vars$dif_r * all_vars$dum_1114,
                   r_cur_purch = all_vars$r_cur_purch,
                   r_cur_purch_1 = lag(all_vars$r_cur_purch, 1),
                   dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                   em_index_ratio = all_vars$dif_em_index_ratio,
                   em_index_ratio_1114 = em_index_ratio * all_vars$dum_1114,
                   dif_usd_rub_ratio = all_vars$dif_usd_rub_ratio,
                   date = all_vars$date) %>% as_tsibble()

R_rub_usd = as_tibble(all_vars) %>%
  select(rub_usd) %>%
  rename('r_real' = 'rub_usd') %>% as.data.frame()

R_rub_usd_quarter = roll_mean(R_rub_usd$r_real, n = 3, by = 3)

mask = create_mask(nrow(all_vars))

pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd,R_rub_usd, mask=mask)
hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final



autoplot(ts.union(real_data = ts(all_vars$rub_usd, start = c(2006, 1), freq = 12),
                  model = ts(hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')


predictions = all_vars%>%select(r_bal_wage, rub_usd) %>%mutate(hat_rub_usd = hat_rub_usd_final,
                                                 hat_bal_wage = r_hat_wage) %>% filter(year(date)>=2018)
export(predictions, 'wage_ru.xlsx')
mape_rub_usd = mase(hat_rub_usd_final[(end_2019-12):end_2019-1], all_vars$rub_usd[(end_2019-12):end_2019-1])
#ggsave('rub_usd.png')
hat_fin_bal = r_hat_errors + r_hat_cur_acc - r_hat_dif_res_short
mape_hat_fin_bal = mase(hat_fin_bal[(end_2019-12):end_2019-1], all_vars$r_bal_fin[(end_2019-12):end_2019-1])
mape_table = tibble(r_bal_wage = mape_r_hat_wage, r_cur_purch = mape_cur_purch,
              p_exp_gas = mape_p_hat_gas,
              p_exp_oil = mape_p_hat_oil, p_exp_op = mape_p_hat_op, r_dif_reserves = mape_r_hat_dif_res,
              r_errors = mape_r_hat_errors, r_exp_serv = mape_r_hat_exp_serv, r_exp_gas = mape_r_hat_gas, r_exp_goods = mape_r_hat_gds,
              r_imp_all = mape_r_hat_imp_all, r_imp_goods = mape_r_hat_imp_gds, r_imp_serv = mape_r_hat_imp_serv,
              r_bal_inv = mape_r_hat_inv, r_exp_oil = mape_r_hat_oil, r_exp_op = mape_r_hat_op, r_exp_othg = mape_r_hat_othg,
              r_bal_rent_sinc = mape_r_hat_rent_sinc, rub_usd = mape_rub_usd, v_exp_gas = mape_v_hat_gas, v_exp_oil = mape_v_hat_oil, v_exp_op =  mape_v_hat_op,
              r_cur_account = mape_r_hat_cur_acc, r_bal_fin = mape_hat_fin_bal,
              r_bal_trade = mape_r_hat_bal_trade,
              r_bal_serv = mape_r_hat_bal_serv,
              r_exp_all = mape_r_hat_exp_all)

mase_our = import('mase_table_pse2.Rds')
mase_our$.model = 'bp_model'
mase_b = import('mase_bench.Rds')

all_metrics = bind_rows(mase_our, mase_b) %>% select(series, mase, `.model`)
all_metr = all_metrics %>% spread( key = '.model', value = 'mase') %>% filter(series != 'aq_assets', series != 'aq_obl', series!= 'r_bal_sinc', series != 'r_bal_rent', series != 'r_cap_account')
all_metr = all_metr %>% select(series, bp_model, arima, ets, snaive)
library(xtable)
all_metr %>% View()
a = xtable(all_metr[1:5], caption = 'MASE')
b = printbold(a, each = 'row')
print(a, include.rownames=FALSE)

mape_tabl_long = pivot_longer(mape_table, cols = "r_bal_wage":"r_exp_all", names_to = 'series', values_to = 'mase')
export(mape_tabl_long, 'mase_table_pse2.Rds')
#mape_tabl_long %>%View()

cur_acc = autoplot(ts.union(real_data = ts(all_vars$r_cur_account, start = c(2006, 1), freq = 12),
                            model = ts(r_hat_cur_acc, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Счет текущих операций, млрд. долл') + theme(legend.position = 'none')

cap_acc = autoplot(ts.union(факт = ts(all_vars$r_cap_account, start = c(2006, 1), freq = 12),
                                модель = ts(r_hat_cap_acc, start = c(2006, 1), freq = 12))) + ylab('') + xlab('') + ggtitle('Счет операций с капиталом, млрд. долл.')

library(tikzDevice)

tikz('myPlot.tex')
bp = (cur_acc + cap_acc) /(bal_fin + errors)
ggsave('bp.png')
dev.off()
