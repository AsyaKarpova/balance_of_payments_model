library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)
library(GenSA)
library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)


prices = import('data/data_month1.xlsx') %>%
  mutate(date = yearmonth(date)) %>%
  filter(year(date)<=2019)
prices_quater = import('data/data_quarter_new.xlsx')
prices_quater = mutate_at(prices_quater, vars(-date), ~ . / 1000) %>%
  mutate(date = yearquarter(date))%>% filter(year(date)<=2019)
par_model = import('script/gensa_par.Rds')
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


# gas, op, oil monthly data until 2013Q12 (end_2013 obs.)
# #export, import, n_' until 2018Q12 (end_2018 obs.)
# r_exp_serv, r_exp_all,r_imp_goods,r_imp_serv,r_imp_all and some r_bal_' from 2012Q1 (start from start_2012 index)



# create dataframe
prices = create_tibble(prices)

# dates
end_2018 = 156
end_2019 = 168
end_2013 = 96
end_2018q4 = 52
start_2012 = 73

# MY OPTIMIZATION for oil model

X_oil = prices %>%
  select(const, brent, brent_1, rub_usd_1)
R_oil = prices %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil)
R_oil_quarter = prices_quater %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil)

par_0 = c(rep(0.02, 7), rep(0.91, 11))

#result = optim(par = par_0, X = X, R = R, R_quarter = R_quarter,
#               fn = error_opt_oil,
 #              method = 'L-BFGS-B')

result_sa = GenSA(lower = rep(-1, 18),
                upper = rep(1.9, 18),
                fn = error_opt_oil,
                  X = X_oil, R = R_oil, R_quarter = R_oil_quarter,
              control = list(verbose = TRUE, max.time = 300))

par_oil = result_sa$par

#export(par_oil, 'par_oil_2019.Rds')

pred_oil = make_pred_oil(par_oil, X_oil, R_oil)
r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter
par_model2 = par_model

autoplot(ts.union(real_data = ts(prices$p_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12)), size = 0.8) + ylab('p_exp_oil') + xlab('') +  ggtitle('Average price of oil #exported')
#ggsave('oil_p.png')

autoplot(ts.union(real_data = ts(prices$v_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12)), size =0.7) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil #exported')
#ggsave('oil_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12)), size= 0.7) + ylab('revenue') + xlab('') + ggtitle('Average revenue from #export of oil')
#ggsave('oil_r.png')
mase(pred_oil$v_hat_oil[1:end_2013], prices$v_exp_oil[1:end_2013])
mase(pred_oil$r_hat_oil[1:end_2013], prices$r_exp_oil[1:end_2013])
mase(pred_oil$p_hat_oil[1:end_2013], prices$p_exp_oil[1:end_2013])


# MY OPTIMIZATION for oil product model

X_op = prices %>%
  select(const,
         brent, brent_1, brent_2, brent_3,
         rub_usd_eur_1, v_prod_op_1)

R_op = prices %>%
  select(p_exp_op, v_exp_op, r_exp_op)

R_op_quarter = prices_quater %>%
  select(p_exp_op, v_exp_op, r_exp_op)

par_0 = c(rep(0.02, 9), rep(0.91, 11))
result_sa = GenSA(lower = rep(-1, length(par_0)),
                  upper = rep(1.9, length(par_0)),
                  fn = error_opt_op,
                  X = X_op, R = R_op, R_quarter = R_op_quarter,
                  control = list(verbose = TRUE, max.time = 300))
par_op = result_sa$par
#export(par_op, 'par_op_2019.Rds')

pred_op = make_pred_op(par_op, X_op, R_op)
r_hat_op = pred_op$r_hat_op
r_hat_op_quarter = pred_op$r_hat_op_quarter

autoplot(ts.union(real_data = ts(prices$p_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('p_exp_op')  + xlab('') + ggtitle('Average price of oil products #exported')
#ggsave('op_p.png')

autoplot(ts.union(real_data = ts(prices$v_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('v_exp_op') + xlab('') + ggtitle('Average volume of oil products #exported')
#ggsave('op_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('r_exp_op') + xlab('') + ggtitle('Average revenue from #export of oil products')

#ggsave('op_r.png')

mase(pred_op$v_hat_op[1:end_2013], prices$v_exp_op[1:end_2013])
mase(pred_op$r_hat_op[1:end_2013], prices$r_exp_op[1:end_2013])
mase(pred_op$p_hat_op[1:end_2013], prices$p_exp_op[1:end_2013])


### optimization for gas model
X_gas = prices %>%
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6,
         v_prod_gas, usd_eur, dif_usd_rub_ratio_1)

R_gas = prices %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_gas_quarter = prices_quater %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas)




par_0 = c(rep(0.02, 15), rep(0.91, 11))

result_sa_gas = GenSA(lower = rep(-1, length(par_0)),
                  upper = rep(1.9, length(par_0)),
                  fn = error_opt_gas,
                  X = X_gas, R = R_gas, R_quarter = R_gas_quarter,
                  control = list(verbose = TRUE, max.time = 300))

par_gas = result_sa_gas$par
#export(par_gas, 'par_gas_2019.Rds')

pred_gas = make_pred_gas(par_gas, X_gas, R_gas)
r_hat_gas = pred_gas$r_hat_gas
r_hat_gas_quarter = pred_gas$r_hat_gas_quarter


autoplot(ts.union(real_data = ts(prices$p_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('p_exp_gas') + xlab('') + ggtitle('Average price of gas #exported')
#ggsave('gas_p.png')
autoplot(ts.union(real_data = ts(prices$v_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('v_exp_gas') + xlab('') + ggtitle('Average volume of gas #exported')
#ggsave('gas_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from #export of gas')

autoplot(ts.union(real_data = ts(prices_quater$r_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$r_hat_gas_quarter, start = c(2006, 1), freq = 12)), size = 0.7) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from #export of gas')
#ggsave('gas_r.png')

mase(pred_gas$v_hat_gas[1:end_2013], prices$v_exp_gas[1:end_2013])
mase(pred_gas$r_hat_gas[1:end_2013], prices$r_exp_gas[1:end_2013])
mase(pred_gas$p_hat_gas[1:end_2013], prices$p_exp_gas[1:end_2013])

### optimisation for #export other goods model

X_othg = tibble(const = 1,
           r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op,
           r_hat_oog_dum = r_hat_oog * prices$dum_1114,
           gpd_defl = lag(prices$n_y, 1) / prices$rub_usd_1)


R_othg = prices %>% select(r_exp_othg, r_exp_goods)

R_othg_quarter = prices_quater %>%
  select(r_exp_othg, r_exp_goods)



par_0 = c(rep(0.5, 4), rep(0.91, 11))

result_sa_othg = GenSA(lower = rep(0, length(par_0)),
                  upper = rep(2.5, length(par_0)),
                  fn = error_opt_othg,
                  X = X_othg, R = R_othg, R_quarter = R_othg_quarter,
                  control = list(verbose = TRUE, max.time = 120))
par_othg = result_sa_othg$par

#export(par_othg, 'par_othg_2019.Rds')

pred_exp = make_pred_exp(par_othg, X_othg, R_othg)
r_hat_othg = pred_exp$r_hat_othg
r_hat_gds = pred_exp$r_hat_gds

autoplot(ts.union(real_data = ts(prices$r_exp_othg[1:end_2013], start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from #export of other goods')
#ggsave('exp_othg.png')


autoplot(ts.union(real_data = ts(prices$r_exp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from #export of all goods')

autoplot(ts.union(real_data = ts(prices_quater$r_exp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_othg_quarter, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from #export of all goods')




mase(r_hat_othg[1:end_2013], prices$r_exp_othg[1:end_2013])
mase(r_hat_gds, prices$r_exp_goods)


# Model for goods import (restore monthly data!!!!) (r_imp_goods^ + r_imp_serv^ + r_imp_all^)

X_imp = tibble(consump_defl = lag(prices$n_c, 1)/prices$rub_usd_1,
           j_defl = lag(prices$n_j, 1)/prices$rub_usd_1,
           ds_defl = (prices$n_ds_1)/prices$rub_usd_1,
           r_hat_gds = r_hat_gds)

R_imp = prices %>%
        select(r_imp_goods, r_imp_serv, r_imp_all)



R_imp_quarter = prices_quater %>%
  select(r_imp_goods, r_imp_serv, r_imp_all)

par_imp = import('par_imp_2019.Rds')
#par = c(par_imp_gds, par_imp_serv)


par_0 = c(rep(0.02, 21), rep(0.91, 11))

result_sa_imp = GenSA(lower = rep(-1, length(par_0)),
                       upper = rep(1.9, length(par_0)),
                       fn = error_opt_imp,
                       X = X_imp, R = R_imp, R_quarter = R_imp_quarter,
                       control = list(verbose = TRUE, max.time = 300))
par_imp = result_sa_imp$par
#export(par_imp, 'par_imp_2019.Rds')

pred_imp = make_pred_imp(par_imp, X_imp, R_imp)

r_hat_imp_gds = pred_imp$r_hat_imp_gds
r_hat_imp_serv = pred_imp$r_hat_imp_serv
r_hat_imp_all = pred_imp$r_imp_all

autoplot(ts.union(real_data = ts(prices$r_imp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_gds, start = c(2006, 1), freq = 12))) + ylab('revenue from import of goods')
autoplot(ts.union(real_data = ts(prices$r_imp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_serv, start = c(2006, 1), freq = 12)), size=1) + ggtitle('Average revenue from import of services') + ylab('revenue') +xlab('')
autoplot(ts.union(real_data = ts(prices$r_imp_all, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_imp_all, start = c(2006, 1), freq = 12))) + ylab('revenue from all import')



mase(r_hat_imp_gds, prices$r_imp_goods)
mase(tail(r_hat_imp_serv,-start_2012), tail(prices$r_imp_serv,-start_2012))
mase(tail(r_hat_imp_all,-start_2012), tail(prices$r_imp_all,-start_2012))


### Model for #export of services

X_exp_serv = tibble(const = 1,
     r_hat_gds = r_hat_gds,
       r_hat_imp_serv = r_hat_imp_serv)


R_exp_serv = prices %>%
  select(r_exp_serv)

R_exp_serv_quarter = prices_quater %>%
  select(r_exp_serv)



par_0 = c(rep(0.02, 3), rep(0.91, 11))

result_sa_exp_serv = GenSA(lower = rep(-1, length(par_0)),
                      upper = rep(1.9, length(par_0)),
                      fn = error_opt_exp_serv,
                      X = X_exp_serv, R = R_exp_serv, R_quarter = R_exp_serv_quarter,
                      control = list(verbose = TRUE, max.time = 300))
par_exp_serv = result_sa_exp_serv$par
#export(par_exp_serv, 'par_exp_serv_2019.Rds')

pred_exp_serv = make_pred_exp_serv(par_exp_serv, X_exp_serv, R_exp_serv)
autoplot(ts.union(real_data = ts(prices$r_exp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp_serv$r_hat_exp_serv, start = c(2006, 1), freq = 12))) + ylab('revenue from #export of services')

r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

mase(tail(r_hat_exp_serv,-start_2012), tail(prices$r_exp_serv,-start_2012))



r_hat_exp_all = r_hat_exp_serv + r_hat_gds
r_hat_exp_all_quarter = c(NA, roll_sum(tail(r_hat_exp_all, -4), n = 3, by = 3))

autoplot(ts.union(real_data = ts(prices$r_exp_all, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_exp_all, start = c(2006, 1), freq = 12))) + ylab('revenue from #export of goods and services')
r_hat_bal_trade = r_hat_gds - r_hat_imp_gds
r_hat_bal_trade_quarter = roll_sum(r_hat_bal_trade, n = 3, by = 3)


r_hat_bal_serv = r_hat_exp_serv - r_hat_imp_serv
r_hat_bal_serv_quarter = roll_sum(r_hat_bal_serv, n = 3, by = 3)




#### model for (1) balance of rent and secondary income; (2) investement; (3) wages

X_rent_sinc = tibble(const = 1,
           r_hat_exp_serv = r_hat_exp_serv,
           r_hat_gds = r_hat_gds)

R_rent_sinc = prices %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>% rename('r_real' = 'r_bal_rent_sinc')

R_rent_sinc_quarter = prices_quater %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>%
  rename('r_real' = 'r_bal_rent_sinc')
X_rent_sinc %>%tail()


par_0 = c(rep(0.02, 21), rep(0.91, 11))

result_sa_rent_sinc = GenSA(lower = rep(-1, length(par_0)),
                      upper = rep(1.9, length(par_0)),
                      fn = error_opt_balances,
                      X = X_rent_sinc, R = R_rent_sinc, R_quarter = R_rent_sinc_quarter,
                      control = list(verbose = TRUE, max.time = 100))

par_rent_sinc = result_sa_rent_sinc$par
#export(par_rent_sinc, 'par_rent_sinc.Rds')

pred_rent_sinc = make_pred_balances(par_rent_sinc, X_rent_sinc, R_rent_sinc)

autoplot(ts.union(real_data = ts(prices$r_bal_rent + prices$r_bal_sinc, start = c(2006, 1), freq = 12),
                  model = ts(pred_rent_sinc$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of rent and sinc')

r_hat_rent_sinc = pred_rent_sinc$r_hat

# error!
mase(tail(r_hat_rent_sinc, -start_2012), tail(prices$r_bal_rent + prices$r_bal_sinc), -start_2012)


# (2) model for the balance of investment

X_inv = tibble(const = 1,
           r_hat_bal_trade = r_hat_bal_trade,
           r_hat_bal_serv = r_hat_bal_serv)

R_inv = prices %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')

R_inv_quarter = prices_quater %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')


par_0 = c(rep(0.02, 3), rep(0.91, 11))

result_sa_inv = GenSA(lower = rep(-1, length(par_0)),
                            upper = rep(1.9, length(par_0)),
                            fn = error_opt_balances,
                            X = X_inv, R = R_inv, R_quarter = R_inv_quarter,
                            control = list(verbose = TRUE, max.time = 300))
par_inv = result_sa_inv$par
#export(par_inv, 'par_inv_2019.Rds')

pred_inv = make_pred_balances(par_inv, X_inv, R_inv)
autoplot(ts.union(real_data = ts(prices$r_bal_inv, start = c(2006, 1), freq = 12),
                  model = ts(pred_inv$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of income')

r_hat_inv = pred_inv$r_hat

mase(r_hat_inv[start_2012:length(r_hat_inv)], prices$r_bal_inv[start_2012:length(r_hat_inv)])


# (3) balance of wages
X_wage = tibble(const = 1,
           r_hat_oil = r_hat_oil,
           r_hat_othg = r_hat_othg,
           r_hat_exp_serv = r_hat_exp_serv,
           r_hat_imp_serv = r_hat_imp_serv)


R_wage = prices %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage')


R_wage_quarter = prices_quater %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage')

par_0 = c(rep(0.02, 5), rep(0.91, 11))

result_sa_wage = GenSA(lower = rep(-1, length(par_0)),
                            upper = rep(1.9, length(par_0)),
                            fn = error_opt_balances,
                            X = X_wage, R = R_wage, R_quarter = R_wage_quarter,
                            control = list(verbose = TRUE, max.time = 300))
par_bal_wage = result_sa_wage$par
#export(par_bal_wage, 'par_bal_wage_2019.Rds')

pred_wage = make_pred_balances(par_bal_wage, X_wage, R_wage)
autoplot(ts.union(real_data = ts(prices$r_bal_wage, start = c(2006, 1), freq = 12),
                  model = ts(pred_wage$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of rent and sinc')

r_hat_wage = pred_wage$r_hat

mase(tail(r_hat_wage,-start_2012), tail(prices$r_bal_wage,-start_2012))



### model for erros

X_errors = select(prices, date, const, dif_brent, dum01:dum12)

R_errors = prices %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

R_errors_quarter = prices_quater %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

par_0 = c(rep(0.02, 2), rep(0.91, 11))

result_sa_errors = GenSA(lower = rep(-1, length(par_0)),
                         upper = rep(1.9, length(par_0)),
                         fn = error_opt_errors,
                         X = X_errors, R = R_errors, R_quarter = R_errors_quarter,
                         control = list(verbose = TRUE, max.time = 300))
X_errors%>%select(date)
par_errors = result_sa_errors$par
#export(par_errors, 'par_errors_2019.Rds')

pred_erros = make_pred_errors(par_errors, X_errors)

r_hat_errors = pred_erros$r_hat_errors

autoplot(ts.union(real_data = ts(prices$r_errors, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('net errors and omissions')

mase(tail(r_hat_errors,-start_2012), tail(prices$r_errors,-start_2012))


### models for difference of reserves

r_hat_cur_acc = r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage


X_difr = tibble(const = 1,
                ###r_hat_difr = 1,
                dif_brent = prices$dif_brent,
                dif_brent_cor = (prices$dif_brent * prices$vcor)/(prices$rub_usd_1),
                dif_brent_1 = prices$brent_1 - prices$brent_2,
                dif_usd_rub = prices$dif_usd_rub,
                dif_usd_rub_1 = prices$rub_usd_1 - prices$rub_usd_2,
                dif_usd_eur = prices$dif_usd_eur,
                r_hat_cur_acc = r_hat_cur_acc,
                r_hat_cur_acc_1 = lag(r_hat_cur_acc, 1),
                quarter = rep((prices_quater$r_dif_reserves)/3, each = 3),
                r_cur_purch = prices$r_cur_purch,
                r_cur_purch_1 = lag(prices$r_cur_purch, 1),
                date = yearmonth(prices$date))
ncol(X_difr)

X_difr_dummy = cbind(X_difr, select(prices, dum01:dum12))

R_dif_reserves = prices %>%
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')

R_dif_reserves_quarter = prices_quater %>%
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')
par_model$par_dif_res
par_dr = par_model$par_difr_res
par = par_dr
X = X_difr_dummy
R = R_dif_reserves
R_quarter = R_dif_reserves_quarter
fv = c(0,0,12.38856924, 13.30591146)
make_pred_dif_res = function(par, X, end = nrow(X), fv = c(0,0,12.38856924, 13.30591146)){
  X_long = filter(X, year(date) < 2015)
  X_2015 = as.matrix(select(as_tibble(X_long), - r_cur_purch, -r_cur_purch_1, -date)) #!
  if (nrow(X_2015) != 0) {
    add_term = X_2015 %*% c(par[1:22])
    r_hat_dif_res = rep(NaN, nrow(X_2015))
    r_hat_dif_res[(length(fv)+1):nrow(X_2015)] = fill_recursive(first_values = fv[length(fv)], add_term = add_term[(length(fv)+2):nrow(X_2015)],
                                                                coefs = par[23]) # from 2006m04 to 2018m12
    r_hat_dif_res_quarter = roll_sum(r_hat_dif_res, n = 3, by = 3)} #from 2Q2006 to 4Q2018

  X_short = X %>% select( dum01:dum12, const, dif_brent, dif_brent_1, dif_usd_rub, dif_usd_rub_1,
                          dif_usd_eur, r_hat_cur_acc, r_hat_cur_acc_1,
                          r_cur_purch, r_cur_purch_1, date) %>% as_tsibble(index = date)
  X_short_end = filter(X_short, year(date) >= 2015) %>% as_tibble() %>% select(-date)
  add_term_short = as.matrix(X_short_end) %*% c(par[11:22], par[25:34])
  fv_2016 = if_else(nrow(X_long) != 0, tail(r_hat_dif_res, 1), tail(fv, 1))
  r_dif_res_short = fill_recursive(first_values = fv_2016,
                                   add_term = add_term_short,
                                   coefs = par[24]) # from 2006m04 to 2018m12

  r_hat_dif_res_short = c(tail(r_hat_dif_res, -1), r_dif_res_short) #prediction from 2015m01
  r_hat_dif_res_short_quarter = roll_sum(r_hat_dif_res_short, n = 3, by = 3) #from 2Q2006 to 4Q2018

  return(list(r_hat_dif_res = r_hat_dif_res,
              r_hat_dif_res_quarter = r_hat_dif_res_quarter,
              r_hat_dif_res_short = r_hat_dif_res_short,
              r_hat_dif_res_short_quarter = r_hat_dif_res_short_quarter))
}

error_opt_dif_res = function(par, X, R, R_quarter){
  frcst = make_pred_dif_res(par, X)
  error_long = pse_abs(pred = frcst$r_hat_dif_res[start_2012:108],
                       pred_quarter = tail(frcst$r_hat_dif_res_quarter, -3),
                       real = R$r_real[start_2012:108],
                       real_quarter = R_quarter$r_real[4:length(frcst$r_hat_dif_res_quarter)])
  error_short = pse_abs(pred = frcst$r_hat_dif_res_short[109:length(frcst$r_hat_dif_res)], pred_quarter = tail(frcst$r_hat_dif_res_short_quarter, -2),
                        real = R$r_real[109:length(frcst$r_hat_dif_res)], real_quarter = tail(R_quarter$r_real, -2))
  error = error_short + error_long
  print(error)
  return(error)
}

par_0 = c(rep(0.8, 34))
length(par)
# test zone
.test_par = rep(1.5, length(par_0))
error_opt_dif_res(.test_par, X = X_difr_dummy, R = R_dif_reserves, R_quarter = R_dif_reserves_quarter)
# end test zone

result_sa_dif_reserves = GenSA(lower = rep(-1, length(par_0)),
                         upper = rep(1.5, length(par_0)),
                         fn = error_opt_dif_res,
                         X = X_difr_dummy, R = R_dif_reserves, R_quarter = R_dif_reserves_quarter,
                         control = list(verbose = TRUE, max.time = 100))

par_difr_res = result_sa_dif_reserves$par
#export(par_difr_res,'par_dif_res_2019.Rds')
pred_res = make_pred_dif_res(par_difr_res, X_difr_dummy)

r_hat_dif_res = pred_res$r_hat_dif_res
r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
r_hat_dif_res_short = pred_res$r_hat_dif_res_short
r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter

autoplot(ts.union(real_data = ts(prices$r_dif_reserves, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('difference in reserves')

mase(tail(r_hat_dif_res_short, -start_2012), tail(prices$r_dif_reserves, -start_2012))


# warning! not the same length
r_hat_bal_fin = r_hat_errors - r_hat_dif_res_short + r_hat_cur_acc


autoplot(ts.union(real_data = ts(prices$r_bal_fin, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_bal_fin, start = c(2006, 1), freq = 12))) + ylab('finance balance')



### currency purchase model


X_cur = tibble(r_price_cur_purch = prices$r_price_cur_purch,
           brent = prices$brent,
           brent_1 = prices$brent_1,
           brent_2 = prices$brent_2,
           r_dum_cur_purch = prices$r_dum_cur_purch)


R_cur = prices %>%
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch')

R_cur_quarter = prices_quater %>%
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch')



par_0 = c(rep(0.8, 5))

result_sa_cur_purch = GenSA(lower = rep(-1, length(par_0)),
                               upper = rep(1.9, length(par_0)),
                               fn = error_opt_cur_purch,
                               X = X_cur, R = R_cur, R_quarter = R_cur_quarter,
                               control = list(verbose = TRUE, max.time = 200))


par_cur_purch = result_sa_cur_purch$par
#export(par_cur_purch, 'par_cur_purch_2019.Rds')

pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur)
r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch
r_hat_cur_purch_quarter = pred_cur_purch$r_hat_cur_purch_quarter


autoplot(ts.union(real_data = ts(prices$r_cur_purch, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_cur_purch, start = c(2006, 1), freq = 12))) + ylab('currency purchase')


### model for exchange rate
X_rub_usd = tibble(const = 1,
                   dif_brent_ratio = prices$dif_brent_ratio,
                   dif_brent_ratio_1114 = prices$dum_1114 * dif_brent_ratio,
                   dif_brent_ratio_cor = (prices$vcor*dif_brent_ratio)/prices$rub_usd_1,
                   dif_r_1114 = prices$dif_r * prices$dum_1114,
                   r_cur_purch = prices$r_cur_purch,
                   r_cur_purch_1 = lag(prices$r_cur_purch, 1),
                   dif_usd_eur_ratio = prices$dif_usd_eur_ratio,
                   em_index_ratio = prices$dif_em_index_ratio,
                   em_index_ratio_1114 = em_index_ratio * prices$dum_1114,
                   dif_usd_rub_ratio = prices$dif_usd_rub_ratio)


R_rub_usd = prices %>%
  select(rub_usd) %>%
  rename('r_real' = 'rub_usd')


R_rub_usd_quarter = roll_mean(R_rub_usd$r_real, n = 3, by = 3)


mask = create_mask(n_months = nrow(prices))
X = X_rub_usd
par = par_rub_usd
R = R_rub_usd
end = nrow(X)
R_quarter = R_rub_usd_quarter
make_pred_rub_usd = function(par, X, R, end = nrow(X), mask){
  add_term = as.matrix(X[4:end,1:10]) %*% c(par[1:10])
  X$dif_usd_rub_ratio[3:end] = fill_recursive(first_values = X$dif_usd_rub_ratio[3], add_term = add_term,
                                              coefs = par[11]) # from 2006m04 to 2018m12
  hat_rub_usd_ratio = X$dif_usd_rub_ratio
  hat_rub_usd = matrix(NaN, 8, end)
  hat_rub_usd[1:8, 1:3] = matrix(R[1:3,1], ncol = 3, nrow = 8, byrow = TRUE)
  vector = 1 + hat_rub_usd_ratio
  real_values = matrix(R[1:end,1], ncol = end, nrow = 8, byrow = TRUE)
  for (i in 1:nrow(hat_rub_usd)) {
    for (j in 4:ncol(hat_rub_usd)) {
      if (mask[i,j] == 0) {
        hat_rub_usd[i,j] = hat_rub_usd[i, j-1] * vector[j]}
      else{
        hat_rub_usd[i,j] = real_values[i,j]

      }
    }
  }
  hat_rub_usd_final = colMeans(hat_rub_usd, na.rm = TRUE)
  hat_rub_usd_quarter = roll_mean(hat_rub_usd_final, n = 3, by = 3)

  return(list(hat_rub_usd_final = hat_rub_usd_final,
              hat_rub_usd_quarter = hat_rub_usd_quarter,
              hat_rub_usd = hat_rub_usd))
}
error_opt_rub_usd = function(par, X, R, R_quarter){
  frcst = make_pred_rub_usd(par = par, X = X, R = R, mask = mask)
  hat_rub_usd_final = frcst$hat_rub_usd_final
  hat_rub_usd = frcst$hat_rub_usd
  hat_rub_usd_quarter = frcst$hat_rub_usd_quarter

  real_values = matrix(R[1:nrow(R),1], ncol = nrow(R), nrow = 8, byrow = TRUE)
  error_matrix = hat_rub_usd - real_values
  error_month = apply(error_matrix, 1, pse0, y = real_values[1,])
  error_month_mean = mean(error_month)
  error_quarter = pse0(y = R_quarter[1:length(R_quarter)], yhat = hat_rub_usd_quarter)
  error = error_month_mean + error_quarter
  return(error)
}





par_0 = c(rep(0.8, 11))

result_sa_rub_usd = GenSA(lower = rep(-1, length(par_0)),
                            upper = rep(1.9, length(par_0)),
                            fn = error_opt_rub_usd,
                            X = X_rub_usd, R = R_rub_usd, R_quarter = R_rub_usd_quarter,
                            control = list(verbose = TRUE, max.time = 300))

par_rub_usd = result_sa_rub_usd$par
par_rub_usd2 = par_model$par_rub_usd
#export(par_rub_usd, 'par_rub_usd_2019.Rds')
pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd,R_rub_usd,mask=mask)
hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final

autoplot(ts.union(real_data = ts(prices$rub_usd, start = c(2006, 1), freq = 12),
                  model = ts(hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('rub/usd')


#gensa_par_is = import('script/gensa_par.Rds')
#gensa_par_is$par_othg = par_othg
#gensa_par = list(par_bal_wage = par_bal_wage,
 #                par_cur_purch = par_cur_purch, par_dif_res = par_dif_res,
  #               par_errors = par_errors, par_exp_serv = par_exp_serv,
   #              par_gas = par_gas, par_oil = par_oil, par_op = par_op,par_imp = par_imp,
    #             par_imp_gds = par_imp_gds, par_imp_serv = par_imp_serv, par_inv = par_inv,
     #            par_rub_usd= par_rub_usd, par_rent_sinc = par_rent_sinc, par_othg = par_otgh)

