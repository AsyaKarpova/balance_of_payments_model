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


prices = import('data/vars_for_model.csv')
prices_quater = import('data/data_quarter.xlsx')

par_model = import('script/gensa_par.Rds')

# gas, op, oil monthly data until 2013Q12 (end_2013 obs.)
# export, import, n_' until 2018Q12 (end_2018 obs.)
# r_exp_serv, r_exp_all,r_imp_goods,r_imp_serv,r_imp_all and some r_bal_' from 2012Q1 (start from start_2012 index)

### parameters from excel
par_oil = par_model$par_oil
par_gas = par_model$par_gas
par_op = par_model$par_op
par_othg = par_model$par_othg
par_imp_gds = par_model$par_imp_gds
par_imp_serv = par_model$par_imp_serv
par_exp_serv = par_model$par_exp_serv
par_bal_wage = par_model$par_bal_wage
par_rent_sinc = par_model$par_rent_sinc
par_inv = par_model$par_inv
par_errors = par_model$par_errors
par_dif_res = par_model$par_dif_res
par_cur_purch = par_model$par_cur_purch
par_rub_usd = par_model$par_rub_usd

lag = function(ts, n){
  return(dplyr::lag(ts, n = n))
}
vars = prices %>% select(-'dum01':-'dum12', -'dum_2012', -'dum_1114', -'n_j':-'n_ds', -'n_y',-'n_c', -'n_g', -'vcor')

# dates
end_2018 = 156 
end_2013 = 96
end_2018q4 = 52
start_2012 = 73

### loss functions (two types)

pse0 = function(y, yhat) {
  return(sqrt(sum((y - yhat)^2)) / sum(y))
}

pse0_abs = function(y, yhat) {
  return(sqrt(sum((y - yhat)^2)) / abs(sum(y)))
}

pse = function(pred, pred_quarter, real, real_quarter){
  return(pse0(yhat = pred, y = real) + pse0(yhat = pred_quarter, y = real_quarter))
}

pse_abs = function(pred, pred_quarter, real, real_quarter){
  return(pse0_abs(yhat = pred, y = real) + pse0_abs(yhat = pred_quarter, y = real_quarter))
}

# oil model

X_oil = prices[1:end_2018, ] %>% 
  select(const, brent, brent_1, rub_usd_1) 
R_oil = prices[1:end_2018, ] %>% 
  select(p_exp_oil, v_exp_oil, r_exp_oil)
R_oil_quarter = prices_quater %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil) %>% na.omit()

make_pred_oil = function(par, X, R){
  X_p = as.matrix(X[2:nrow(X), 1:3])
  p_hat_oil = c(R[1,1], (X_p %*% par[1:3])) # считает прогноз цены
  X_v = tibble(const2 = 1, rub_usd_1 = X$rub_usd_1, 
               p_hat_oil_3 = lag(p_hat_oil, 3))
  X_v_by_par = as.matrix(X_v) %*% par[4:6]
  dummies = rep(c(par[8:13], 1, par[14:18]), 13)
  v_hat_oil = rep(NA, nrow(X))
  v_hat_oil[1:4] = R$v_exp_oil[1:4]
  for (i in 5:nrow(X)) {
    v_hat_oil[i] = (X_v_by_par[i] + par[7] * v_hat_oil[i - 1]) * dummies[i]
  }
  r_hat_oil = p_hat_oil * v_hat_oil
  v_hat_oil_quarter = roll_sum(v_hat_oil, n = 3, by = 3) # рассчет квартального объема
  r_hat_oil_quarter = roll_sum(r_hat_oil, n = 3, by = 3) # рассчет квартальной выручки
  p_hat_oil_quarter = r_hat_oil_quarter / v_hat_oil_quarter
  return(list(p_hat_oil = p_hat_oil, v_hat_oil = v_hat_oil, 
              r_hat_oil = r_hat_oil, 
              p_hat_oil_quarter = p_hat_oil_quarter, 
              v_hat_oil_quarter = v_hat_oil_quarter, 
              r_hat_oil_quarter = r_hat_oil_quarter))
}

pred_oil = make_pred_oil(par_oil, X_oil, R_oil)

autoplot(ts.union(real_data = ts(prices$p_exp_oil[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil') + xlab('') +  ggtitle('Average price of oil exported')
ggsave('oil_p.png')

autoplot(ts.union(real_data = ts(prices$v_exp_oil[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil exported')
ggsave('oil_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_oil[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')
ggsave('oil_r.png')   


mape(pred_oil$v_hat_oil[1:end_2013], prices$v_exp_oil[1:end_2013])
mape(pred_oil$r_hat_oil[1:end_2013], prices$r_exp_oil[1:end_2013])
mape(pred_oil$p_hat_oil[1:end_2013], prices$p_exp_oil[1:end_2013])


r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter



# oil product model

X_op = prices[1:end_2018, ] %>% 
  select(const,
         brent, brent_1, brent_2, brent_3, 
         rub_usd_eur_1, v_prod_op_1) 

R_op = prices[1:end_2018, ] %>% 
  select(p_exp_op, v_exp_op, r_exp_op)

R_op_quarter = prices_quater %>% 
  select(p_exp_op, v_exp_op, r_exp_op) %>% na.omit()



make_pred_op = function(par, X, R){
  X_p = as.matrix(X[4:nrow(X), 1:5])
  p_hat_op = c(R[1:3,1], (X_p %*% par[1:5])) # считает прогноз цены
  
  X_v = tibble(const2 = 1, curr = X$rub_usd_eur_1, 
               v_prod_op_1 = X$v_prod_op_1,
               p_hat_op = lag(p_hat_op, 1))
  X_v_by_par = as.matrix(X_v) %*% par[6:9]
  dummies = rep(c(par[10:15], 1, par[16:20]), 13)
  v_hat_op = rep(NA, nrow(X))
  v_hat_op[1:4] = R$v_exp_op[1:4]
  v_hat_op[5:end_2018] = X_v_by_par[5:end_2018] * dummies[5:end_2018]
  r_hat_op = p_hat_op * v_hat_op
  v_hat_op_quarter = roll_sum(v_hat_op, n = 3, by = 3) 
  r_hat_op_quarter = roll_sum(r_hat_op, n = 3, by = 3) 
  p_hat_op_quarter = r_hat_op_quarter / v_hat_op_quarter
  return(list(p_hat_op = p_hat_op, v_hat_op = v_hat_op, 
              r_hat_op = r_hat_op, 
              p_hat_op_quarter = p_hat_op_quarter, 
              v_hat_op_quarter = v_hat_op_quarter, 
              r_hat_op_quarter = r_hat_op_quarter))
}

pred_op = make_pred_op(par_op, X_op, R_op)
autoplot(ts.union(real_data = ts(prices$p_exp_op[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12))) + ylab('p_exp_op')  + xlab('') + ggtitle('Average price of oil products exported')
ggsave('op_p.png')

autoplot(ts.union(real_data = ts(prices$v_exp_op[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12))) + ylab('v_exp_op') + xlab('') + ggtitle('Average volume of oil products exported')
ggsave('op_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_op[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12))) + ylab('r_exp_op') + xlab('') + ggtitle('Average revenue from export of oil products')

ggsave('op_r.png')

mape(pred_op$v_hat_op[1:end_2013], prices$v_exp_op[1:end_2013])
mape(pred_op$r_hat_op[1:end_2013], prices$r_exp_op[1:end_2013])
mape(pred_op$p_hat_op[1:end_2013], prices$p_exp_op[1:end_2013])

r_hat_op = pred_op$r_hat_op
r_hat_op_quarter = pred_op$r_hat_op_quarter

### optimization for gas model


X_gas = prices[1:end_2018, ] %>% 
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6, 
         v_prod_gas, usd_eur, dif_usd_rub_ratio_1) 

R_gas = prices[1:end_2018, ] %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_gas_quarter = prices_quater %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas) %>% na.omit()


make_pred_gas = function(par, X, R){
  X_p = as.matrix(X[7:nrow(X), 1:9]) 
  p_hat_gas = c(R[1:6,1], (X_p %*% par[1:9])) 
  
  X_v = tibble(const2 = 1, 
               v_prod_gas = X$v_prod_gas,
               p_hat_gas_1 = lag(p_hat_gas, 1),
               p_hat_gas_7 = lag(p_hat_gas, 7),
               usd_eur = X$usd_eur, 
               dif_usd_rub_ratio_1 = X$dif_usd_rub_ratio_1)
  X_v_by_par = as.matrix(X_v) %*% par[10:15]
  dummies = rep(c(par[16:21], 1, par[22:26]), 13)
  v_hat_gas = rep(NA, nrow(X))
  v_hat_gas[1:8] = R$v_exp_gas[1:8]
  v_hat_gas[9:nrow(X)] = X_v_by_par[9:nrow(X)] * dummies[9:nrow(X)]
  r_hat_gas = p_hat_gas * v_hat_gas
  v_hat_gas_quarter = roll_sum(v_hat_gas, n = 3, by = 3) # рассчет квартального объема
  r_hat_gas_quarter = roll_sum(r_hat_gas, n = 3, by = 3) # рассчет квартальной выручки
  p_hat_gas_quarter = r_hat_gas_quarter / v_hat_gas_quarter
  return(list(p_hat_gas = p_hat_gas, v_hat_gas = v_hat_gas, 
              r_hat_gas = r_hat_gas, 
              p_hat_gas_quarter = p_hat_gas_quarter, 
              v_hat_gas_quarter = v_hat_gas_quarter, 
              r_hat_gas_quarter = r_hat_gas_quarter))
}

pred_gas = make_pred_gas(par_gas, X_gas, R_gas)

r_hat_gas = pred_gas$r_hat_gas
r_hat_gas_quarter = pred_gas$r_hat_gas_quarter


autoplot(ts.union(real_data = ts(prices$p_exp_gas[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12))) + ylab('p_exp_gas') + xlab('') + ggtitle('Average price of gas exported')
ggsave('gas_p.png')
autoplot(ts.union(real_data = ts(prices$v_exp_gas[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12))) + ylab('v_exp_gas') + xlab('') + ggtitle('Average volume of gas exported')
ggsave('gas_v.png')
autoplot(ts.union(real_data = ts(prices$r_exp_gas[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12))) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from export of gas')
ggsave('gas_r.png')

mape(pred_gas$v_hat_gas[1:end_2013], prices$v_exp_gas[1:end_2013])
mape(pred_gas$r_hat_gas[1:end_2013], prices$r_exp_gas[1:end_2013])
mape(pred_gas$p_hat_gas[1:end_2013], prices$p_exp_gas[1:end_2013])

### optimisation for export other goods model

X_othg = tibble(const = 1, 
                r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op, 
                r_hat_oog_dum = r_hat_oog * prices$dum_1114[1:end_2018],
                gpd_defl = lag(prices$n_y, 1)[1:end_2018] / prices$rub_usd_1[1:end_2018])

R_othg = prices %>% select(r_exp_othg, r_exp_goods)

R_othg_quarter = prices_quater %>% 
  select(r_exp_othg, r_exp_goods) %>% na.omit()


make_pred_exp = function(par, X, R){
  dummies = rep(c(par[5:10], 1, par[11:15]), 13)
  r_hat_othg = rep(NA, nrow(X))
  r_hat_othg[1] = R[1,1]
  r_hat_othg[2:end_2018] = (as.matrix(X[2:nrow(X), ]) %*% par[1:4]) * dummies[2:end_2018]
  r_hat_othg_quarter = roll_sum(r_hat_othg, n = 3, by = 3) # рассчет квартальной выручки
  r_hat_gds = X$r_hat_oog + r_hat_othg
  r_hat_gds_quarter = roll_sum(r_hat_gds, n = 3, by = 3)
  return(list(r_hat_othg = r_hat_othg, 
              r_hat_othg_quarter = r_hat_othg_quarter,
              r_hat_gds = r_hat_gds, 
              r_hat_gds_quater = r_hat_gds_quarter))
}


pred_exp = make_pred_exp(par_othg, X_othg, R_othg)

autoplot(ts.union(real_data = ts(prices$r_exp_othg[1:end_2013], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of other goods')
ggsave('exp_othg.png')


autoplot(ts.union(real_data = ts(prices$r_exp_goods[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of all goods')

ggsave('exp_all.png')

r_hat_othg = pred_exp$r_hat_othg
r_hat_gds = pred_exp$r_hat_gds

mape(r_hat_othg[1:end_2013], prices$r_exp_othg[1:end_2013])
mape(r_hat_gds[1:end_2018], prices$r_exp_goods[1:end_2018])


# Model for goods import (restore monthly data!!!!) (r_imp_goods^ + r_imp_serv^ + r_imp_all^)

X_imp = tibble(consump_defl = lag(prices$n_c, 1)[1:end_2018]/prices$rub_usd_1[1:end_2018],
               j_defl = lag(prices$n_j, 1)[1:end_2018]/prices$rub_usd_1[1:end_2018],
               ds_defl = (prices$n_ds_1)[1:end_2018]/prices$rub_usd_1[1:end_2018],
               r_hat_gds = r_hat_gds)

R_imp = prices %>% 
  select(r_imp_goods, r_imp_serv, r_imp_all)



R_imp_quarter = prices_quater %>% 
  select(r_imp_goods, r_imp_serv, r_imp_all) %>% na.omit()


par_imp = c(par_imp_gds, par_imp_serv)

make_pred_imp = function(par, X, R){
  dummies_gds = rep(c(par[6:11], 1, par[12:16]), 13)
  r_hat_imp_gds = rep(0, nrow(X))
  r_hat_imp_gds[1] = R[1,1]
  r_hat_imp_gds[2:end_2018] = (as.matrix(X[2:nrow(X), ]) %*% par[2:5]) * dummies_gds[2:end_2018] + par[1]
  r_hat_imp_gds_quarter = roll_sum(r_hat_imp_gds, n = 3, by = 3)
  X_serv = select(X, -r_hat_gds) %>% mutate(r_hat_imp_gds = r_hat_imp_gds)
  dummies_serv = rep(c(par[22:27], 1, par[28:32]), 13)
  r_hat_imp_serv = rep(0, nrow(X_serv))
  r_hat_imp_serv[2:end_2018] = (as.matrix(X_serv[2:nrow(X_serv), ]) %*% par[18:21]) * dummies_serv[2:end_2018] + par[17]
  r_hat_imp_serv_quarter = c(NA, roll_sum(r_hat_imp_serv[4:length(r_hat_imp_serv)], n = 3, by = 3))
  r_imp_all = r_hat_imp_gds + r_hat_imp_serv
  r_imp_all_quarter = c(NA, roll_sum(r_imp_all[4:length(r_imp_all)], n = 3, by = 3))
  return(list(r_hat_imp_gds = r_hat_imp_gds, 
              r_hat_imp_gds_quarter = r_hat_imp_gds_quarter,
              r_hat_imp_serv = r_hat_imp_serv, 
              r_hat_imp_serv_quarter = r_hat_imp_serv_quarter,
              r_imp_all = r_imp_all,
              r_imp_all_quarter = r_imp_all_quarter))
}

pred_imp = make_pred_imp(par_imp, X_imp, R_imp)
autoplot(ts.union(real_data = ts(prices$r_imp_goods, start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_hat_imp_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import of goods')

ggsave('imp_goods.png')

autoplot(ts.union(real_data = ts(prices$r_imp_serv[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_hat_imp_serv, start = c(2006, 1), freq = 12))) + ggtitle('Average revenue from import of services') + ylab('revenue') + xlab('')

ggsave('imp_services.png')

autoplot(ts.union(real_data = ts(prices$r_imp_all[1:end_2018], start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_imp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import')
ggsave('imp_all.png')

r_hat_imp_gds = pred_imp$r_hat_imp_gds[1:end_2018]
r_hat_imp_serv = pred_imp$r_hat_imp_serv[1:end_2018]
r_hat_imp_all = pred_imp$r_imp_all[1:end_2018]


mape(r_hat_imp_gds, prices$r_imp_goods[1:end_2018])
mape(r_hat_imp_serv[start_2012:end_2018], prices$r_imp_serv[start_2012:end_2018])
mape(r_hat_imp_all[start_2012:end_2018], prices$r_imp_all[start_2012:end_2018])


### Model for export of services

par = par_exp_serv

X_exp_serv = tibble(const = 1,
                    r_hat_gds = r_hat_gds,
                    r_hat_imp_serv = r_hat_imp_serv)
R_exp_serv = prices %>% 
  select(r_exp_serv)
R_exp_serv_quarter = prices_quater %>% 
  select(r_exp_serv) %>% na.omit()


make_pred_exp_serv = function(par, X, R){
  dummies = rep(c(par[4:9], 1, par[10:14]), 13)
  r_hat_exp_serv = rep(NA, nrow(X))
  r_hat_exp_serv[2:end_2018] = (as.matrix(X[2:nrow(X), ]) %*% par[1:3]) * dummies[2:end_2018]
  r_hat_exp_serv_quarter = roll_sum(r_hat_exp_serv, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_exp_serv = r_hat_exp_serv, 
              r_hat_exp_serv_quarter = r_hat_exp_serv_quarter))
}

pred_exp_serv = make_pred_exp_serv(par_exp_serv, X_exp_serv, R_exp_serv)
autoplot(ts.union(real_data = ts(prices$r_exp_serv, start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp_serv$r_hat_exp_serv, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of services')

ggsave('exp_serv.png')

r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

mape(r_hat_exp_serv[start_2012:end_2018], prices$r_exp_serv[start_2012:end_2018])



r_hat_exp_all = r_hat_exp_serv + r_hat_gds
r_hat_exp_all_quarter = c(NA, roll_sum(r_hat_exp_all[4:end_2018], n = 3, by = 3))

autoplot(ts.union(real_data = ts(prices$r_exp_all, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_exp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of goods and services')

ggsave('exp_gs.png')

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
  rename('r_real' = 'r_bal_rent_sinc')%>%
  na.omit()

make_pred_balances = function(par, X, R){
  n_pred = ncol(X)
  dummies = rep(c(par[(n_pred + 1): (n_pred + 6)], 1, par[(n_pred + 7):(n_pred + 11)]), 13)
  r_hat = rep(NA, nrow(X))
  r_hat[2:end_2018] = (as.matrix(X[2:nrow(X), ]) %*% par[1:n_pred]) * dummies[2:end_2018]
  r_hat_quarter = roll_sum(r_hat, n = 3, by = 3) 
  return(list(r_hat = r_hat, 
              r_hat_quarter = r_hat_quarter))
}


pred_rent_sinc = make_pred_balances(par_rent_sinc, X_rent_sinc, R_rent_sinc)

autoplot(ts.union(real_data = ts(prices$r_bal_rent + prices$r_bal_sinc, start = c(2006, 1), freq = 12),  
                  model = ts(pred_rent_sinc$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of rent and secondary income')

ggsave('rent_sink.png')

r_hat_rent_sinc = pred_rent_sinc$r_hat

mape(r_hat_rent_sinc[start_2012:end_2018], (prices$r_bal_rent + prices$r_bal_sinc)[start_2012:end_2018])
mase(r_hat_rent_sinc[start_2012:end_2018], (prices$r_bal_rent + prices$r_bal_sinc)[start_2012:end_2018])
smape(r_hat_rent_sinc[start_2012:end_2018], (prices$r_bal_rent + prices$r_bal_sinc)[start_2012:end_2018])

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

pred_inv = make_pred_balances(par_inv, X_inv, R_inv)

autoplot(ts.union(real_data = ts(prices$r_bal_inv, start = c(2006, 1), freq = 12),  
                  model = ts(pred_inv$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of investment income')

ggsave('inv_income.png')
r_hat_inv = pred_inv$r_hat

mape(r_hat_inv[start_2012:end_2018], prices$r_bal_inv[start_2012:end_2018])


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
  rename('r_real' = 'r_bal_wage') %>%
  na.omit()

pred_wage = make_pred_balances(par_bal_wage, X_wage, R_wage)
autoplot(ts.union(real_data = ts(prices$r_bal_wage, start = c(2006, 1), freq = 12),  
                  model = ts(pred_wage$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of wages')

ggsave('bwages.png')
r_hat_wage = pred_wage$r_hat

mape(r_hat_wage[start_2012:end_2018], prices$r_bal_wage[start_2012:end_2018])
mase(r_hat_wage[start_2012:end_2018], prices$r_bal_wage[start_2012:end_2018])
smape(r_hat_wage[start_2012:end_2018], prices$r_bal_wage[start_2012:end_2018])



### model for erros

X_errors = select(prices, const, dif_brent, dum01:dum12)

R_errors = prices %>% 
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

R_errors_quarter = prices_quater %>% 
  select(r_errors) %>%
  rename('r_real' = 'r_errors') %>%
  na.omit()

make_pred_errors = function(par, X){
  r_hat_errors = as.matrix(X) %*% c(par[1:8], 1, par[9:13])
  r_hat_errors_quarter = roll_sum(r_hat_errors, n = 3, by = 3) 
  return(list(r_hat_errors = r_hat_errors, 
              r_hat_errors_quarter = r_hat_errors_quarter))
}


pred_erros = make_pred_errors(par_errors, X_errors)

r_hat_errors = pred_erros$r_hat_errors

autoplot(ts.union(real_data = ts(prices$r_errors, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('net errors and omissions')
ggsave('errors.png')

mape(r_hat_errors[start_2012:end_2018], prices$r_errors[start_2012:end_2018])
mase(r_hat_errors[start_2012:end_2018], prices$r_errors[start_2012:end_2018])
smape(r_hat_errors[start_2012:end_2018], prices$r_errors[start_2012:end_2018])


### models for difference of reserves

r_hat_cap_acc = c(unlist(import('data/r_hat_cap_account.csv')[1,]))

# until 2018Q12
r_hat_cur_acc = r_hat_cap_acc[1:end_2018] + r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage

r_hat_cur_acc = c(r_hat_cur_acc,rep(NaN, 12))

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
                r_cur_purch_1 = lag(prices$r_cur_purch, 1))


X_difr_dummy = cbind(X_difr, select(prices, dum01:dum12))

R_dif_reserves = prices %>% 
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')

R_dif_reserves_quarter = prices_quater %>% 
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves') %>%
  na.omit()

fill_recursive = function(first_values = 0, add_term = rep(0, 10), coefs = 1,
                          multiplier = rep(1, length(add_term))) {
  add_term = unlist(add_term)
  multiplier = unlist(multiplier)
  nsteps = length(add_term)
  vector = c(first_values, rep(NA, nsteps))
  for (i in 1:nsteps) {
    recur = sum(rev(coefs) * vector[(i + length(first_values) - length(coefs)):(i + length(first_values) - 1)])
    vector[i + length(first_values)] = multiplier[i] * (add_term[i] + recur)
  }  
  
  return(vector)
}



par_dr = c(par_model$par_dif_res_long, par_model$par_dif_res_short)


make_pred_dif_res = function(par, X){
  r_hat_dif_res = rep(NaN, end_2018)
  X_long = as.matrix(select(X, - r_cur_purch, -r_cur_purch_1))
  add_term = X_long %*% c(par[1:22])
  
  r_hat_dif_res[4:end_2018] = fill_recursive(first_values = 13.3059, add_term = add_term[5:end_2018],
                                             coefs = par[23]) # from 2006m04 to 2018m12
  r_hat_dif_res_quarter = roll_sum(r_hat_dif_res, n = 3, by = 3) #from 2Q2006 to 4Q2018
  
  X_short = X %>% select( dum01:dum12, const, dif_brent, dif_brent_1, dif_usd_rub, dif_usd_rub_1,
                          dif_usd_eur, r_hat_cur_acc, r_hat_cur_acc_1,
                          r_cur_purch, r_cur_purch_1)
  X_short_end = X_short[(109:end_2018), ]
  add_term_short = as.matrix(X_short_end) %*% c(par[11:22], par[25:34])
  r_dif_res_short = fill_recursive(first_values = r_hat_dif_res[108], 
                                   add_term = add_term_short,
                                   coefs = par[24]) # from 2006m04 to 2018m12
  
  r_hat_dif_res_short = c(r_hat_dif_res[1:107], r_dif_res_short) #prediction from 2015m01
  r_hat_dif_res_short_quarter = roll_sum(r_hat_dif_res_short, n = 3, by = 3) #from 2Q2006 to 4Q2018
  
  return(list(r_hat_dif_res = r_hat_dif_res, 
              r_hat_dif_res_quarter = r_hat_dif_res_quarter,
              r_hat_dif_res_short = r_hat_dif_res_short, 
              r_hat_dif_res_short_quarter = r_hat_dif_res_short_quarter))
}


pred_res = make_pred_dif_res(par_dif_res, X_difr_dummy)

r_hat_dif_res = pred_res$r_hat_dif_res 
r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
r_hat_dif_res_short = pred_res$r_hat_dif_res_short
r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter

autoplot(ts.union(real_data = ts(prices$r_dif_reserves, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('change') + xlab('') + ggtitle('Difference of reserves')

ggsave('dif_res.png')

mape(r_hat_dif_res_short[start_2012:(end_2018)], prices$r_dif_reserves[start_2012:(end_2018)])


r_hat_bal_fin = r_hat_errors[1:end_2018] - r_hat_dif_res[1:end_2018] + r_hat_cur_acc[1:end_2018]


autoplot(ts.union(real_data = ts(prices$r_bal_fin, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_bal_fin, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of finance')

ggsave('balfin.png')

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
  rename('r_real' = 'r_cur_purch') %>%
  na.omit()



make_pred_cur_purch = function(par, X){
  r_hat_cur_purch = rep(0, end_2018)
  add_term = (as.matrix(X[3:end_2018,1:4]) %*% par[1:4])
  r_hat_cur_purch[2:end_2018] = fill_recursive(add_term = add_term, multiplier = X[3:end_2018,5], coefs = par[5])
  r_hat_cur_purch_quarter = roll_sum(r_hat_cur_purch, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_cur_purch = r_hat_cur_purch, 
              r_hat_cur_purch_quarter = r_hat_cur_purch_quarter))
}



pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur)
r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch 
r_hat_cur_purch_quarter = pred_cur_purch$r_hat_cur_purch_quarter


autoplot(ts.union(real_data = ts(prices$r_cur_purch, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_cur_purch, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Currency purchase')

ggsave('cur_purch.png')

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


mask = matrix(0, 8, end_2018)
mask[1,] = c(rep(1,3), rep(0, 23),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[2,] = c(rep(1,3), rep(0, 2),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[3,] = c(rep(1,3), rep(0, 5),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[4,] = c(rep(1,3), rep(0, 8),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[5,] = c(rep(1,3),rep(0, 11),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[6,] = c(rep(1,3),rep(0, 14),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[7,] = c(rep(1,3),rep(0, 17),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[8,] = c(rep(1,3),rep(0, 20),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]

make_pred_rub_usd = function(par, X, R, mask){
  hat_rub_usd_ratio = rep(NaN, end_2018)
  hat_rub_usd_ratio[2:3] = X$dif_usd_rub_ratio[2:3] 
  
  add_term = as.matrix(X[4:end_2018,1:10]) %*% c(par[1:10])
  
  hat_rub_usd_ratio[3:end_2018] = fill_recursive(first_values = hat_rub_usd_ratio[3], add_term = add_term,
                                                 coefs = par[11]) # from 2006m04 to 2018m12
  
  hat_rub_usd = matrix(NaN, 8, end_2018)
  hat_rub_usd[1:8, 1:3] = matrix(R[1:3,1], ncol = 3, nrow = 8, byrow = TRUE) 
  vector = 1 + hat_rub_usd_ratio
  real_values = matrix(R[1:end_2018,1], ncol = end_2018, nrow = 8, byrow = TRUE)
  
  for (i in 1:nrow(hat_rub_usd)) {
    for (j in 4:ncol(hat_rub_usd)) {
      if (mask[i,j] == 0){
        hat_rub_usd[i,j] = hat_rub_usd[i, j-1] * vector[j]}
      else{
        hat_rub_usd[i,j] = real_values[i,j]
        
      }
    }
  }
  
  hat_rub_usd_final = colMeans(hat_rub_usd)
  hat_rub_usd_quarter = roll_mean(hat_rub_usd_final, n = 3, by = 3)
  
  return(list(hat_rub_usd_final = hat_rub_usd_final, 
              hat_rub_usd_quarter = hat_rub_usd_quarter,
              hat_rub_usd = hat_rub_usd))
}

pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd,R_rub_usd,mask=mask)
hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final

autoplot(ts.union(real_data = ts(prices$rub_usd, start = c(2006, 1), freq = 12),  
                  model = ts(hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')



ggsave('rub_usd.png')

