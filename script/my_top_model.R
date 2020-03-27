library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(tidyverse)
library(MLmetrics)
library(GenSA)
library(stats)

library(Metrics)

prices = import('data/data_month.xlsx')
prices_quater = import('data/data_quarter.xlsx')
par_model = import('data/par_model.Rds')

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
par_dif_res = par_model$par_dif_res_long
par_dif_res_short = par_model$par_dif_res_short

lag = function(ts, n){
  return(dplyr::lag(ts, n = n))
}

# make dataframe 
prices = mutate(prices, 
                const = 1,
                brent_1 = lag(prices$brent, n = 1),
                brent_2 = lag(prices$brent, n = 2),
                brent_3 = lag(prices$brent, n = 3),
                brent_4 = lag(prices$brent, n = 4),
                brent_5 = lag(prices$brent, n = 5),
                brent_6 = lag(prices$brent, n = 6),
                gas_europe_1 = lag(prices$gas_europe, n = 1), 
                gas_europe_2 = lag(prices$gas_europe, n = 2), 
                gas_europe_3 = lag(prices$gas_europe, n = 3), 
                gas_lng_1 = lag(prices$gas_lng, n = 1), 
                gas_lng_2 = lag(prices$gas_lng, n = 2), 
                gas_lng_3 = lag(prices$gas_lng, n = 3), 
                gas_lng_4 = lag(prices$gas_lng, n = 4),
                gas_lng_5 = lag(prices$gas_lng, n = 5), 
                rub_usd_1 = lag(prices$rub_usd, 1),
                rub_usd_2 = lag(prices$rub_usd, 2), 
                usd_eur_1 = lag(prices$usd_eur, 1),
                v_prod_op_1 = lag(prices$v_prod_op, 1), 
                gdp_1 = lag(prices$n_y, 1), 
                vnok_1 = lag(prices$n_j, 1), 
                n_ds_1 = lag(prices$n_ds, 1),
                n_c_1 = lag(prices$n_c, 1),
                n_j_1 = lag(prices$n_j, 1), # n_j — ВНОК
                n_ds = lag(prices$n_ds, 1),
                v_exp_oil_1 = lag(prices$v_exp_oil, 1), 
                p_exp_oil_3 = lag(prices$p_exp_oil, 3),
                rub_usd_eur_1 = rub_usd_1 * usd_eur_1,
                r_rent_sinc = r_bal_rent + r_bal_sinc,
                dif_usd_rub_ratio = (rub_usd_1 - rub_usd_2)/rub_usd_2,
                dif_brent = brent - brent_1,
                dif_usd_rub = rub_usd - rub_usd_1,
                dif_usd_eur = usd_eur - usd_eur_1)

prices$dif_brent[1] = 0

end = 156 # number of months with full gas, oil, op data


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

# MY OPTIMIZATION for oil model
par = par_oil

X = prices[1:end, ] %>% 
  select(const, brent, brent_1, rub_usd_1) 
R = prices[1:end, ] %>% 
  select(p_exp_oil, v_exp_oil, r_exp_oil)
R_quarter = prices_quater %>%
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


error_opt_oil = function(par, X, R, R_quarter){
  fcst = make_pred_oil(par, X, R)
  error_p = pse(pred = fcst$p_hat_oil[1:96], pred_quarter = fcst$p_hat_oil_quarter,
                real = R$p_exp_oil[1:96], real_quarter = R_quarter$p_exp_oil)
  error_v = pse(pred = fcst$v_hat_oil[1:96], pred_quarter = fcst$v_hat_oil_quarter,
                real = R$v_exp_oil[1:96], real_quarter = R_quarter$v_exp_oil)
  error_r = pse(pred = fcst$r_hat_oil[1:96], pred_quarter = fcst$r_hat_oil_quarter,
                real = R$r_exp_oil[1:96], real_quarter = R_quarter$r_exp_oil)
  return(error_p + error_v + error_r)
}


#par_0 = c(rep(0.02, 7), rep(0.91, 11))

#result = optim(par = par_0, X = X, R = R, R_quarter = R_quarter,
#               fn = error_opt_oil, 
 #              method = 'L-BFGS-B')

#result_sa = GenSA(lower = rep(-1, 18),
 #                 upper = rep(1.9, 18),
  #                fn = error_opt_oil,
   #               X = X, R = R, R_quarter = R_quarter,
    #              control = list(verbose = TRUE, max.time = 300))

#par_oil = result_sa$par

pred_oil = make_pred_oil(par_oil, X, R)

autoplot(ts.union(real_data = ts(prices$p_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil')

autoplot(ts.union(real_data = ts(prices$v_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil') + ggtitle('Average volume of oil exported')
autoplot(ts.union(real_data = ts(prices$r_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12)),size=1) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')
      
MAPE(pred_oil$v_hat_oil[1:96], prices$v_exp_oil[1:96])
MAPE(pred_oil$r_hat_oil[1:96], prices$r_exp_oil[1:96])
MAPE(pred_oil$p_hat_oil[1:96], prices$p_exp_oil[1:96])

r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter



# MY OPTIMIZATION for oil product model

par = par_op
X = prices[1:end, ] %>% 
  select(const,
         brent, brent_1, brent_2, brent_3, 
         rub_usd_eur_1, v_prod_op_1) 

R = prices[1:end, ] %>% 
  select(p_exp_op, v_exp_op, r_exp_op)

R_quarter = prices_quater %>% 
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
  v_hat_op[5:end] = X_v_by_par[5:end] * dummies[5:end]
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



error_opt_op = function(par, X, R, R_quarter){
  frcst = make_pred_op(par, X, R)
  error_p = pse(pred = frcst$p_hat_op[1:96], pred_quarter = frcst$p_hat_op_quarter,
                real = R$p_exp_op[1:96], real_quarter = R_quarter$p_exp_op)
  
  error_v = pse(pred = frcst$v_hat_op[1:96], pred_quarter = frcst$v_hat_op_quarter,
                real = R$v_exp_op[1:96], real_quarter = R_quarter$v_exp_op)
  error_r = pse(pred = frcst$r_hat_op[1:96], pred_quarter = frcst$r_hat_op_quarter,
                real = R$r_exp_op[1:96], real_quarter = R_quarter$r_exp_op)
  return(error_p + error_v + error_r)
}

#result = optim(par = rep(0.02, 20), X = X, R = R, R_quarter = R_quarter,
 #              fn = error_opt_op, 
  #             method = 'L-BFGS-B')

#par_op = result$par

pred_op = make_pred_op(par_op, X, R)
autoplot(ts.union(real_data = ts(prices$p_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12))) + ylab('p_exp_op')
autoplot(ts.union(real_data = ts(prices$v_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12))) + ylab('v_exp_op')
autoplot(ts.union(real_data = ts(prices$r_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12))) + ylab('r_exp_op')

MAPE(pred_op$v_hat_op[1:96], prices$v_exp_op[1:96])
MAPE(pred_op$r_hat_op[1:96], prices$r_exp_op[1:96])
MAPE(pred_op$p_hat_op[1:96], prices$p_exp_op[1:96])

r_hat_op = pred_op$r_hat_op
r_hat_op_quarter = pred_op$r_hat_op_quarter

### optimization for gas model
par = par_gas
X = prices[1:end, ] %>% 
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6, 
         v_prod_gas, usd_eur, dif_usd_rub_ratio) 

R = prices[1:end, ] %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_quarter = prices_quater %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas) %>% na.omit()


make_pred_gas = function(par, X, R){
  X_p = as.matrix(X[7:nrow(X), 1:9]) 
  p_hat_gas = c(R[1:6,1], (X_p %*% par[1:9])) 
  
  X_v = tibble(const2 = 1, 
               v_prod_gas = X$v_prod_gas,
               p_hat_gas_1 = lag(p_hat_gas, 1),
               p_hat_gas_7 = lag(p_hat_gas, 7),
               usd_eur = X$usd_eur, 
               dif_usd_rub_ratio = X$dif_usd_rub_ratio)
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


error_opt_gas = function(par, X, R, R_quarter){
  frcst = make_pred_gas(par, X, R)
  error_p = pse(pred = frcst$p_hat_gas[1:96], pred_quarter = frcst$p_hat_gas_quarter,
                real = R$p_exp_gas[1:96], real_quarter = R_quarter$p_exp_gas)
  error_v = pse(pred = frcst$v_hat_gas[1:96], pred_quarter = frcst$v_hat_gas_quarter,
                real = R$v_exp_gas[1:96], real_quarter = R_quarter$v_exp_gas)
  error_r = pse(pred = frcst$r_hat_gas[1:96], pred_quarter = frcst$r_hat_gas_quarter,
                real = R$r_exp_gas[1:96], real_quarter = R_quarter$r_exp_gas)
  return(error_p + error_v + error_r)
}


#result = optim(par = c(rep(0.02, 15), rep(0.99, 11)), X = X, R = R, R_quarter = R_quarter,
 #              fn = error_opt_gas, 
  #             method = 'L-BFGS-B')

#result_sa = GenSA(lower = rep(-1, 18),
 #                 upper = rep(1.9, 18),
  #                fn = error_opt_gas,
   #               X = X, R = R, R_quarter = R_quarter,
    #              control = list(verbose = TRUE, max.time = 300))

#par_gas = result$par
pred_gas = make_pred_gas(par_gas, X, R)
autoplot(ts.union(real_data = ts(prices$p_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12))) + ylab('p_exp_gas')
autoplot(ts.union(real_data = ts(prices$v_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12))) + ylab('v_exp_gas')
autoplot(ts.union(real_data = ts(prices$r_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12))) + ylab('r_exp_gas')

MAPE(pred_gas$v_hat_gas[1:96], prices$v_exp_gas[1:96])
MAPE(pred_gas$r_hat_gas[1:96], prices$r_exp_gas[1:96])
MAPE(pred_gas$p_hat_gas[1:96], prices$p_exp_gas[1:96])

r_hat_gas = pred_gas$r_hat_gas
r_hat_gas_quarter = pred_gas$r_hat_gas_quarter

# optimisation for export other goods model

par = par_othg

X = tibble(const = 1, 
           r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op, 
           r_hat_oog_dum = r_hat_oog * prices$dum_1114[1:end],
           gpd_defl = lag(prices$n_y, 1)[1:end] / prices$rub_usd_1[1:end])

R = prices %>% select(r_exp_othg, r_exp_goods)

R_quarter = prices_quater %>% 
  select(r_exp_othg, r_exp_goods) %>% na.omit()



make_pred_exp = function(par, X, R){
  dummies = rep(c(par[5:10], 1, par[11:15]), 13)
  r_hat_othg = rep(NA, nrow(X))
  r_hat_othg[1] = R[1,1]
  r_hat_othg[2:end] = (as.matrix(X[2:nrow(X), ]) %*% par[1:4]) * dummies[2:end]
  r_hat_othg_quarter = roll_sum(r_hat_othg, n = 3, by = 3) # рассчет квартальной выручки
  r_hat_gds = X$r_hat_oog + r_hat_othg
  r_hat_gds_quarter = roll_sum(r_hat_gds, n = 3, by = 3)
  return(list(r_hat_othg = r_hat_othg, 
              r_hat_othg_quarter = r_hat_othg_quarter,
              r_hat_gds = r_hat_gds, 
              r_hat_gds_quater = r_hat_gds_quarter))
}


error_opt_othg = function(par, X, R, R_quarter){
  frcst = make_pred_exp_othg(par, X, R)
  error_othg = pse(pred = frcst$r_hat_othg[1:96], pred_quarter = frcst$r_hat_othg_quarter,
                real = R$r_exp_othg[1:96], real_quarter = R_quarter$r_exp_othg)
  error_gds  = pse(pred = frcst$r_hat_gds[1:156], pred_quarter = frcst$r_hat_gds_quater,
                    real = R$r_exp_gds[1:156], real_quarter = R_quarter$r_exp_gds)
  error = error_gds + error_othg

  return(error)
}


pred_exp = make_pred_exp(par_othg, X, R)
autoplot(ts.union(real_data = ts(prices$r_exp_othg[1:96], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue from export of other goods')
autoplot(ts.union(real_data = ts(prices$r_exp_goods[1:156], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_gds, start = c(2006, 1), freq = 12))) + ylab('revenue from export of all goods')

r_hat_othg = pred_exp$r_hat_othg
r_hat_gds = pred_exp$r_hat_gds


MAPE(r_hat_othg[1:96], prices$r_exp_othg[1:96])
MAPE(r_hat_gds[1:156], prices$r_exp_goods[1:156])


# Model for goods import (ПРОГНОЗИРУЕМ НАЗАД!!!!) (r_imp_goods^ + r_imp_serv^ + r_imp_all^)

par = par_imp_gds
X = tibble(consump_defl = lag(prices$n_c, 1)[1:end]/prices$rub_usd_1[1:end],
           j_defl = lag(prices$n_j, 1)[1:end]/prices$rub_usd_1[1:end],
           ds_defl = (prices$n_ds_1)[1:end]/prices$rub_usd_1[1:end],
           r_hat_gds = r_hat_gds)

R = prices %>% 
        select(r_imp_goods, r_imp_serv, r_imp_all)



R_quarter = prices_quater %>% 
  select(r_imp_goods, r_imp_serv, r_imp_all) %>% na.omit()


par = c(par_imp_gds, par_imp_serv)

make_pred_imp = function(par, X, R){
  dummies_gds = rep(c(par[6:11], 1, par[12:16]), 13)
  r_hat_imp_gds = rep(0, nrow(X))
  r_hat_imp_gds[1] = R[1,1]
  r_hat_imp_gds[2:end] = (as.matrix(X[2:nrow(X), ]) %*% par[2:5]) * dummies_gds[2:end] + par[1]
  r_hat_imp_gds_quarter = roll_sum(r_hat_imp_gds, n = 3, by = 3)
  X_serv = select(X, -r_hat_gds) %>% mutate(r_hat_imp_gds = r_hat_imp_gds)
  dummies_serv = rep(c(par[22:27], 1, par[28:32]), 13)
  r_hat_imp_serv = rep(0, nrow(X_serv))
  r_hat_imp_serv[2:end] = (as.matrix(X_serv[2:nrow(X_serv), ]) %*% par[18:21]) * dummies_serv[2:end] + par[17]
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

error_opt_imp = function(par, X, R, R_quarter){
  frcst = make_pred_imp(par, X, R)
  error_imp_gds = pse(pred = frcst$r_hat_imp_gds[1:156], pred_quarter = frcst$r_hat_imp_gds_quarter,
                   real = R$r_imp_goods[1:156], real_quarter = R_quarter$r_imp_goods)

  error_imp_serv  = pse(pred = frcst$r_hat_imp_serv[73:156], pred_quarter = (frcst$r_hat_imp_serv_quarter[2:52]),
                   real = R$r_imp_serv[73:156], real_quarter = (R_quarter$r_imp_serv)[2:52])

  error_imp_all  = pse(pred = frcst$r_imp_all[73:156], pred_quarter = frcst$r_imp_all_quarter[2:52],
                        real = R$r_imp_all[73:156], real_quarter = R_quarter$r_imp_all[2:52])
  error = error_imp_gds + error_imp_serv + error_imp_all
  return(error)
}


par_imp = c(par_imp_gds, par_imp_serv)

pred_imp = make_pred_imp(par_imp, X, R)
autoplot(ts.union(real_data = ts(prices$r_imp_goods, start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_hat_imp_gds, start = c(2006, 1), freq = 12))) + ylab('revenue from import of goods')
autoplot(ts.union(real_data = ts(prices$r_imp_serv[1:156], start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_hat_imp_serv, start = c(2006, 1), freq = 12)), size=1) + ggtitle('Average revenue from import of services') + ylab('revenue') +xlab('')
autoplot(ts.union(real_data = ts(prices$r_imp_all[1:156], start = c(2006, 1), freq = 12),  
                  model = ts(pred_imp$r_imp_all, start = c(2006, 1), freq = 12))) + ylab('revenue from all import')

r_hat_imp_gds = pred_imp$r_hat_imp_gds[1:156]
r_hat_imp_serv = pred_imp$r_hat_imp_serv[1:156]
r_hat_imp_all = pred_imp$r_imp_all[1:156]


MAPE(r_hat_imp_gds, prices$r_imp_goods[1:156])
MAPE(r_hat_imp_serv[73:156], prices$r_imp_serv[73:156])
MAPE(r_hat_imp_all[73:156], prices$r_imp_all[73:156])


### Model for export of services

par = par_exp_serv
X = tibble(const = 1,
           r_hat_gds = r_hat_gds,
           r_hat_imp_serv = r_hat_imp_serv)


R = prices %>% 
  select(r_exp_serv)

R_quarter = prices_quater %>% 
  select(r_exp_serv) %>% na.omit()


make_pred_exp_serv = function(par, X, R){
  dummies = rep(c(par[4:9], 1, par[10:14]), 13)
  r_hat_exp_serv = rep(NA, nrow(X))
  r_hat_exp_serv[2:end] = (as.matrix(X[2:nrow(X), ]) %*% par[1:3]) * dummies[2:end]
  r_hat_exp_serv_quarter = roll_sum(r_hat_exp_serv, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_exp_serv = r_hat_exp_serv, 
              r_hat_exp_serv_quarter = r_hat_exp_serv_quarter))
}

error_opt_exp_serv= function(par, X, R, R_quarter){
  frcst = make_pred_exp_serv(par, X, R)
  error = pse(pred = frcst$r_hat_exp_serv[73:end], pred_quarter = frcst$r_hat_exp_serv_quarter[2:52],
                      real = R$r_exp_serv[73:end], real_quarter = R_quarter$r_exp_serv[2:52])
  return(error)
}


par_exp_serv = par_exp_serv
pred_exp_serv = make_pred_exp_serv(par_exp_serv, X, R)
autoplot(ts.union(real_data = ts(prices$r_exp_serv, start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp_serv$r_hat_exp_serv, start = c(2006, 1), freq = 12))) + ylab('revenue from import of goods')

r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

MAPE(r_hat_exp_serv[73:end], prices$r_exp_serv[73:end])



r_hat_exp_all = r_hat_exp_serv + r_hat_gds
r_hat_exp_all_quarter = c(NA, roll_sum(r_hat_exp_all[4:end], n = 3, by = 3))


r_hat_bal_trade = r_hat_gds - r_hat_imp_gds
r_hat_bal_trade_quarter = roll_sum(r_hat_bal_trade, n = 3, by = 3)


r_hat_bal_serv = r_hat_exp_serv - r_hat_imp_serv
r_hat_bal_serv_quarter = roll_sum(r_hat_bal_serv, n = 3, by = 3)


#### model for balance of wages

par = par_bal_wage

X = tibble(const = 1,
           r_hat_oil = r_hat_oil,
           r_hat_othg = r_hat_othg,
           r_hat_exp_serv = r_hat_exp_serv,
           r_hat_imp_serv = r_hat_imp_serv)


R = prices %>% 
  select(r_bal_wage)

R_quarter = prices_quater %>% 
  select(r_bal_wage) %>% na.omit()


make_pred_bal_wage = function(par, X, R){
  dummies = rep(c(par[6:11], 1, par[12:16]), 13)
  r_hat_bal_wage = rep(NA, nrow(X))
  r_hat_bal_wage[2:end] = (as.matrix(X[2:nrow(X), ]) %*% par[1:5]) * dummies[2:end]
  r_hat_bal_wage_quarter = roll_sum(r_hat_bal_wage, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_bal_wage = r_hat_bal_wage, 
              r_hat_bal_wage_quarter = r_hat_bal_wage_quarter))
}


error_opt_bal_wage = function(par, X, R, R_quarter){
  frcst = make_pred_bal_wage(par, X, R)
  error = pse_abs(pred = frcst$r_hat_bal_wage[73:156], pred_quarter = frcst$r_hat_bal_wage_quarter[2:52],
                      real = R$r_bal_wage[73:156], real_quarter = R_quarter$r_bal_wage[2:52])
  return(error)
}


pred_bal_wage = make_pred_bal_wage(par_bal_wage, X, R)
autoplot(ts.union(real_data = ts(prices$r_bal_wage, start = c(2006, 1), freq = 12),  
                  model = ts(pred_bal_wage$r_hat_bal_wage, start = c(2006, 1), freq = 12))) + ylab('revenue from import of goods')

r_hat_bal_wage = pred_bal_wage$r_hat_bal_wage
# as there are null values
mase(r_hat_bal_wage[73:end], prices$r_bal_wage[73:end])
smape(r_hat_bal_wage[73:end], prices$r_bal_wage[73:end])


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
  r_hat[2:end] = (as.matrix(X[2:nrow(X), ]) %*% par[1:n_pred]) * dummies[2:end]
  r_hat_quarter = roll_sum(r_hat, n = 3, by = 3) 
  return(list(r_hat = r_hat, 
              r_hat_quarter = r_hat_quarter))
}

error_opt_balances = function(par, X, R, R_quarter){
  frcst = make_pred_balances(par, X, R)
  error = pse_abs(pred = frcst$r_hat[73:end], pred_quarter = frcst$r_hat_quarter[2:52],
                  real = R$r_real[73:end], real_quarter = R_quarter$r_real[2:52])
  return(error)
}

pred_rent_sinc = make_pred_balances(par_rent_sinc, X_rent_sinc, R_rent_sinc)
autoplot(ts.union(real_data = ts(prices$r_bal_rent + prices$r_bal_sinc, start = c(2006, 1), freq = 12),  
                  model = ts(pred_rent_sinc$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of rent and sinc')

r_hat_rent_sinc= pred_rent_sinc$r_hat

MAPE(r_hat_rent_sinc[73:end], (prices$r_bal_rent + prices$r_bal_sinc)[73:end])
mase(r_hat_rent_sinc[73:end], (prices$r_bal_rent + prices$r_bal_sinc)[73:end])
smape(r_hat_rent_sinc[73:end], (prices$r_bal_rent + prices$r_bal_sinc)[73:end])

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
                  model = ts(pred_inv$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of income')

r_hat_inv = pred_inv$r_hat

MAPE(r_hat_inv[73:end], prices$r_bal_inv[73:156])


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
                  model = ts(pred_wage$r_hat, start = c(2006, 1), freq = 12))) + ylab('balance of rent and sinc')

r_hat_wage = pred_wage$r_hat

MAPE(r_hat_wage[73:end], prices$r_bal_wage[73:end])
mase(r_hat_wage[73:end], prices$r_bal_wage[73:end])
smape(r_hat_wage[73:end], prices$r_bal_wage[73:end])



# model for erros
X_errors = select(prices, const, dif_brent, dum01:dum12)

R_errors = prices %>% 
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

R_errors_quarter = prices_quater %>% 
  select(r_errors) %>%
  rename('r_real' = 'r_errors') %>%
  na.omit()
par = par_errors

make_pred_errors = function(par, X){
  r_hat_errors = as.matrix(X) %*% c(par[1:8], 1, par[9:13])
  r_hat_errors_quarter = roll_sum(r_hat_errors, n = 3, by = 3) 
  return(list(r_hat_errors = r_hat_errors, 
              r_hat_errors_quarter = r_hat_errors_quarter))
}

pred_erros = make_pred_errors(par_errors, X_errors)

r_hat_errors = pred_erros$r_hat_errors

autoplot(ts.union(real_data = ts(prices$r_errors, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('net errors and omissions')

MAPE(r_hat_errors[73:end], prices$r_errors[73:end])
mase(r_hat_errors[73:end], prices$r_errors[73:end])
smape(r_hat_errors[73:end], prices$r_errors[73:end])


### models for difference of reserves
r_hat_cap_acc = c(unlist(import('data/r_hat_cap_account.csv')[1,]))
r_hat_cur_acc = r_hat_cap_acc[1:156] + r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_bal_wage
r_hat_cur_acc = c(r_hat_cur_acc,rep(NaN, 12))

X_difr = tibble(const=1,
                ###r_hat_difr = 1,
                dif_brent = prices$dif_brent, 
                dif_brent_cor = (prices$dif_brent * prices$vcor)/(prices$rub_usd_1),
                dif_brent_1 = prices$brent_1 - prices$brent_2,
                dif_usd_rub = prices$dif_usd_rub,
                dif_usd_rub_1 = prices$rub_usd_1 - prices$rub_usd_1,
                dif_usd_eur = prices$dif_usd_eur,
                r_hat_cur_acc = r_hat_cur_acc,
                r_hat_cur_acc_1 = lag(r_hat_cur_acc, 1),
                quarter = rep((prices_quater$r_dif_reserves)/3, each = 3))

X_difr_dummy = cbind(X_difr, select(prices, dum01:dum12))

R_dif_reserves = prices %>% 
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')

R_dif_reserves = prices_quater %>% 
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves') %>%
  na.omit()

fill_recursive = function(first_values = 0, add_term = rep(0, 10), coefs = 1) {
  len_filter = length(coefs)
  init = rev(tail(first_values, len_filter))
  vector = stats::filter(x = add_term, filter = coefs, method = "recursive",
                         init = init)
  vector = c(first_values, vector)
  return(vector)
}

par_dif_res = par_model$par_dif_res_long
par = par_dif_res
ncol(X)
length(par_dif_res)
X = X_difr_dummy[3:168,]

make_pred_dif_res = function(par, X){
  add_term = as.matrix(X) %*% c(par[1:22])
  r_hat_dif_res = fill_recursive(first_values = 13.3059, add_term = add_term,
                                coefs = par[23])
  r_hat_dif_res_quarter = roll_sum(r_hat_dif_res, n = 3, by = 3) 
  return(list(r_hat_errors = r_hat_errors, 
              r_hat_errors_quarter = r_hat_errors_quarter))
}
add_term
par_oil = c(-0.002649611,
              0.002994676,
              0.003762426,
              10.80411621,
              0.001935955,
              -1.642223975,  
              0.526563047,
              0.910193853,
              0.97407938,
              1.017019325,
              0.988074124,
              1.003387379,
              0.962717453,
              0.98113109,
              0.959367023,
              1.046286405,
              0.980265077,
              1.030176065)

par_gas = c(-0.016227812,
            -0.051620962,
            0.012070903,
            0.004550939,
            0.004138223,
            0.000667546,
            0.000222498,
            0.000436952,
            -0.000155105,
            -9.417258386,
            0.418671158,
            -20.50709686,
            9.32452048,
            5.243157422,
            -21.5521098,
            0.924810863,
            0.980330959,
            0.800746361,
            0.807327112,
            0.938861461,
            1.024199646,
            0.960144564,
            0.985051664,
            0.848908983,
            0.91513756,
            0.972719699)


par_op = c(0.021455161,
           0.001086559,
           0.0032033,
           0.001482887,
           0.000701422,
           -4.289339137,
           0.019229557,
           0.658309357,
           0.217390488,
           0.896034396,
           1.016929155,
           1.133439858,
           1.020017427,
           1.117458467,
           1.102230487,
           1.005589886,
           0.994274199,
           1.053676796,
           0.993775918,
           1.090384854)


par_othg = c(2.147388081,
             0.180296573,
             0.179039559,
             0.040866914,
             0.744895119,
             0.882107635,
             1.05233508,
             1.04812429,
             0.962667943,
             0.960230991,
             1.002860226,
             1.050919469,
             1.071683371,
             1.049034852,
             1.151747398)
             

par_imp_gds = c(1.049324006, 
                0.015074535,
                0.519047894,
                0.324019778,
                0.125191148,
                0.688527214,
                0.839136841,
                0.941038374,
                0.935851812,
                0.892819078,
                0.939531671,
                0.981979958,
                0.970953027,
                1.041314847,
                0.988000363,
                1.068876725)
                
                
par_imp_serv = c(0.319283562,
               0.245707675,
               -0.179028469,
               0.124577229,
               -0.193807357,
               0.622087753,
               0.584436033,
               0.718042244,
               0.75345585,
               0.742137073,
               0.867907353,
               1.047074181,
               0.830107646,
               0.900416689,
               0.697987397,
               1.011055266)


par_exp_serv = c(1.835400425,
               0.012179729,
               0.330359785,
               0.9028616,
               0.978315981,
               0.976041317,
               0.994301665,
               0.989340012,
               1.022732484,
               0.926343089,
               0.965758614,
               0.995769699,
               0.957568178,
               1.08019597)

par_bal_wage = c(0.359017988,
                 -0.104453068,
                 0.059944624,
                 -0.233723366,
                 0.058128175,
                  0.845030902,
                  0.851455153,
                  0.83767428,
                  0.890423198,
                  0.865324334,
                  0.821820415,
                  1.139716096,
                  1.166461168,
                  1.107022578,
                  1.259722863,
                  1.057434429)

par_rent_sinc =c(0.374874168,
                 -0.324972905,
                 0.015547373,
                 0.713948752,
                 1.005337505,
                 1.111242913,
                 0.860500913,
                 0.849931525,
                 -0.202181588,
                 1.06896146,
                 1.37167092,
                 1.280343957,
                 1.343229192,
                 0.535901919)


par_inv = c(-0.037042181,
            -0.081827446,
            0.667868321,
            0.530188797,
            0.848909867,
            0.887474478,
            1.221844703,
            1.114364998,
            2.850092133,
            0.781893301,
            1.170634679,
            0.977866141,
            1.048185117,
            1.706085333)
              
par_errors = c(-0.268656711,
               -0.045928344,
               -2.245322059,
               -0.058132154,
               0.006440311,
               1.608801704,
               0.568254878,
               0.524879154,
               1.408741466,
               -0.450781627,
               0.324313862,
               0.36223661,
               -1.999760656)

par_dif_res_long = c(0.9255412,
                     0.159525424,
                     0.109144127,
                     -0.672599129,
                     0.150885438,
                     -0.529688218,
                     -0.474386585,
                     -35.02701172,
                     -0.384285341,
                     0.203246249,
                     0.794075682,
        #dummy
                     1.232661673,
                     4.145030554,
                     -0.79612462,
                     0.26835764,
                     0.779549363,
                     -0.990841149,
                     -2.979998157,
                     -0.399225178,
                     2.207450963,
                     0.577016344,
                     -0.711361663,
                     -3.33251577)

par_dif_res_short = c(1.446267094,
                      0.193956975,
                      -0.329231003,
                      0.104140249,
                      0.45203721,
                      -0.354722165,
                      25.23290968,
                      0.271550721,
                      -0.419336279,
                      0.959184008,
                      -0.460290563)

par_rub_usd = c(0.007296222,
                0.096013377,
                0.331630027,
                -0.70592926,
                -6.416661093,
                -0.001095364,
                0.020056705,
                -0.020056705,
                -0.086216279,
                0.763533367,
                0.817577214)

par_cur_purch = c(-0.526082393,
                -0.223539427,
                0.024824984,
                0.184782157,
                0.013932286)


par_model = list(par_oil = par_oil, par_gas = par_gas, par_op = par_op, par_othg = par_othg,
                   par_imp_gds = par_imp_gds, par_imp_serv = par_imp_serv, par_exp_serv = par_exp_serv,
                 par_bal_wage = par_bal_wage, 
                 par_rent_sinc = par_rent_sinc,
                 par_errors = par_errors,
                 par_inv = par_inv, 
                 par_dif_res_long = par_dif_res_long, 
                 par_dif_res_short = par_dif_res_short,
                 par_cur_purch = par_cur_purch,
                 par_rub_usd = par_rub_usd)

length(par_model)
export(par_model, 'par_model.Rds')
par = import('par_model.Rds')

