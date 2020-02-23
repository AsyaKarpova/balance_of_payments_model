library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(tidyverse)
library(MLmetrics)
library(GenSA)

prices = import('data_month.xlsx')
prices_quater = import('data_quarter.xlsx')

# make dataframe with lags
prices = mutate(prices, 
                const1 = 1,
                const2 = 1,
                brent_1 = lag(prices$brent),
                brent_2 = lag(prices$brent, n = 2),
                brent_3 = lag(prices$brent, n = 3),
                brent_4 = lag(prices$brent, n = 4),
                brent_5 = lag(prices$brent, n = 5),
                brent_6 = lag(prices$brent, n = 6),
                gas_europe_1 = lag(prices$gas_europe), 
                gas_europe_2 = lag(prices$gas_europe, n = 2), 
                gas_europe_3 = lag(prices$gas_europe, n = 3), 
                gas_lng_1 = lag(prices$gas_lng), 
                gas_lng_2 = lag(prices$gas_lng, n = 2), 
                gas_lng_3 = lag(prices$gas_lng, n = 3), 
                gas_lng_4 = lag(prices$gas_lng, n = 4),
                gas_lng_5 = lag(prices$gas_lng, n = 5), 
                rub_usd_1 = lag(prices$rub_usd, 1),
                rub_usd_2 = lag(prices$rub_usd, 2), 
                usd_eur_1 = lag(prices$usd_eur, 1),
                v_prop_op_1 = lag(prices$v_prod_op, 1), 
                gdp_1 = lag(prices$n_y, 1), 
                vnok_1 = lag(prices$n_j, 1), 
                n_ds_1 = lag(prices$n_ds, 1),
                n_c_1 = lag(prices$n_c, 1),
                v_exp_oil_1 = lag(prices$v_exp_oil, 1), 
                p_exp_oil_3 = lag(prices$p_exp_oil, 3),
                rub_usd_eur_1 = rub_usd_1 * usd_eur_1,
                dif_usd_rub = rub_usd_1/rub_usd_2 - lag(rub_usd_1/rub_usd_2))

end = 96
train_size = 84

make_pred_oil = function(par, X, R){
  X_p = as.matrix(X[2:nrow(X), 1:3]) # const1 + brent + brent_1
  p_hat_oil = c(R[1,1], (X_p %*% par[1:3])) # считает прогноз цены
  X_v = tibble(const2 = 1, rub_usd_1 = X$rub_usd_1, 
               p_hat_oil_3 = lag(p_hat_oil, 3))
  X_v_by_par = as.matrix(X_v) %*% par[4:6]
  dummies = rep(c(par[8:13], 1, par[14:18]), 8)
  v_hat_oil = rep(NA, nrow(R))
  v_hat_oil[1:4] = R$v_exp_oil[1:4]
  for (i in 5:nrow(R)) {
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

pse = function(pred, pred_quarter, real, real_quarter){
  sqrt(sum(pred - real)^2) / sum(real) + 
    sqrt(sum(pred_quarter - real_quarter)^2) / sum(real_quarter)
}

# MY OPTIMIZATION for oil model
X = prices[1:end, ] %>% 
  select(const1, brent, brent_1, const2, rub_usd_1) 
R = prices[1:end, ] %>% 
  select(p_exp_oil, v_exp_oil, r_exp_oil)
R_quarter = prices_quater[1:32, ] %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil)

error_opt_oil = function(par, X, R, R_quarter){
  fcst = make_pred_oil(par, X, R)
  error_p = pse(pred = fcst$p_hat_oil, pred_quarter = fcst$p_hat_oil_quarter,
                real = R$p_exp_oil, real_quarter = R_quarter$p_exp_oil)
  error_v = pse(pred = fcst$v_hat_oil, pred_quarter = fcst$v_hat_oil_quarter,
                real = R$v_exp_oil, real_quarter = R_quarter$v_exp_oil)
  error_r = pse(pred = fcst$r_hat_oil, pred_quarter = fcst$r_hat_oil_quarter,
                real = R$r_exp_oil, real_quarter = R_quarter$r_exp_oil)
  return(error_p + error_v + error_r)
}

par_0 = c(rep(0.02, 7), rep(0.91, 11))

par_sa = c(-0.959886441126994, 0.00791032802573506, 0.00962942582957172, 
           -0.66701939358194, 1.12494925684718, 1.13947857222876, -0.722257157856236, 
           0.683710772722907, 1.8047865764408, 0.358808179729874, -0.704462190421898, 
           1.5704515252412, 1.58129517756235, -0.927518606692626, -0.734046937569133, 
           1.49189949772988, -0.609636457846204, 1.72665699893003)
result = optim(par = par_0, X = X, R = R, R_quarter = R_quarter,
               fn = error_opt_oil, 
               method = 'L-BFGS-B')

par_oil = result$par
par_oil
result$value

result_sa = GenSA(lower = rep(-1, 18),
                  upper = rep(1.9, 18),
                  fn = error_opt_oil,
                  X = X, R = R, R_quarter = R_quarter,
                  control = list(verbose = TRUE, max.time = 600))


pred_oil = make_pred_oil(par_oil, X, R)

autoplot(ts.union(real_data = ts(prices$p_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil')

autoplot(ts.union(real_data = ts(prices$v_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil')
autoplot(ts.union(real_data = ts(prices$r_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('r_exp_oil')
      
MAPE(pred_oil$v_hat_oil, prices$v_exp_oil[1:end])
MAPE(pred_oil$r_hat_oil, prices$r_exp_oil[1:end])
MAPE(pred_oil$p_hat_oil, prices$p_exp_oil[1:end])

# MY OPTIMIZATION for oil product model
make_pred_op = function(par, X, R){
  X_p = as.matrix(X[4:nrow(X), 1:5])
  p_hat_op = c(R[1:3,1], (X_p %*% par[1:5])) # считает прогноз цены
  X_v = tibble(const2 = 1, curr = X$rub_usd_eur_1, 
               v_prod_op_1 = X$v_prop_op_1,
               p_hat_op = p_hat_op)
  
  X_v_by_par = as.matrix(X_v) %*% par[6:9]
  dummies = rep(c(par[9:14], 1, par[15:19]), 8)
  
  v_hat_op = rep(NA, nrow(R))
  v_hat_op[1:4] = R$v_exp_op[1:4]
  v_hat_op[5:end] = X_v_by_par[5:end]
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

X = prices[1:end, ] %>% 
  select(const1,
         brent, brent_1, brent_2, brent_3, 
         rub_usd_eur_1, v_prop_op_1) 

R = prices[1:end, ] %>% 
  select(p_exp_op, v_exp_op, r_exp_op)

R_quarter = prices_quater[1:32, ] %>% 
  select(p_exp_op, v_exp_op, r_exp_op)

error_opt_op = function(par, X, R, R_quarter){
  frcst = make_pred_op(par, X, R)
  error_p = pse(pred = frcst$p_exp_op, pred_quarter = frcst$p_exp_op_quarter,
                real = R$p_exp_op, real_quarter = R_quarter$p_exp_op)
  error_v = pse(pred = frcst$v_exp_op, pred_quarter = frcst$v_exp_op_quarter,
                real = R$v_exp_op, real_quarter = R_quarter$v_exp_op)
  error_r = pse(pred = frcst$r_exp_op, pred_quarter = frcst$r_exp_op_quarter,
                real = R$r_exp_op, real_quarter = R_quarter$r_exp_op)
  return(error_p + error_v + error_r)
}

result = optim(par = rep(0.02, 19), X = X, R = R, R_quarter = R_quarter,
               fn = error_opt_op, 
               method = 'L-BFGS-B')

par_op = result$par
pred_op = make_pred_op(par_op, X, R)
autoplot(ts.union(real_data = ts(prices$p_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12))) + ylab('p_exp_op')
autoplot(ts.union(real_data = ts(prices$v_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12))) + ylab('v_exp_op')
autoplot(ts.union(real_data = ts(prices$r_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12))) + ylab('r_exp_op')

MAPE(pred_op$v_hat_op, prices$v_exp_op[1:end])
MAPE(pred_op$r_hat_op, prices$r_exp_op[1:end])
MAPE(pred_op$p_hat_op, prices$p_exp_op[1:end])

### optimization for gas model
make_pred_gas = function(par, X, R){
  X_p = as.matrix(X[7:nrow(X), 1:9]) 
  p_hat_gas = c(R[1:6,1], (X_p %*% par[1:9])) 
  X_v = tibble(const2 = 1, rub_usd_1 = X$usd_eur, 
               v_prod_gas = X$v_prod_gas,
               p_hat_gas_1 = lag(p_hat_gas, 1),
               p_hat_gas_7 = lag(p_hat_gas, 7),
               dif_usd_rub = X$dif_usd_rub)
  X_v_by_par = as.matrix(X_v) %*% par[9:14]
  dummies = rep(c(par[15:20], 1, par[21:25]), 8)
  v_hat_gas = rep(NA, nrow(R))
  v_hat_gas[1:8] = R$v_exp_gas[1:8]
  v_hat_gas[9:nrow(X)] = X_v_by_par[9:nrow(X)]
  
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

X = prices[1:end, ] %>% 
  select(const1, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6, 
         v_prod_gas, usd_eur, dif_usd_rub) 

R = prices[1:end, ] %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_quarter = prices_quater[1:32, ] %>% 
  select(p_exp_gas, v_exp_gas, r_exp_gas)

error_opt_gas = function(par, X, R, R_quarter){
  frcst = make_pred_gas(par, X, R)
  error_p = pse(pred = frcst$p_exp_gas, pred_quarter = frcst$p_exp_gas_quarter,
                real = R$p_exp_gas, real_quarter = R_quarter$p_exp_gas)
  error_v = pse(pred = frcst$v_exp_gas, pred_quarter = frcst$v_exp_gas_quarter,
                real = R$v_exp_gas, real_quarter = R_quarter$v_exp_gas)
  error_r = pse(pred = frcst$r_exp_gas, pred_quarter = frcst$r_exp_gas_quarter,
                real = R$r_exp_gas, real_quarter = R_quarter$r_exp_gas)
  return(error_p + error_v + error_r)
}


result = optim(par = c(rep(0.02, 14), rep(0.99, 11)), X = X, R = R, R_quarter = R_quarter,
               fn = error_opt_gas, 
               method = 'L-BFGS-B')
par_gas = result$par
pred_gas = make_pred_gas(par_gas, X, R)

autoplot(ts.union(real_data = ts(prices$p_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12))) + ylab('p_exp_gas')
autoplot(ts.union(real_data = ts(prices$v_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12))) + ylab('v_exp_gas')
autoplot(ts.union(real_data = ts(prices$r_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12))) + ylab('r_exp_gas')

MAPE(pred_gas$v_hat_gas, prices$v_exp_gas[1:end])
MAPE(pred_gas$r_hat_gas, prices$r_exp_gas[1:end])
MAPE(pred_gas$p_hat_gas, prices$p_exp_gas[1:end])







par_model = c(0.001323046,
              0.002919546,
              0.003791471,
              # oil_brent_1
              1.386629767,
              0.000624605,
              -0.345704821,
              0.940670524,
              # usd_rub_1
              0.910193853, # dum01
              0.97407938, # dum01
              1.017019325,# dum01
              0.988074124,# dum01
              1.003387379,# dum01
              0.962717453,# dum06
              0.98113109,# dum01
              0.959367023,# dum01
              1.046286405,# dum01
              0.980265077,# dum01
              1.030176065) # dum12