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
                v_prod_op_1 = lag(prices$v_prod_op, 1), 
                gdp_1 = lag(prices$n_y, 1), 
                vnok_1 = lag(prices$n_j, 1), 
                n_ds_1 = lag(prices$n_ds, 1),
                n_c_1 = lag(prices$n_c, 1),
                v_exp_oil_1 = lag(prices$v_exp_oil, 1), 
                p_exp_oil_3 = lag(prices$p_exp_oil, 3),
                rub_usd_eur_1 = rub_usd_1 * usd_eur_1,
                dif_usd_rub = (rub_usd_1 - rub_usd_2)/rub_usd_2)

end = 156 # number of months with full gas, oil, op data

pse0 = function(y, yhat) {
  return(sqrt(sum((y - yhat)^2)) / sum(y))
}

pse = function(pred, pred_quarter, real, real_quarter){
  return(pse0(yhat = pred, y = real) + pse0(yhat = pred_quarter, y = real_quarter))
}

# MY OPTIMIZATION for oil model
par = par_oil
X = prices[1:end, ] %>% 
  select(const1, brent, brent_1, const2, rub_usd_1) 
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
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil')
autoplot(ts.union(real_data = ts(prices$r_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('r_exp_oil')
      
MAPE(pred_oil$v_hat_oil[1:96], prices$v_exp_oil[1:96])
MAPE(pred_oil$r_hat_oil[1:96], prices$r_exp_oil[1:96])
MAPE(pred_oil$p_hat_oil[1:96], prices$p_exp_oil[1:96])

r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter



# MY OPTIMIZATION for oil product model

par = par_op
X = prices[1:end, ] %>% 
  select(const1,
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
  select(const1, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6, 
         v_prod_gas, usd_eur, dif_usd_rub) 

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
               dif_usd_rub = X$dif_usd_rub)
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


result = optim(par = c(rep(0.02, 15), rep(0.99, 11)), X = X, R = R, R_quarter = R_quarter,
               fn = error_opt_gas, 
               method = 'L-BFGS-B')

result_sa = GenSA(lower = rep(-1, 18),
                  upper = rep(1.9, 18),
                  fn = error_opt_gas,
                  X = X, R = R, R_quarter = R_quarter,
                  control = list(verbose = TRUE, max.time = 300))

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
           r_hat_goods = r_hat_gas + r_hat_oil + r_hat_op, 
           r_hat_good_dum = r_hat_goods * prices$dum_1114[1:end],
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
  r_hat_goods
  r_hat_goods = X$r_hat_goods + r_hat_othg
  r_hat_goods_quarter = roll_sum(r_hat_goods, n = 3, by = 3)
  return(list(r_hat_othg = r_hat_othg, 
              r_hat_othg_quarter = r_hat_othg_quarter,
              r_hat_goods = r_hat_goods, 
              r_hat_goods_quater = r_hat_goods_quarter))
}



error_opt_othg = function(par, X, R, R_quarter){
  frcst = make_pred_exp_othg(par, X, R)
  error_othg = pse(pred = frcst$r_hat_othg[1:96], pred_quarter = frcst$r_hat_othg_quarter,
                real = R$r_exp_othg[1:96], real_quarter = R_quarter$r_exp_othg)
  error_gds  = pse(pred = frcst$r_hat_goods[1:156], pred_quarter = frcst$r_hat_goods_quater,
                    real = R$r_exp_goods[1:156], real_quarter = R_quarter$r_exp_goods)
  error = error_gds + error_othg

  return(error)
}


pred_exp = make_pred_exp(par_othg, X, R)
autoplot(ts.union(real_data = ts(prices$r_exp_othg[1:96], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue from export of other goods')
autoplot(ts.union(real_data = ts(prices$r_exp_goods[1:156], start = c(2006, 1), freq = 12),  
                  model = ts(pred_exp$r_hat_goods, start = c(2006, 1), freq = 12))) + ylab('revenue from export of all goods')

r_hat_othg = pred_exp$r_hat_othg[1:96]
r_hat_goods = pred_exp$r_hat_goods[1:156]

MAPE(r_hat_othg, prices$r_exp_othg[1:96])
MAPE(r_hat_goods, prices$r_exp_goods[1:156])


# Model for goods import

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
