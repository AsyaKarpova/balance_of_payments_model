library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(tidyverse)



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
                v_prop_op_1 = lag(prices$v_prod_op, 1), 
                gdp_1 = lag(prices$n_y, 1), 
                vnok_1 = lag(prices$n_j, 1), 
                n_ds_1 = lag(prices$n_ds, 1),
                n_c_1 = lag(prices$n_c, 1),
                v_exp_oil_1 = lag(prices$v_exp_oil, 1), 
                p_exp_oil_3 = lag(prices$p_exp_oil, 3))

end = 96
train_size = 84

X = prices[1:end, ] %>% select(const1, brent, brent_1, const2, rub_usd_1) 
R = prices[1:end, ] %>% select(p_exp_oil, v_exp_oil, r_exp_oil)
R_quarter = prices_quater[1:32, ] %>% select(p_exp_oil, v_exp_oil, r_exp_oil)

# MY OPTIMIZATION for oil model
error_opt_oil = function(par, X, R, R_quarter){
  X_p = as.matrix(X[2:nrow(X), 1:3]) # const1 + brent + brent_1
  p_hat_oil = c(R[1,1], (X_p %*% par[1:3])) # считает прогноз цены

  print("p_hat_oil") # ok
  print(head(p_hat_oil, 10))
  
  X_v = tibble(const2 = 1, rub_usd_1 = X$rub_usd_1, 
               p_hat_oil_3 = lag(p_hat_oil, 3))
  X_v_by_par = as.matrix(X_v) %*% par[4:6]

  
  
  print("X_v_by_par") # ok
  print(head(as.vector(X_v_by_par), 10))  

  dummies = rep(c(par[8:13], 1, par[14:18]), 8)
  print("dummies") # ok
  print(head(dummies, 10))
  
  v_hat_oil = rep(NA, nrow(R))
  v_hat_oil[1:4] = R$v_exp_oil[1:4]
  for (i in 5:nrow(R)) {
    v_hat_oil[i] = (X_v_by_par[i] + par[7] * v_hat_oil[i - 1]) * dummies[i]
  }
  
  print("v_hat_oil") # ok
  print(head(v_hat_oil, 10))
  
  r_hat_oil = p_hat_oil * v_hat_oil
  v_hat_oil_quarter = roll_sum(v_hat_oil, n = 3, by = 3) # рассчет квартального объема
  r_hat_oil_quarter = roll_sum(r_hat_oil, n = 3, by = 3) # рассчет квартальной выручки
  p_hat_oil_quarter = r_hat_oil_quarter / v_hat_oil_quarter
  error_p = sqrt(sum(p_hat_oil - R$p_exp_oil)^2)/sum(R$p_exp_oil) + 
           sqrt(sum(p_hat_oil_quarter - R_quarter$p_exp_oil)^2)/sum(R$p_exp_oil)
  error_v = sqrt(sum(v_hat_oil -  R$v_exp_oil)^2)/sum( R$v_exp_oil) + 
           sqrt(sum(v_hat_oil_quarter - R_quarter$v_exp_oil)^2)/sum( R_quarter$v_exp_oil)
  error_r = sqrt(sum(r_hat_oil -  R$r_exp_oil)^2)/sum( R$r_exp_oil) +
           sqrt(sum(r_hat_oil_quarter - R_quarter$r_exp_oil)^2)/sum(R_quarter$r_exp_oil)
  return(error_p + error_v + error_r)
}
par_model = c(-0.002649611,
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
              # 1,
              0.98113109,
              0.959367023,
              1.046286405,
              0.980265077,
              1.030176065)

test = error_opt_oil(par_model, X, R, R_quarter)
test
              
              
X_p = as.matrix(X[, 1:3]) # const1 + brent + brent_1
p_hat_oil = c(R[1,1], (X_p %*% par[1:3])) # считает прогноз цены
v_prel_oil = rep(NA, nrow(R))
v_prel_oil[1:4] = R$v_exp_oil[1:4]
X_v = tibble(const2 = 1, rub_usd_1 = X$rub_usd_1[4:nrow(X)], p_hat_oil_3 = lag(p_hat_oil, 3)[5:(nrow(X)+1)])
X_v_by_par = as.matrix(X_v) %*% par[4:6]

v_prel_oil[5:nrow(R)] = stats::filter(method = "recursive",
                                      filter = par[7],
                                      init = v_prel_oil[4],
                                      x = X_v_by_par) # берем X с пятого значения
print(v_prel_oil)
dummies = rep(par[8:length(par_test)], 8)
v_hat_oil[1:4] = R$v_exp_oil[1:4]
v_hat_oil[5:nrow(R)] = v_prel_oil[5:nrow(R)] * dummies[5:nrow(R)]
v_prel_oil[5:nrow(R)] * dummies[5:nrow(R)]
error_opt_oil(par_model, X = X, R = R, R_quarter = R_quarter)

make_pred = function(X, par){
  as.matrix(X) %*% par
}




### oil_export

X = prices[2:end, ] %>% select(const1, brent, brent_1, const2, rub_usd_1) 
R = prices[1:end, ] %>% select(p_exp_oil, v_exp_oil, r_exp_oil)
R_quarter = prices_quater[1:32, ] %>% select(p_exp_oil, v_exp_oil, r_exp_oil)
#y = prices$p_exp_oil[2:train_size]
#X_test = prices[(train_size+1):end, ] %>% 
#              select(const, brent, brent_1)
#y_test = prices$p_exp_oil[(train_size+1):end]  

result = optim(par = par_model, X = X, R = R, R_quarter = R_quarter,
               fn = error_opt_oil, 
               method = 'L-BFGS-B')

par = result$par
par = result$par
par = par_model
p_hat_oil = c(R[1,1], (make_pred(X[, 1:3], par[1:3]))) 
X_v = as.matrix(cbind(X[3:nrow(X),5:6], p_hat_oil[-1:-3]))
dummies = rep(c(1, par[8:18]), 8)[-1:-3]
length(dummies)
v_prel_oil = (X_v %*% par[5:7]) * dummies + par[4]
v_hat_oil = c(R[1:4,2], v_prel_oil[-1])
length(v_hat_oil)
result$value
autoplot(ts.union(real_data = ts(prices$p_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil')

autoplot(ts.union(real_data = ts(prices$v_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil')
error_opt_oil(par_model, X = X, R = R, R_quarter = R_quarter)
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
              1.030176065)# dum12
      
MAPE(y_hat_poil, y_test)


# MY OPTIMIZATION for oil product model
X = prices[4:train_size,] %>% 
  select(const, brent, brent_1, brent_2, brent_3) 
y = prices$p_exp_op[4:train_size]

X_test = prices[(train_size+1):end,] %>% 
  select(const, brent, brent_1, brent_2, brent_3) 
y_test = prices$p_exp_op[(train_size+1):end]


result = optim(par = rep(0.02, ncol(X)),
               fn = error_opt, 
               method = 'L-BFGS-B')
par = result$par
y_hat_op = make_pred(X_test, par)

autoplot(ts.union(real_data = ts(prices$p_exp_op[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(y_hat_op, start = c(2013,1), freq = 12))) + ylab('p_exp_op')
MAPE(y_hat_op, y_test)

### gas
X = prices[7:train_size, ] %>% 
    select(const, dum_2012,
           gas_europe,
           gas_lng_3, gas_lng_5,
           brent_1, brent_3, brent_5, brent_6)
y = prices$p_exp_gas[7:train_size]

X_test = prices[c(train_size+1):end, ] %>% 
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6)
y_test = prices$p_exp_gas[(train_size+1):end]

result = optim(par = rep(0.0002, ncol(X)),
               fn = error_opt, 
               method = 'L-BFGS-B')
par = result$par
par_excel = c(-0.016227812,-0.051620962, 0.012070903,
        0.004550939,
        0.004138223,
        0.000667546,
        0.000222498,
        0.000436952,
        -0.000155105)

y_hat_gas = ts(make_pred(X_test, par_excel))
MAPE(y_hat_gas, y_test)


autoplot(ts.union(real_data = ts(prices$p_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(y_hat_gas, start = c(2013,1), freq = 12))) + ylab('p_exp_gas')

model_gas_p = lm(y ~ dum_2012 + 
                 brent_1 + brent_3 + brent_5 + brent_6 +
                 gas_europe +
                 gas_lng_3 + gas_lng_5, data = X)

MAPE(predict(model_gas_p, newdata = X_test), y_test)
autoplot(ts.union(real_data = ts(prices$p_exp_gas[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(predict(model_gas_p, newdata = X_test), start = c(2013,1), freq = 12)))+ ylab('p_exp_gas')


# v_exp_oil
dum_month = select(prices, contains('dum0'))
dum_month = dum_month %>% mutate(dum11 = prices$dum11,
                     dum12 = prices$dum12)
dum_month_train = dum_month[4:train_size, ]


#X = t(as.matrix(X)) %*% as.matrix(dum_month_train)

X = prices[4:train_size, ] %>% 
  select(const, v_exp_oil_1, p_exp_oil_3) %>% cbind(dum_month_train)
y = prices$v_exp_oil[4:train_size]
X_test = prices[c(train_size+1):end, ] %>%  select(const, 
         v_exp_oil_1, 
         p_exp_oil_3) %>% cbind(dum_month[c(train_size+1):end,])
y_test = prices$v_exp_oil[(train_size+1):end]

result = optim(par = rep(0.0002, ncol(X)),
               fn = error_opt, 
               method = 'L-BFGS-B')
par = result$par
par
v_exp_oil_hat = ts(make_pred(X_test, par))
MAPE(v_exp_oil_hat, y_test)


autoplot(ts.union(real_data = ts(prices$v_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(v_exp_oil_hat, start = c(2013,1), freq = 12))) + ylab('p_exp_gas')


ts.union(real_data = ts(prices$v_exp_oil[1:end], start = c(2006, 1), freq = 12),  
         model = ts(v_exp_oil_hat, start = c(2013,1), freq = 12))


ts(v_exp_oil_hat, start = c(2013,1), freq = 12)

pse = function(pred, pred_quarter, real){
  sqrt(sum(pred - R$`real`)^2) / sum(R$`real`) + 
    sqrt(sum(pred_quarter - R_quarter$`real``)^2) / sum(R_quarter$`real`)
}


