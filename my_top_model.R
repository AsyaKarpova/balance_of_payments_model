library(forecast)
library(rio)
library(tidyverse)
library(MLmetrics)
library(caret)
library(systemfit)

#prices = import('data.xlsx')

prices = import('data_month.xlsx')
as.vector(tapply(1:9,cut(1:9,3),FUN = sum))
month_to_quarter = function(vec){
  as.vector(tapply(vec,cut(vec,3),FUN = sum))
}

# MY OPTIMIZATION for oil model

error_opt_oil = function(par){
  p_hat_oil = as.matrix(X[, 1:3]) %*% par(1:3) # считает прогноз цены
  X_v = cbind(X, p_hat_oil)
  v_hat_oil = as.matrix(X_v[, 4:ncol(X_v)]) %*% par(4:ncol(X_v))
  r_hat_oil = p_hat_oil * v_hat_oil
  v_hat_oil_quarter = month_to_quarter(v_hat_oil)
  r_hat_oil_quarter = month_to_quarter(r_hat_oil)
  p_hat_oil_quarter = r_hat_oil_quarter / v_hat_oil_quarter
  sqrt(sum((y_hat - y)^2))/sum(y)
  error_p = sqrt(sum(p_hat_oil - p)^2))/sum(p) + sqrt(sum(p_hat_oil_quarter - p_quarter)^2))/sum(p_quarter)
  error_v = sqrt(sum(v_hat_oil - v)^2))/sum(p) + sqrt(sum(v_hat_oil_quarter - v_month)^2))/sum(v_quarter)
  error_r = sqrt(sum(r_hat_oil - r)^2))/sum(r) + sqrt(sum(r_hat_oil_quarter - r_quarter)^2))/sum(r_quarter)
  error_p + error_v + error+r
}

make_pred = function(X, par){
  as.matrix(X) %*% par
}




prices = mutate(prices, 
                const = 1,
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
                usd_rub_1 = lag(prices$usd_eur, 1),
                usd_rub_2 = lag(prices$usd_eur, 2), 
                v_prop_op_1 = lag(prices$v_prod_op, 1), 
                gdp_1 = lag(prices$n_y, 1), 
                vnok_1 = lag(prices$n_j, 1), 
                n_ds_1 = lag(prices$n_ds, 1),
                n_c_1 = lag(prices$n_c, 1),
                v_exp_oil_1 = lag(prices$v_exp_oil, 1), 
                p_exp_oil_3 = lag(prices$p_exp_oil, 3))


end = 96
train_size = 84
### oil_export

X = prices[2:train_size, ] %>% select(const, brent, brent_1)
y = prices$p_exp_oil[2:train_size]
X_test = prices[(train_size+1):end, ] %>% 
              select(const, brent, brent_1)
y_test = prices$p_exp_oil[(train_size+1):end]  


result = optim(par = c(-0.02, 0, 0.1),
               fn = error_opt, 
               method = 'L-BFGS-B')

par = result$par
y_hat_poil = make_pred(X_test, par)
MAPE(y_hat_poil, y_test)
fit_poil = c(prices$p_exp_oil[1], 
               make_pred(rbind(X, X_test), par))

model_oil = lm(y ~ brent + brent_1, data = X)
MAPE(predict(model_oil, newdata = X_test), y_test)


autoplot(ts.union(real_data = ts(prices$p_exp_oil[1:end], start = c(2006, 1), freq = 12),  
                  model = ts(y_hat_poil, start = c(2013,1), freq = 12))) + ylab('p_exp_oil')

### oil products'
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
