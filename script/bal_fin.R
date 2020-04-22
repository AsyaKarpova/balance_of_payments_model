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
library(skimr)


all_vars = import('data/data_month.xlsx') %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble() 
all_vars = all_vars %>% filter(year(date) > 2011)
source('script/functions.R')

length(all_vars$date)/3

all_vars_quater = import('data/data_quarter_new.xlsx') 
quater_date = all_vars_quater$date
all_vars_quater = select(all_vars_quater, -date)/1000
all_vars_quater = mutate(all_vars_quater, date = yearquarter(quater_date)) %>% as_tsibble() %>% filter(year(date) > 2011)

## март 2013 - большие значений aq_obl и aq_assets
autoplot(ts(all_vars$aq_obl, start = c(2012, 1), freq = 12)) + ylab('aq_obl')  + xlab('') + ggtitle('Принятие обязательств')
autoplot(ts(all_vars$aq_assets, start = c(2012, 1), freq = 12)) +  ylab('aq_assets')  + xlab('') + ggtitle('Покупка активов')

cors = cor(all_vars$aq_assets, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>% 
  as.data.frame(row.names = 'val') %>% abs() 

cors_obl = cor(all_vars$aq_obl, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>% 
  as.data.frame(row.names = 'val') %>% abs() 

is_high = (cors > 0.6) %>% as.data.frame 
high_cor_assets = which(unlist(transpose(is_high))) 
names(high_cor_assets)

is_high_obl = (cors_obl > 0.6) %>% as.data.frame 
high_cor_obl = which(unlist(transpose(is_high_obl))) 
names(high_cor_obl)

all_vars %>% select(names(high_cor_obl), dum01:dum12, dum_1114)




X_aq_assets = tibble(const = 1,
                     em_index = all_vars$r_cur_account, 
                  rate_repo = all_vars$rate_repo,
                  rub_usd = all_vars$rub_usd)
X_aq_assets_dum
X_aq_assets_dum = bind_cols(X_aq_assets, select(all_vars, dum01:dum11))
ncol(X_aq_assets_dum)
R_assets = all_vars %>% 
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets')

R_assets_quarter = all_vars_quater %>% 
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets') %>%
  na.omit()
X = X_aq_assets[1:96, ]
par = par_0
as.matrix(X) %*% par[1:4]
length(dummies)
summary(lm(all_vars$aq_assets ~ ., data = X_aq_assets))
make_pred_assets = function(par, X){
  dummies = rep(c(par[5:10], 1, par[11:15]), nrow(X)/12)
  hat_aq_assets = as.vector((as.matrix(X) %*% par[1:4]) * dummies)
  #hat_aq_assets = fill_recursive(add_term = add_term, coefs = par[16])
  hat_aq_assets_quarter = roll_sum(hat_aq_assets, n = 3, by = 3) # рассчет квартальной выручки
  return(list(hat_aq_assets = hat_aq_assets, 
              hat_aq_assets_quarter = hat_aq_assets_quarter))
}

nrow(all_vars_quater)
error_opt_assets = function(par, X, R, R_quarter){
  frcst = make_pred_assets(par, X)
  error = pse_abs(pred = frcst$hat_aq_assets, pred_quarter = frcst$hat_aq_assets_quarter,
                  real = R_assets[1:96,]$r_real, real_quarter = R_assets_quarter$r_real)
  
  return(error)
}
all_vars_quater
R_assets_quarter$r_real
R_assets_quarter$r_real
roll_sum(R_assets[1:84,]$r_real, n = 3, by = 3)
par_0 = c(rep(0.8, 15))

result_sa_aq_assets = GenSA(lower = rep(-1, length(par_0)),
                               upper = rep(1.9, length(par_0)),
                               fn = error_opt_assets,
                               X = X_aq_assets, R = R_assets, R_quarter = R_assets_quarter,
                               control = list(verbose = TRUE, max.time = 600))

par_difr_res = result_sa_aq_assets$par

pred_res = make_pred_dif_res(par_difr_res, X_difr_dummy)

r_hat_dif_res = pred_res$r_hat_dif_res 
r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
r_hat_dif_res_short = pred_res$r_hat_dif_res_short
r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter

autoplot(ts.union(real_data = ts(prices$r_dif_reserves, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('difference in reserves')

