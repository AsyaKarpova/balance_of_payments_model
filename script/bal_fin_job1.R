# library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)

library(GenSA) # GenSA optimizator
library(nloptr) # stogo optimizator

library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(skimr)

# all data until the end of 2019
# montly data on balance of finance start with 2012

all_vars = import('data/all_vars_pred.csv') %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble()
export(all_vars, 'data/all_vars_scenarios.xlsx')
all_vars = all_vars %>% mutate(em_index_ratio = em_index / lag(em_index, 1))
all_vars = all_vars %>% filter(year(date) > 2011, year(date) < 2020)
source('script/functions.R')
all_vars %>% tail()

all_vars_quater = import('data/data_quarter_new.xlsx')
all_vars_quater = mutate_at(all_vars_quater, vars(-date), ~ . / 1000)
all_vars_quater = mutate(all_vars_quater, date = yearquarter(date)) %>%
  as_tsibble() %>% filter(year(date) > 2011)

## март 2013 - большие значений aq_obl и aq_assets
autoplot(all_vars, dif_usd_eur_ratio) + ylab('aq_obl')  + xlab('') + ggtitle('Принятие обязательств')
autoplot(all_vars, ) +  ylab('aq_assets')  + xlab('') + ggtitle('Покупка активов')

cors = cor(all_vars$aq_assets, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()


cors_obl = cor(all_vars$aq_obl, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()

is_high = (cors > 0.3) %>% as.data.frame
high_cor_assets = which(unlist(transpose(is_high)))
names(high_cor_assets)

is_high_obl = (cors_obl > 0.3) %>% as.data.frame
high_cor_obl = which(unlist(transpose(is_high_obl)))
names(high_cor_obl)



X_aq_assets = tibble(const = 1,
                     r_cur_account = all_vars$r_cur_account,
                     rate_repo = all_vars$rate_repo,
                     rub_usd = all_vars$rub_usd,
                     em_index_ratio = all_vars$em_index_ratio,
                     dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio)
X_aq_assets$em_index_ratio

X_aq_assets_dum = bind_cols(X_aq_assets, select(all_vars, dum01:dum11))


R_assets = all_vars %>%
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets')

R_assets_quarter = all_vars_quater %>%
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets') %>%
  na.omit()


X = X_aq_assets
X %>% tail()
par = rep(0.6, 17)
par_0 = par
make_pred_assets = function(par, X){
  dummies = rep(c(par[7:10], 1, par[11:17]), nrow(X)/12)
  coef = as.matrix(X) %*% par[1:6]
  hat_aq_assets = as.vector(coef * dummies)
  #hat_aq_assets = fill_recursive(add_term = add_term, coefs = par[16])
  hat_aq_assets_quarter = roll_sum(hat_aq_assets, n = 3, by = 3) # рассчет квартальной выручки
  return(list(hat_aq_assets = hat_aq_assets,
              hat_aq_assets_quarter = hat_aq_assets_quarter))
}

R = R_assets
length(frcst$hat_aq_assets)
R_quarter = R_assets_quarter
error_opt_assets = function(par, X, R, R_quarter){
  frcst = make_pred_assets(par, X)
  pse0(frcst$hat_aq_assets, R$r_real)
  error = pse_abs(pred = frcst$hat_aq_assets, pred_quarter = frcst$hat_aq_assets_quarter,
                  real = R$r_real, real_quarter = R_quarter$r_real)

  return(error)
}



error_opt_assets(par_0, X = X_aq_assets, R = R_assets, R_quarter = R_assets_quarter)


result_sa_aq_assets = GenSA(lower = rep(-0.5, length(par_0)),
                            upper = rep(1.9, length(par_0)),
                            fn = error_opt_assets,
                            X = X_aq_assets, R = R_assets, R_quarter = R_assets_quarter,
                            control = list(verbose = TRUE, max.time = 300))

# result_stogo_aq_assets = stogo(lower = rep(-0.5, length(par_0)),
#                            upper = rep(1.9, length(par_0)),
#                            fn = error_opt_assets,
#                            x0 = par_0,
#                            maxeval = 100,
#                            X = X_aq_assets[1:96, ], R = R_assets, R_quarter = R_assets_quarter)
# result_stogo_aq_assets
par_aq_assets = result_sa_aq_assets$par
export(par_aq_assets, 'par_aq_assets.Rds')
pred_res_assets = make_pred_assets(par_aq_assets, X_aq_assets)

tsib2plot = mutate(all_vars, model = pred_res_assets$hat_aq_assets)
autoplot(tsib2plot, vars(aq_assets, model))
tsib2plot %>% select(model, aq_assets) %>%
  pivot_longer(cols = c("model", "aq_assets")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)




cors_obl = cor(all_vars$aq_obl, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()

is_high_obl = (cors_obl > 0.3) %>% as.data.frame
high_cor_obl = which(unlist(transpose(is_high_obl)))
names(high_cor_obl)



X_aq_obl = tibble(const = 1,
                  em_index = all_vars$em_index,
                  rate_repo = all_vars$rate_repo,
                  r_dif_resrves = all_vars$r_dif_reserves,
                  rate_10tr = all_vars$rate_10tr,
                  dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                  diff_r = all_vars$gas_lng_4,
                  rub_usd = all_vars$rub_usd)

ncol(X_aq_obl)
R_obl = all_vars %>%
  select(aq_obl) %>%
  rename('r_real' = 'aq_obl')

R_obl_quarter = all_vars_quater %>%
  select(aq_obl) %>%
  rename('r_real' = 'aq_obl') %>%
  na.omit()


par = rep(0.6, 19)

make_pred_obl = function(par, X){
  dummies = rep(c(par[9:10], 1, par[11:19]), nrow(X)/12)
  coef = as.matrix(X) %*% par[1:8]
  hat_aq_obl = as.vector(coef * dummies)
  #hat_aq_assets = fill_recursive(add_term = add_term, coefs = par[16])
  hat_aq_obl_quarter = roll_sum(hat_aq_obl, n = 3, by = 3) # рассчет квартальной выручки
  return(list(hat_aq_obl = hat_aq_obl,
              hat_aq_obl_quarter = hat_aq_obl_quarter))
}
X = X_aq_obl
R = R_assets
R_quarter = R_obl_quarter
as.matrix(X_aq_obl) %*% par[1:8]
error_opt_obl = function(par, X, R, R_quarter){
  frcst = make_pred_obl(par, X)
  pse0(frcst$hat_aq_obl_quarter, R_quarter$r_real)
  error = pse_abs(pred = frcst$hat_aq_obl, pred_quarter = frcst$hat_aq_obl_quarter,
                  real = R$r_real, real_quarter = R_quarter$r_real)


  return(error)
}


ncol(X_aq_obl)
par_0 = par
error_opt_obl(par_0, X = X_aq_obl, R = R_obl, R_quarter = R_obl_quarter)


result_sa_obl = GenSA(lower = rep(-0.5, length(par_0)),
                      upper = rep(1.9, length(par_0)),
                      fn = error_opt_obl,
                      X = X_aq_obl, R = R_obl, R_quarter = R_obl_quarter,
                      control = list(verbose = TRUE, max.time = 300))


par_aq_obl = result_sa_obl$par
export(par_aq_obl, 'par_aq_obl.Rds')
pred_res_obl = make_pred_obl(par_aq_obl, X_aq_obl)

tsib2plot2 = mutate(all_vars, model = pred_res_obl$hat_aq_obl)
autoplot(tsib2plot2, vars(aq_obl, model))
tsib2plot %>% select(model, aq_obl) %>%
  pivot_longer(cols = c("model", "aq_obl")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)



r_bal_fin_hat = pred_res_assets$hat_aq_assets - pred_res_obl$hat_aq_obl
tsib2plot3 = mutate(all_vars, model = r_bal_fin_hat)
autoplot(tsib2plot3, vars(r_bal_fin, model))
tsib2plot %>% select(model, r_bal_fin) %>%
  pivot_longer(cols = c("model", "r_bal_fin")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)

mse(all_vars$r_bal_fin, r_bal_fin_hat)
