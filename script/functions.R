# error functions

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

# function for recursive forecast

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

lag = dplyr::lag

# function to create table with lags we need

create_tibble = function(data_raw){
  prices = data_raw
  data = mutate(data_raw,
                               const = 1,
                               date = yearmonth(date),
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
                               dif_usd_rub_ratio = (rub_usd - rub_usd_1)/rub_usd_1,
                               dif_usd_rub_ratio_1 = (rub_usd_1 - rub_usd_2)/rub_usd_2,
                               dif_brent = brent - brent_1,
                               dif_usd_rub = rub_usd - rub_usd_1,
                               dif_usd_eur = usd_eur - usd_eur_1,
                               dif_em_index = em_index - lag(em_index, 1),
                               dif_em_index_ratio = dif_em_index/lag(em_index, 1),
                               dif_brent_ratio = dif_brent/brent_1,
                               dif_usd_eur_ratio = dif_usd_eur/usd_eur_1,
                               dif_r = rate_repo - rate_10tr,
                               dif_r_1 = lag(dif_r, 1))
  return(data)
}


# function for making prediction

make_pred_cur_purch = function(par, X, end = nrow(X)){
  r_hat_cur_purch = rep(0, end)
  add_term = (as.matrix(X[3:end, 1:4]) %*% par[1:4])
  r_hat_cur_purch[2:end] = fill_recursive(add_term = add_term, multiplier = X[3:end,5], coefs = par[5])
  r_hat_cur_purch_quarter = roll_sum(r_hat_cur_purch, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_cur_purch = r_hat_cur_purch,
              r_hat_cur_purch_quarter = r_hat_cur_purch_quarter))
}

make_pred_oil = function(par, X, R, end = nrow(X)){
  X = X[1:end, ]
  X_p = as.matrix(X[, 1:3])
  p_hat_oil = rep(NA, end)
  p_hat_oil = (X_p %*% par[1:3]) # считает прогноз цены
  p_hat_oil[1] = R$p_exp_oil[1]
  p_hat_oil = as.vector(p_hat_oil)
  X_v = tibble(const2 = 1, rub_usd_1 = X$rub_usd_1,
               p_hat_oil_3 = lag(p_hat_oil, 3))
  X_v_by_par = as.matrix(X_v) %*% par[4:6]
  dummies = rep(c(par[8:13], 1, par[14:18]), nrow(X)/12)
  v_hat_oil = rep(NA, end)
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


make_pred_op = function(par, X, R, end = nrow(X)){
  X = X[1:end, ]
  X_p = as.matrix(X[, 1:5])
  p_hat_op = (X_p %*% par[1:5]) # считает прогноз цены
  p_hat_op[1:3] = R$p_exp_op[1:3]
  p_hat_op = as.vector(p_hat_op)
  X_v = tibble(const2 = 1, curr = X$rub_usd_eur_1,
               v_prod_op_1 = X$v_prod_op_1,
               p_hat_op = lag(p_hat_op, 1))
  X_v_by_par = as.matrix(X_v) %*% par[6:9]
  dummies = rep(c(par[10:15], 1, par[16:20]), end/12)
  v_hat_op = X_v_by_par * dummies
  v_hat_op[1:5] = R$v_exp_op[1:5]
  v_hat_op = as.vector(v_hat_op)
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


make_pred_gas = function(par, X, R, end = nrow(X)){
  X = X[1:end,]
  X_p = as.matrix(X[, 1:9])
  p_hat_gas = (X_p %*% par[1:9])
  p_hat_gas[1:6] = R$p_exp_gas[1:6]
  p_hat_gas = as.vector(p_hat_gas)

  X_v = tibble(const2 = 1,
               v_prod_gas = X$v_prod_gas,
               p_hat_gas_1 = lag(p_hat_gas, 1),
               p_hat_gas_7 = lag(p_hat_gas, 7),
               usd_eur = X$usd_eur,
               dif_usd_rub_ratio_1 = X$dif_usd_rub_ratio_1)
  X_v_by_par = as.matrix(X_v) %*% par[10:15]
  dummies = rep(c(par[16:21], 1, par[22:26]), nrow(X)/12)
  v_hat_gas = rep(NA, end)
  v_hat_gas[1:end] = X_v_by_par * dummies
  v_hat_gas[1:8] = R$v_exp_gas[1:8]
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


make_pred_exp = function(par, X, R, end = nrow(X)){
  X = X[1:end,]
  dummies = rep(c(par[5:10], 1, par[11:15]), nrow(X)/12)
  r_hat_othg = rep(NA, end)
  r_hat_othg = (as.matrix(X) %*% par[1:4]) * dummies
  r_hat_othg[1] = R$r_exp_othg[1]

  r_hat_othg_quarter = roll_sum(r_hat_othg, n = 3, by = 3) # рассчет квартальной выручки

  r_hat_gds = X$r_hat_oog + r_hat_othg
  r_hat_gds_quarter = roll_sum(r_hat_gds, n = 3, by = 3)
  return(list(r_hat_othg = r_hat_othg,
              r_hat_othg_quarter = r_hat_othg_quarter,
              r_hat_gds = r_hat_gds,
              r_hat_gds_quater = r_hat_gds_quarter))
}

make_pred_imp = function(par, X, R, end = nrow(X)){

  X = X[1:end, ]
  dummies_gds = rep(c(par[6:11], 1, par[12:16]), nrow(X)/12)
  r_hat_imp_gds = (as.matrix(X) %*% par[2:5]) * dummies_gds + par[1]
  r_hat_imp_gds[1] = R$r_imp_goods[1]

  r_hat_imp_gds_quarter = roll_sum(r_hat_imp_gds, n = 3, by = 3)

  X_serv = select(X, -r_hat_gds) %>% mutate(r_hat_imp_gds = r_hat_imp_gds)
  dummies_serv = rep(c(par[22:27], 1, par[28:32]), nrow(X)/12)
  r_hat_imp_serv = (as.matrix(X_serv) %*% par[18:21]) * dummies_serv + par[17]
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

make_pred_exp_serv = function(par, X, R, end = nrow(X)){
  dummies = rep(c(par[4:9], 1, par[10:14]), nrow(X)/12)
  r_hat_exp_serv = (as.matrix(X) %*% par[1:3]) * dummies
  r_hat_exp_serv_quarter = roll_sum(r_hat_exp_serv, n = 3, by = 3) # рассчет квартальной выручки
  return(list(r_hat_exp_serv = r_hat_exp_serv,
              r_hat_exp_serv_quarter = r_hat_exp_serv_quarter))
}

make_pred_balances = function(par, X, R, end = nrow(X)){
  X = X[1:end, ]
  n_pred = ncol(X)
  dummies = rep(c(par[(n_pred + 1):(n_pred + 6)], 1, par[(n_pred + 7):(n_pred + 11)]), nrow(X)/12)
  r_hat = (as.matrix(X) %*% par[1:n_pred]) * dummies
  r_hat_quarter = roll_sum(r_hat, n = 3, by = 3)
  return(list(r_hat = r_hat,
              r_hat_quarter = r_hat_quarter))
}

make_pred_errors = function(par, X, end = nrow(X)){
  r_hat_errors = X %>% as_tibble() %>% select(-date)%>%
    as.matrix() %*% c(par[1:8], 1, par[9:13])
  r_hat_errors_quarter = roll_sum(r_hat_errors, n = 3, by = 3)
  return(list(r_hat_errors = r_hat_errors,
              r_hat_errors_quarter = r_hat_errors_quarter))
}

make_pred_dif_res = function(par, X, end = nrow(X)){
  r_hat_dif_res = rep(NaN, end)
  X_long = as.matrix(select(as_tibble(X), - r_cur_purch, -r_cur_purch_1, -date)) #!
  add_term = X_long %*% c(par[1:22])
  print(X_long)
  r_hat_dif_res[4:end] = fill_recursive(first_values = 13.3059, add_term = add_term[5:end],
                                             coefs = par[23]) # from 2006m04 to 2018m12
  r_hat_dif_res_quarter = roll_sum(r_hat_dif_res, n = 3, by = 3) #from 2Q2006 to 4Q2018

  X_short = X %>% select( dum01:dum12, const, dif_brent, dif_brent_1, dif_usd_rub, dif_usd_rub_1,
                          dif_usd_eur, r_hat_cur_acc, r_hat_cur_acc_1,
                          r_cur_purch, r_cur_purch_1)
  X_short_end = X_short[(109:end), ]
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


create_mask = function(n_months){
  mask = matrix(0, 8, n_months)
  mask[2,] = c(rep(1,3), rep(0, 2),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[3,] = c(rep(1,3), rep(0, 5),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[4,] = c(rep(1,3), rep(0, 8),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[5,] = c(rep(1,3), rep(0, 11),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[6,] = c(rep(1,3), rep(0, 14),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[7,] = c(rep(1,3), rep(0, 17),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[8,] = c(rep(1,3), rep(0, 20),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  mask[1,] = c(rep(1,3), rep(0, 23),1, rep(c(rep(0,23), 1), times = 8))[1:n_months]
  return(mask)
}


prolonge_data = function(data, var_name, var_pred){
  data_prolonge = data %>% mutate(!!(var_name) := ifelse(is.na(!!sym(var_name)) == TRUE,
                                                         var_pred, !!sym(var_name)))
  return(data_prolonge)
}


predict_bp = function(data, par_model){
  all_vars = create_tibble(all_vars)
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
  par_difr_res = par_model$par_difr_res
  par_cur_purch = par_model$par_cur_purch
  par_rub_usd = par_model$par_rub_usd

  X_cur = tibble(r_price_cur_purch = all_vars$r_price_cur_purch,
                 brent = all_vars$brent,
                 brent_1 = all_vars$brent_1,
                 brent_2 = all_vars$brent_2,
                 r_dum_cur_purch = all_vars$r_dum_cur_purch)

  R_cur = all_vars %>%
    select(r_cur_purch) %>%
    rename('r_real' = 'r_cur_purch')

  pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur)
  r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch
  all_vars = prolonge_data(all_vars, 'r_cur_purch', r_hat_cur_purch)

  # rub_usd
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

  pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd, R_rub_usd, mask=mask)
  hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final


  all_vars = prolonge_data(all_vars, 'rub_usd', hat_rub_usd_final)

  all_vars = mutate(all_vars,
                    rub_usd_1 = lag(rub_usd, 1), rub_usd_2 = lag(rub_usd, 2),
                    rub_usd_eur_1 = rub_usd_1 * usd_eur_1,
                    dif_usd_rub_ratio = (rub_usd - rub_usd_1)/rub_usd_1,
                    dif_usd_rub_ratio_1 = (rub_usd_1 - rub_usd_2)/rub_usd_2,
                    dif_usd_eur_ratio = dif_usd_eur/usd_eur_1,
                    dif_usd_rub = rub_usd - rub_usd_1)

  X_oil = all_vars %>%
    select(const, brent, brent_1, rub_usd_1)

  R_oil = all_vars %>%
    select(p_exp_oil, v_exp_oil, r_exp_oil)

  R_oil_quarter = all_vars_quater %>%
    select(p_exp_oil, v_exp_oil, r_exp_oil) %>% na.omit()

  pred_oil = make_pred_oil(par_oil, X_oil, R_oil)
  r_hat_oil = pred_oil$r_hat_oil
  v_hat_oil = pred_oil$v_hat_oil
  p_hat_oil = pred_oil$p_hat_oil


  all_vars = prolonge_data(all_vars, 'r_exp_oil', r_hat_oil)
  all_vars = prolonge_data(all_vars, 'v_exp_oil', v_hat_oil)
  all_vars = prolonge_data(all_vars, 'p_exp_oil', p_hat_oil)

  # oil product model

  X_op = all_vars %>%
    select(const,
           brent, brent_1, brent_2, brent_3,
           rub_usd_eur_1, v_prod_op_1)

  R_op = all_vars %>%
    select(p_exp_op, v_exp_op, r_exp_op)

  R_op_quarter = all_vars_quater %>%
    select(p_exp_op, v_exp_op, r_exp_op) %>% na.omit()

  pred_op = make_pred_op(par_op, X_op, R_op)

  r_hat_op = pred_op$r_hat_op
  v_hat_op = pred_op$v_hat_op
  p_hat_op = pred_op$p_hat_op


  all_vars = prolonge_data(all_vars, 'r_exp_op', r_hat_op)
  all_vars = prolonge_data(all_vars, 'v_exp_op', v_hat_op)
  all_vars = prolonge_data(all_vars, 'p_exp_op', p_hat_op)

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


  pred_gas = make_pred_gas(par_gas, X_gas, R_gas)

  r_hat_gas = pred_gas$r_hat_gas
  v_hat_gas = pred_gas$v_hat_gas
  p_hat_gas = pred_gas$p_hat_gas


  all_vars = prolonge_data(all_vars, 'r_exp_gas', r_hat_gas)
  all_vars = prolonge_data(all_vars, 'v_exp_gas', v_hat_gas)
  all_vars = prolonge_data(all_vars, 'p_exp_gas', p_hat_gas)



  X_othg = tibble(const = 1,
                  r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op,
                  r_hat_oog_dum = r_hat_oog * all_vars$dum_1114,
                  gpd_defl = lag(all_vars$n_y, 1)/ all_vars$rub_usd_1)

  R_othg = all_vars %>% select(r_exp_othg, r_exp_goods)

  R_othg_quarter = all_vars_quater %>%
    select(r_exp_othg, r_exp_goods) %>% na.omit()

  pred_exp = make_pred_exp(par_othg, X_othg, R_othg)


  r_hat_othg = pred_exp$r_hat_othg
  r_hat_gds = pred_exp$r_hat_gds
  all_vars = prolonge_data(all_vars, 'r_exp_othg', r_hat_othg)
  all_vars = prolonge_data(all_vars, 'r_exp_goods', r_hat_gds)

  # Model for goods import
  X_imp = tibble(consump_defl = lag(all_vars$n_c, 1)/all_vars$rub_usd_1,
                 j_defl = lag(all_vars$n_j, 1)/all_vars$rub_usd_1,
                 ds_defl = (all_vars$n_ds_1)/all_vars$rub_usd_1,
                 r_hat_gds = r_hat_gds)


  R_imp = all_vars %>%
    select(r_imp_goods, r_imp_serv, r_imp_all)

  R_imp_quarter = all_vars_quater %>%
    select(r_imp_goods, r_imp_serv, r_imp_all) %>% na.omit()


  par_imp = c(par_imp_gds, par_imp_serv)

  pred_imp = make_pred_imp(par_imp, X_imp, R_imp)


  r_hat_imp_gds = pred_imp$r_hat_imp_gds
  r_hat_imp_serv = pred_imp$r_hat_imp_serv
  r_hat_imp_all = pred_imp$r_imp_all

  all_vars = prolonge_data(all_vars, 'r_imp_goods', r_hat_imp_gds)
  all_vars = prolonge_data(all_vars, 'r_imp_serv', r_hat_imp_serv)
  all_vars = prolonge_data(all_vars, 'r_imp_all', r_hat_imp_all)

  ### Model for export of services

  X_exp_serv = tibble(const = 1,
                      r_hat_gds = r_hat_gds,
                      r_hat_imp_serv = r_hat_imp_serv)

  R_exp_serv = all_vars %>%
    select(r_exp_serv)

  R_exp_serv_quarter = all_vars_quater %>%
    select(r_exp_serv) %>% na.omit()


  pred_exp_serv = make_pred_exp_serv(par_exp_serv, X_exp_serv, R_exp_serv)
  r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

  r_hat_exp_all = r_hat_exp_serv + r_hat_gds
  r_hat_exp_all_quarter = c(NA, roll_sum(r_hat_exp_all[4:length(r_hat_exp_all)], n = 3, by = 3))

  r_hat_bal_trade = r_hat_gds - r_hat_imp_gds
  r_hat_bal_trade_quarter = roll_sum(r_hat_bal_trade, n = 3, by = 3)


  r_hat_bal_serv = r_hat_exp_serv - r_hat_imp_serv
  r_hat_bal_serv_quarter = roll_sum(r_hat_bal_serv, n = 3, by = 3)

  all_vars = prolonge_data(all_vars, 'r_exp_serv', r_hat_exp_serv)
  all_vars = prolonge_data(all_vars, 'r_exp_all', r_hat_exp_all)
  all_vars = prolonge_data(all_vars, 'r_bal_trade', r_hat_bal_trade)
  all_vars = prolonge_data(all_vars, 'r_bal_serv', r_hat_bal_serv)

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

  r_hat_rent_sinc = pred_rent_sinc$r_hat
  all_vars = prolonge_data(all_vars, 'r_rent_sinc', r_hat_rent_sinc)

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

  r_hat_inv = pred_inv$r_hat
  all_vars = prolonge_data(all_vars, 'r_bal_inv', r_hat_inv)

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
  r_hat_wage = pred_wage$r_hat
  all_vars = prolonge_data(all_vars, 'r_bal_wage', r_hat_wage)

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
  r_hat_cur_acc = r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage

  all_vars = prolonge_data(all_vars, 'r_errors', r_hat_errors)
  all_vars = prolonge_data(all_vars, 'r_cur_account', r_hat_cur_acc)

  X_difr = tibble(const = 1,
                  dif_brent = all_vars$dif_brent,
                  dif_brent_cor = (all_vars$dif_brent * all_vars$vcor)/(all_vars$rub_usd_1),
                  dif_brent_1 = all_vars$brent_1 - all_vars$brent_2,
                  dif_usd_rub = all_vars$dif_usd_rub,
                  dif_usd_rub_1 = all_vars$rub_usd_1 - all_vars$rub_usd_2,
                  dif_usd_eur = all_vars$dif_usd_eur,
                  r_hat_cur_acc = r_hat_cur_acc,
                  r_hat_cur_acc_1 = lag(r_hat_cur_acc, 1),
                  quarter = 0,
                  r_cur_purch = all_vars$r_cur_purch,
                  r_cur_purch_1 = lag(all_vars$r_cur_purch, 1))


  X_difr_dummy = bind_cols(X_difr, select(all_vars, dum01:dum12))

  R_dif_reserves = all_vars %>%
    select(r_dif_reserves) %>%
    rename('r_real' = 'r_dif_reserves')
  pred_res = make_pred_dif_res(par_difr_res, X_difr_dummy)
  r_hat_dif_res = pred_res$r_hat_dif_res
  r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
  r_hat_dif_res_short = pred_res$r_hat_dif_res_short
  r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter
  all_vars = prolonge_data(all_vars, 'r_dif_reserves', r_hat_dif_res_short)
  return(all_vars)
}
