library(Metrics)
library(tsibble)
library(tidyverse)
library(fable)
library(fabletools)


# !!!сравниваем значения mape и mase в двух разных пакетах

# искусственные данные
data = tsibble(date = 1:100, value = rnorm(100), index = date)
data_train = data %>% filter(date<=90) # 10 наблюдений отправили в тест

# оценим несколько моделей с помощью пакета fable
models = data_train %>%
  model(
    ets = ETS(value),
    arima = ARIMA(value),
    rw  = RW(value),
    naive = NAIVE(value)
  )

# прогноз на 10 шагов вперед
data_forecasts = models %>%
  forecast(h = 10)

# считаем метрики по формулам из fable на тесте!!! по всем моделям
all_metrics = data_forecasts %>%
  fabletools::accuracy(data) %>% #чтобы не путалась с accuracy из Metrics
  arrange(MAPE)

# дальше вручную пробуем посчитать mase — ответы не сошлись, так что может и неправильно :)
# считали как отношение mae модели к mae random walk
mae = all_metrics %>% select(.model, MAE)
mae_long = mae %>% pivot_longer(cols = MAE)
mae_rw = mae_long %>%filter(.model == 'rw') %>%
  rename(value_rw = value)%>% pull(value_rw)
mase_final = mae %>%
  mutate(mase = MAE / mae_rw) %>%filter(.model != 'rw')


# а тут mase пакетом Metrics
ets_fcst = data_forecasts %>%
  as_tibble() %>%
  select(-.distribution) %>%
  filter(.model == 'ets')
mase(tail(data$value, 10), ets_fcst$value)
