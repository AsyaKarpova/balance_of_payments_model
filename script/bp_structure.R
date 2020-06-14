library(tidyverse)
library(lubridate)
library(rio)
library(feasts)
library(tseries)
library(tsibble)
library(patchwork)


# загрузка данных и превращение их в удобный формат tsibble
all_data = import('data/data_month1.xlsx') %>%
  mutate(date = yearmonth(date)) %>% as_tsibble()
all_data_quarter = import('data/data_quarter_new.xlsx')%>%
  mutate(date = yearquarter(date)) %>% as_tsibble()

# отобрали основные агрегаты платежного баланса
quarter_structure = all_data_quarter %>%
  select(r_cur_account, r_bal_fin, r_cap_account, r_errors)

# вот так можно посмотреть на автокорреляционную функцию
all_data %>%
  feasts::ACF(rub_usd) %>% autoplot() + xlab('lag, месяц') + ylab('ACF')

# и на динамику переменных по месяцам в течение разных лет
all_data %>%
  feasts::gg_subseries(r_dif_reserves, period = 12)


# это не самый лучший способ сделать kpss тест
rub_usd = select(all_data, rub_usd) %>%
  as_tibble() %>% select(-date) %>% ts(start = c(2006,1))
kpss.test(rub_usd, null = c("Level", "Trend"), lshort = TRUE)


# тут намечается картинка с доля различных видов топлива в экспорте
data_export = all_data_quarter %>%
  select(r_exp_oil, r_exp_op, r_exp_gas, r_exp_othg) %>%
  filter(year(date)==2019) # считаем за 2019 год


data_share = data_export%>%
  pivot_longer(cols = -date, names_to='export_type', values_to='value')%>%
  group_by(export_type) %>%
  summarize(revenue = sum(value)) %>% # считаем суммарную выручку за период в каждой группе
  mutate(share = revenue/sum(revenue), # делим эту сумму на сумму вообще всего
         # дальше добавляем переменны с названиями групп — для красоты картинки
         struct = if_else(export_type == 'r_exp_othg', 'Другие товары', 'Топливо')) %>%
  mutate(Товары = if_else(export_type == 'r_exp_othg', 'Другое',
                          if_else(export_type == 'r_exp_oil','Нефть',
                                  if_else(export_type == 'r_exp_gas','Газ',
                                          'Нефтепродукты')))) %>%
        # это нужно, чтобы на графике, в легенде, топливо в нужном мне порядке было перечислено
  mutate(Товары = factor(Товары, levels = c('Нефть', 'Нефтепродукты', 'Газ', 'Другое')))

# сам график
ggplot(data_share, aes(x = struct, y = share, fill = Товары)) +
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() + # чтобы ось с долями расположилась горизонтально
  ylab("Доля в экспорте") +
  ggtitle('Структура экспорта РФ, 2019 год') +
  xlab("")


# тут заранее подготовленные данные по структуре пб

for_plot = import('data/structure.csv', header=TRUE, # чтобы даты стали названиями столбиков
                  encoding = "UTF-8")

# дальше идет борьба с разрядами  из экселя, которые он поделил запятыми
# просто меняем везде запятую на ничто
for_plot2 = mutate_all(for_plot, ~str_replace(., ",", ""))

# для удобства сделаем табличку длинной и типы переменных изменим
plot_spread = for_plot2 %>%
    pivot_longer(cols = `01.01.2000`:`01.10.2019`, names_to = 'date', values_to = 'Value') %>%
    mutate(date = yearquarter(dmy(date)),
           Value = as.numeric(Value))

# оставим только компоненты счета текущих операций
structure = filter(plot_spread,
                   V1 != 'Счет текущих операций',  V1 != 'Счет операций с капиталом',
                   V1 != 'Финансовый счет', V1 != 'Чистые пропуски и ошибки', V1 != 'Изменение резервных активов')


ggplot(structure, aes(x = date, y = Value/1000)) +
  geom_line(color = 'red') + facet_wrap(.~V1) + xlab('') + ylab('млрд. долл.')

# достали отдельные счета для графков
cur_acc = filter(plot_spread, V1 == 'Счет текущих операций') %>%
  ungroup()

fin_acc = filter(plot_spread, V1 == 'Финансовый счет') %>%
  ungroup()

cap_acc = filter(plot_spread, V1 == 'Счет операций с капиталом') %>%
  ungroup()

r_errors = filter(plot_spread, V1 == 'Чистые пропуски и ошибки') %>%
  ungroup()


f = ggplot(cur_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Счет текущих операций')
a = ggplot(fin_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Финансовый баланс')
b = ggplot(cap_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Cчет операций с капиталом' )
c = ggplot(r_errors, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Чистые ошибки и пропуски' )

# это все балгодаря библиотеке pathcwork
f/a/b/c

# тут я зачем-то переводила квартальные данные по компонентам ВВП в месячные
n_y = rep(all_data_quarter$n_y/3, each = 3)
n_c = rep(all_data_quarter$n_c/3, each = 3)
n_j = rep(all_data_quarter$n_j/3, each = 3)
n_ex = rep(all_data_quarter$n_ex/3, each = 3)
n_im = rep(all_data_quarter$n_im/3, each = 3)
n_ds = rep(all_data_quarter$n_ds/3, each = 3)
n_g = rep(all_data_quarter$n_g/3, each = 3)
prolonged_n = tibble(n_y, n_c, n_j, n_ex, n_im, n_ds, n_g)/1000
#export(prolonged_n, 'prol_n.csv')

