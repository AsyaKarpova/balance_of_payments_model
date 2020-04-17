library(tidyverse)
library(lubridate)
library(rio)
glimpse(plot_spread)
for_plot = import('data/structure.csv')
 
plot_spread = gather(for_plot, key = 'date', value = 'Value', `01.01.2000`:`01.04.2019`)%>% select(V1, Value, date)
plot_spread$date = parse_date_time(plot_spread$date, orders = 'dmY')
plot_spread %>% select(V1) %>% unique()

fuels = plot_spread %>% pivot_wider(names_from = `V1`, values_from = Value) %>% mutate(fuels = r_exp_op + r_exp_oil + r_exp_gas)

fuels %>% group_by(date) %>% mutate(share = 1000 * fuels/`Current Account`) %>% select(date, share, `Current Account`, fuels) 

structure = filter(plot_spread,
                   V1 != 'Current Account', V1 != 'Financial Account', V1 != 'Capital Account', V1 != 'Balance on Rent',
                   V1 != 'Financial Account', V1 != 'Net Errors and Omissions', V1 != 'Reserves and Related Items')


ggplot(structure, aes(x = date, y = Value)) + 
  geom_line(color = 'red') + facet_wrap(.~V1) + xlab('') +ggtitle('Components of the Current Account')

cur_acc = filter(plot_spread, V1 == 'Current Account') %>% 
  ungroup()

fin_acc = filter(plot_spread, V1 == 'Financial Account') %>% 
  ungroup()

cap_acc = filter(plot_spread, V1 == 'Capital Account') %>% 
  ungroup()




ggplot(cur_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Current account')
ggplot(fin_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Financial account')
ggplot(cap_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Capital account')
ggplot(fin_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Current account') 

