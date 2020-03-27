for_plot = import('data/structure.csv')
plot_spread = gather(for_plot, key = 'date', value = 'Value', `01.01.2000`:`01.04.2019`)
plot_spread$date = parse_date_time(plot_spread$date, orders = 'dmY')
plot_spread$V1 %>% unique()
plot_spread$V1 %>% unique()

structure = filter(plot_spread,
                   V1 != 'Current Account', V1 != 'Financial Account', V1 != 'Capital Account', V1 != 'Balance on Rent',
                   V1 != 'Financial Account', V1 != 'Net Errors and Omissions', V1 != 'Reserves and Related Items')
structure

ggplot(structure, aes(x = date, y = Value)) + 
  geom_line(color = 'red') + facet_wrap(.~V1) + xlab('') +ggtitle('Components of the Current Account')

cur_acc = filter(plot_spread, V1 == 'Current Account') %>% 
  ungroup()

fin_acc = filter(plot_spread, V1 == 'Financial Account') %>% 
  ungroup()

cap_acc = filter(plot_spread, V1 == 'Capital Account') %>% 
  ungroup()


cur_acc$date = parse_date_time(cur_acc$date, orders = 'dmY')
fin_acc$date = parse_date_time(fin_acc$date, orders = 'dmY')
cap_acc$date = parse_date_time(cap_acc$date, orders = 'dmY')

ggplot(cur_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Current account')
ggplot(fin_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Financial account')
ggplot(cap_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Capital account')
ggplot(fin_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Current account')