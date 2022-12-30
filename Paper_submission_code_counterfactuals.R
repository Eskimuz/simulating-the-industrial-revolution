#install.packages(c("tidyverse", "readr", "dplyr", "ggpubr", "plm", "jtools",
#                   "janitor", "fixest", "modelsummary","tidyverse","texreg",gridExtra"
#                   ), lib="C:/Program Files/R/R-4.2.2/library")

library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr)
library(plm)
library(jtools)
library(janitor)
library(texreg)
library(fixest)
library(modelsummary)
library(gridExtra)



################################################################################
#COUNTERFACTUAL
################################################################################


rm(list = ls())

# Define the list of CSV files
csv_files <- c("output_06-10-2022/initial-labor-price_5.0_cat.txt",
               "output_06-10-2022/initial-labor-price_10.0_cat.txt",
               "output_06-10-2022/initial-labor-price_15.0_cat.txt"
)

# Import and merge the CSV files
data <- map_df(csv_files, read_csv, skip = 6, show_col_types = FALSE) #imports the list of file names 
data <- clean_names(data) # very useful, automatically cleans all names to easy R handling names
data <- data %>% mutate(id = run_number * starting_seed)

data_5 <- data %>%                  #keeps only the data of that sample
  filter(initial_labor_price == 5)
data_10 <- data %>%                  #keeps only the data of that sample
  filter(initial_labor_price == 10)
data_15 <- data %>%                  #keeps only the data of that sample
  filter(initial_labor_price == 15)


#################################### Counterfactuals comparisons

bourgeoisiesprice <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_bourgeoisie ),list(bourgeoisieprices = mean))

bourgeoisiespricessshades <- data_5 %>%
  group_by(step) %>%
  summarize(highbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.975),
            lowbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.025))
bourgeoisiesprice <- merge(bourgeoisiesprice,bourgeoisiespricessshades, by = "step")

firmsprice <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_firms ),list(firmsprices = mean))

firmspricesshades <- data_5 %>%
  group_by(step) %>%
  summarize(highfirmpr = quantile( mean_price_of_firms , probs = 0.975),
            lowfirmpr = quantile( mean_price_of_firms , probs = 0.025))
firmsprice <- merge(firmsprice,firmspricesshades, by = "step")


farmsprice <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( mean_farm_price ),list(farmsprices = mean))

farmspricesshades <- data_5 %>%
  group_by(step) %>%
  summarize(highfarmpr = quantile( mean_farm_price , probs = 0.975),
            lowfarmpr = quantile( mean_farm_price , probs = 0.025))
farmsprice <- merge(farmsprice,farmspricesshades, by = "step")

forprices <- merge(bourgeoisiesprice,firmsprice,by="step")
forprices <- merge(forprices,farmsprice,by="step")

prices_5 <- ggplot(data = forprices, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisieprices,color="Services")) +
  geom_line(aes(y=firmsprices, color="Goods")) +
  geom_line(aes(y=farmsprices, color="Food")) +
  geom_ribbon(data = forprices,aes(x=step,y=firmsprices,ymin = lowfirmpr, ymax = highfirmpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=bourgeoisieprices,ymin = lowbourgpr, ymax = highbourgpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=farmsprices,ymin = lowfarmpr, ymax = highfarmpr), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Prices - 5 ') +
  scale_color_manual(name = "", values = c("Services" = "orange", "Goods" = "yellow", "Food" = "green"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#prices
#prices_5
#pdf(prices)
#print(prices)


bourgeoisiesprice <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_bourgeoisie ),list(bourgeoisieprices = mean))

bourgeoisiespricessshades <- data_10 %>%
  group_by(step) %>%
  summarize(highbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.975),
            lowbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.025))
bourgeoisiesprice <- merge(bourgeoisiesprice,bourgeoisiespricessshades, by = "step")

firmsprice <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_firms ),list(firmsprices = mean))

firmspricesshades <- data_10 %>%
  group_by(step) %>%
  summarize(highfirmpr = quantile( mean_price_of_firms , probs = 0.975),
            lowfirmpr = quantile( mean_price_of_firms , probs = 0.025))
firmsprice <- merge(firmsprice,firmspricesshades, by = "step")


farmsprice <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( mean_farm_price ),list(farmsprices = mean))

farmspricesshades <- data_10 %>%
  group_by(step) %>%
  summarize(highfarmpr = quantile( mean_farm_price , probs = 0.975),
            lowfarmpr = quantile( mean_farm_price , probs = 0.025))
farmsprice <- merge(farmsprice,farmspricesshades, by = "step")


forprices <- merge(bourgeoisiesprice,firmsprice,by="step")
forprices <- merge(forprices,farmsprice,by="step")

prices_10 <- ggplot(data = forprices, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisieprices,color="Services")) +
  geom_line(aes(y=firmsprices, color="Goods")) +
  geom_line(aes(y=farmsprices, color="Food")) +
  geom_ribbon(data = forprices,aes(x=step,y=firmsprices,ymin = lowfirmpr, ymax = highfirmpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=bourgeoisieprices,ymin = lowbourgpr, ymax = highbourgpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=farmsprices,ymin = lowfarmpr, ymax = highfarmpr), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Prices - 10 ') +
  scale_color_manual(name = "", values = c("Services" = "orange", "Goods" = "yellow", "Food" = "green"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#prices
#prices_10
#pdf(prices)
#print(prices)


bourgeoisiesprice <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_bourgeoisie ),list(bourgeoisieprices = mean))

bourgeoisiespricessshades <- data_15 %>%
  group_by(step) %>%
  summarize(highbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.975),
            lowbourgpr = quantile( mean_price_of_bourgeoisie , probs = 0.025))
bourgeoisiesprice <- merge(bourgeoisiesprice,bourgeoisiespricessshades, by = "step")

firmsprice <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( mean_price_of_firms ),list(firmsprices = mean))

firmspricesshades <- data_15 %>%
  group_by(step) %>%
  summarize(highfirmpr = quantile( mean_price_of_firms , probs = 0.975),
            lowfirmpr = quantile( mean_price_of_firms , probs = 0.025))
firmsprice <- merge(firmsprice,firmspricesshades, by = "step")


farmsprice <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( mean_farm_price ),list(farmsprices = mean))

farmspricesshades <- data_15 %>%
  group_by(step) %>%
  summarize(highfarmpr = quantile( mean_farm_price , probs = 0.975),
            lowfarmpr = quantile( mean_farm_price , probs = 0.025))
farmsprice <- merge(farmsprice,farmspricesshades, by = "step")

forprices <- merge(bourgeoisiesprice,firmsprice,by="step")
forprices <- merge(forprices,farmsprice,by="step")

prices_15 <- ggplot(data = forprices, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisieprices,color="Services")) +
  geom_line(aes(y=firmsprices, color="Goods")) +
  geom_line(aes(y=farmsprices, color="Food")) +
  geom_ribbon(data = forprices,aes(x=step,y=firmsprices,ymin = lowfirmpr, ymax = highfirmpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=bourgeoisieprices,ymin = lowbourgpr, ymax = highbourgpr), alpha=0.1) +
  geom_ribbon(data = forprices,aes(x=step,y=farmsprices,ymin = lowfarmpr, ymax = highfarmpr), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Prices - 15 ') +
  scale_color_manual(name = "", values = c("Services" = "orange", "Goods" = "yellow", "Food" = "green"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#prices
#prices_15
#pdf(prices)
#print(prices)


counterfactual_prices <- ggarrange(prices_5,prices_10,prices_15, ncol=1, nrow=3, align = "hv", common.legend = TRUE, legend="bottom")

counterfactual_prices
ggsave(
  "prices.pdf"
)



##WEALTH
###############################################################################


bourgeoisiesincomes <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_bourgeoisie ),list(bourgeoisiewealth = mean))

bourgeoisiesincomesshades <- data_5 %>%
  group_by(step) %>%
  summarize(highbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.975),
            lowbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.025))

bourgeoisiesincomes <- merge(bourgeoisiesincomes,bourgeoisiesincomesshades, by = "step")

workersincomes <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_workers ), list(workerswealth = mean))
workersincomesshades <- data_5 %>%
  group_by(step) %>%
  summarize(highwork = quantile(  mean_wealth_of_workers , probs = 0.975),
            lowwork = quantile(  mean_wealth_of_workers , probs = 0.025))
workersincomes <- merge(workersincomes,workersincomesshades, by = "step")

noblesincomes <- data_5 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_nobles ),list(nobleswealth = mean))
noblesincomeshades <- data_5 %>%
  group_by(step) %>%
  summarize(highnobles = quantile(  mean_wealth_of_nobles , probs = 0.975),
            lownobles = quantile(  mean_wealth_of_nobles , probs = 0.025))

noblesincomes <- merge(noblesincomes,noblesincomeshades,by="step")

forincomes <- merge(bourgeoisiesincomes,workersincomes,by="step")
forincomes <- merge(forincomes,noblesincomes,by="step")

names<-c('Bourg.','workers','nobles')


wealth_5 <- ggplot(data = forincomes, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisiewealth,color='Bourg.')) +
  geom_line(aes(y=workerswealth,color='Workers')) +
  geom_line(aes(y=nobleswealth,color='Nobles')) +
  geom_ribbon(data = forincomes,aes(x=step,y=bourgeoisiewealth,ymin = lowbourg, ymax = highbourg), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=nobleswealth,ymin = lownobles, ymax = highnobles), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=workerswealth,ymin = lowwork, ymax = highwork), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Wealth - 5 ') +
  scale_color_manual(name = "", values = c("Bourg." = "orange", "Workers" = "red","Nobles" = "blue"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#wealth

#pdf(wealth)
#print(wealth)

bourgeoisiesincomes <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_bourgeoisie ),list(bourgeoisiewealth = mean))

bourgeoisiesincomesshades <- data_10 %>%
  group_by(step) %>%
  summarize(highbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.975),
            lowbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.025))

bourgeoisiesincomes <- merge(bourgeoisiesincomes,bourgeoisiesincomesshades, by = "step")

workersincomes <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_workers ), list(workerswealth = mean))
workersincomesshades <- data_10 %>%
  group_by(step) %>%
  summarize(highwork = quantile(  mean_wealth_of_workers , probs = 0.975),
            lowwork = quantile(  mean_wealth_of_workers , probs = 0.025))
workersincomes <- merge(workersincomes,workersincomesshades, by = "step")

noblesincomes <- data_10 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_nobles ),list(nobleswealth = mean))
noblesincomeshades <- data_10 %>%
  group_by(step) %>%
  summarize(highnobles = quantile(  mean_wealth_of_nobles , probs = 0.975),
            lownobles = quantile(  mean_wealth_of_nobles , probs = 0.025))

noblesincomes <- merge(noblesincomes,noblesincomeshades,by="step")

forincomes <- merge(bourgeoisiesincomes,workersincomes,by="step")
forincomes <- merge(forincomes,noblesincomes,by="step")
names<-c('Bourg.','workers','nobles')


wealth_10 <- ggplot(data = forincomes, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisiewealth,color='Bourg.')) +
  geom_line(aes(y=workerswealth,color='Workers')) +
  geom_line(aes(y=nobleswealth,color='Nobles')) +
  geom_ribbon(data = forincomes,aes(x=step,y=bourgeoisiewealth,ymin = lowbourg, ymax = highbourg), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=nobleswealth,ymin = lownobles, ymax = highnobles), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=workerswealth,ymin = lowwork, ymax = highwork), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Wealth - 10 ') +
  scale_color_manual(name = "", values = c("Bourg." = "orange", "Workers" = "red","Nobles" = "blue"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#wealth

#pdf(wealth)
#print(wealth)

bourgeoisiesincomes <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_bourgeoisie ),list(bourgeoisiewealth = mean))

bourgeoisiesincomesshades <- data_15 %>%
  group_by(step) %>%
  summarize(highbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.975),
            lowbourg = quantile(  mean_wealth_of_bourgeoisie , probs = 0.025))

bourgeoisiesincomes <- merge(bourgeoisiesincomes,bourgeoisiesincomesshades, by = "step")

workersincomes <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_workers ), list(workerswealth = mean))
workersincomesshades <- data_15 %>%
  group_by(step) %>%
  summarize(highwork = quantile(  mean_wealth_of_workers , probs = 0.975),
            lowwork = quantile(  mean_wealth_of_workers , probs = 0.025))
workersincomes <- merge(workersincomes,workersincomesshades, by = "step")

noblesincomes <- data_15 %>%
  group_by(step) %>%
  summarise_at(vars(  mean_wealth_of_nobles ),list(nobleswealth = mean))
noblesincomeshades <- data_15 %>%
  group_by(step) %>%
  summarize(highnobles = quantile(  mean_wealth_of_nobles , probs = 0.975),
            lownobles = quantile(  mean_wealth_of_nobles , probs = 0.025))

noblesincomes <- merge(noblesincomes,noblesincomeshades,by="step")

forincomes <- merge(bourgeoisiesincomes,workersincomes,by="step")
forincomes <- merge(forincomes,noblesincomes,by="step")

names<-c('Bourg.','workers','nobles')


wealth_15 <- ggplot(data = forincomes, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=bourgeoisiewealth,color='Bourg.')) +
  geom_line(aes(y=workerswealth,color='Workers')) +
  geom_line(aes(y=nobleswealth,color='Nobles')) +
  geom_ribbon(data = forincomes,aes(x=step,y=bourgeoisiewealth,ymin = lowbourg, ymax = highbourg), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=nobleswealth,ymin = lownobles, ymax = highnobles), alpha=0.1) +
  geom_ribbon(data = forincomes,aes(x=step,y=workerswealth,ymin = lowwork, ymax = highwork), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('Wealth - 15 ') +
  scale_color_manual(name = "", values = c("Bourg." = "orange", "Workers" = "red","Nobles" = "blue"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


counterfactual_wealth <- ggarrange(wealth_5,wealth_10,wealth_15, ncol=1, nrow=3, align = "hv", common.legend = TRUE, legend="bottom")

ggsave(
  "wealths.pdf"
)

counterfactual_wealth
#wealth

#pdf(wealth)
#print(wealth)

####GDP

### GDP 5 Initial Labor Costs

gdp<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( gdp_spending ),list(GDP= mean))

gdpshades<- data_5 %>%
  group_by(step) %>%
  summarize(highgdp = quantile( gdp_spending , probs = 0.975),
            lowgdp = quantile( gdp_spending , probs = 0.025))
gdp <- merge(gdp,gdpshades,by="step")

goods<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( goods_income_value ),list(goodsvalue= mean))

goodsshape<- data_5 %>%
  group_by(step) %>%
  summarize(highgoods = quantile( goods_income_value , probs = 0.975),
            lowgoods = quantile( goods_income_value , probs = 0.025))

goods<- merge(goods,goodsshape,by="step")

land<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( land_income_value ),list(landvalue= mean))

landshapes<- data_5 %>%
  group_by(step) %>%
  summarize(highland = quantile( land_income_value , probs = 0.975),
            lowland = quantile( land_income_value , probs = 0.025))

land<- merge(land,landshapes,by="step")

labor<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( labor_income_value ),list( laborvalue = mean))

laborshapes<- data_5 %>%
  group_by(step) %>%
  summarize(highlabor = quantile( labor_income_value , probs = 0.975),
            lowlabor = quantile( labor_income_value , probs = 0.025))

labor<- merge(labor,laborshapes,by="step")


service<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( service_income_value ),list(servicevalue = mean))

serviceshades<- data_5 %>%
  group_by(step) %>%
  summarize(highservice = quantile( service_income_value , probs = 0.975),
            lowservice = quantile( service_income_value , probs = 0.025))

service<- merge(service,serviceshades,by="step")

profits<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( profit_income_value ),list(profitvalue= mean))

profitsshades<- data_5 %>%
  group_by(step) %>%
  summarize(highprofits = quantile( profit_income_value , probs = 0.975),
            lowprofits = quantile( profit_income_value , probs = 0.025))

profits<- merge(profits,profitsshades,by="step")


realgdp<- data_5 %>%
  group_by(step) %>%
  summarise_at(vars( real_gdp_spending ),list(real=mean))

realgdpshades<- data_5%>%
  group_by(step) %>%
  summarise(highreal = quantile( real_gdp_spending , probs = 0.975),
            lowreal = quantile( real_gdp_spending , probs = 0.025))

realgdp<- merge(realgdp,realgdpshades,by="step")



forgdp <- merge(gdp,goods,by="step")
forgdp <- merge(forgdp,land,by="step")
forgdp <- merge(forgdp,labor,by="step")
forgdp <- merge(forgdp,service,by="step")
forgdp <- merge(forgdp,profits,by="step")
forgdp <- merge(forgdp,realgdp,by="step")




forGDP_5 <- ggplot(data = forgdp, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #ylim(0,250000)+
  geom_line(aes(y=GDP,color='GDP')) +
  geom_line(aes(y=goodsvalue,color='Goods')) +
  geom_line(aes(y=landvalue,color='Food')) +
  geom_line(aes(y=servicevalue,color='Services')) +
  geom_line(aes(y=profitvalue,color='Profits')) +
  geom_line(aes(y=laborvalue,color='Labor')) +
  geom_line(aes(y=real,color='Real GDP')) +
  geom_ribbon(data = forgdp,aes(x=step,y=GDP,ymin = lowgdp, ymax = highgdp), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=goodsvalue,ymin = lowgoods, ymax = highgoods), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=landvalue,ymin = lowland, ymax = highland), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=servicevalue,ymin = lowservice, ymax = highservice), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=profitvalue,ymin = lowprofits, ymax = highprofits,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=laborvalue,ymin = lowlabor, ymax = highlabor,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=real,ymin = lowreal, ymax = highreal,), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('GDP - 5 ') +
  scale_color_manual(name = "", values = c("GDP"="black","Real GDP"="black","Services" = "orange", "Labor" = "red","Profits" = "blue", "Food" = "green", "Goods" = "yellow"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


forGDP_5

###########
#forGDP - 10 Initial Labor Cost

gdp<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( gdp_spending ),list(GDP= mean))

gdpshades<- data_10 %>%
  group_by(step) %>%
  summarize(highgdp = quantile( gdp_spending , probs = 0.975),
            lowgdp = quantile( gdp_spending , probs = 0.025))
gdp <- merge(gdp,gdpshades,by="step")

goods<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( goods_income_value ),list(goodsvalue= mean))

goodsshape<- data_10 %>%
  group_by(step) %>%
  summarize(highgoods = quantile( goods_income_value , probs = 0.975),
            lowgoods = quantile( goods_income_value , probs = 0.025))

goods<- merge(goods,goodsshape,by="step")

land<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( land_income_value ),list(landvalue= mean))

landshapes<- data_10 %>%
  group_by(step) %>%
  summarize(highland = quantile( land_income_value , probs = 0.975),
            lowland = quantile( land_income_value , probs = 0.025))

land<- merge(land,landshapes,by="step")

labor<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( labor_income_value ),list( laborvalue = mean))

laborshapes<- data_10 %>%
  group_by(step) %>%
  summarize(highlabor = quantile( labor_income_value , probs = 0.975),
            lowlabor = quantile( labor_income_value , probs = 0.025))

labor<- merge(labor,laborshapes,by="step")


service<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( service_income_value ),list(servicevalue = mean))

serviceshades<- data_10 %>%
  group_by(step) %>%
  summarize(highservice = quantile( service_income_value , probs = 0.975),
            lowservice = quantile( service_income_value , probs = 0.025))

service<- merge(service,serviceshades,by="step")

profits<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( profit_income_value ),list(profitvalue= mean))

profitsshades<- data_10 %>%
  group_by(step) %>%
  summarize(highprofits = quantile( profit_income_value , probs = 0.975),
            lowprofits = quantile( profit_income_value , probs = 0.025))

profits<- merge(profits,profitsshades,by="step")


realgdp<- data_10 %>%
  group_by(step) %>%
  summarise_at(vars( real_gdp_spending ),list(real=mean))

realgdpshades<- data_10%>%
  group_by(step) %>%
  summarise(highreal = quantile( real_gdp_spending , probs = 0.975),
            lowreal = quantile( real_gdp_spending , probs = 0.025))

realgdp<- merge(realgdp,realgdpshades,by="step")



forgdp <- merge(gdp,goods,by="step")
forgdp <- merge(forgdp,land,by="step")
forgdp <- merge(forgdp,labor,by="step")
forgdp <- merge(forgdp,service,by="step")
forgdp <- merge(forgdp,profits,by="step")
forgdp <- merge(forgdp,realgdp,by="step")




forGDP_10 <- ggplot(data = forgdp, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=GDP,color='GDP')) +
  geom_line(aes(y=goodsvalue,color='Goods')) +
  geom_line(aes(y=landvalue,color='Food')) +
  geom_line(aes(y=servicevalue,color='Services')) +
  geom_line(aes(y=profitvalue,color='Profits')) +
  geom_line(aes(y=laborvalue,color='Labor')) +
  geom_line(aes(y=real,color='Real GDP')) +
  geom_ribbon(data = forgdp,aes(x=step,y=GDP,ymin = lowgdp, ymax = highgdp), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=goodsvalue,ymin = lowgoods, ymax = highgoods), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=landvalue,ymin = lowland, ymax = highland), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=servicevalue,ymin = lowservice, ymax = highservice), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=profitvalue,ymin = lowprofits, ymax = highprofits,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=laborvalue,ymin = lowlabor, ymax = highlabor,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=real,ymin = lowreal, ymax = highreal,), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('GDP - 10') +
  scale_color_manual(name = "", values = c("GDP"="black","Real GDP"="black","Services" = "orange", "Labor" = "red","Profits" = "blue", "Food" = "green", "Goods" = "yellow"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#forGDP


gdp<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( gdp_spending ),list(GDP= mean))

gdpshades<- data_15 %>%
  group_by(step) %>%
  summarize(highgdp = quantile( gdp_spending , probs = 0.975),
            lowgdp = quantile( gdp_spending , probs = 0.025))
gdp <- merge(gdp,gdpshades,by="step")

goods<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( goods_income_value ),list(goodsvalue= mean))

goodsshape<- data_15 %>%
  group_by(step) %>%
  summarize(highgoods = quantile( goods_income_value , probs = 0.975),
            lowgoods = quantile( goods_income_value , probs = 0.025))

goods<- merge(goods,goodsshape,by="step")

land<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( land_income_value ),list(landvalue= mean))

landshapes<- data_15 %>%
  group_by(step) %>%
  summarize(highland = quantile( land_income_value , probs = 0.975),
            lowland = quantile( land_income_value , probs = 0.025))

land<- merge(land,landshapes,by="step")

labor<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( labor_income_value ),list( laborvalue = mean))

laborshapes<- data_15 %>%
  group_by(step) %>%
  summarize(highlabor = quantile( labor_income_value , probs = 0.975),
            lowlabor = quantile( labor_income_value , probs = 0.025))

labor<- merge(labor,laborshapes,by="step")


service<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( service_income_value ),list(servicevalue = mean))

serviceshades<- data_15 %>%
  group_by(step) %>%
  summarize(highservice = quantile( service_income_value , probs = 0.975),
            lowservice = quantile( service_income_value , probs = 0.025))

service<- merge(service,serviceshades,by="step")

profits<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( profit_income_value ),list(profitvalue= mean))

profitsshades<- data_15 %>%
  group_by(step) %>%
  summarize(highprofits = quantile( profit_income_value , probs = 0.975),
            lowprofits = quantile( profit_income_value , probs = 0.025))

profits<- merge(profits,profitsshades,by="step")


realgdp<- data_15 %>%
  group_by(step) %>%
  summarise_at(vars( real_gdp_spending ),list(real=mean))

realgdpshades<- data_15%>%
  group_by(step) %>%
  summarise(highreal = quantile( real_gdp_spending , probs = 0.975),
            lowreal = quantile( real_gdp_spending , probs = 0.025))

realgdp<- merge(realgdp,realgdpshades,by="step")



forgdp <- merge(gdp,goods,by="step")
forgdp <- merge(forgdp,land,by="step")
forgdp <- merge(forgdp,labor,by="step")
forgdp <- merge(forgdp,service,by="step")
forgdp <- merge(forgdp,profits,by="step")
forgdp <- merge(forgdp,realgdp,by="step")

forGDP_15 <- ggplot(data = forgdp, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=GDP,color='GDP')) +
  geom_line(aes(y=goodsvalue,color='Goods')) +
  geom_line(aes(y=landvalue,color='Food')) +
  geom_line(aes(y=servicevalue,color='Services')) +
  geom_line(aes(y=profitvalue,color='Profits')) +
  geom_line(aes(y=laborvalue,color='Labor')) +
  geom_line(aes(y=real,color='Real GDP')) +
  geom_ribbon(data = forgdp,aes(x=step,y=GDP,ymin = lowgdp, ymax = highgdp), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=goodsvalue,ymin = lowgoods, ymax = highgoods), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=landvalue,ymin = lowland, ymax = highland), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=servicevalue,ymin = lowservice, ymax = highservice), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=profitvalue,ymin = lowprofits, ymax = highprofits,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=laborvalue,ymin = lowlabor, ymax = highlabor,), alpha=0.1) +
  geom_ribbon(data = forgdp,aes(x=step,y=real,ymin = lowreal, ymax = highreal,), alpha=0.1) +
  labs(x = '', y = '' ) + #changes the plot to a line
  ggtitle('GDP - 15') +
  scale_color_manual(name = "", values = c("GDP"="black","Real GDP"="black","Services" = "orange", "Labor" = "red","Profits" = "blue", "Food" = "green", "Goods" = "yellow"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#forGDP

#y_limits_forGDP <- ggplot_build(forGDP_15)$layout$panel_params[[1]]$y.range

#forGDP_5 <- forGDP_5 + scale_y_continuous(limits = y_limits_forGDP) 
#forGDP_10 <- forGDP_10 + scale_y_continuous(limits = y_limits_forGDP) 

#facet_grid 




counterfactual_gdp<- ggarrange(forGDP_5,forGDP_10,forGDP_15, ncol=1, nrow=3, align = "h", common.legend = TRUE, legend="bottom")

counterfactual_gdp2 <- ggarrange(
  forGDP_5 +
    scale_y_continuous(limits = c(0, 1800000)),
  forGDP_10 + 
    scale_y_continuous(limits = c(0, 1800000)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  forGDP_15 +
    scale_y_continuous(limits = c(0, 1800000)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  ncol=3, nrow=1,
  labels = c("a)","b)","c)"),
  label.x = 0.85,
  align = "h", 
  common.legend = TRUE, 
  legend="right")

counterfactual_wealth2 <- ggarrange(
  wealth_5 +
    scale_y_continuous(limits = c(0, 2000)),
  wealth_10 +
    scale_y_continuous(limits = c(0, 2000))+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  wealth_15 +
    scale_y_continuous(limits = c(0, 2000))+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  ncol=3, nrow=1,
  labels = c("g)","h)","i)"),
  label.x = 0.85,
  align = "h",
  common.legend = TRUE, 
  legend="right")

counterfactual_prices2 <- ggarrange(
  prices_5 +
    scale_y_continuous(limits = c(0, 30)),
  prices_10 +
    scale_y_continuous(limits = c(0, 30)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  prices_15 +
    scale_y_continuous(limits = c(0, 30)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank() ),
  ncol=3, nrow=1,
  labels = c("d)","e)","f)"),
  label.x = 0.85,
  align = "h", 
  common.legend = TRUE,
  legend="right")


full_graph <- ggarrange(counterfactual_gdp2, counterfactual_prices2, counterfactual_wealth2 ,
                        ncol=1, nrow=3, align = "v", legend="right")

ggsave( "fullgraph.pdf")
