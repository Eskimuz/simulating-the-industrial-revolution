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

################################### Historical model

### PRICES
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
  ggtitle('Prices') +
  scale_color_manual(name = "", values = c("Services" = "orange", "Goods" = "yellow", "Food" = "green"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#### WEALTH


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

names<-c('bourg.','workers','nobles')


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
  ggtitle('Wealth') +
  scale_color_manual(name = "", values = c("Bourg." = "orange", "Workers" = "red","Nobles" = "blue"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

######### GDP



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
  ggtitle('GDP') +
  scale_color_manual(name = "", values = c("GDP"="black","Real GDP"="black","Services" = "orange", "Labor" = "red","Profits" = "blue", "Food" = "green", "Goods" = "yellow"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




historical <- ggarrange(forGDP_15, prices_15, wealth_15, ncol=1, nrow=3, align = "hv", legend="right")
ggsave("historical.pdf")
