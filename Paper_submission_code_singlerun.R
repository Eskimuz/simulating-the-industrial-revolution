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
#HISTORICAL
################################################################################


rm(list = ls())

data <- read_csv("25052022/test_output/initial-labor-price_15_cat.csv", 
                 skip = 6, show_col_types = FALSE)


set.seed(1917) ##allows for reproducibility of single run graphs

data <- clean_names(data) # very useful, automatically cleans all names to easy R handling names
data <- data %>% mutate(id = run_number * starting_seed) #creates unique id
random_value <- sample(unique(data$id), 1) #selects one sample
data_selected <- data %>%                  #keeps only the data of that sample
  filter(id == random_value)
data_selected <- data_selected %>%
  mutate(land_share = (land_income_value / gdp_spending) * 100,
         goods_share = (goods_income_value / gdp_spending) * 100,
         service_share = (service_income_value / gdp_spending) * 100
  )


#Price
prices <- ggplot(data = data_selected, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=mean_price_of_bourgeoisie,color="Services"), linetype=1) +
  geom_line(aes(y=mean_price_of_firms, color="Goods")) +
  geom_line(aes(y=mean_farm_price, color="Food")) +
  labs(x = 'Time', y = 'Value' ) + #changes the plot to a line
  ggtitle('Prices averages') +
  scale_color_manual(name = "Classes", values = c("Services" = "orange", "Food" = "green", "Goods" = "yellow"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

prices

wealth <- ggplot(data = data_selected, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=mean_wealth_of_bourgeoisie,color='Bourg.')) +
  geom_line(aes(y=mean_wealth_of_workers,color='Workers')) +
  geom_line(aes(y=mean_wealth_of_nobles,color='Nobles')) +
  labs(x = 'Time', y = 'Wealth' ) + #changes the plot to a line
  ggtitle('Wealth Averages') +
  scale_color_manual(name = "Classes", values = c("Bourg." = "orange","Nobles" = "blue","Workers" = "red"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
wealth


forGDP <- ggplot(data = data_selected, aes(x=step)) + ##produces the plot
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(aes(y=gdp_spending,color='GDP')) +
  geom_line(aes(y=goods_income_value,color='Goods')) +
  geom_line(aes(y=land_income_value,color='Food')) +
  geom_line(aes(y=service_income_value,color='Services')) + 
  geom_line(aes(y=profit_income_value,color='Profits')) +
  geom_line(aes(y=labor_income_value,color='Labor')) +
  geom_line(aes(y=real_gdp_spending,color='Real GDP')) +
  labs(x = 'Time', y = 'Value' ) + #changes the plot to a line
  ggtitle('GDP and sectors contributions') +
  scale_color_manual(name = "Classes", values = c("GDP"="black","Goods" = "yellow","Profits" = "blue","Real GDP"="black","Labor" = "red","Services" = "orange", "Food" = "green"))+
  geom_vline(xintercept = 100) + #line for time change
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

forGDP


singlerun <- ggarrange(forGDP,prices,wealth, ncol=1, nrow=3,align = "hv",legend = "right")

singlerun

ggsave("singlerun.pdf")


