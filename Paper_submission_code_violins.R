#### Violin plots for the sensitivity analysis


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

rm(list = ls())

# Define the list of CSV files
csv_distance <- c("output_06-10-2022/distance-setup_3.0_cat.txt","output_06-10-2022/distance-setup_5.0_cat.txt",
                 "output_06-10-2022/distance-setup_7.0_cat.txt","output_06-10-2022/distance-setup_9.0_cat.txt")

csv_industrial <- c("output_06-10-2022/industrial-switch-probability_0.1_cat.txt","output_06-10-2022/industrial-switch-probability_0.2_cat.txt",
                "output_06-10-2022/industrial-switch-probability_0.3_cat.txt","output_06-10-2022/industrial-switch-probability_0.4_cat.txt")

csv_labor <- c("output_06-10-2022/initial-labor-price_5.0_cat.txt","output_06-10-2022/initial-labor-price_10.0_cat.txt",
               "output_06-10-2022/initial-labor-price_15.0_cat.txt")

csv_change <- c("output_06-10-2022/price-change-chance_0.1_cat.txt","output_06-10-2022/price-change-chance_0.2_cat.txt",
                "output_06-10-2022/price-change-chance_0.3_cat.txt","output_06-10-2022/price-change-chance_0.4_cat.txt")

csv_delta <-  c("output_06-10-2022/price-delta_0.1_cat.txt","output_06-10-2022/price-delta_0.2_cat.txt",
                "output_06-10-2022/price-delta_0.3_cat.txt","output_06-10-2022/price-delta_0.4_cat.txt")

data_distance <- map_df(csv_distance, read_csv, skip = 6,
                     show_col_types = FALSE)
data_distance <- clean_names(data_distance) # very useful, automatically cleans all names to easy R handling names
data_distance <- data_distance %>% mutate(id = run_number * starting_seed, #creates unique id
                                          distance_factor = as.factor(distance_setup)) #factor variable

data_industrial <- map_df(csv_industrial, read_csv, skip = 6,
               show_col_types = FALSE)
data_industrial <- clean_names(data_industrial) # very useful, automatically cleans all names to easy R handling names
data_industrial <- data_industrial %>% mutate(id = run_number * starting_seed,
                                      industrial_factor = as.factor(industrial_switch_probability)) #creates unique id

data_labor <- map_df(csv_labor, read_csv, skip = 6,
               show_col_types = FALSE)
data_labor <- clean_names(data_labor) # very useful, automatically cleans all names to easy R handling names
data_labor <- data_labor %>% mutate(id = run_number * starting_seed,
                                    labor_factor = as.factor(initial_labor_price)) #creates unique id

data_change <- map_df(csv_change, read_csv, skip = 6,
               show_col_types = FALSE)
data_change <- clean_names(data_change) # very useful, automatically cleans all names to easy R handling names
data_change <- data_change %>% mutate(id = run_number * starting_seed,
                                      price_change_factor = as.factor(price_change_chance)) #creates unique id

data_delta <- map_df(csv_delta, read_csv, skip = 6,
               show_col_types = FALSE)
data_delta <- clean_names(data_delta) # very useful, automatically cleans all names to easy R handling names
data_delta <- data_delta %>% mutate(id = run_number * starting_seed,
                                    price_delta_factor = as_factor(price_delta)) #creates unique id


labor_violin_plot <- ggplot(data = data_labor, aes(x = initial_labor_price, y = gdp_spending, fill = labor_factor)) +
  geom_violin() + ## this is basic graph, time is not necessary
  scale_x_continuous(expand = c(0, 0),breaks=c(5,10,15)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Labor Price', y = 'GDP', fill = "") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#labor_violin_plot

distance_violin_plot <- ggplot(data = data_distance, aes(x = distance_setup, y = gdp_spending, fill = distance_factor)) +
  geom_violin() + 
  scale_x_continuous(expand = c(0, 0),breaks=c(3,5,7,9)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Distance Setup', y = 'GDP', fill = " " ) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#distance_violin_plot

industrial_violin_plot <- ggplot(data = data_industrial, aes(x = industrial_switch_probability, y = gdp_spending, fill = industrial_factor)) +
  geom_violin() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Industrial Switch Chance', y = 'GDP', fill = " " ) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#industrial_violin_plot 

price_delta_violin_plot <- ggplot(data = data_delta, aes(x = price_delta, y = gdp_spending, fill = price_delta_factor)) +
  geom_violin() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Price Delta', y = 'GDP', fill = " " ) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#price_delta_violin_plot

price_change_violin_plot <- ggplot(data = data_change, aes(x = price_change_chance, y = gdp_spending, fill = price_change_factor)) +
  geom_violin() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Price Change Chance', y = 'GDP', fill = " " ) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#price_change_violin_plot


#all_violins <- grid.arrange( grobs=base_violins, nrow = 3)

all_violins <- ggarrange(labor_violin_plot
                         + theme(legend.position = "none"),
                         distance_violin_plot
                         + theme(legend.position = "none"),
                         industrial_violin_plot
                         + theme(legend.position = "none"),
                         price_change_violin_plot
                         + theme(legend.position = "none"),
                         price_delta_violin_plot
                         + theme(legend.position = "none"),
                         ncol=2, nrow=3, align = "hv")

all_violins

ggsave("violins.pdf")