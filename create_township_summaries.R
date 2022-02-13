#Script to create pdf summaries of housing trends by Cook County Township
#Gabe Morrison

library(tidyverse)
library(arrow)
library(glue)
library(scales)
library(ggpubr)


#Read and clean data=======
legacy_sales = read_parquet("input/legacy_sales.parquet")
ias_data <- read_parquet("input/ias_data.parquet")



#FILTER to exclude sales below 30,000 and that are only residential:
filter_price_res <- function(df, res, res_col, price, price_col){
  #Function to filter data by by its residential category and by price
  #Inputs:
  # df - a dataframe
  # res - vector of strings - contains the categories to keep
  # res_col - column in df - the name of the column in df which stores residential type
  # price -  float - price under which houses are excluded
  # price_col -column in df - the name of the column in the df which stores price
  
  df_filtered <- df %>%
    filter({{res_col}} %in% res) %>%
    filter({{price_col}} >= price) 
  return(df_filtered)
}


ias_filtered <- filter_price_res(ias_data, 
                                 classes_to_keep,
                                 class,
                                 30000, 
                                 sale_price) %>%
  distinct()

legacy_filtered <- legacy_sales %>%
  mutate(CLASS = as.character(CLASS)) %>% #Convert class to character
  filter_price_res(classes_to_keep,
                   CLASS,
                   30000,
                   sale_price) %>%
  filter(TAX_YEAR >= 2014) %>%
  mutate(nbhd_code = str_c(TOWN, str_pad(as.character(NBHD), width = 3, side = "left", pad = "0")))



#Get township
township_codes <- unique(ias_data$township_code)

#Get residential codes:
classes <- sort(unique(ias_data$class))
classes_to_keep_bool <- str_detect(classes, "^2")
classes_to_keep <- classes[classes_to_keep_bool]
classes_to_keep <- classes_to_keep[!classes_to_keep == "299"]


# Identify North Triad towns:
town_dict <- ccao::town_dict 
north_triad_towns <- town_dict %>%
  filter(triad_name == "North") %>%
  pull(township_code)


#Write functions to further process data and visualize it ========

clean_for_changes_by_year <- function(df, town_var, town, sales, year_var){
  #Function to compute summary statistics for property sales in a given township
  #Inputs:
  # df (data.frame) - sales dataframe
  # town_var (column in df) - column indicating the township variable
  # town (string) - code identifying a town
  # sales (column in df) - column identifying sale price
  # year_var (column in df) - column identifying year
  
  #Returns: df_clean (data.frame) - a cleaned/processed dataframe
  
  #Clean data for plots 1 and 2: 
  df_clean <- df %>%
    filter({{town_var}} == {{town}}) %>%
    filter({{sales}}>=30000) %>%
    group_by({{year_var}}) %>%
    summarize(across({{sales}}, list(
      Mean = mean,
      IQR = IQR, 
      "10th Percentile" = ~quantile(., probs = 0.10), 
      "20th Percentile" = ~quantile(., probs = 0.25), 
      "50th Percentile" = median,
      "70th Percentile" = ~quantile(., probs = 0.75), 
      "90th Percentile" = ~quantile(., probs = 0.90))
     )) %>%
    pivot_longer(cols = sale_price_Mean:"sale_price_90th Percentile", 
                 names_to = "var", 
                 values_to = "values") %>%
    mutate(across(var, str_remove, "sale_price_"))
}

#test: 
#clean <- clean_for_changes_by_year(df = ias_data, 
#                                   town_var = township_code, 
#                                   town = "30", 
#                                   sales = sale_price,
#                                   year_var = year)






plot_changes_by_year_non_high <- function(df_clean, town){
  #Function plots Mean, IQR, 10th - 70th percentile prices by year in line plot
  #Input:
  # df_clean (dataframe) - a dataframe cleaned by clean_for_changes_by_year
  # town (string) - an identifier for the town
  
  #Returns: 
  # p1 - a ggplot lineplot showing changes over time of the variables inputted
  
  
  df_clean_p1 <- {{df_clean}} %>%
    filter(var != "90th Percentile" & 
             var != "Mean")
  p1 <- ggplot(data = df_clean_p1, aes(x = year, y = values, group = var)) + 
    geom_line(aes(color=var)) + 
    labs(title = glue("Township {town}: Sales Prices by Year"),
         subtitle = "For sales Greater than 30,000") + 
    xlab("Year") + 
    ylab("Sale Price") + 
    scale_y_continuous(label=dollar_format(), 
                       limits = c(0, 
                                  max(df_clean_p1$values)))
}

#Test:
#a <- plot_changes_by_year_non_high(clean, "30")



plot_changes_by_year_high <- function(df_clean, town) {
  #Function plots mean and 90th percentile housing prices change over time
  # in a lineplot. Function similar to above 
  #Input:
  # df_clean (dataframe) - a dataframe cleaned by clean_for_changes_by_year
  # town (string) - an identifier for the town
  
  #Returns: 
  # p2 - a ggplot lineplot showing changes over time of the variables inputted
  
  
  
  
  df_clean_p2 <- df_clean %>%
    filter(var %in% c("Mean", "90th Percentile"))
  
  p2 <- ggplot(data = df_clean_p2, aes(x = year, y = values, group = var)) + 
    geom_line(aes(color=var)) + 
    labs(title = glue("Township {town}: Sales Prices by Year"),
         subtitle = "For Sales Greater than $30,000") + 
    xlab("Year") + 
    ylab("Sale Price") + 
    scale_y_continuous(label=dollar_format(), 
                       limits = c(0, 
                                  max(df_clean_p2$values)))
  
}

#Test:
#b <- plot_changes_by_year_high(clean, "30")
#b



#Get data for plot 3:
clean_for_yoy_change <- function(df, town_var, town, sales_var, year){
  #Function manipulates data for other visuals 
  #Inputs:
  # df (data.frame) - sales dataframe
  # town_var (column in df) - column indicating the township variable
  # town (string) - code identifying a town
  # sales (column in df) - column identifying sale price
  # year_var (column in df) - column identifying year
  
  #Returns: df_grouped (data.frame) - manipulated df for visualizations
  
  df_grouped <- df %>%
    rename(year = {{year}}, 
           town_var = {{town_var}}, 
           sale_price = {{sales_var}}) %>%
    filter(town_var == town) %>%
    group_by(year) %>%
    summarize(med_price_sale = median(sale_price)) %>%
    pivot_wider(names_from = "year", values_from = "med_price_sale") %>%
    mutate(med_2014 = `2014`) %>%
    pivot_longer(cols = !any_of(c("town_var", "med_2014")),
                 names_to = "Year", 
                 values_to = "med_sales_price") %>%
    mutate(ratio= med_sales_price / med_2014) %>%
    select(Year, ratio) 
  
  return(df_grouped)
}
#Test:
#clean_yoy <- clean_for_yoy_change(ias_data, township_code, "30", sale_price, year)


plot_for_yoy <- function(df, township_code){
  #Function to plot change over time for housing prices
  #Inputs:
  # df (data.frame) - cleaned by clean_for_yoy_change
  # township_code (string) - identifier for township code
  
  #Returns p3 - a ggplot showing housing price % change over time
  
  p3 <- ggplot(data = {{df}}, aes(x = Year, y = ratio, group = 1)) + 
    geom_line() + 
    xlab("Year") + 
    ylab("Percentage Change from 2014") + 
    scale_y_continuous(labels = scales::percent) +
    labs(title = glue("Township {township_code}: Median Sale Price Percent Change from 2014
                        For Sales Greater than $30,000",
                      subtitle = "For sales Greater than 30,000"))
  
  return(p3)
}
#test:
#c <- plot_for_yoy(clean_yoy, "30")
#c

plot_dists_by_year <-function(data, town_var, town, year_var, sales_var){
  #Function to return distribution of sales prices by year
  #Inputs:
  # df (data.frame) - sales dataframe
  # town_var (column in df) - column indicating the township variable
  # town (string) - code identifying a town
  # year_var (column in df) - column identifying year
  # sales_var (column in df) - column identifying sale price
  
  #Returns:
  # p4 - a ggplot of multiple distributions of housing prices
  
  df_small <- data %>%
    mutate(year = as.factor({{year_var}})) %>%
    filter({{town_var}} == town)
  
  
  p4 <- ggplot(df_small, aes(x = sale_price, color = year)) +
    geom_freqpoly() + 
    scale_x_log10(labels = comma) + 
    scale_color_brewer(palette="RdYlBu") +
    xlab("Sales Price") + 
    ylab("Count") + 
    labs(title = glue("Township {town}: Change in Distribution of Sales"),
         subtitle = "For Sales Greater than $30,000") 
  
  return(p4)
}

#b <- plot_dists_by_year(ias_data, township_code, "20", year, sale_price)





aggregate_plots<- function(df, town_var, town, sales_var, year, output_file){
  #Wrapper function to call previously written plotting functions to make an
  # aggregate chart
  
  #Inputs:
  # df (data.frame) - sales dataframe
  # town_var (column in df) - column indicating the township variable
  # town (string) - code identifying a town
  # year_var (column in df) - column identifying year
  # sales_var (column in df) - column identifying sale price
  # output_file (string) - path to folder location to save pdfs 
  
  #Output:
  # Function prints plots 
  # It also writes them to the file specified in output_file
  
  
  
  town_char <- as.character({{town}})
  pdf(str_c(output_file, town, ".pdf"), width = 11, height = 8.5)
  clean <- clean_for_changes_by_year({{df}}, {{town_var}}, {{town}}, {{sales_var}}, {{year}})
  clean_yoy <- clean_for_yoy_change({{df}}, {{town_var}}, {{town}}, {{sales_var}}, {{year}})
  
  p1 <- plot_changes_by_year_non_high(clean, {{town}})
  p2 <- plot_changes_by_year_high(clean, {{town}})
  p3 <- plot_for_yoy(clean_yoy, {{town}})
  p4 <- plot_dists_by_year(df, {{town_var}}, {{town}}, {{year}}, {{sales_var}})
  
  
  plots <- ggarrange(p1, p2, p3, p4, ncol =  2, nrow = 2)
  print(plots)
  dev.off()
  
}




#Run for only Residential properties: 
lapply(north_triad_towns, 
       aggregate_plots, 
       df = ias_filtered, 
       town_var = township_code, 
       sales_var = sale_price, 
       year = year,
       output_file = "output/north_tri_summaries_res/township_")


#Run for all properties:
ias_data_all <- ias_data %>%
  filter(sale_price > 30000)

lapply(north_triad_towns, 
       aggregate_plots, 
       df = ias_data_all, 
       town_var = township_code, 
       sales_var = sale_price, 
       year = year,
       output_file = "output/north_tri_summaries_all/township_")


#Run for only condos: 
ias_data_299 <- ias_data %>%
  filter(sale_price > 30000, 
         class == "299")


lapply(north_triad_towns, 
       aggregate_plots, 
       df = ias_data_299, 
       town_var = township_code, 
       sales_var = sale_price, 
       year = year,
       output_file = "output/north_tri_summaries_299/township_")


