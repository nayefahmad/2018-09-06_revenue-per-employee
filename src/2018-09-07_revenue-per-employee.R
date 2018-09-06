

#*****************************************************************
# PLOTTING REVENUE VERSUS EMPLOYEES FOR LARGEST COMPANIES BY REVENUE 
#*****************************************************************
# 2018-09-07
# Nayef Ahmad 

library("tidyverse")
library("here")
library("rvest")
library("purrr")
library("stringr")

# rm(list = ls())

# 0) rvest example: --------------
page <- read_html("https://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/")

houses <- page %>%
      html_nodes(".photo-cards li article")

z_id <- houses %>% html_attr("id")

address <- houses %>%
      html_node(".zsg-photo-card-address") %>%
      html_text()

address
str(address)



# 1) trying with wikipedia: -------------
page.wiki <- read_html("https://en.wikipedia.org/wiki/List_of_largest_companies_by_revenue")

df1.companies <- page.wiki %>% 
      html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>% 
      html_table()

str(df1.companies)


# 2) cleaning data: -------------
df2.clean <- df1.companies %>% 
      set_names(tolower(names(.))) %>% 
      select(-c(country,
                ref, 
                `revenue growth`)) %>% 
      rename(revenue = `revenue(usd millions)`) %>% 
      mutate(revenue = str_sub(revenue, 2)) %>% 
      mutate(revenue = str_replace_all(revenue, ",", ""), 
             employees = str_replace_all(employees, ",", "")) %>% 
      mutate(revenue = as.numeric(revenue), 
             employees = as.numeric(employees))

str(df2.clean)

