

#*****************************************************************
# PLOTTING REVENUE VERSUS EMPLOYEES FOR LARGEST COMPANIES BY REVENUE 
#*****************************************************************
# 2018-09-07
# Nayef Ahmad 

library("tidyverse")
library("here")
library("rvest")

# rm(list = ls())

# rvest example: --------------
page <- read_html("http://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/")

houses <- page %>%
      html_nodes(".photo-cards li article")

z_id <- houses %>% html_attr("id")

address <- houses %>%
      html_node(".zsg-photo-card-address") %>%
      html_text()

address
str(address)
