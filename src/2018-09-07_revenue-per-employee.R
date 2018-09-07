

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
# page <- read_html("https://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/")
# 
# houses <- page %>%
#       html_nodes(".photo-cards li article")
# 
# z_id <- houses %>% html_attr("id")
# 
# address <- houses %>%
#       html_node(".zsg-photo-card-address") %>%
#       html_text()
# 
# address
# str(address)



# 1) Pull data from wikipedia: -------------
page.wiki <- read_html("https://en.wikipedia.org/wiki/List_of_largest_companies_by_revenue")

df1.companies <- page.wiki %>% 
      html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>% 
      html_table()

str(df1.companies)


# 2) cleaning data: -------------
df2.companies.clean <- df1.companies %>% 
      set_names(tolower(names(.))) %>% 
      select(-c(country,
                ref, 
                `revenue growth`)) %>% 
      rename(revenue = `revenue(usd millions)`) %>% 
      mutate(revenue = str_sub(revenue, 2)) %>% 
      mutate(revenue = str_replace_all(revenue, ",", ""), 
             employees = str_replace_all(employees, ",", "")) %>% 
      mutate(revenue = as.numeric(revenue)*1e6,  # convert from unit of millions to single dollars 
             employees = as.numeric(employees))

str(df2.companies.clean)



# 3) plot the data: ---------

p1.rev.versus.emp <- df2.companies.clean %>%
      ggplot(aes(x = employees, 
                 y = revenue)) + 
      geom_point(aes(col = industry)) + 
      
      scale_x_continuous(limits = c(0, max(df2.companies.clean$employees))) +  # companies with num employees in range [150, 3.2e6]
      scale_y_continuous(limits = c(0, 5.5e11)) +  # companies with revenue in range [22,000, ...]
      
      geom_smooth(method = "lm", 
                  se = FALSE) + 
      geom_smooth(col = "firebrick", 
                  linetype = "dashed", 
                  se = TRUE) + 
      
      labs(x = "Num of employees", 
           y = "Revenue") + 
      
      theme_classic(base_size = 14); p1.rev.versus.emp




# with log axes: 
p2.rev.versus.emp.logged <- df2.companies.clean %>%
      ggplot(aes(x = log(employees), 
                 y = log(revenue))) + 
      geom_point(aes(col = industry)) + 
      
      scale_x_continuous(limits = c(5, max(log(df2.companies.clean$employees)))) +  # companies with num employees in range [150, 3.2e6]
      scale_y_continuous(limits = c(20, max(log(df2.companies.clean$revenue)))) +  # companies with revenue in range [22,000, ...]
      
      geom_smooth(method = "lm", 
                  se=FALSE) + 
      geom_smooth(col = "firebrick", 
                  linetype = "dashed", 
                  se = FALSE) + 
      
      
      labs(x = "log of num employees", 
           y = "log of revenue") + 
      
      theme_classic(base_size = 14); p2.rev.versus.emp.logged

