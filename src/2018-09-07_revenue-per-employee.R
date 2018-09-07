

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
      geom_point(aes(colour = industry)) + 
      stat_smooth(aes(group = industry,
                      colour = industry),
                  method = "lm",
                  fullrange = FALSE,
                  se = FALSE,
                  linetype = "dashed", 
                  size = .3) +
      
      scale_x_continuous(limits = c(0, max(df2.companies.clean$employees))) +  # companies with num employees in range [150, 3.2e6]
      scale_y_continuous(limits = c(0, 5.5e11)) +  # companies with revenue in range [22,000, ...]
      
      geom_smooth(method = "lm", 
                  se = TRUE) + 
      # geom_smooth(col = "firebrick", 
      #             linetype = "dashed", 
      #             se = FALSE) + 
      
      labs(x = "Num of employees", 
           y = "Revenue in dollars") + 
      
      theme_classic(base_size = 14); p1.rev.versus.emp









# 4) regression models: ------------------
# > first model: ----- 
m1.rev.emp <- lm(revenue ~ employees, data = df2.companies.clean)
summary(m1.rev.emp)

# diagnostics: 
par(mfrow = c(2,2))
plot(m1.rev.emp)

# row 1 is highly influential on the regression. Row 5 might be a problem too. 

df2.companies.clean %>% slice(-c(1, 5))

# > model without influential points: --------
m2.rev.emp.slice <- lm(revenue ~ employees, 
                       data = df2.companies.clean %>% slice(-c(1, 5)))
summary(m2.rev.emp.slice)

# diagnostics: 
plot(m2.rev.emp.slice)  # is this better?? 

