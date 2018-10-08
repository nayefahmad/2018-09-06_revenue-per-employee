

#*******************************************************
# GPD Per person in US Metropolitan Statistical Areas
#*******************************************************

library("tidyverse")
library("here") 


# 1) Read in and clean GDP data: --------
# source: https://www.bea.gov/data/gdp/gdp-metropolitan-area 

df1.gdp <- readxl::read_excel(here("data", 
                                   "gdp_metro0918.xlsx"),
                              sheet = "Table1", 
                              range = "A5:G387",
                              col_names = FALSE) 

colnames(df1.gdp) <- c("msa", 
                       paste0("gdp_", 2012:2017))

str(df1.gdp)
summary(df1.gdp)



# 2) Read in and clean population data: --------
# source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/metro/totals/cbsa-est2017-alldata.csv 

df2.pop <- read_csv(here("data", 
                         "cbsa-est2017-alldata.csv")) %>% 
      set_names(tolower(names(.))) %>% 
      select(name, 
             lsad, 
             popestimate2017) %>% 
      filter(lsad == "Metropolitan Statistical Area") %>%
      rename(msa = name, 
             pop2017 = popestimate2017)
      # mutate_if(is.character, 
      #            factor)

str(df2.pop, max.level = 1)
summary(df2.pop)


# which MSA is missing in df2? 
anti_join(df1.gdp, 
          df2.pop)

# Macon, GA and Twin Fall... are missing in the population
# figures


# 3) Join population and gdp: 
df3.gdp.and.pop <- df1.gdp %>% 
      full_join(df2.pop) %>% 
      mutate_if(is.character, 
                factor)

str(df3.gdp.and.pop)



# 4) Graph the relationship: -------
p1.gdp.per.person <- df3.gdp.and.pop %>% 
      ggplot(aes(x = pop2017, 
                 y = gdp_2017)) + 
      geom_point(alpha = 0.5)  + 
      geom_smooth(col = "dodgerblue4", 
                  se = FALSE) +
      geom_smooth(method = "lm", 
                  colour = "red", 
                  se = FALSE) + 
      
      theme_classic(); p1.gdp.per.person


# we can't see most of the points; let's 
# zoom in on values under 90th percentile: 

p1.1.gdp.per.person.zoom <- p1.gdp.per.person + 
      
      coord_cartesian(xlim = c(0, quantile(df3.gdp.and.pop$pop2017, 
                                           .90, 
                                           na.rm = TRUE)), 
                      ylim = c(0, quantile(df3.gdp.and.pop$gdp_2017, 
                                           .90, 
                                           na.rm = TRUE))) +
      
      theme_classic(); p1.1.gdp.per.person.zoom




p2.logged.x <- p1.gdp.per.person + 
      scale_x_log10(limits = c(1, 1e8), 
                    breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8), 
                    labels = c("1", "10", "100", "1000", "10,000", 
                               "100,000", "1,000,000", "10,000,000", 
                               "100,000,000")); p2.logged.x
    


p3.logged.both.axis <- p2.logged.x + 
      scale_y_log10(); p3.logged.both.axis



# 4) Select random LSA in bottom and top decile of GDP -------



# 5) Use GDP per cap to estimate unknown msa GDP: ------


# 6) Use log-log linear relationship to estimate unknown msa GDP: ------



