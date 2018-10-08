

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
