

#*******************************************************
# GPD Per person in US Metropolitan Statistical Areas
#*******************************************************

library(tidyverse)
library(here) 
library(broom)
library(ggrepel)

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
                 y = gdp_2017, 
                 label = msa)) + 
      geom_point(alpha = 0.5)  + 
      geom_smooth(col = "dodgerblue4", 
                  se = FALSE, 
                  size = 0.5) +
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
                               "100,000,000")) + 
      scale_y_log10(breaks = c(1e4, 1e5, 1e6), 
                    labels = c("10,000", "100,000", "1,000,000")); p2.logged.x
    


p3.logged.both.axis <- p2.logged.x + 
      scale_y_log10(); p3.logged.both.axis

ggsave(here("results", 
            "dst", 
            "2018-11-20_loggdp_vs_logpop.pdf"))

# 4) Select random LSA in bottom and top decile of GDP -------
msa1 <- df3.gdp.and.pop %>%
      filter(pop2017 < quantile(df3.gdp.and.pop$pop2017,
                                0.10, na.rm = TRUE)) %>% 
      sample_n(1) %>% 
      select(msa, 
             gdp_2017, 
             pop2017) %>%
      mutate(gdp.per.cap = gdp_2017/pop2017*1e6) %>% print


msa2 <- df3.gdp.and.pop %>%
      filter(pop2017 > quantile(df3.gdp.and.pop$pop2017,
                                0.40, na.rm = TRUE),
             pop2017 < quantile(df3.gdp.and.pop$pop2017,
                                0.60, na.rm = TRUE)) %>% 
      sample_n(1) %>% 
      select(msa, 
             gdp_2017, 
             pop2017) %>% 
      mutate(gdp.per.cap = gdp_2017/pop2017*1e6) %>% print


# > get pop numbers and gdp per cap: ----
msa1.gdp.per.cap <- msa1 %>% pull(gdp.per.cap) %>% print

msa2.pop <- msa2 %>% pull(pop2017) %>% print  # predictor var
msa2.gdp <- msa2 %>% pull(gdp_2017) %>% print  # response var

# > estimate gdp per cap for larger msa using ratio
# of smaller msa: -----
msa2.estimate.gdp <- (msa1.gdp.per.cap * msa2.pop/1e6) %>% print

msa2.diff.prop <- (msa2.gdp - msa2.estimate.gdp)/msa2.gdp
msa2.diff.prop

# +ve difference means we are underestimating actual GDP
# -ve difference means we are overestimating actual GDP


# 5) Use GDP per cap to estimate unknown msa GDP: ------


# 6) Use log-log linear relationship to estimate unknown msa GDP: ------





# 7) regression models -------
# > 7.1) unlogged model -----
m1.unlogged <- lm(gdp_2017 ~ pop2017, 
                  data = df3.gdp.and.pop)

summary(m1.unlogged)
glance(m1.unlogged)

par(mfrow = c(2,2))
plot(m1.unlogged)

# 1) As fitted values increase, resids become 
# negatively skewed. 
# 2) pretty significant non-normality 
# 3) and 4) at least one very significant outlier


# > 7.2) logged model -----
m2.logged <- lm(log(gdp_2017) ~ log(pop2017),
                data = df3.gdp.and.pop)

summary(m2.logged)  
# note: you can't compare R^2 for models with different 
# response variables 

glance(m2.logged)

par(mfrow = c(2,2))
plot(m2.logged)
# this looks pretty good! 

# Which MSAs are the biggest outliers? 
df4.logged.model.output <- 
      df3.gdp.and.pop %>% 
      select(msa, 
             gdp_2017, 
             pop2017) %>% 
      filter(complete.cases(.)) %>% 
      
      bind_cols(augment(m2.logged)) %>% 
      
      mutate(abs.resid = abs(.resid)) %>% 
      arrange(desc(abs.resid))

df4.logged.model.output %>% head


# identify outliers on plot: 
p4.outliers <- p2.logged.x + 
      
      geom_point(data = df3.gdp.and.pop %>% 
                       slice(c(229, 312, 59)),
                 colour = "red", 
                 size = 2) + 
      geom_text_repel(data = df3.gdp.and.pop %>%
                            slice(c(229, 312, 59)),
                      nudge_x = -3, 
                      segment.colour = "grey80") + 
      
      labs(title = "There's something special about Casper, Midland, and San Jose", 
           subtitle = "These areas' GDPs deviate most significantly from the overall relationship between population size and GDP. \nIn other words, they are more economically productive than we would expect, given their population sizes. \n\n", 
           caption = "\n\nData sources: \nhttps://www.bea.gov/data/gdp/gdp-metropolitan-area \nhttps://www2.census.gov/programs-surveys/popest/datasets/2010-2017/metro/totals", 
           x = "MSA population in 2017", 
           y = "MSA GDP in 2017 (millions of USD)") +
      
      
      theme(axis.text.x = element_text(angle = 45, 
                                       hjust = 1), 
            plot.caption = element_text(hjust = -0, 
                                        size = 8)); p4.outliers

ggsave(here("results", 
            "dst", 
            "2018-11-20_outliers-in-terms-of-GDP.pdf"), 
       width = 8)



# Write outputs: ---------------
write_csv(df3.gdp.and.pop,
          here("results", 
               "dst", 
               "us-msa-gdp-and-population.csv"))
