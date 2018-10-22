library(tidyverse)
library(DT)
library(htmlwidgets)
library(widgetframe)

# unloadNamespace('data.table')
# unloadNamespace('reshape2')
# unloadNamespace('plyr')

dat <- read_csv('data/processed/pd_data_cleaned.csv')

dat <- dat %>% 
          mutate(pd_lawyer_ratio_15 = pd_cases_2015/pd_lawyers_2015)

dat_clean <- dat %>% select(County, total_cases_2015, pd_cases_2015_pct, 
                            pd_lawyer_ratio_15, pd_budget_per_capita_2015, pd_budget_per_capita_pct_chng)

#Rank for Total Cases
  q1_case <- quantile(dat_clean$total_cases_2015)[[2]]
  q2_case <- quantile(dat_clean$total_cases_2015)[[3]]
  q3_case <- quantile(dat_clean$total_cases_2015)[[4]]
  q4_case <- quantile(dat_clean$total_cases_2015)[[5]]
  
  
  dat_clean <- dat_clean %>% mutate(total_cases_rank = case_when(
    total_cases_2015 <= q4_case & total_cases_2015 > q3_case ~ 4,
    total_cases_2015 <= q3_case & total_cases_2015 > q2_case ~ 3,
    total_cases_2015 <= q2_case & total_cases_2015 > q1_case ~ 2,
    total_cases_2015 <= q1_case ~ 1
    
  ))
  
#Rank for Percent PD cases
  q1_pd_pct <- quantile(dat_clean$pd_cases_2015_pct)[[2]]
  q2_pd_pct <- quantile(dat_clean$pd_cases_2015_pct)[[3]]
  q3_pd_pct <- quantile(dat_clean$pd_cases_2015_pct)[[4]]
  q4_pd_pct <- quantile(dat_clean$pd_cases_2015_pct)[[5]]
  
  
  dat_clean <- dat_clean %>% mutate(pd_pct_rank = case_when(
    pd_cases_2015_pct <= q4_pd_pct & pd_cases_2015_pct > q3_pd_pct ~ 4,
    pd_cases_2015_pct <= q3_pd_pct & pd_cases_2015_pct > q2_pd_pct ~ 3,
    pd_cases_2015_pct <= q2_pd_pct & pd_cases_2015_pct > q1_pd_pct ~ 2,
    pd_cases_2015_pct <= q1_pd_pct ~ 1
    
  ))
  
#Rank for Lawyer Ratio
  
  dat_clean_law <- dat_clean %>% filter(!is.na(pd_lawyer_ratio_15))
  
  q1_law <- quantile(dat_clean_law$pd_lawyer_ratio_15)[[2]]
  q2_law <- quantile(dat_clean_law$pd_lawyer_ratio_15)[[3]]
  q3_law <- quantile(dat_clean_law$pd_lawyer_ratio_15)[[4]]
  q4_law <- quantile(dat_clean_law$pd_lawyer_ratio_15)[[5]]
  
  
  dat_clean <- dat_clean %>% mutate(ratio_rank = case_when(
    pd_lawyer_ratio_15 <= q4_law & pd_lawyer_ratio_15 > q3_law ~ 4,
    pd_lawyer_ratio_15 <= q3_law & pd_lawyer_ratio_15 > q2_law ~ 3,
    pd_lawyer_ratio_15 <= q2_law & pd_lawyer_ratio_15 > q1_law ~ 2,
    pd_lawyer_ratio_15 <= q1_law ~ 1
    
  ))

#Rank for Budget per Capita
  q1 <- quantile(dat_clean$pd_budget_per_capita_2015)[[2]]
  q2 <- quantile(dat_clean$pd_budget_per_capita_2015)[[3]]
  q3 <- quantile(dat_clean$pd_budget_per_capita_2015)[[4]]
  q4 <- quantile(dat_clean$pd_budget_per_capita_2015)[[5]]
  
  
  dat_clean <- dat_clean %>% mutate(budget_per_capita_rank = case_when(
    pd_budget_per_capita_2015 <= q4 & pd_budget_per_capita_2015 > q3 ~ 4,
    pd_budget_per_capita_2015 <= q3 & pd_budget_per_capita_2015 > q2 ~ 3,
    pd_budget_per_capita_2015 <= q2 & pd_budget_per_capita_2015 > q1 ~ 2,
    pd_budget_per_capita_2015 <= q1 ~ 1
    
  ))
  
#Rank for Percent Change in Budget
  
  dat_clean_pct <- dat_clean %>% filter(!is.na(pd_budget_per_capita_pct_chng))
  
  q1_pct <- quantile(dat_clean_pct$pd_budget_per_capita_pct_chng)[[2]]
  q2_pct <- quantile(dat_clean_pct$pd_budget_per_capita_pct_chng)[[3]]
  q3_pct <- quantile(dat_clean_pct$pd_budget_per_capita_pct_chng)[[4]]
  q4_pct <- quantile(dat_clean_pct$pd_budget_per_capita_pct_chng)[[5]]
  
  
  dat_clean <- dat_clean %>% mutate(pct_chng_budget_rank = case_when(
    pd_budget_per_capita_pct_chng <= q4_pct & pd_budget_per_capita_pct_chng > q3_pct ~ 4,
    pd_budget_per_capita_pct_chng <= q3_pct & pd_budget_per_capita_pct_chng > q2_pct ~ 3,
    pd_budget_per_capita_pct_chng <= q2_pct & pd_budget_per_capita_pct_chng > q1_pct ~ 2,
    pd_budget_per_capita_pct_chng <= q1_pct ~ 1
    
  ))

# Colors for rank

    # Blue scale
    blue_1 <- '#D1E7F9'
    blue_2 <- '#81C0F3'
    blue_3 <- '#2C7ABB'
    blue_4 <- '#055698'
    
    black <- '#000000'
    gray <- '#CECECE'

    
#create data table as R object
pd_table <- 
  datatable(dat_clean, rownames = FALSE,
                             colnames=c("COUNTY", "TOTAL CASES","% PD CASES", "CASES PER FT DEFENDER", 
                                        "PD BUDGET PER CAPITA", "% CHANGE IN BUDGET PER CAPITA",
                                        "TOTAL_CASES_RANK", "PD_PCT_RANK", "RATIO_RANK",
                                        "BUDGET_PER_CAPITA_RANK", "PCT_CHNG_BUDGET_RANK"), 
                             extensions = 'Responsive',
                             class = 'compact hover row-border',
                             options = list(
                              dom = 't',
                              autowidth = TRUE,
                              order = list(list(2, 'desc')),
                              columnDefs = list(
                                (list(className = 'dt-left', targets = c(0,1,2,3,4,5)) ),
                                (list(targets = c(6,7,8,9,10), visible = FALSE))
                                ),
                              #scrollCollapse=TRUE,
                               pageLength = 67,
                               paging = FALSE,
                               searching = FALSE,
                               initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({
                                        'font-family': 'Arial',
                                        'font-size': '14px',
                                        'color': '#000000'});",
                                   "$(this.api().table().header()).css({
                                       'color': '#232C32', 
                                       'font-family': 'Arial',
                                       'font-size': '14px'});",
                                   "}"
                                 ))) %>% 
  formatStyle('total_cases_2015', 'total_cases_rank', backgroundColor = styleEqual(c(1, 2, 3, 4),
                                                 c(blue_1, blue_2, blue_3, blue_4))) %>%
  formatStyle('total_cases_2015', 'total_cases_rank', color = styleEqual(c(1, 2, 3, 4),
                                                 c(black, black, black, gray))) %>%
  formatStyle('pd_cases_2015_pct', 'pd_pct_rank', backgroundColor = styleEqual(c(1, 2, 3, 4),
                                                 c(blue_1, blue_2, blue_3, blue_4))) %>%
  formatStyle('pd_cases_2015_pct', 'pd_pct_rank', color = styleEqual(c(1, 2, 3, 4),
                                                 c(black, black, black, gray))) %>%
  formatStyle('pd_lawyer_ratio_15', 'ratio_rank', backgroundColor = styleEqual(c(1, 2, 3, 4),
                                                 c(blue_1, blue_2, blue_3, blue_4))) %>%
  formatStyle('pd_lawyer_ratio_15', 'ratio_rank', color = styleEqual(c(1, 2, 3, 4),
                                                 c(black, black, black, gray))) %>%
  formatStyle('pd_budget_per_capita_2015', 'budget_per_capita_rank', backgroundColor = styleEqual(c(1, 2, 3, 4),
                                                 c(blue_1, blue_2, blue_3, blue_4))) %>%
  formatStyle('pd_budget_per_capita_2015', 'budget_per_capita_rank', color = styleEqual(c(1, 2, 3, 4),
                                                 c(black, black, black, gray))) %>%
  formatStyle('pd_budget_per_capita_pct_chng', 'pct_chng_budget_rank', backgroundColor = styleEqual(c(1, 2, 3, 4),
                                                 c(blue_1, blue_2, blue_3, blue_4))) %>%
  formatStyle('pd_budget_per_capita_pct_chng', 'pct_chng_budget_rank', color = styleEqual(c(1, 2, 3, 4),
                                                 c(black, black, black, gray))) %>%
  formatCurrency(columns = (2), currency = " ", interval = 3, mark = ",", digits = 0) %>%
  formatCurrency(columns = (5), currency = "$", interval = 3, mark = ",", 
                 digits = 2, dec.mark = getOption("OutDec"), before = TRUE) %>%
  formatPercentage(columns = (3), 2) %>%
  formatPercentage(columns = (6), 0) %>%
  formatRound(columns = (4), 0) %>%
  formatStyle(c('County'), fontWeight = 'bold')

saveWidget(frameableWidget(pd_table), "index.html", selfcontained = FALSE, libdir = "src/")



