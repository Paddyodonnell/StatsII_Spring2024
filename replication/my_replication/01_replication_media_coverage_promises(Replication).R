#####################################################
#####################################################
## Replication Material for: 
## Media Coverage of Campaign Promises Throughout the Electoral Cycle
## Author: Stefan Müller
## Contact author at stefan.mueller@ucd.ie
## This script reproduces all plots and tables reported in the
## main paper
#####################################################
#####################################################


# load required packages

# note: if a package is not installed, it can installed using 
# install.packages("name_of_package")



library(Hmisc)
library(xtable)
library(rms)
library(zoo)
library(caret)
library(texreg)
library(effects)
library(quanteda)
library(car)
library(rio)
library(lme4)
library(MASS)
library(tidyverse)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Note: the script was run using the following package and software versions
# If you have any issues reproducing some of the results, please contact the author

sessionInfo()
# R version 3.6.0 (2019-04-26)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#     [1] en_IE.UTF-8/en_IE.UTF-8/en_IE.UTF-8/C/en_IE.UTF-8/en_IE.UTF-8
# 
# attached base packages:
#     [1] stats     graphics  grDevices utils     datasets  methods  
# [7] base     
# 
# other attached packages:
#     [1] gridExtra_2.3     forcats_0.4.0     stringr_1.4.0    
# [4] dplyr_0.8.3       purrr_0.3.3       readr_1.3.1      
# [7] tidyr_1.0.2       tibble_2.1.3      tidyverse_1.2.1  
# [10] MASS_7.3-51.4     lme4_1.1-21       Matrix_1.2-17    
# [13] rio_0.5.16        car_3.0-2         quanteda_1.5.3   
# [16] effects_4.1-0     carData_3.0-2     texreg_1.36.23   
# [19] caret_6.0-84      zoo_1.8-5         rms_5.1-3.1      
# [22] SparseM_1.78      xtable_1.8-4      Hmisc_4.2-0      
# [25] ggplot2_3.2.1     Formula_1.2-3     survival_2.44-1.1
# [28] lattice_0.20-38  
# 
# loaded via a namespace (and not attached):
#     [1] TH.data_1.0-10      minqa_1.2.4         colorspace_1.4-1   
# [4] class_7.3-15        htmlTable_1.13.1    base64enc_0.1-3    
# [7] rstudioapi_0.10     MatrixModels_0.4-1  fansi_0.4.1        
# [10] prodlim_2018.04.18  mvtnorm_1.0-10      lubridate_1.7.4    
# [13] xml2_1.2.2          codetools_0.2-16    splines_3.6.0      
# [16] knitr_1.22          jsonlite_1.6        nloptr_1.2.1       
# [19] packrat_0.5.0       broom_0.5.2         cluster_2.0.8      
# [22] compiler_3.6.0      httr_1.4.0          backports_1.1.5    
# [25] assertthat_0.2.1    lazyeval_0.2.2      cli_2.0.1          
# [28] survey_3.36         acepack_1.4.1       htmltools_0.3.6    
# [31] quantreg_5.38       tools_3.6.0         gtable_0.3.0       
# [34] glue_1.3.1          reshape2_1.4.3      fastmatch_1.1-0    
# [37] Rcpp_1.0.3          cellranger_1.1.0    vctrs_0.2.2        
# [40] nlme_3.1-139        iterators_1.0.10    timeDate_3043.102  
# [43] gower_0.2.0         xfun_0.6            stopwords_1.0      
# [46] openxlsx_4.1.0      rvest_0.3.3         lifecycle_0.1.0    
# [49] polspline_1.1.14    scales_1.1.0        ipred_0.9-9        
# [52] hms_0.4.2           sandwich_2.5-1      RColorBrewer_1.1-2 
# [55] curl_3.3            rpart_4.1-15        latticeExtra_0.6-28
# [58] stringi_1.4.5       foreach_1.4.4       checkmate_1.9.3    
# [61] boot_1.3-22         zip_2.0.1           lava_1.6.5         
# [64] rlang_0.4.4         pkgconfig_2.0.3     recipes_0.1.5      
# [67] htmlwidgets_1.3     tidyselect_1.0.0    plyr_1.8.5         
# [70] magrittr_1.5        R6_2.4.1            generics_0.0.2     
# [73] multcomp_1.4-10     DBI_1.0.0           pillar_1.4.3       
# [76] haven_2.1.0         foreign_0.8-71      withr_2.1.2        
# [79] abind_1.4-5         nnet_7.3-12         modelr_0.1.4       
# [82] crayon_1.3.4        grid_3.6.0          readxl_1.3.1       
# [85] data.table_1.12.8   ModelMetrics_1.2.2  digest_0.6.23      
# [88] spacyr_1.2          RcppParallel_4.4.4  stats4_3.6.0       
# [91] munsell_0.5.0       mitools_2.4   
# function for custom ggplot2 theme

theme_baser <- function (){
    theme_minimal()  %+replace%
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_rect(fill=NA,color="black", size=0.5,
                                          linetype="solid"),
              legend.title = element_text(size = 15),
              title = element_text(size = 15, vjust = 1.5, hjust = 0),
              legend.position = "bottom",
              axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(0.3,"cm"),
              legend.text=element_text(size = 13),
              strip.text = element_text(size = 15, hjust = 0.5,
                                        margin=margin(b=5, r = 5, l = 5, t = 5)),
              axis.text = element_text(colour="black", size = 13),
              axis.title = element_text(size = 13, hjust = 0.5))
}

# set theme as default
theme_set(theme_baser())

dat_timeseries <- readRDS("data_timeseries.rds")

# only filter observations with statements from newspapers on promises
dat_complete <- filter(dat_timeseries, !is.na(text_id))

# factorise variables
dat_complete$countryname <- factor(dat_complete$countryname)
dat_complete$tabloid_broadsheet <- factor(dat_complete$tabloid_broadsheet)

# check the number of sentences for each country and cycle
dat_complete %>% 
    select(nsentences_cycle, country_cycle) %>% 
    unique() %>% 
    summarise(n_sentences_total = sum(nsentences_cycle))


## Figure 1: Newspapers included in the analysis ----

# create new variables with number of sentences per year and number of articles per newspaper
dat_plot_newspapers <- dat_complete %>%
    filter(class_inductive != "Other") %>% # filter classified as non-promise related with inductive classification
    group_by(countryname, newspaper, year) %>% 
    summarise(n_sentences_year = n()) %>% 
    filter(newspaper != "" & !is.na(newspaper)) %>% 
    mutate(country_short = car::recode(countryname, "'United Kingdom'='UK';
                                     'Ireland'='IRE'; 'Australia'='AUS'; 'Canada'='CAN'")) %>% 
    mutate(n_total = prettyNum(sum(n_sentences_year), big.mark=",")) %>% 
    mutate(newspaper_articles = paste0(newspaper, " (", n_total, ")"))


# get minimum and maximum year values and calculate time span of available papers
dat_plot_newspapers_min_max <- dat_plot_newspapers %>% 
    group_by(newspaper) %>% 
    summarise(min = min(year),
              max = max(year)) %>% 
    mutate(timespan = max - min)

# descriptive statistics of data availability (reported in paper)
summary(dat_plot_newspapers_min_max$timespan)
sd(dat_plot_newspapers_min_max$timespan)

# create Figure 1
ggplot(dat_plot_newspapers, 
       aes(x = year, y = forcats::fct_rev(newspaper_articles))) + 
    geom_line() +
    geom_point(size = 2, colour = "grey50") +
    scale_x_continuous(limits = c(1979, 2017), breaks = c(seq(1980, 2015, 5))) + 
    facet_grid(country_short~., scales = "free", space = "free") + 
    labs(x = "Year", y = NULL) 
ggsave("R_figures/fig_01.pdf", width = 8, height = 7)

## Figure 2: The daily proportion of pledge-related statements throughout the electoral cycle ----

table(dat_timeseries$countryname, useNA = "always")

dat_metadata <- dat_timeseries %>% 
    filter(!is.na(text_id)) %>% 
    mutate(quarter = lubridate::floor_date(date, "quarter")) %>% 
    arrange(countryname, cycle, class_inductive, quarter) %>% 
    dplyr::select(countryname, cycle, quarter, priorelecdate, 
                  elecdate, gov_type, gov_type_aus_single,
                  gov_type_maj_min,
                  log_gdp_change_lag,
                  poll_change_to_election,
                  date) %>% 
    unique()


# get daily counts of newspaper articles for each class (using the inductive classification)
dat_timeseries_day_count <- dat_timeseries %>% 
    mutate(class_inductive = car::recode(class_inductive, "'Broken/Fulfilled'='Ongoing'")) %>% 
    group_by(countryname, class_inductive, date) %>% 
    count() %>% 
    summarise(n_articles_day = n - 1) # subtract 1 from each day to set day-class observations without coverage to 0

# same number of days in each class?
table(dat_timeseries_day_count$class_inductive)

# merge metadata to this data frame
dat_timeseries_day <- dat_timeseries_day_count %>% 
    left_join(dat_metadata, by = c("countryname", "date"))

# carry last observations forward
dat_timeseries_day <- dat_timeseries_day %>% 
    arrange(countryname, date, class_inductive) %>% 
    mutate(cycle = zoo::na.locf(cycle, na.rm = FALSE),
           quarter = zoo::na.locf(quarter, na.rm = FALSE),
           priorelecdate = zoo::na.locf(priorelecdate, na.rm = FALSE),
           elecdate = zoo::na.locf(elecdate, na.rm = FALSE),
           gov_type = zoo::na.locf(gov_type, na.rm = FALSE),
           gov_type_aus_single = zoo::na.locf(gov_type_aus_single, na.rm = FALSE),
           gov_type_maj_min = zoo::na.locf(gov_type_maj_min, na.rm = FALSE),
           log_gdp_change_lag = zoo::na.locf(log_gdp_change_lag, na.rm = FALSE),
           poll_change_to_election = zoo::na.locf(poll_change_to_election, na.rm = FALSE))


dat_timeseries_day <- dat_timeseries_day %>% 
    filter(class_inductive != "Other") # exclude non-relevant sentences

# calculate values for electoral cycle
dat_timeseries_day_full <- dat_timeseries_day %>% 
    mutate(country_cycle = paste(countryname, cycle)) %>% 
    group_by(countryname, cycle) %>% 
    mutate(electoral_cycle = as.numeric(as.Date(date) - as.Date(priorelecdate)) / as.numeric(as.Date(elecdate) - as.Date(priorelecdate))) %>% 
    mutate(electoral_cycle_alt = as.numeric(as.Date(date) - as.Date(priorelecdate)) / as.numeric(as.Date(elecdate) - as.Date(priorelecdate))) %>% 
    mutate(days_cycle = as.numeric(elecdate - priorelecdate)) %>% 
    group_by(countryname, cycle, priorelecdate, elecdate, class_inductive) %>% 
    mutate(n_articles_cycle = sum(n_articles_day)) %>% 
    mutate(n_articles_prop = n_articles_day / n_articles_cycle) %>% 
    mutate(country_cycle = paste(countryname, cycle, sep = ": ")) %>% 
    ungroup() %>% 
    arrange(countryname, cycle, class_inductive) %>% 
    group_by(countryname, cycle, class_inductive) %>% 
    mutate(n_articles_day_lag = lag(n_articles_day)) %>% 
    filter(electoral_cycle <= 1) %>% # only include sentences that fall into the electoral cycle
    filter(!is.na(electoral_cycle))

# create Figure 2
ggplot(dat_timeseries_day_full, aes(x = electoral_cycle,
                                    y = n_articles_prop)) +
    geom_smooth(colour = "black") +
    facet_grid(countryname~class_inductive, scales = "free") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(NA,NA)) +
    labs(x = "Electoral cycle",
         y = "Daily coverage (standardized by cycle)")
ggsave("R_figures/fig_02.pdf", width = 10, height = 8)


## Figure 3: Predicting the number of articles per day for single-party and multiparty governments, separately for each class ----

dat_timeseries_day_full$country_cycle <- factor(dat_timeseries_day_full$country_cycle)


# subset datasets by government type
dat_timeseries_day_single <- dat_timeseries_day_full %>% 
    filter(gov_type == "Single-party Government")

dat_timeseries_day_multi <- dat_timeseries_day_full %>% 
    filter(gov_type == "Multiparty Government")

# get descriptpive statistics on government type and cycle
dat_timeseries_day_sum <- dat_timeseries_day_full %>% 
    group_by(gov_type, country_cycle) %>% 
    summarise(n = n())

write.csv(dat_timeseries_day_single, "dat_timeseries_day_single.csv")

write.csv(dat_timeseries_day_multi, "dat_timeseries_day_multi.csv")


# Run negative binomial regressions predicting the number of articles per day

# single-party government

nb_single_ongoing <- MASS::glm.nb(
    n_articles_day ~ 
        n_articles_day_lag + 
        poly(electoral_cycle, 3) +
        log_gdp_change_lag + 
        poll_change_to_election +
        country_cycle,
    data = filter(dat_timeseries_day_single,
                  class_inductive == "Ongoing"))


nb_single_broken <- update(
    nb_single_ongoing, . ~ .,
    data = filter(dat_timeseries_day_single,
                  class_inductive == "Broken"))


nb_single_fulfilled <- update(
    nb_single_ongoing, . ~ .,
    data = filter(dat_timeseries_day_single,
                  class_inductive == "Fulfilled"))


# multiparty government

nb_multi_ongoing <- update(
    nb_single_ongoing, . ~ .,
    data = filter(dat_timeseries_day_multi,
                  class_inductive == "Ongoing"))

nb_multi_fulfilled <- update(
    nb_single_ongoing, . ~ .,
    data = filter(dat_timeseries_day_multi,
                  class_inductive == "Fulfilled"))

nb_multi_broken <- update(
    nb_single_ongoing, . ~ .,
    data = filter(dat_timeseries_day_multi,
                  class_inductive == "Broken"))


# get data frames with predicted counts
effect_single_ongoing <- Effect(c("electoral_cycle"), 
                                nb_single_ongoing, 
                                xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Ongoing") %>% 
    mutate(type = "Single-party government")


effect_single_broken <- Effect(c("electoral_cycle"), 
                               nb_single_broken, 
                               xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Broken") %>% 
    mutate(type = "Single-party government")


effect_single_fulfilled <- Effect(c("electoral_cycle"), 
                                  nb_single_fulfilled, 
                                  xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Fulfilled") %>% 
    mutate(type = "Single-party government")


effect_multi_ongoing <- Effect(c("electoral_cycle"), 
                               nb_multi_ongoing, 
                               xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Ongoing") %>% 
    mutate(type = "Multiparty government")


effect_multi_broken <- Effect(c("electoral_cycle"), 
                              nb_multi_broken, 
                              xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Broken") %>% 
    mutate(type = "Multiparty government")


effect_multi_fulfilled <- Effect(c("electoral_cycle"), 
                                 nb_multi_fulfilled, 
                                 xlevels = 150) %>% 
    as.data.frame() %>% 
    mutate(model = "Fulfilled") %>% 
    mutate(type = "Multiparty government")


# bind all data frames for plotting
effect_single_multi <- bind_rows(effect_single_fulfilled,
                                 effect_single_broken,
                                 effect_single_ongoing,
                                 effect_multi_fulfilled,
                                 effect_multi_broken,
                                 effect_multi_ongoing)




effect_single_multi$type <- factor(effect_single_multi$type,
                                   levels = c("Single-party government", 
                                              "Multiparty government"))

effect_single_multi$facet <- paste0(effect_single_multi$type, "\n",
                                    effect_single_multi$model)

ggplot(data = effect_single_multi,
       aes(x = electoral_cycle, y = fit,
           ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.4, fill = "grey60") + 
    facet_wrap(~facet, scales = "free", nrow = 2) +
    geom_line() + 
    labs(x = "Electoral cycle", y = "Number of articles per day") +
    theme(legend.position = "none")
ggsave("R_figures/fig_03.jpg", width = 10, height = 7)


## Table A8: Predicting the number of articles on promises published throughout the electoral cycle ----

## Note: Table A8 is part of the appendix, but all models are estimated to 
## construct Figure 3. Therefore, the table is part of this script and not 
## the replication script for the Supporting Information

screenreg(list(nb_single_ongoing,
               nb_single_broken,
               nb_single_fulfilled,
               nb_multi_ongoing,
               nb_multi_broken,
               nb_multi_fulfilled),
          omit.coef = c("country_cycle*"))


texreg(list(nb_single_ongoing,
            nb_single_broken,
            nb_single_fulfilled,
            nb_multi_ongoing,
            nb_multi_broken,
            nb_multi_fulfilled),
       custom.gof.names = c("AIC", "BIC", "Log likelihood",
                            "Deviance", "N"), 
       omit.coef = c("country_cycle*"),
       file = "R_figures/tab_a08.tex",
       caption.above =  TRUE,
       label = "tab:reg_cycle",
       custom.coef.names = c(
           "(Intercept)",
           "Number of articles per class (lag)",
           "Electoral cycle",
           "Electoral cycle$^2$",
           "Electoral cycle$^3$",
           "Lagged GDP change (log)",
           "Poll change to previous election"),
       fontsize = "scriptsize",
       custom.model.names = c("M1: Ongoing (single)", 
                              "M2: Broken (single)",
                              "M3: Fulfilled (single)",
                              "M4: Ongoing (multi)",
                              "M5: Broken (multi)",
                              "M6: Fulfilled (multi)"),
       caption = "Predicting the number of articles on promises published throughout the electoral cycle",
       float.pos = "!h",
       custom.note = ("\\parbox{.9\\linewidth}{\\footnotesize \\vspace{2pt}%stars. \\\\
       \\textit{Note}: Models 1--3  limit the sample to single-party cabinets, Model 4--6 focus only on multiparty cabinets.  
         All models include dummy variables for each country-cycle observation which are omitted from the table.
                      Standard errors in parentheses.}"))


## Table 1: Number of relevant sentences for each category and country (inductive dictionary, full sample) ----

# get proportions per country and make some variables nicer and more informative for table
tab_articles_country_inductive <- dat_complete %>% 
    ungroup() %>% 
    filter(class_inductive != "Other") %>% 
    filter(!is.na(text_id) & text_id != "") %>% 
    group_by(countryname, class_inductive) %>% 
    summarise(n_sentences_class = n()) %>% 
    spread(class_inductive, n_sentences_class) %>% 
    mutate(Total = Broken + Ongoing + Fulfilled) %>% 
    mutate(perc_broken = as.character(sprintf("%.1f", round(100 * Broken / Total, 1)))) %>% 
    mutate(perc_fulfilled = as.character(sprintf("%.1f", round(100 * Fulfilled / Total, 1)))) %>% 
    mutate(perc_ongoing = as.character(sprintf("%.1f", round(100 * Ongoing / Total, 1)))) %>% 
    mutate(Ongoing = paste0(formatC(Ongoing, format="d", big.mark=","), 
                            " (", perc_ongoing, "\\%)")) %>% 
    mutate(Broken = paste0(formatC(Broken, format="d", big.mark = ","), 
                           " (", perc_broken, "\\%)")) %>% 
    mutate(Fulfilled = paste0(formatC(Fulfilled, format="d", big.mark = ","),
                              " (", perc_fulfilled, "\\%)")) %>% 
    mutate(Total = formatC(Total, format = "d", big.mark = ",")) %>% 
    rename(Country = countryname) %>% 
    dplyr::select(Country, Ongoing, Broken, Fulfilled, Total)


# necessary to create table
addtorow <-  list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste("\\hline \n",
                            "\\endhead \n",
                            "\\hline \n",
                            "\\endfoot \n",
                            "\\endlastfoot \n", sep = ""))


xtable::print.xtable(xtable(tab_articles_country_inductive, 
                            caption="Number of relevant sentences for each category and country (inductive dictionary, full sample)",
                            label="tab:articles_inductive_full",
                            tabular.environment='longtable',
                            align= c("p{0.22\\textwidth}", 
                                     "p{0.22\\textwidth}",
                                     "p{0.18\\textwidth}",
                                     "p{0.18\\textwidth}",
                                     "p{0.14\\textwidth}", 
                                     "p{0.1\\textwidth}")),
                     type="latex",
                     format.args = list(big.mark = ","),
                     size = "footnotesize",
                     file="R_figures/table_01.tex",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")


xtable::print.xtable(xtable(tab_articles_country_inductive, 
                            caption="Number of relevant sentences for each category and country (inductive dictionary, full sample)",
                            label="tab:articles_inductive_full",
                            tabular.environment='longtable',
                            align= c("p{0.22\\textwidth}", 
                                     "p{0.22\\textwidth}",
                                     "p{0.18\\textwidth}",
                                     "p{0.18\\textwidth}",
                                     "p{0.14\\textwidth}", 
                                     "p{0.1\\textwidth}")),
                     type="html",
                     format.args = list(big.mark = ","),
                     size = "footnotesize",
                     file="R_figures/table_01.html",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")

## Table 2: Predicting the ratio of broken to fulfilled promises per quarter and newspaper ----

# prepare dataset for the regression analysis
dat_quarter_classes_inductive <- dat_complete %>%
    mutate(quarter = lubridate::floor_date(date, unit = "quarter")) %>%
    mutate(country_cycle = paste(countryname, cycle, sep = "_")) %>%
    group_by(countryname, country_cycle, 
             newspaper,
             gov_type_maj_min,
             gdp_change_lag,
             poll_change_to_election,
             tabloid_broadsheet,
             gov_type,
             gov_type_aus_single, ## added AUS
             quarter, 
             class_inductive) %>%
    summarise(n_sentences_class = n()) %>% 
    spread(class_inductive, n_sentences_class) %>% 
    mutate(log_gdp_change_lag = log(gdp_change_lag + 1)) %>% 
    mutate(year = lubridate::year(quarter))


# calculate ratios
dat_quarter_classes_inductive <- dat_quarter_classes_inductive %>% 
    mutate(broken_divided_by_fulfilled_in = Broken / Fulfilled) %>% 
    mutate(log_broken_divided_by_fulfilled_in = log((Broken + 0.5) / (Fulfilled + 0.5))) # alternative aggregation


# repeat for deductively derived dictionary
dat_quarter_classes_deductive <- dat_complete %>%
    mutate(quarter = lubridate::floor_date(date, unit = "quarter")) %>%
    mutate(country_cycle = paste(countryname, cycle, sep = "_")) %>%
    group_by(countryname, country_cycle,
             newspaper,
             gov_type_maj_min,
             gdp_change_lag,
             poll_change_to_election,
             tabloid_broadsheet,
             gov_type,
             gov_type_aus_single, ## Australia, different classification of governments
             quarter, 
             class_deductive) %>%
    summarise(n_sentences_class = n()) %>% 
    spread(class_deductive, n_sentences_class) 


dat_quarter_classes_deductive <- dat_quarter_classes_deductive %>% 
    ungroup() %>% 
    mutate(broken_divided_by_fulfilled_de = Broken / Fulfilled) %>% 
    mutate(log_broken_divided_by_fulfilled_de = log((Broken + 0.5) / (Fulfilled + 0.5))) %>% 
    dplyr::select(countryname, country_cycle, newspaper, quarter,
                  contains("_divided_by_")) 


# join to one data frame
dat_quarter_classes_joined <- left_join(
    dat_quarter_classes_inductive,
    dat_quarter_classes_deductive, 
    by = c("countryname", "country_cycle", "newspaper", "quarter")
)


# repeat inductive classification but only consider sentences that mention a political actor

dat_quarter_classes_inductive_actor <- dat_complete %>%
    filter(mention_party_mp_context == TRUE) %>% 
    mutate(quarter = lubridate::floor_date(date, unit = "quarter")) %>%
    mutate(country_cycle = paste(countryname, cycle, sep = "_")) %>%
    group_by(countryname, country_cycle, 
             newspaper,
             gov_type_maj_min,
             gdp_change_lag,
             poll_change_to_election,
             tabloid_broadsheet,
             gov_type,
             gov_type_aus_single, ## added AUS
             quarter, 
             class_inductive) %>%
    summarise(n_sentences_class = n()) %>% 
    spread(class_inductive, n_sentences_class) %>% 
    mutate(log_gdp_change_lag = log(gdp_change_lag + 1)) %>% 
    mutate(year = lubridate::year(quarter))


dat_quarter_classes_inductive_actor <- dat_quarter_classes_inductive_actor %>% 
    mutate(broken_divided_by_fulfilled_in = Broken / Fulfilled) %>% 
    mutate(log_broken_divided_by_fulfilled_in = log((Broken + 0.5) / (Fulfilled + 0.5))) %>% 
    mutate(broken_minus_fulfilled = (Broken - Fulfilled) / (Broken + Fulfilled))

# create factor variables (necessary for estimating the predicted values)
dat_quarter_classes_joined$countryname <- factor(dat_quarter_classes_joined$countryname)
dat_quarter_classes_joined$gov_type <- factor(dat_quarter_classes_joined$gov_type)
dat_quarter_classes_joined$gov_type_aus_single <- factor(dat_quarter_classes_joined$gov_type_aus_single)

dat_quarter_classes_inductive_actor$countryname <- factor(dat_quarter_classes_inductive_actor$countryname)
dat_quarter_classes_inductive_actor$gov_type <- factor(dat_quarter_classes_inductive_actor$gov_type)
dat_quarter_classes_inductive_actor$gov_type_aus_single <- factor(dat_quarter_classes_inductive_actor$gov_type_aus_single)


lme_1_inductive <- lmer(broken_divided_by_fulfilled_in ~ 
                            tabloid_broadsheet +
                            countryname + 
                            log_gdp_change_lag + 
                            poll_change_to_election + 
                            year + 
                            gov_type + 
                            (1 | newspaper) + (1 | country_cycle),
                        data = dat_quarter_classes_joined)


lme_1_inductive_actor <- update(lme_1_inductive, . ~ .,
                                data = dat_quarter_classes_inductive_actor)


lme_1_inductive_log <- lmer(log_broken_divided_by_fulfilled_in ~ 
                                tabloid_broadsheet +
                                countryname + 
                                log_gdp_change_lag + 
                                poll_change_to_election + 
                                year + 
                                gov_type + 
                                (1 | newspaper) + (1 | country_cycle),
                            data = dat_quarter_classes_joined)


screenreg(list(lme_1_inductive, lme_1_inductive_actor, lme_1_inductive_log))

# create Table 2
texreg(list(lme_1_inductive, lme_1_inductive_actor, lme_1_inductive_log),
       include.variance = FALSE,
       custom.gof.names = c("AIC", "BIC", "Log likelihood",
                            "N", "N (Cycles)", 
                            "N (Newspapers)"),
       file = "R_figures/table_02.tex",
       label = "tab:reg_brokenfulfilld_main",
       caption.above =  TRUE,
       omit.coef = c("(Intercept)"), 
       fontsize = "footnotesize",
       custom.coef.names = c(
           "Newspaper: Tabloid", 
           "Canada", "Ireland", "United Kingdom",
           "Lagged GDP change (log)",
           "Poll change to previous election",
           "Year", 
           "Gov. type: Single-party government"),
       custom.model.names = c("M1: Full sample", "M2: Actor mentioned", "M3: Logged DV"),
       caption = "Predicting the ratio of broken to fulfilled promises per quarter and newspaper",
       float.pos = "!h",
       custom.note = ("\\parbox{.9\\linewidth}{\\footnotesize \\vspace{2pt}%stars. \\\\
       \\textit{Note}: Model 1 uses the full sample of quarters with at least one sentence on broken and one sentence on
       fulfilled promises.
       Model 2 considers only sentences that also mention a political party or MP serving in the respective cyle. 
       Model 3 uses a logged dependent variable of the ratio. 
       Models include random intercepts for each newspaper and cycle. Intercepts omitted from table. Standard errors in parentheses.}"))


# create Table 2
htmlreg(list(lme_1_inductive, lme_1_inductive_actor, lme_1_inductive_log),
       include.variance = FALSE,
       custom.gof.names = c("AIC", "BIC", "Log likelihood",
                            "N", "N (Cycles)", 
                            "N (Newspapers)"),
       file = "R_figures/table_02.html",
       label = "tab:reg_brokenfulfilld_main",
       caption.above =  TRUE,
       omit.coef = c("(Intercept)"), 
       fontsize = "footnotesize",
       custom.coef.names = c(
           "Newspaper: Tabloid", 
           "Canada", "Ireland", "United Kingdom",
           "Lagged GDP change (log)",
           "Poll change to previous election",
           "Year", 
           "Gov. type: Single-party government"),
       custom.model.names = c("M1: Full sample", "M2: Actor mentioned", "M3: Logged DV"),
       caption = "Predicting the ratio of broken to fulfilled promises per quarter and newspaper",
       float.pos = "!h",
       custom.note = ("\\parbox{.9\\linewidth}{\\footnotesize \\vspace{2pt}%stars. \\\\
       \\textit{Note}: Model 1 uses the full sample of quarters with at least one sentence on broken and one sentence on
       fulfilled promises.
       Model 2 considers only sentences that also mention a political party or MP serving in the respective cyle. 
       Model 3 uses a logged dependent variable of the ratio. 
       Models include random intercepts for each newspaper and cycle. Intercepts omitted from table. Standard errors in parentheses.}"))


## Figure 4: Predicting the ratio of reports on broken to fulfilled promises per quarter and newspaper ----

## Figure 4a: Predicted values for each country

# get predicted values
effect_country_in <- as.data.frame(
    Effect(c("countryname"), 
           lme_1_inductive))

effect_country_in <- effect_country_in %>% 
    mutate(countryname = car::recode(countryname, "'United Kingdom'='UK'"))

fig_04a <- ggplot(effect_country_in, 
       aes(x = countryname,
           y = fit,
           ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red", size = 0.8) +
    geom_pointrange(size = 0.8) +
    annotate("text", x = 1.5, y = 1.4, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 1.5, y = 0.6, label = "Fulfilled promises", size = 4,
             color = "grey30") +
    annotate("segment", x = 0.75, xend = 0.75, y = 1.2, yend = 1.7, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    annotate("segment", x = 0.75, xend = 0.75, y = 0.8, yend = 0.3, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    scale_y_continuous(breaks = c(seq(0, 3.5, 0.5)),
                       limits = c(0, 3.5)) +
    ylab("Ratio of broken to fulfilled promises") +
    xlab(NULL) +
    labs(title = "(a) Predicted values for each country") +
    theme(plot.title = element_text(family = "Times"))
ggsave(fig_04a, filename = "R_figures/fig_04a.pdf",
       width = 5, height = 4.5)


## Figure 4b: Predicted values over time 

effect_year_in <- as.data.frame(
    Effect(c("year"), 
           lme_1_inductive, 
           xlevels = 150))


fig_04b <- ggplot(data = effect_year_in,
       aes(x = year, y = fit)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red", size = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.4, fill = "grey60") +
    geom_line() +
    scale_x_continuous(limits = c(1979, 2017), 
                       breaks = c(seq(1980, 2015, 5))) +
    scale_y_continuous(breaks = c(seq(0, 3.5, 0.5)),
                       limits = c(0, 3.5)) +
    ylab("Ratio of broken to fulfilled promises") +
    xlab(NULL) + 
    annotate("text", x = 2008, y = 1.4, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 2008, y = 0.6, label = "Fulfilled promises", size = 4,
             color = "grey30") +
    annotate("segment", x = 2016, xend = 2016, y = 1.2, yend = 1.7, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    annotate("segment", x = 2016, xend = 2016, y = 0.8, yend = 0.3, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    labs(title = "(b) Predicted values over time") +
    theme(plot.title = element_text(family = "Times"))
ggsave(fig_04b, filename = "R_figures/fig_04b.pdf",
       width = 5, height = 4.5)

pdf("R_figures/fig_04.pdf", width = 10, height = 5)
grid.arrange(fig_04a, fig_04b, nrow = 1)
dev.off()


## Figure 5: The quarterly focus on broken promises relative to fulfilled promises in newspapers ---- 

dat_boot_brokenfulfilled_news <- dat_quarter_classes_inductive %>% 
    group_by(countryname, tabloid_broadsheet, newspaper) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$broken_divided_by_fulfilled_in)))) %>% 
    filter(!is.na(newspaper))

dat_boot_brokenfulfilled_news <- dat_boot_brokenfulfilled_news %>% 
    ungroup() %>% 
    mutate(countryname = car::recode(countryname, "'Ireland'='IRE';
                                   'Australia'='AUS'; 'Canada'='CAN';
                                   'United Kingdom'='UK'"))


ggplot(dat_boot_brokenfulfilled_news, 
       aes(x = reorder(newspaper, Mean), 
           y = Mean,
           ymin = Lower, ymax = Upper,
           shape = tabloid_broadsheet,
           colour = tabloid_broadsheet)) + 
    geom_pointrange(size = 0.8, position = position_dodge(width = 0.5)) + 
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_grey(start = 0.5, end = 0) +
    facet_grid(countryname~., scales = "free_y", space = "free_y") +
    coord_flip() + 
    xlab(NULL) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    ylab("Ratio of broken to fulfilled promises (and 95% bootstrapped CIs)") +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
ggsave("R_figures/fig_05.pdf", width = 10, 
       height = 6)


## Figure 6: Aggregated sentiment (per quarter and newspaper) for sentences classified either as 'broken' or 'fulfilled', 
## against the baseline of sentences classified as 'ongoing' ----




# calculate mean sentiment and bootstrap standard errors for each class

dat_sent_cycle_class_inductive <- dat_complete %>% 
    mutate(quarter = lubridate::floor_date(date, unit = "quarter")) %>% 
    mutate(country_cycle = paste(countryname, cycle, sep = "_")) %>% 
    group_by(countryname, newspaper, tabloid_broadsheet, 
             country_cycle, quarter, 
             class_inductive, gdp_change, gdp_change_lag) %>% 
    summarise(sum_positive = sum(positive),
              sum_negative = sum(negative),
              sum_neg_positive = sum(neg_positive),
              sum_neg_negative = sum(neg_negative)) %>% 
    mutate(sentiment_quarter = log((sum_positive + sum_neg_negative + 0.5) / 
                                       (sum_negative + sum_neg_positive + 0.5))) %>% 
    ungroup() 

# estimate sentiment for the two classes (broken and fulfilled vs. ongoing (as baseline))
set.seed(123)
dat_boot_sent_country_class_dummy_inductive <- dat_sent_cycle_class_inductive %>% 
    filter(class_inductive != "Other") %>% # remove irrelevant sentences
    mutate(class_ongoing_dummy = ifelse(class_inductive == "Ongoing", "Sentence: Ongoing (baseline)", "Sentence: Broken or Fulfilled")) %>% 
    group_by(countryname, class_ongoing_dummy) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$sentiment_quarter)))) 

# create Figure 6
ggplot(dat_boot_sent_country_class_dummy_inductive, 
       aes(x = reorder(class_ongoing_dummy, desc(class_ongoing_dummy)), 
           y = Mean,
           ymin = Lower, ymax = Upper)) + 
    geom_pointrange(size = 0.8) + 
    facet_wrap(~countryname) +
    coord_flip() + 
    labs(x = NULL, y = "Sentiment (and 95% bootstrapped CIs)") 
ggsave("R_figures/fig_06.pdf", 
       width = 10, 
       height = 3)

