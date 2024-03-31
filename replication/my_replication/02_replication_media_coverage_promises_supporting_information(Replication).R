#####################################################
#####################################################
## Replication Material for: 
## Media Coverage of Campaign Promises Throughout the Electoral Cycle
## Author: Stefan Müller
## Contact author at stefan.mueller@ucd.ie
## This script reproduces all plots and tables reported in the
## Supporting Information
#####################################################
#####################################################


# load required packages

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
library(quanteda.textplots) #FORGOTTEN IN ORIGIONAL 
library(quanteda.textstats) #FORGOTTEN IN ORIGIONAL 
# Quanteda broken up the text functions into separate packages.
# Not Stefan's fault.


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

# load full data
dat_timeseries <- readRDS("data_timeseries.rds")


# only filter observations with statements from newspapers on promises
dat_complete <- filter(dat_timeseries, !is.na(text_id))


# load data required to reproduce Figures A1 and A2
data_evaluations <- read_csv("data_evaluations.csv")

# use 99 per cent CIs
#z <-  2.576

# 95 per cent CIs
z <-1.96 

# get confidence intervals based on sample size of surveys
data_evaluations <- data_evaluations %>% 
    mutate(incorrect = incorrect /100) %>% 
    mutate(dont_know = dont_know /100) %>% 
    mutate(ci_upper_incorrect = incorrect - z * sqrt((incorrect * (1-incorrect))/ respondents)) %>% 
    mutate(ci_lower_incorrect = incorrect + z * sqrt((incorrect * (1-incorrect))/ respondents)) %>% 
    mutate(ci_lower_dont_know = dont_know - z * sqrt((dont_know * (1-dont_know))/ respondents)) %>% 
    mutate(ci_upper_dont_know = dont_know + z * sqrt((dont_know * (1-dont_know))/ respondents)) %>% 
    mutate(sum_inaccurate = incorrect + dont_know) %>% 
    mutate(ci_lower_inaccurate = sum_inaccurate - z * sqrt((sum_inaccurate * (1-sum_inaccurate))/ respondents)) %>% 
    mutate(ci_upper_inaccurate = sum_inaccurate + z * sqrt((sum_inaccurate * (1-sum_inaccurate))/ respondents)) %>% 
    select(c(starts_with("ci_"), country, sum_inaccurate, incorrect, dont_know, study, pledge))

#View(data_evaluations)

# transform to long format
data_evaluations_long <- data_evaluations %>% 
    gather(key, value, -c(study, country, pledge, starts_with("ci_"), sum_inaccurate)) %>% 
    mutate(ci_lower = ifelse(key == "incorrect", ci_lower_incorrect, ci_lower_dont_know)) %>% 
    mutate(ci_upper = ifelse(key == "incorrect", ci_upper_incorrect, ci_upper_dont_know)) %>% 
    group_by(key) %>% 
    mutate(mean_key = mean(value)) %>% 
    ungroup() %>% 
    mutate(key = car::recode(key, "'dont_know'='Dont know';'incorrect'='Incorrect'")) %>% 
    mutate(country_study = paste0(country, "\n", study))

#View(data_evaluations_long)


# recode "Don't know"
data_evaluations_long$key[data_evaluations_long$key=="Dont know"] <- "Don't know"


# change factor levels
data_evaluations_long$country_study <- factor(data_evaluations_long$country_study, 
                                  levels = c("Ireland\nThomson (2011)", 
                                             "Sweden\nNaurin and Oscarsson (2017)",
                                             "Canada\nDuval and Pétry (2018)", 
                                             "United Kingdom\nThomson and Brandenburg (2018)", "Portugal\nBelchior (2019)"))



data_evaluations_long_inaccurate <- data_evaluations_long %>%
    filter(key == "Incorrect") %>% 
    arrange(-sum_inaccurate)

#View(data_evaluations_long_inaccurate)

# get average value for plot
data_evaluations_long_inaccurate_means <- data_evaluations_long_inaccurate %>% 
    summarise(mean = mean(sum_inaccurate))

#View(data_evaluations_long_inaccurate_means)

# Figure A1: 
ggplot(data_evaluations_long_inaccurate, aes(x = nrow(data_evaluations_long_inaccurate):1, 
                                                    y = sum_inaccurate, 
                                                    ymin = ci_lower_inaccurate, 
                                                    ymax = ci_upper_inaccurate)) +
    scale_colour_brewer(palette = "Set1", name = NULL) + 
    geom_hline(data = data_evaluations_long_inaccurate_means, aes(yintercept = mean), 
               colour = "red",
               linetype = "dashed") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(seq(0, 1, 0.1)), limits = c(0, 1)) + 
    scale_x_discrete(breaks = NULL) +
    geom_pointrange() +
    labs(y = "Inaccurate and Don't know responses to campaign pledge fulfilment", x = NULL) + 
    coord_flip() +
    annotate("text", x = 20, size = 4.5, 
             y = data_evaluations_long_inaccurate_means$mean + .06, label = "Average", colour = "red") +
    theme(axis.text.y = element_blank())
#ggsave(filename = "R_figures_supporting_information/fig_a01.pdf", width = 10, height = 4)


ggplot(data_evaluations_long, aes(x = reorder(pledge, value), 
                      y = value)) + 
    geom_point(size = 3) + 
    coord_flip() + 
    scale_y_continuous(limits = c(0, 0.8), breaks = c(0, 0.2, 0.4, 0.6)) + 
    facet_grid(country_study~key, scales = "free_y",
               space = "free") + 
    theme(legend.position = "none",
          axis.text = element_text(size = 13),
          strip.text = element_text(size = 12)
    ) + 
    labs(x = NULL, y = "Proportion")
#ggsave("R_figures_supporting_information/fig_a02.pdf", width = 10, height = 14)


## Figure A3: Flowchart (was created manually, replication neither required nor possible ) ----


# Validate dictionary using crowdcoding

# Validate dictionary using crowdcoding

data_cf <- read_csv("data_crowdcoded.csv") %>%
  filter(`_golden` != "true") # exclude gold questions (test questions)

data_cf_select <- data_cf %>%
  group_by(`_unit_id`, fulfilment) %>%
  mutate(n_eval = n()) %>%
  filter(n_eval >= 2) %>%
  select(`_unit_id`, sentence, n_eval, fulfilment) %>%
  ungroup() %>%
  unique() %>%
  mutate(class = ifelse(is.na(fulfilment), "irrelevant", fulfilment)) %>%
  mutate(class_broad = str_replace_all(class, "expected", "")) %>%
  mutate(class_broad = str_trim(class_broad)) %>% 
  mutate(class_manual = ifelse(!class_broad %in% c("broken", "fulfilled"),
                               "other", class_broad)) %>% 
  select(sentence, class_manual, n_eval) %>% 
  unique()

table(data_cf_select$class_manual)

cf_corpus <- data_cf_select %>% 
  corpus(text_field = "sentence") 


pledge_dict <- dictionary(list(pledge_core = list(pledge = c("pledge*"),
                                                  promise = c("promise*")),
                               pledge_alt = list(guarantee = c("guarantee*"),
                                                 assure = c("assure*"),
                                                 ensure = c("ensur*")),
                               pledge_broken = c("broken", "break*", "fail_to",
                                                 "failed_to", "failure",
                                                 "not_fulfil*", "not_keep*",
                                                 "not_kep*", "not_fullfil*",
                                                 "not_deliver*"),
                               pledge_fulfilled = c("fulfil*", 
                                                    "kept", "delivered"),
                               pledge_ongoing = c("ensur*", "review*",
                                                  "continu*", "pledged", 
                                                  "promised", "future", "delivered")))


#############################################################################

# BROKEN

#cf_dict_dfm <- cf_corpus %>% 
#    tokens() %>% 
#    tokens_compound(phrase(c("not fulfil*", "not keep*",
#                             "not kep*", "not fullfil*", "not deliver*"))) %>% 
#    dfm(tolower = TRUE, 
#        stem = FALSE, 
#        remove_punct = TRUE,
#        remove_numbers = TRUE) %>% 
#    dfm_lookup(pledge_dict, levels = 1:2) %>% 
#    quanteda::convert(to = "data.frame")

##############################################################################

# FIXED

cf_dict_dfm <- cf_corpus %>% 
  tokens() %>% 
  tokens_compound(phrase(c("not fulfil*", "not keep*",
                           "not kep*", "not fullfil*", "not deliver*"))) %>% 
  dfm(tolower = TRUE, 
      remove_punct = TRUE,
      remove_numbers = TRUE) %>% 
  dfm_lookup(pledge_dict, levels = 1:2) %>% 
  quanteda::convert(to = "data.frame")


cf_dict_df <- cf_dict_dfm %>%
  mutate(class = ifelse(pledge_fulfilled > pledge_broken, "fulfilled",
                        ifelse(pledge_broken > pledge_fulfilled,
                               "broken", "other"))) %>% 
  mutate(class = stringr::str_to_title(class)) 


cf_merged <- bind_cols(cf_dict_df, data_cf_select) %>% 
  filter(pledge_core.pledge > 0 | pledge_core.promise > 0) %>% 
  mutate(class_manual = stringr::str_to_title(class_manual)) %>% 
  mutate(class_manual_print = paste("Crowd:", class_manual, sep = " "))


tab <- table(Crowd = cf_merged$class_manual,
             Dictionary = cf_merged$class)
tab

tab_print <- table(Crowd = cf_merged$class_manual_print,
                   Dictionary = cf_merged$class)


ConMat <- caret::confusionMatrix(tab)

ConMat
ConMat$byClass[, "F1"]

ConMat$byClass[, "Precision"]
ConMat$byClass[, "Recall"]

mat_f1 <- as.data.frame(ConMat$byClass[, "F1"]) %>% 
    rename(`F1 score` = `ConMat$byClass[, "F1"]`) 

mat_prec <- as.data.frame(ConMat$byClass[, "Precision"]) %>% 
    rename(`Precision` = `ConMat$byClass[, "Precision"]`) 

mat_rec <- as.data.frame(ConMat$byClass[, "Recall"]) %>% 
    rename(`Recall` = `ConMat$byClass[, "Recall"]`) 

mat_bind <- bind_cols(mat_f1, mat_prec, mat_rec)
mat_bind$Class <- rownames(mat_f1)

mat_bind <- mat_bind %>% 
    select(Class, everything())

## Table A1: Performance of deductively developed dictionary-based classification ----
    
xtable::print.xtable(xtable(tab_print,
                            caption = "Cross-table of dictionary-based classifier",
                            label="tab:classification"),
                     caption.placement = "top",
                     size = "footnotesize",
                     file = "R_figures_supporting_information/tab_a01.tex")


## Table A2: Performance of dictionary-based classification ----
xtable::print.xtable(xtable(mat_bind,
                            caption = "Performance of dictionary-based classification",
                            label="tab:performance_classification"),
                     caption.placement = "top",
                     include.rownames=FALSE,
                     size = "footnotesize",
                     file = "R_figures_supporting_information/tab_a02.tex")


## Table A3: Overview of terms used in dictionary ----

dat_promiserelated <- read_csv("data_tokens_promiserelated.csv")

nrow(dat_promiserelated)

pledge_dict <- dictionary(list(
    pledge_ongoing = dat_promiserelated$feature[1:75],
    pledge_broken_inductive = c("broken", 
                                "broke", 
                                "failed",
                                "not_fulfilled",
                                "not_been_fulfilled",
                                "not_implemented",
                                "not_been_implemented",
                                "not_been_fulfilled",
                                "not_delivered",
                                "not_been_delivered",
                                "not_kept",
                                "not_been_kept"),
    pledge_fulfilled_inductive = c("fulfilled",
                                   "kept",
                                   "implemented",
                                   "delivered",
                                   "not_break",
                                   "not_broken",
                                   "not_been_broken",
                                   "not_fail*"),
    pledge_broken_deductive = c("broken", #"break*", 
                                "fail*_to", 
                                "broke",
                                "failure",
                                "not_fulfil*",
                                "not_keep*",
                                "not_kept"),
    pledge_fulfilled_deductive = c("fulfil*",
                                   "not_break*",
                                   "not_broken",
                                   "kept")))


pledge_dict_raw <- dictionary(
    list(pledge_core = list(pledge = c("pledge*"),
                            promise = c("promise*")),
         pledge_alt = list(guarantee = c("guarantee*"),
                           assure = c("assure*"),
                           ensure = c("ensur*"))))


dat_words_pledge <- data.frame(
    Category = c("Pledge"),
    Terms = c(as.character(c(pledge_dict_raw$pledge_core,
                             pledge_dict_raw$pledge_alt)))
)

dat_words_fulfilled_in <- data.frame(
    Category = c("Fulfilled (inductive)"),
    Terms = c(as.character(c(pledge_dict$pledge_fulfilled_inductive)))
)

dat_words_broken_in <- data.frame(
    Category = c("Broken (inductive)"),
    Terms = c(as.character(c(pledge_dict$pledge_broken_inductive)))
)


dat_words_fulfilled_de <- data.frame(
    Category = c("Fulfilled (deductive)"),
    Terms = c(as.character(c(pledge_dict$pledge_fulfilled_deductive)))
)

dat_words_broken_de <- data.frame(
    Category = c("Broken (deductive)"),
    Terms = c(as.character(c(pledge_dict$pledge_broken_deductive)))
)

dat_words_ongoing <- data.frame(
    Category = c("Ongoing"),
    Terms = c(as.character(c(pledge_dict$pledge_ongoing)))
)


dat_words <- bind_rows(
    dat_words_pledge,
    dat_words_broken_de,
    dat_words_broken_in,
    dat_words_fulfilled_de,
    dat_words_fulfilled_in,
    dat_words_ongoing
)

dat_words <- dat_words %>% 
    arrange(Category, Terms) %>% 
    mutate(Terms = gsub('list\\("', '', Terms)) %>% 
    mutate(Terms = gsub('"\\)', '', Terms)) 

dat_words_wide <- dat_words %>% 
    group_by(Category) %>% 
    summarise(Terms = paste(Terms, collapse = "; ")) %>% 
    ungroup() %>% 
    mutate(Terms = str_replace_all(Terms, "_", " ")) %>% 
    arrange(rev(Category))



addtorow <-  list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste("\\hline \n",
                            "\\endhead \n",
                            "\\hline \n",
                            "\\endfoot \n",
                            "\\endlastfoot \n", sep = ""))

# Table A3
xtable::print.xtable(xtable(dat_words_wide, 
                            caption="Words used for dictionary classification",
                            label="tab:words_dictionary",
                            tabular.environment='longtable',
                            align= c("p{0.2\\textwidth}", 
                                     "p{0.2\\textwidth}",
                                     "p{0.7\\textwidth}")),
                     type="latex",
                     format.args = list(big.mark = ","),
                     size = "footnotesize",
                     file="R_figures_supporting_information/tab_a03.tex",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")


## Figure A5: Network plots of word co-occurrences for sentences classified as 'broken promise' ----

# exclude sentences from "other" class
dat_complete_no_other <- filter(dat_complete, class_inductive != "Other")

# create quanteda text corpus
corp_no_other <- corpus(dat_complete_no_other, text_field = "sentence")

countrynames <- unique(dat_complete_no_other$countryname)

# loop to plot feature co-occurrence matrix per country
for (i in countrynames){
    # construct the feature co-occurrence matrix
    
    cat(paste("Analyse", i, "\n"))
    
    fcmat_broken_country <- corp_no_other %>% 
        corpus_subset(countryname == i) %>% 
        # corpus_sample(size = 10000) %>% 
        tokens(remove_punct = TRUE) %>% 
        tokens_tolower() %>%
        tokens_compound(pattern = phrase("not been *")) %>% 
        tokens_compound(pattern = "not *") %>% 
        tokens_remove(c(stopwords("english"), "not_to"), padding = FALSE) %>% 
        tokens_keep(pattern = pledge_dict$pledge_broken_inductive, window = 5) %>% 
        fcm(context = "window", window = 3, tri = FALSE)
    
    # choose 30 most frequency features
    topfeats_broken_country <- names(topfeatures(fcmat_broken_country, 30))
    
    # select the top 30 features only, plot the network
    set.seed(23)
    textplot_network(fcm_select(fcmat_broken_country, topfeats_broken_country), 
                     min_freq = 0.6, vertex_labelcolor = "black")
    
    i_save <- str_to_lower(str_replace_all(i, " ", "_"))
    ggsave(paste0("R_figures_supporting_information/fig_a05_", i_save, ".pdf"), width = 5, height = 3)
}


## Figure A6: Network plots of word co-occurrences for sentences classified as 'fulfilled promise' ----

# loop to plot feature co-occurrence matrix per country
for (i in countrynames){
    # construct the feature co-occurrence matrix
    
    cat(paste("Analyse", i, "\n"))
    
    fcmat_fulfilled_country <- corp_no_other %>% 
        corpus_subset(countryname == i) %>% 
        #corpus_sample(size = 1000) %>% 
        tokens(remove_punct = TRUE) %>% 
        tokens_tolower() %>%
        tokens_compound(pattern = phrase("not been *")) %>% 
        tokens_compound(pattern = "not *") %>% 
        tokens_remove(c(stopwords("english"), "not_to"), padding = FALSE) %>% 
        tokens_keep(pattern = pledge_dict$pledge_fulfilled_inductive, window = 5) %>% 
        fcm(context = "window", window = 3, tri = FALSE)
    
    # choose 30 most frequency features
    topfeats_fulfilled_country <- names(topfeatures(fcmat_fulfilled_country, 30))
    
    # select the top 30 features only, plot the network
    set.seed(23)
    textplot_network(fcm_select(fcmat_fulfilled_country, topfeats_fulfilled_country),
                     min_freq = 0.6, vertex_labelcolor = "black")
    
    i_save <- str_to_lower(str_replace_all(i, " ", "_"))
    
    ggsave(paste0("R_figures_supporting_information/fig_a06_", i_save, ".pdf"), width = 5, height = 3)
}


## Figure A7: Network plots of word co-occurrences for sentences classified as 'ongoing promise' ----

# loop to plot feature co-occurrence matrix per country
for (i in countrynames){
    # construct the feature co-occurrence matrix
    
    cat(paste("Analyse", i, "\n"))
    
    fcmat_ongoing_country <- corp_no_other %>% 
        corpus_subset(countryname == i) %>% 
        # corpus_sample(size = 10000) %>% 
        tokens(remove_punct = TRUE) %>% 
        tokens_tolower() %>%
        tokens_compound(pattern = phrase("not been *")) %>% 
        tokens_compound(pattern = "not *") %>% 
        tokens_remove(c(stopwords("english"), "not_to"), padding = FALSE) %>% 
        tokens_remove(pattern = pledge_dict$pledge_fulfilled_inductive, padding = FALSE) %>%
        tokens_remove(pattern = pledge_dict$pledge_broken_inductive, padding = FALSE) %>% 
        tokens_keep(pattern = pledge_dict$pledge_ongoing, window = 5) %>% 
        fcm(context = "window", window = 3, tri = FALSE)
    
    # choose 30 most frequency features
    topfeats_ongoing_country <- names(topfeatures(fcmat_ongoing_country, 30))
    
    # select the top 30 features only, plot the network
    set.seed(23)
    textplot_network(fcm_select(fcmat_ongoing_country, topfeats_ongoing_country), 
                     min_freq = 0.6, vertex_labelcolor = "black")
    
    i_save <- str_to_lower(str_replace_all(i, " ", "_"))
    ggsave(paste0("R_figures_supporting_information/fig_a07_", i_save, ".pdf"), width = 5, height = 3)
}


## Table A4: Search query for retrieving articles about pledges mentioned in Thomson & Brandenburg (created manually) ----

## Figure A8: Articles about policies used in Thomson & Brandenburg (2019) that include pledge-related terms ----

## Figure A9: The ratio of articles classified as mentioning the breaking and fulfillment of a promise ----

## Note: Figures A8 and A9 require the full-texts to all articles which cannot be uploaded 
## at Dataverse for copyright reasons. 
## Please contact the author if you have specific queries.

## Figure A10: A manual inspection of the content of 'ongoing promises' ----

# get random sample for codings (don't run in replication script)
# dat_ongoing <- filter(dat_complete, class_inductive == "Ongoing")
# 
# nrow(dat_ongoing)
# 
# set.seed(34)
# dat_ongoing_sample <- dat_ongoing %>% 
#     group_by(countryname) %>% 
#     sample_n(size = 100)
# 
# nrow(dat_ongoing_sample)
# 
# table(dat_ongoing_sample$countryname)
# 
# dat_ongoing_sample_select <- dat_ongoing_sample %>% 
#     mutate(info_about_pledge = "",
#            info_broken_fulfilled = "",
#            info_broken_fulfilled_vague_concrete = "",
#            date,
#            ) %>% 
#     select(sentence, starts_with("info_"), date,
#            countryname, text_id, newspaper, sentence_pre, sentence_post)
# 
# #rio::export(dat_ongoing_sample_select, "data/data_sample_ongoing.xlsx")

# import coded dataset
dat_coded <- rio::import("data_sample_ongoing.xlsx")

dat_coded$info_about_pledge <- as.character(dat_coded$info_about_pledge)

# give coding categories nicer names
dat_coded <- dat_coded %>% 
    mutate(info_about_pledge_detail = car::recode(info_about_pledge,
                                                  "'political_promise'='Political promise (without information on breaking or fulfilment)';
                                           'not_related_to_politics'='Sentence not related to politics';
                                           'politics_but_no_pledge'='Sentence related to politics, but no promise mentioned'"))

# classify sentences
dat_coded <- dat_coded %>% 
    mutate(info_about_pledge_detail = ifelse(info_about_pledge == "political_promise" & 
                                                 info_broken_fulfilled == "broken", "Political promise (broken)",
                                             ifelse(info_about_pledge == "political_promise" & 
                                                        info_broken_fulfilled == "fulfilled", "Political promise (fulfilled)",
                                                    info_about_pledge_detail)))

dat_coded_sum <- dat_coded %>% 
    count(info_about_pledge_detail) %>%
    mutate(percentage = n / 400) %>% 
    mutate(pledge_dummy = ifelse(str_detect(info_about_pledge_detail, "Political promise"), TRUE, FALSE))


ggplot(dat_coded_sum, aes(x = info_about_pledge_detail,
                          y = percentage, fill = pledge_dummy)) +
    geom_bar(stat = "identity") +
    scale_fill_grey(start = 0.7, end = 0.3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Percent") +
    coord_flip() +
    theme(legend.position = "none")
ggsave("R_figures_supporting_information/fig_a10.pdf", 
       width = 10, height = 3)


## Figure A11: A manual inspection of the content of 'ongoing promises' (separately for each country) ----

# get counts per country
dat_coded_sum_country <- dat_coded %>% 
    group_by(countryname) %>% 
    count(info_about_pledge_detail)

# create dummy indicating whether sentence contains information on political promise
dat_coded_sum_country <- dat_coded_sum_country %>% 
    mutate(pledge_dummy = ifelse(str_detect(info_about_pledge_detail, "Political promise"), TRUE, FALSE))

ggplot(dat_coded_sum_country, aes(x = info_about_pledge_detail,
                                  y = n, fill = pledge_dummy)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), nudge_y = 3, colour = "grey50") +
    scale_fill_grey(start = 0.7, end = 0.3) +
    facet_wrap(~countryname, nrow = 4) +
    labs(x = NULL, y = "Frequency") +
    coord_flip() +
    theme(legend.position = "none")
ggsave("R_figures_supporting_information/fig_a11.pdf", width = 10, height = 8)

## Table A5: Number of relevant sentences for each category and country (inductive, sample of statements directly referring to politician or party) ----

tab_articles_country_inductive_party_mp <- dat_complete %>% 
    ungroup() %>% 
    filter(class_inductive != "Other") %>% 
    filter(!is.na(text_id) & text_id != "") %>% 
    filter(mention_party_mp_context == TRUE) %>% 
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


# code required to create LaTeX tables
addtorow <-  list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command <- c(paste("\\hline \n",
                            "\\endhead \n",
                            "\\hline \n",
                            "\\endfoot \n",
                            "\\endlastfoot \n", sep = ""))



# Table A5
xtable::print.xtable(xtable(tab_articles_country_inductive_party_mp, 
                            caption="Number of relevant sentences for each category and country (inductive, sample of statements directly referring to politician or party)",
                            label="tab:articles_inductive_partymp",
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
                     file="R_figures_supporting_information/tab_a05.tex",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")


## Table A6: Number of relevant sentences for each category and country (deductive, full sample) ----

tab_articles_country_deductive <- dat_complete %>% 
    ungroup() %>% 
    filter(class_deductive != "Other") %>% 
    filter(!is.na(text_id) & text_id != "") %>% 
    group_by(countryname, class_deductive) %>% 
    summarise(n_sentences_class = n()) %>% 
    spread(class_deductive, n_sentences_class) %>% 
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



xtable::print.xtable(xtable(tab_articles_country_deductive, 
                            caption="Number of relevant sentences for each category and country (deductive, full sample)",
                            label="tab:articles_deductive_full",
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
                     file="R_figures_supporting_information/tab_a06.tex",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")


## Table A7: Number of relevant sentences for each category and country (deductive, sample of statements directly referring to politician or party) ----

tab_articles_country_deductive_party_mp <- dat_complete %>% 
    ungroup() %>% 
    filter(class_deductive != "Other") %>% 
    filter(!is.na(text_id) & text_id != "") %>% 
    filter(mention_party_mp_context == TRUE) %>% 
    group_by(countryname, class_deductive) %>% 
    summarise(n_sentences_class = n()) %>% 
    spread(class_deductive, n_sentences_class) %>% 
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


# Table A7
xtable::print.xtable(xtable(tab_articles_country_deductive_party_mp, 
                            caption="Number of relevant sentences for each category and country (deductive, sample of statements directly referring to politician or party)",
                            label="tab:articles_deductive_partymp",
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
                     file="R_figures_supporting_information/tab_a07.tex",
                     include.rownames=FALSE,
                     floating = FALSE,
                     tabular.environment='longtable',
                     sanitize.text.function=function(x){x},
                     add.to.row = addtorow,
                     hline.after = c(-1),
                     caption.placement="top")



## Table A8: Predicting the number of articles on promises published throughout the electoral cycle ----

## Note: Table A8 is part of the appendix, but all models are estimated to 
## construct Figure 3. 
## Therefore, the Table A8 is part of the script for the reproduction of 
## the analyses reported in the paper. 


## Figure A12: The daily proportion of pledge-related statements throughout the electoral cycle under single-party governments and multiparty cabinets ----


# get daily counts of newspaper articles for each class (using the inductive classification)
dat_timeseries_day_count <- dat_timeseries %>% 
    mutate(class_inductive = car::recode(class_inductive, "'Broken/Fulfilled'='Ongoing'")) %>% 
    group_by(countryname, class_inductive, date) %>% 
    count() %>% 
    summarise(n_articles_day = n - 1) # subtract 1 from each day to set day-class observations without coverage to 0

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

# same number of days in each class?
table(dat_timeseries_day_count$class_inductive)

# merge metadata to this data frame
dat_timeseries_day <- dat_timeseries_day_count %>% 
    left_join(dat_metadata, by = c("countryname", "date"))


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

ggplot(dat_timeseries_day_full, aes(x = electoral_cycle,
                                    y = n_articles_prop)) +
    geom_smooth(colour = "black") +
    facet_grid(gov_type~class_inductive, scales = "free") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(NA,NA)) +
    labs(x = "Electoral cycle",
         y = "Daily coverage (standardized by cycle)")
ggsave("R_figures_supporting_information/fig_a12.pdf", width = 10, height = 6)


## Figure A13: The daily proportion of pledge-related statements throughout the electoral cycle under single-party governments and multiparty cabinets (with a different measurement of cabinets in Australia) ----

# repeat plot for different coding of Australian government
table(dat_timeseries_day_full$gov_type_aus_single, useNA = "always")

ggplot(dat_timeseries_day_full, aes(x = electoral_cycle,
                                    y = n_articles_prop)) +
    geom_smooth(colour = "black") +
    facet_grid(gov_type_aus_single~class_inductive, scales = "free") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(NA,NA)) +
    labs(x = "Electoral cycle",
         y = "Daily coverage (standardized by cycle)")
ggsave("R_figures_supporting_information/fig_a13.pdf", width = 10, height = 6)


## Figure A14: -----


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


# create factor variables (necessary for estimating the expected values)
dat_quarter_classes_joined$countryname <- factor(dat_quarter_classes_joined$countryname)
dat_quarter_classes_joined$gov_type <- factor(dat_quarter_classes_joined$gov_type)
dat_quarter_classes_joined$gov_type_aus_single <- factor(dat_quarter_classes_joined$gov_type_aus_single)
dat_quarter_classes_joined$tabloid_broadsheet <- factor(dat_quarter_classes_joined$tabloid_broadsheet)

# run regression model 
lme_1_inductive <- lmer(broken_divided_by_fulfilled_in ~ 
                            tabloid_broadsheet +
                            countryname + 
                            log_gdp_change_lag + 
                            poll_change_to_election + 
                            year + 
                            gov_type + 
                            (1 | newspaper) + (1 | country_cycle),
                        data = dat_quarter_classes_joined)


# get expected values
effect_braodsheet_tabloid_in <- as.data.frame(
    Effect(c("tabloid_broadsheet"), 
           lme_1_inductive))

ggplot(effect_braodsheet_tabloid_in, 
       aes(x = tabloid_broadsheet,
           y = fit,
           ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red", size = 0.8) +
    geom_pointrange(size = 0.8) +
    annotate("text", x = 1.2, y = 1.4, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 1.2, y = 0.6, label = "Fulfilled promises", size = 4,
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
    xlab(NULL)
ggsave(filename = "R_figures_supporting_information/fig_a14.pdf", 
       width = 5, height = 4.5)


## Figure A15: Predicting the ratio of reports on broken to fulfilled promises per quarter and newspaper ----

ggplot(dat_quarter_classes_inductive,
       aes(x = year, 
           y = broken_divided_by_fulfilled_in,
           colour = tabloid_broadsheet,
           linetype = tabloid_broadsheet)) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "red", size = 0.8) +
    geom_smooth(method = "loess") +
    scale_colour_manual(values = c("black", "blue")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    facet_wrap(~countryname, nrow = 1) +
    labs(x = NULL, 
         y = "Ratio of broken to fulfilled promises\n(and 95% CIs)") +
    theme(legend.title = element_blank())
ggsave("R_figures_supporting_information/fig_a15.pdf", 
       width = 10,
       height = 5)

## Figure A16: The proportion of fully or partially fulfilled pledges by government and opposition parties ----

## Note: This Figure requires the replication data from Thomson et al. (2017) which can be downloaded here:
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YJUIBI
##
## The plot simply summarises the proportion of fulfilled pledges by governments and opposition parties 
## for each year. If you would like to have the code to reproduce this plot, get in contact with the author. 


## Figure A17: The percentage of statements from broadsheet papers relative to tabloids ----

dat_complete <- dat_complete %>% 
    mutate(prop_broadsheet = case_when(
        tabloid_broadsheet == "Broadsheet" ~ 1,
        tabloid_broadsheet == "Tabloid" ~ 0
    )) 


set.seed(123)
dat_broadsheet_tabloid <- dat_complete %>% 
    group_by(countryname, year) %>% 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$prop_broadsheet))))

means_broadsheet <- dat_complete %>% 
    group_by(countryname) %>% 
    summarise(mean_broadsheet = mean(prop_broadsheet))

dat_broadsheet_tabloid <- left_join(dat_broadsheet_tabloid, means_broadsheet) %>% 
    mutate(countryname_mean = paste0(countryname, "\n(Broadsheet: ", round(mean_broadsheet * 100, 1), "%)"))

ggplot(dat_broadsheet_tabloid, aes(x = year, 
                                   y = Mean,
                                   ymin = Lower,
                                   max = Upper)) +
    geom_pointrange(alpha = 0.4) +
    geom_smooth(se = FALSE) +
    facet_wrap(~countryname_mean, nrow = 1) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Percentage of observations\nfrom broadsheet papers")
ggsave("R_figures_supporting_information/fig_a17.pdf", width = 10, height = 5)  


## Table A9: Predicting the ratio of broken to fulfilled promises by quarter and newspaper based on the deductively developed dictionary ----

lme_1_deductive <- update(lme_1_inductive, 
                          broken_divided_by_fulfilled_de ~ .,
                          data = dat_quarter_classes_joined)


lme_1_deductive_log <- lmer(log_broken_divided_by_fulfilled_de ~ 
                                tabloid_broadsheet +
                                countryname + 
                                log_gdp_change_lag + 
                                poll_change_to_election + 
                                year + 
                                gov_type + 
                                (1 | newspaper) + (1 | country_cycle),
                            data = dat_quarter_classes_joined)

# Table A9
texreg(list(lme_1_deductive, lme_1_deductive_log),
       include.variance = FALSE,
       custom.gof.names = c("AIC", "BIC", "Log likelihood",
                            "N", "N (Cycles)", 
                            "N (Newspapers)"),
       file = "R_figures_supporting_information/tab_a09.tex",
       label = "tab:reg_brokenfulfilld_deductive",
       fontsize = "footnotesize",
       omit.coef = c("(Intercept)"), 
       caption.above =  TRUE,
       custom.coef.names = c(
           "Newspaper: Tabloid", 
           "Canada", "Ireland", "United Kingdom",
           "Lagged GDP change (log)",
           "Poll change to previous election",
           "Year", 
           "Gov. type: Single-party government"),
       custom.model.names = c("M1: Full sample", "M2: Logged DV"),
       caption = "Predicting the ratio of broken to fulfilled promises by quarter and newspaper 
       based on the deductively developed dictionary",
       float.pos = "!h",
       custom.note = ("\\parbox{.7\\linewidth}{\\footnotesize \\vspace{2pt}%stars. \\\\
       \\textit{Note}: Model 1 uses the full sample of quarters with at least one sentence on broken and one sentence on
       fulfilled promises.  Model 2 uses a logged dependent variable of the ratio. 
       Models include random intercepts for each newspaper and cycle. Intercepts omitted from table. Standard errors in parentheses.}"))


## Figure A18: Predicting the ratio of reports on broken to fulfilled promises per quarter and newspaper using a deductively developed dictionary ----


## Figure A18 (a) Expected values for each country

effect_country_de <- as.data.frame(
    Effect(c("countryname"), 
           lme_1_deductive))

effect_country_de <- effect_country_de %>% 
    mutate(countryname = car::recode(countryname, "'United Kingdom'='UK'"))

ggplot(effect_country_de, 
       aes(x = countryname,
           y = fit,
           ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red", size = 0.8) +
    geom_pointrange(size = 0.8) +
    annotate("text", x = 1.3, y = 1.4, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 1.3, y = 0.6, label = "Fulfilled promises", size = 4,
             color = "grey30") +
    annotate("segment", x = 0.6, xend = 0.6, y = 1.2, yend = 1.7, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    annotate("segment", x = 0.6, xend = 0.6, y = 0.8, yend = 0.3, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    scale_y_continuous(limits = c(0, 4)) + 
    ylab("Ratio of broken to fulfilled promises") +
    xlab(NULL)
ggsave(filename = "R_figures_supporting_information/fig_a18_a.pdf", 
       width = 5, height = 4.5)


## Figure A18 (b) Expected values over time

effect_year_de <- as.data.frame(
    Effect(c("year"), 
           lme_1_deductive, 
           xlevels = 150))

ggplot(data = effect_year_de,
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
    xlab("Year") +
    annotate("text", x = 2008, y = 1.4, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 2008, y = 0.6, label = "Fulfilled promises", size = 4,
             color = "grey30") +
    annotate("segment", x = 2016, xend = 2016, y = 1.2, yend = 1.7, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    annotate("segment", x = 2016, xend = 2016, y = 0.8, yend = 0.3, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm")))
ggsave(filename = "R_figures_supporting_information/fig_a18_b.pdf", 
       width = 5, height = 4.5)

## Figure A19: Comparing the focus on broken and fulfilled promises, using two different dictionaries ----

cors_in_de_country <- dat_quarter_classes_joined %>% 
    group_by(countryname) %>% 
    summarise(cor = cor(log_broken_divided_by_fulfilled_in,
                        log_broken_divided_by_fulfilled_de, 
                        use = "pairwise.complete")) %>% 
    mutate(cor = paste0("R=", round(cor, 2)))

ggplot(dat_quarter_classes_joined, 
       aes(x = log_broken_divided_by_fulfilled_in, 
           y = log_broken_divided_by_fulfilled_de)) +
    geom_point(alpha = 0.2) + 
    geom_smooth(method = "lm") +
    facet_wrap(~countryname) +
    geom_text(data = cors_in_de_country,
              mapping = aes(x = -1.5, y = 2, label = cor),
              size = 5,
              colour = "grey30",
              hjust   = -0.1,
              vjust   = -1) +
    xlab(expression(paste("Inductive dictionary: ", log, bgroup("(", frac(Broken + 0.5, Fulfilled + 0.5), ")")))) +
    ylab(expression(paste("Deductive dictionary: ", log, bgroup("(", frac(Broken + 0.5, Fulfilled + 0.5), ")"))))
ggsave("R_figures_supporting_information/fig_a19.pdf", width = 10, height = 8)

## Figure A20: Predicting the ratio of reports on broken and fulfilled promises per quarter and newspaper using a logged measure of the ratio of broken to fulfilled promises ----

lme_1_inductive_log <- lmer(log_broken_divided_by_fulfilled_in ~ 
                                tabloid_broadsheet +
                                countryname + 
                                log_gdp_change_lag + 
                                poll_change_to_election + 
                                year + 
                                gov_type + 
                                (1 | newspaper) + (1 | country_cycle),
                            data = dat_quarter_classes_joined)


effect_country_in_log <- as.data.frame(
    Effect(c("countryname"), 
           lme_1_inductive_log))

effect_country_in_log <- effect_country_in_log %>% 
    mutate(countryname = car::recode(countryname, "'United Kingdom'='UK'"))

ggplot(effect_country_in_log, 
       aes(x = countryname,
           y = fit,
           ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red", size = 0.8) +
    geom_pointrange(size = 0.8) +
    annotate("text", x = 1.5, y = -0.1, label = "Broken promises", size = 4,
             color = "grey30") +
    annotate("text", x = 1.5, y = 0.1, label = "Fulfilled promises", size = 4,
             color = "grey30") +
    annotate("segment", x = 0.75, xend = 0.75, y = .1, yend = 0.4, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    annotate("segment", x = 0.75, xend = 0.75, y = -0.1, yend = -0.4, colour = "grey40", 
             size = 0.5, 
             arrow = arrow(angle = 25, length = unit(0.3, "cm"))) +
    scale_y_continuous(limits = c(-0.5, 1)) + 
    ylab("Logged ratio of\nbroken to fulfilled promises") +
    xlab(NULL)
ggsave(filename = "R_figures_supporting_information/fig_a20.pdf", 
       width = 5, height = 4.5)


## Table A10: Predicting sentiment in sentences about political promises ----

dat_complete$countryname <- factor(dat_complete$countryname)
dat_complete$tabloid_broadsheet <- factor(dat_complete$tabloid_broadsheet)

dat_sent_inductive <- dat_complete %>% 
    filter(class_inductive != "Other") %>% 
    mutate(class_ongoing_dummy = ifelse(class_inductive == "Ongoing", "1_Ongoing (baseline)", "2_Broken or Fulfilled"))

dat_sent_inductive$class_ongoing_dummy <- factor(dat_sent_inductive$class_ongoing_dummy)
dat_sent_inductive$gov_type <- factor(dat_sent_inductive$gov_type)

dat_sent_deductive <- dat_complete %>% 
    filter(class_deductive != "Other") %>% 
    mutate(class_ongoing_dummy = ifelse(class_deductive == "Ongoing", "1_Ongoing (baseline)", "2_Broken or Fulfilled"))

dat_sent_deductive$class_ongoing_dummy <- factor(dat_sent_deductive$class_ongoing_dummy)
dat_sent_deductive$gov_type <- factor(dat_sent_deductive$gov_type)

lmer_sent_inductive <- lmer(
    sentiment ~ class_ongoing_dummy + 
        tabloid_broadsheet +
        log_gdp_change_lag + 
        poll_change_to_election +
        year + 
        gov_type +
        (1 | newspaper) + (1 | country_cycle),
    data = dat_sent_inductive)

sd(dat_complete$sentiment)

# estimate effect of coefficient of 0.62
0.62 /sd(dat_complete$sentiment)

lmer_sent_deductive <- lmer(
    sentiment ~ class_ongoing_dummy + 
        tabloid_broadsheet +
        log_gdp_change_lag + 
        poll_change_to_election + 
        year + 
        gov_type + 
        (1 | newspaper) +
        (1 | country_cycle),
    data = dat_sent_deductive)

screenreg(list(lmer_sent_inductive, lmer_sent_deductive),
          include.variance=FALSE)

texreg(list(lmer_sent_inductive, lmer_sent_deductive),
       include.variance = FALSE,
       custom.gof.names = c("AIC", "BIC", "Log likelihood",
                            "N", "N (Cycles)", 
                            "N (Newspapers)"),
       file = "R_figures_supporting_information/tab_a10.tex",
       label = "tab:reg_sentiment",
       fontsize = "footnotesize",
       caption.above =  TRUE,
       custom.coef.names = c(
           "(Intercept)",
           "Class: Broken or Fulfilled",
           "Newspaper: Tabloid", 
           "Lagged GDP change (log)",
           "Poll change to previous election",
           "Year",
           "Gov. type: Single-party government"),
       custom.model.names = c("M1: Inductive", "M2: Deductive"),
       caption = "Predicting sentiment in sentences about political promises",
       float.pos = "!h",
       custom.note = ("\\parbox{.7\\linewidth}{\\footnotesize \\vspace{2pt}%stars. \\\\
       \\textit{Note}: Model 1 uses the inductive classification of fulfilled, broken, and ongoing promises, 
                      Model 2 the deductive classification.  
                      Both models include random intercepts for each cycle and newspaper. 
                      Standard errors in parentheses.}"))


## Figure A21: The development of sentiment in statements on ongoing promises and on broken and fulfilled promises over time ----

dat_sent_quarter_year <- dat_sent_inductive %>% 
    group_by(countryname, country_cycle, newspaper, year, tabloid_broadsheet, class_ongoing_dummy) %>% 
    summarise(sentiment_mean = mean(sentiment))

ggplot(dat_sent_quarter_year, aes(x = year, y = sentiment_mean,
                                  colour = class_ongoing_dummy,
                                  linetype = class_ongoing_dummy)) +
    geom_smooth(method = "loess") +
    scale_colour_manual(values = c("darkgreen", "darkred"),
                        labels = c("Ongoing (baseline)", "Broken and Fulfilled")) +
    scale_linetype_manual(values = c("dotted", "solid"),
                          labels = c("Ongoing (baseline)", "Broken and Fulfilled")) +
    facet_grid(countryname~tabloid_broadsheet) +
    annotate("text", label = "Positive sentiment", size = 4, x = 1985, y = 0.5, colour = "grey30") +
    annotate("text", label = "Negative sentiment", size = 4, x = 1985, y = -1, colour = "grey30") +
    labs(x = NULL, y = "Sentiment (aggregated by year and newspaper)") +
    theme(legend.title = element_blank())
ggsave("R_figures_supporting_information/fig_a21.pdf", width = 10, height = 12)


## Figure A22: Keyness plots with important terms for sentences classified as fulfilled and broken ----

dat_brokenfulfilled_in <- dat_complete %>% 
    filter(class_inductive %in% c("Fulfilled", "Broken")) 

dfmat_brokenfulfilled <- dat_brokenfulfilled_in %>% 
    corpus(text_field = "sentence") %>% 
    tokens(remove_punct = TRUE) %>% 
    tokens_compound(pattern = phrase("not been *")) %>% 
    tokens_compound(pattern = "not *") %>% 
    tokens_remove(c(stopwords("english"), "not_to"), padding = FALSE) %>% 
    tokens_remove(pledge_dict$pledge_fulfilled_inductive) %>% 
    tokens_remove(pledge_dict$pledge_broken_inductive) %>% 
    tokens_remove(pledge_dict$pledge_ongoing) %>% 
    tokens_remove(pledge_dict_raw$pledge_core) %>% 
    tokens_remove(pledge_dict_raw$pledge_alt) %>% 
    tokens_remove(pattern = '^[A-Z]', 
                  valuetype = 'regex', 
                  case_insensitive = FALSE) %>% # remove names and entities
    dfm(remove_numbers = TRUE) %>% 
    dfm_select(min_nchar = 3)


# do same for each country
countries <- unique(dat_complete$countryname)


library(quanteda.textstats)
?quanteda

example("textstat_keyness")

help(package = "quanteda")

for (i in countries) {
    
    dfmat_brokenfulfilled_country <- dfmat_brokenfulfilled %>%  
        dfm_subset(countryname == i) %>% 
        dfm_group(groups = class_inductive) # Removed quotes
    
    tstat_key_brokenfulfilled_country <- dfmat_brokenfulfilled_country %>% 
      textstat_keyness(target = "Fulfilled") 
    
    textplot_keyness(tstat_key_brokenfulfilled_country, n = 15, 
                     labelcolor = "black",
                     labelsize = 5, margin = 0.2,
                     color = c("darkgreen", "darkred")) + 
        theme_baser() +
        labs(x = expression("Chi"^2)) +
        theme(axis.line = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.background = element_blank(),
              legend.position = c(0.2, 0.8))
    
    i_save <- str_to_lower(str_replace_all(i, " ", "_"))
    ggsave(paste0("R_figures_supporting_information/fig_a22_", i_save, "_brokenfulfilled.pdf"), 
           width = 5, height = 7)
}

# Issues with quanteda due to the breakup of the package.




