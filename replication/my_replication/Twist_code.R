
# Twist code

library(AER)
library(MASS)
library(AER)
library(pscl)

dat_timeseries_day_single <- read.csv("dat_timeseries_day_single.csv")

dat_timeseries_day_multi <- read.csv("dat_timeseries_day_multi.csv")


# Poisson regression

# single-party government

poisson_single_ongoing <- glm(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3) +
    log_gdp_change_lag + 
    poll_change_to_election +
    country_cycle,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Ongoing"), family = "poisson")

dispersiontest(poisson_single_ongoing)

poisson_single_broken <- update(
  poisson_single_ongoing, . ~ .,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Broken"))

dispersiontest(poisson_single_broken)

poisson_single_fulfilled <- update(
  poisson_single_ongoing, . ~ .,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Fulfilled"))

dispersiontest(poisson_single_fulfilled)


# multiparty government

poisson_multi_ongoing <- update(
  poisson_single_ongoing, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Ongoing"))

dispersiontest(poisson_multi_ongoing)


poisson_multi_fulfilled <- update(
  poisson_single_ongoing, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Fulfilled"))

dispersiontest(poisson_multi_fulfilled)

poisson_multi_broken <- update(
  poisson_single_ongoing, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Broken"))

dispersiontest(poisson_multi_broken)

lrtest(poisson_single_ongoing, nb_single_ongoing)

############################################################

# plotting residuals, poisson vs nb, ongoing promises

# Set up the plotting area for a 2x2 layout
par(mfrow=c(2, 2), mar=c(2, 2, 2, 2))

# Plot 1: Poisson (Multi Ongoing)
poisson_resid_mo <- resid(poisson_multi_ongoing)
plot(fitted(poisson_multi_ongoing), 
     poisson_resid_mo, col = "blue", pch = 10,
     xlab = "Number of articles", ylab = "Residuals", 
     main = "Poisson (Multi Ongoing)")
abline(0, 0)

# Plot 2: Negative Binomial (Multi Ongoing)
nb_resid_mo <- resid(nb_multi_ongoing)
plot(fitted(nb_multi_ongoing), 
     nb_resid_mo, col = "blue", pch = 10,
     xlab = "Number of articles", ylab = "Residuals", 
     main = "Negative Binomial (Multi Ongoing)")
abline(0, 0)

poisson_resid_so <- resid(poisson_single_ongoing)
plot(fitted(poisson_single_ongoing), 
     poisson_resid_so, col = "blue", pch = 10,
     xlab = "Number of articles", ylab = "Residuals", 
     main = "Poisson (Single Ongoing)")
abline(0, 0)

# Plot 4: Negative Binomial (Single Ongoing)
nb_resid_so <- resid(nb_single_ongoing)
plot(fitted(nb_single_ongoing),
     nb_resid_so, col = "blue", pch = 10,
     xlab = "Number of articles", ylab = "Residuals", 
     main = "Negative Binomial (Single Ongoing)")
abline(0, 0)

# Reset plotting layout to default
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

############################################################

# Zero inflated negative binomial


# Fitting a Zero-Inflated Negative Binomial models

zinb_single_ongoing <- zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_single,
                class_inductive == "Ongoing"), 
  dist = "negbin")


AIC(nb_single_ongoing)
AIC(zinb_single_ongoing)

BIC(nb_single_ongoing)
BIC(zinb_single_ongoing)


# single_ongoing

zinb_single_fulfilled <-  zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_single,
                class_inductive == "Fulfilled"), 
  dist = "negbin")

AIC(nb_single_fulfilled)
AIC(zinb_single_fulfilled)

BIC(nb_single_fulfilled)
BIC(zinb_single_fulfilled)

# single_broken

zinb_single_broken <- zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_single,
                class_inductive == "Broken"), 
  dist = "negbin")

AIC(nb_single_broken)
AIC(zinb_single_broken)

BIC(nb_single_broken)
BIC(zinb_single_broken)


# multi_ongoing Zero Inflation Model

zinb_multi_ongoing <- zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Ongoing"), 
  dist = "negbin")

AIC(nb_multi_ongoing)
AIC(zinb_multi_ongoing)

BIC(nb_multi_ongoing)
BIC(zinb_multi_ongoing)



# multi_fulfilled Zero Inflated Model

# Fit a Zero-Inflated Negative Binomial model
zinb_multi_fulfilled <- zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Fulfilled"), 
  dist = "negbin")

AIC(nb_multi_fulfilled)
AIC(zinb_multi_fulfilled)

BIC(nb_multi_fulfilled)
BIC(zinb_multi_fulfilled)



# multi_broken Zero Inflated Model

zinb_multi_broken <-  zeroinfl(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3)+
    log_gdp_change_lag + 
    poll_change_to_election + 
    country_cycle | 1, 
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Broken"), 
  dist = "negbin")

AIC(nb_multi_broken)
AIC(zinb_multi_broken)

BIC(nb_multi_broken)
BIC(zinb_multi_broken)


###########################################################

# No polynomials

nb_single_ongoing_np <- MASS::glm.nb(
  n_articles_day ~ 
    n_articles_day_lag + 
    poly(electoral_cycle, 3) +
    log_gdp_change_lag + 
    poll_change_to_election +
    country_cycle,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Ongoing"))


nb_single_broken_np <- update(
  nb_single_ongoing_np, . ~ .,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Broken"))


nb_single_fulfilled_np <- update(
  nb_single_ongoing_np, . ~ .,
  data = filter(dat_timeseries_day_single,
                class_inductive == "Fulfilled"))


# multiparty government

nb_multi_ongoing_np <- update(
  nb_single_ongoing_np, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Ongoing"))

nb_multi_fulfilled_np <- update(
  nb_single_ongoing_np, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Fulfilled"))

nb_multi_broken_np <- update(
  nb_single_ongoing_np, . ~ .,
  data = filter(dat_timeseries_day_multi,
                class_inductive == "Broken"))

# get data frames with predicted counts
effect_single_ongoing_np <- Effect(c("electoral_cycle"), 
                                   nb_single_ongoing_np, 
                                   xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Ongoing") %>% 
  mutate(type = "Single-party government")


effect_single_broken_np <- Effect(c("electoral_cycle"), 
                                  nb_single_broken_np, 
                                  xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Broken") %>% 
  mutate(type = "Single-party government")


effect_single_fulfilled_np <- Effect(c("electoral_cycle"), 
                                     nb_single_fulfilled_np, 
                                     xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Fulfilled") %>% 
  mutate(type = "Single-party government")


effect_multi_ongoing_np <- Effect(c("electoral_cycle"), 
                                  nb_multi_ongoing_np, 
                                  xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Ongoing") %>% 
  mutate(type = "Multiparty government")


effect_multi_broken_np <- Effect(c("electoral_cycle"), 
                                 nb_multi_broken_np, 
                                 xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Broken") %>% 
  mutate(type = "Multiparty government")


effect_multi_fulfilled_np <- Effect(c("electoral_cycle"), 
                                    nb_multi_fulfilled_np, 
                                    xlevels = 150) %>% 
  as.data.frame() %>% 
  mutate(model = "Fulfilled") %>% 
  mutate(type = "Multiparty government")


# bind all data frames for plotting
effect_single_multi_np <- bind_rows(effect_single_fulfilled_np,
                                    effect_single_broken_np,
                                    effect_single_ongoing_np,
                                    effect_multi_fulfilled_np,
                                    effect_multi_broken_np,
                                    effect_multi_ongoing_np)




effect_single_multi_np$type <- factor(effect_single_multi_np$type,
                                      levels = c("Single-party government", 
                                                 "Multiparty government"))

effect_single_multi_np$facet <- paste0(effect_single_multi_np$type, "\n",
                                       effect_single_multi_np$model)

ggplot(data = effect_single_multi_np,
       aes(x = electoral_cycle, y = fit,
           ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.4, fill = "grey60") + 
  facet_wrap(~facet, scales = "free", nrow = 2) +
  geom_line() + 
  labs(x = "Electoral cycle", y = "Number of articles per day") +
  theme(legend.position = "none")
ggsave("R_figures/no_polynomial.png", width = 10, height = 7)

#############################################################

# Likelihood ratio test, polynomial vs non-polynomial models

library(lmtest)

lmtest::lrtest(nb_single_ongoing_np, nb_single_ongoing)

lrtest(nb_single_broken_np, nb_single_broken)

lrtest(nb_single_fulfilled_np, nb_single_fulfilled)


lrtest(nb_multi_ongoing_np, nb_multi_ongoing)

lrtest(nb_multi_broken_np, nb_multi_broken)

lrtest(nb_multi_fulfilled_np, nb_multi_fulfilled)

#############################################################




















