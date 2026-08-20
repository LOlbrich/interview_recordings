# Title: analysis of panel data

# run setup file ---------------------------------------------------------------

source("code/setup.R")


## function to extract fixest coefficients -------------------------------------

get_coefs_capi <- function(fit){
  as_tibble(coeftable(fit, cluster = ~pnr)) %>%
    mutate(
      lower = confint(fit, cluster = ~pnr)[,1],
      upper = confint(fit, cluster = ~pnr)[,2],
      time_to_treat = c(7, 8, 10)
    ) %>%
    rename(
      estimate = Estimate,
      se = "Std. Error",
      t_value = "t value",
      p_value = "Pr(>|t|)"
    ) %>%
    add_row(
      estimate = 0, time_to_treat = 9
    )
}

# load data --------------------------------------------------------------------

# full pass panel
pass_panel <- readRDS("Daten/processed/pass_panel.rds")

# pass social participation
pass_social <- readRDS("Daten/processed/pass_social.rds")

# pass life satisfaction
pass_sat <- readRDS("Daten/processed/pass_sat.rds")

# exclude durations exceeding 100 seconds per item
df_social <- pass_social %>%
  filter(d_social <= 2*100) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

df_sat <- pass_sat %>%
  filter(d_satisfaction <= 100) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()


# fixed effects analysis -------------------------------------------------------

## fit models ----

# fit models
fit_sat <- fixest::feols(log_satisfaction ~ i(time_to_treat, 9) | pnr + welle, df_sat)
fit_social <- fixest::feols(log_social ~ i(time_to_treat, 9)  | pnr + welle, df_social)

## print model summary
summary(fit_sat)
summary(fit_social)

# calculate exact relative change
exp(fit_sat$coefficients) - 1
exp(fit_social$coefficients) - 1

# calculate change in seconds
predict_fit_sat <- df_sat %>%
  mutate(
    predict_dur = predict(fit_sat)
  ) %>%
  group_by(welle, treatment) %>%
  summarize(
    exp(mean(predict_dur) + 0.5*sigma(fit_sat)^2)
  )
predict_fit_sat


# calculate change in seconds
predict_fit_social <- df_social %>%
  mutate(
    predict_dur = predict(fit_social)
  ) %>%
  group_by(welle, treatment) %>%
  summarize(
    exp(mean(predict_dur) + 0.5*sigma(fit_social)^2)
  )

predict_fit_social


# extract results
results <- bind_rows(
  get_coefs_capi(fit_sat) %>% mutate(variable = "Life satisfaction", depvar = "ln(duration)"),
  get_coefs_capi(fit_social) %>% mutate(variable = "Social participation", depvar = "ln(duration)"),
)

## plot results ----

results <- results %>%
  mutate(
    variable = factor(variable, levels = c("Social participation", "Life satisfaction"))
  )

# for manuscript
results %>%
  ggplot(aes(y = estimate, ymin = lower, ymax = upper, x = as.factor(time_to_treat))) +
  geom_point() +
  geom_linerange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(y = "Estimate with 95% CI", x = "Wave") +
  facet_wrap(~variable, scale = "free_y")

ggsave("results/panel/event_hist_CAPI.pdf", height = 4, width = 7, device = cairo_pdf)
ggsave("results/panel/event_hist_CAPI.jpg", height = 4, width = 7, dpi = 300)


## fit models for different outlier thresholds ---------------------------------

# define thresholds
thresholds <- c(60,80,100,120,140,160,180,200)

# fit models
results_thresholds <- lapply(thresholds, function(x){
  
  # exclude durations exceeding x seconds per item
  df_social <- pass_social %>%
    filter(d_social <= 2*x) %>%
    group_by(pnr) %>%
    filter(n() == 4) %>%
    ungroup()
  
  df_sat <- pass_sat %>%
    filter(d_satisfaction <= x) %>%
    group_by(pnr) %>%
    filter(n() == 4) %>%
    ungroup()
  
  # fit models
  fit_sat <- fixest::feols(log_satisfaction ~ i(time_to_treat, 9) | pnr + welle, df_sat)
  fit_social <- fixest::feols(log_social ~ i(time_to_treat, 9)  | pnr + welle, df_social)
  
  # extract results
  results <- bind_rows(
    get_coefs_capi(fit_sat) %>% mutate(variable = "Life satisfaction", depvar = "ln(duration)", threshold = x, threshold_lab = paste0(x, " (N=", nrow(df_sat), ")")),
    get_coefs_capi(fit_social) %>% mutate(variable = "Social participation", depvar = "ln(duration)", threshold = x, threshold_lab = paste0(x, " (N=", nrow(df_social), ")")))
})

# bind in single df
results_thresholds <- do.call(bind_rows, results_thresholds)

## plot results ----

results_thresholds <- results_thresholds %>%
  mutate(
    threshold_lab = fct_reorder(as.factor(threshold_lab), threshold, mean)
  )

# generate plots
plot_social <- results_thresholds %>%
  filter(variable == "Social participation") %>%
  ggplot(aes(y = estimate, ymin = lower, ymax = upper, x = as.factor(time_to_treat), color = threshold_lab)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_linerange(position = position_dodge2(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_grey() +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(y = "Estimate with 95% CI", x = "Wave", title = "Social participation") +
  theme(legend.title = element_blank())

plot_sat <- results_thresholds %>%
  filter(variable == "Life satisfaction") %>%
  ggplot(aes(y = estimate, ymin = lower, ymax = upper, x = as.factor(time_to_treat), color = threshold_lab)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_linerange(position = position_dodge2(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_grey() +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(y = "Estimate with 95% CI", x = "Wave", title = "Life satisfaction") +
  theme(legend.title = element_blank())

# combine plots
ggarrange(plot_social, plot_sat, nrow = 2)

# save plots
ggsave("results/panel/effects_thresholds.pdf", height = 7, width = 5, device = cairo_pdf) 


# common trend -----------------------------------------------------------------

## plot raw developments ----

pass10_summary_social <- df_social %>%
  group_by(treatment, welle) %>%
  summarise(
    mean_social = mean(log_social),
    se_social = sd(log_social)/sqrt(n())
  ) %>%
  mutate(
    recorded = ifelse(treatment == TRUE, "Yes", "No")
  )

pass10_summary_sat <- df_sat %>%
  group_by(treatment, welle) %>%
  summarise(
    mean_sat = mean(log_satisfaction),
    se_sat = sd(log_satisfaction)/sqrt(n())
  ) %>%
  mutate(
    recorded = ifelse(treatment == TRUE, "Yes", "No")
  )

# plot development of average durations over time for social participation
plot_social_mean <- pass10_summary_social %>%
  ggplot(aes(x = welle, y = mean_social, ymin = mean_social - 1.96*se_social, ymax = mean_social + 1.96*se_social, color = recorded, group = recorded)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(position = position_dodge2(width = 0.3)) +
  geom_line(show.legend = FALSE, position = position_dodge2(width = 0.3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  scale_color_grey(start = 0.5, end = 0.2) +
  labs(y = "ln(duration)", x = "Wave", color = "Recorded in wave 10", group = "Recorded in wave 10", title = "Social participation") +
  theme(legend.position = "bottom")


# plot development of average durations over time for life satisfaction
plot_sat_mean <- pass10_summary_sat %>%
  ggplot(aes(x = welle, y = mean_sat, ymin = mean_sat - 1.96*se_sat, ymax = mean_sat + 1.96*se_sat, color = recorded, group = recorded)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(position = position_dodge2(width = 0.3)) +
  geom_line(show.legend = FALSE, position = position_dodge2(width = 0.3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  scale_color_grey(start = 0.5, end = 0.2) +
  labs(y = "ln(duration)", x = "Wave", color = "Recorded in wave 10", group = "Recorded in wave 10", title = "Life satisfaction") +
  theme(legend.position = "bottom")

# combine both plots
ggarrange(plot_social_mean, plot_sat_mean, common.legend = TRUE, legend = "bottom")

# save plots
ggsave("results/panel/common_trend.pdf", height = 4, width = 7, device = cairo_pdf)


## test common trend assumption ----

fit_sat <- fixest::feols(log_satisfaction ~ i(time_to_treat, 9) | pnr + welle, filter(df_sat, welle != 10))
fit_social <- fixest::feols(log_social ~ i(time_to_treat, 9)  | pnr + welle, filter(df_social, welle != 10))

summary(fit_sat)
summary(fit_social)
