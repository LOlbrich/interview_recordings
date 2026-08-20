# Title: analysis of w14 data

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

# pass wave 14
pass14 <- readRDS("Daten/processed/pass14.rds")


# define functions -------------------------------------------------------------

## function for distribution regression ----

dist_reg <- function(depvar = "d_social_part", 
                     values = seq(3,120,3), 
                     data = pass14, 
                     formula = "i(time_to_treat, 9) | pnr + welle"){
  
  est_results <- lapply(values, function(x){
    
    # generate dummy variable
    data$d_var <- data[depvar] <= x
    
    # fit model
    fit <- glm(as.formula(paste0("d_var ~ ", formula)), family = "binomial", data = data)
    
    # store observed and counterfactual
    tibble(
      method_covid = c(rep("CAPI, f2f", 2), rep("CAPI-by-phone", 2), rep("CATI", 2)),
      cat = c("Factual (recorded)", "Counterfactual (non-recorded)", "Factual (recorded)", "Counterfactual (non-recorded)", "Recorded", "Non-recorded"),
      estimate = c(
        summary(prediction(fit, 
                           data = filter(pass14, recording == "Yes" & method_covid == "CAPI, f2f"), 
                           at = list(recording = c("Yes")),
                           calculate_se = FALSE))$Prediction,
        summary(prediction(fit, 
                           data = filter(pass14, recording == "Yes" & method_covid == "CAPI, f2f"), 
                           at = list(recording = c("Yes")),
                           calculate_se = FALSE))$Prediction - summary(margins(fit, variables = "recording", at = list(method_covid = "CAPI, f2f"), vce = "none"))$AME[[1]],
        summary(prediction(fit, 
                           data = filter(pass14, recording == "Yes" & method_covid == "CAPI, tel."), 
                           at = list(recording = c("Yes")),
                           calculate_se = FALSE))$Prediction,
        summary(prediction(fit, 
                           data = filter(pass14, recording == "Yes" & method_covid == "CAPI, tel."), 
                           at = list(recording = c("Yes")),
                           calculate_se = FALSE))$Prediction - summary(margins(fit, variables = "recording", at = list(method_covid = "CAPI, tel."), vce = "none"))$AME[[1]],
      summary(prediction(fit, 
                         data = filter(pass14, recording == "Yes" & method_covid == "CATI"), 
                         at = list(recording = c("Yes")),
                         calculate_se = FALSE))$Prediction,
      summary(prediction(fit, 
                         data = filter(pass14, recording == "No" & method_covid == "CATI"), 
                         at = list(recording = c("No")),
                         calculate_se = FALSE))$Prediction),
      second = {{ x }}
    
)
  })
  
  # combine in single data frame
  do.call(rbind, est_results) %>%
    clean_names() %>%
    group_by(method_covid, cat) %>%
    mutate(
      rearranged = as.numeric(rearrangement(list(second), estimate))
    )
  
}

## function for bootstrapping ----

dist_reg_boot <- function(data = pass14,
                          depvar = "d_democracy",
                          times = 10,
                          sec_range = 1:10){
  
  # drop missings
  data <- data[!is.na(data[[depvar]]),]
  
  # draw bootstrap samples
  bs <- bootstraps(data, times = times)
  
  # run dist_reg on each sample
  bs_results <- bs$splits %>%
    map(function(x) dist_reg(depvar = depvar, 
                             values = sec_range, 
                             data = as_tibble(x), 
                             formula = "recording*method_covid + age + I(age^2) + gender + german + education + month + n_pnr + unemployed"))
  
  # bind results
  df_bs <- do.call(rbind, bs_results)
  
  # run dist_reg on original sample
  orig_results <- dist_reg(depvar = depvar, 
                             values = sec_range, 
                             data = data, 
                             formula = "recording*method_covid + age + I(age^2) + gender + german + education + month + n_pnr + unemployed")
  
  # summarize bootstrap results
  df_bs_sum <- df_bs %>%
    group_by(cat, second, method_covid) %>%
    summarize(
      lower = quantile(rearranged, 0.05),
      upper = quantile(rearranged, 0.95)
    ) 
  
  # merge and return results
  orig_results <- orig_results %>%
    left_join(df_bs_sum)
  
}

# functions for plotting results

plot_distreg <- function(data, xlabel){
  
  # plot
  data %>%
    filter(method_covid != "CATI") %>%
    ggplot(aes(y = rearranged, ymin = lower, ymax = upper, x = second, color = cat, fill = cat)) +
    geom_line(size = 0.2) +
    geom_ribbon(alpha = 0.6, size = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_fill_grey(start = 0.5, end = 0.2) +
    scale_color_grey(start = 0.5, end = 0.2) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(x = xlabel, y = "F(x)") +
    theme(legend.title = element_blank()) +
    facet_wrap(~method_covid)
}




# fit and plot regressions -----------------------------------------------------

## attitude to life
att_life_results <- dist_reg_boot(data = pass14,
                                depvar = "d_att_life",
                                times = 100,
                                sec_range = seq(quantile(pass14$d_att_life, 0.01, na.rm = TRUE),
                                                quantile(pass14$d_att_life, 0.975, na.rm = TRUE),
                                                1))

plot_distreg(att_life_results, "Duration for attitude to life in seconds")

ggsave("results/cross/cross_distreg_attlife.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_attlife.png", height = 3.5, width = 7)

## attitude to self ----
att_self_results <- dist_reg_boot(data = pass14,
                                  depvar = "d_att_self",
                                  times = 100,
                                  sec_range = seq(quantile(pass14$d_att_self, 0.01, na.rm = TRUE),
                                                  quantile(pass14$d_att_self, 0.975, na.rm = TRUE),
                                                  1))

plot_distreg(att_self_results, "Duration for attitude to self in seconds")

ggsave("results/cross/cross_distreg_attself.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_attself.png", height = 3.5, width = 7)

## social trust ----
social_trust_results <- dist_reg_boot(data = pass14,
                                  depvar = "d_social_trust",
                                  times = 100,
                                  sec_range = seq(quantile(pass14$d_social_trust, 0.01, na.rm = TRUE),
                                                  quantile(pass14$d_social_trust, 0.975, na.rm = TRUE),
                                                  1))


plot_distreg(social_trust_results, "Duration for social trust in seconds")

ggsave("results/cross/cross_distreg_socialtrust.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_socialtrust.png", height = 3.5, width = 7)

## role model ----
role_model_results <- dist_reg_boot(data = pass14,
                                      depvar = "d_role_model",
                                      times = 100,
                                      sec_range = seq(quantile(pass14$d_role_model, 0.01, na.rm = TRUE),
                                                      quantile(pass14$d_role_model, 0.975, na.rm = TRUE),
                                                      1))

plot_distreg(role_model_results, "Duration for role model in seconds")

ggsave("results/cross/cross_distreg_rolemodel.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_rolemodel.png", height = 3.5, width = 7)

## social participation ----
social_part_results <- dist_reg_boot(data = pass14,
                                    depvar = "d_social_part",
                                    times = 100,
                                    sec_range = seq(quantile(pass14$d_social_part, 0.01, na.rm = TRUE),
                                                    quantile(pass14$d_social_part, 0.975, na.rm = TRUE),
                                                    1))

plot_distreg(social_part_results, "Duration for social participation in seconds")

ggsave("results/cross/cross_distreg_socpart.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_socpart.png", height = 3.5, width = 7)

## politics ----
politics_results <- dist_reg_boot(data = pass14,
                                     depvar = "d_politics",
                                     times = 100,
                                     sec_range = seq(quantile(pass14$d_politics, 0.01, na.rm = TRUE),
                                                     quantile(pass14$d_politics, 0.975, na.rm = TRUE),
                                                     1))

plot_distreg(politics_results, "Duration for politics in seconds")

ggsave("results/cross/cross_distreg_politics.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_politics.png", height = 3.5, width = 7)

## democracy ----
democracy_results <- dist_reg_boot(data = pass14,
                                  depvar = "d_democracy",
                                  times = 100,
                                  sec_range = seq(quantile(pass14$d_democracy, 0.01, na.rm = TRUE),
                                                  quantile(pass14$d_democracy, 0.975, na.rm = TRUE),
                                                  1))

plot_distreg(democracy_results, "Duration for democracy in seconds")

ggsave("results/cross/cross_distreg_democracy.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_democracy.png", height = 3.5, width = 7)

## left right-scale ----
leftright_results <- dist_reg_boot(data = pass14,
                                   depvar = "d_leftright",
                                   times = 100,
                                   sec_range = seq(quantile(pass14$d_leftright, 0.01, na.rm = TRUE),
                                                   quantile(pass14$d_leftright, 0.975, na.rm = TRUE),
                                                   1))

plot_distreg(leftright_results, "Duration for left-right scale in seconds")

ggsave("results/cross/cross_distreg_leftright.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_leftright.png", height = 3.5, width = 7)

## activities -----
activity_results <- dist_reg_boot(data = pass14,
                                   depvar = "d_activity",
                                   times = 100,
                                   sec_range = seq(quantile(pass14$d_activity, 0.01, na.rm = TRUE),
                                                   quantile(pass14$d_activity, 0.975, na.rm = TRUE),
                                                   1))


plot_distreg(activity_results, "Duration for activities in seconds")

ggsave("results/cross/cross_distreg_activity.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_activity.png", height = 3.5, width = 7)

## leisure ----
leisure_results <- dist_reg_boot(data = pass14,
                                  depvar = "d_leisure",
                                  times = 100,
                                  sec_range = seq(quantile(pass14$d_leisure, 0.01, na.rm = TRUE),
                                                  quantile(pass14$d_leisure, 0.975, na.rm = TRUE),
                                                  1))

plot_distreg(leisure_results, "Duration for leisure in seconds")

ggsave("results/cross/cross_distreg_leisure.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_leisure.png", height = 3.5, width = 7)

## functions of work ----
funcwork_results <- dist_reg_boot(data = pass14,
                                 depvar = "d_func_work",
                                 times = 100,
                                 sec_range = seq(quantile(pass14$d_func_work, 0.01, na.rm = TRUE),
                                                 quantile(pass14$d_func_work, 0.975, na.rm = TRUE),
                                                 1))

plot_distreg(funcwork_results, "Duration for functions of work in seconds")

ggsave("results/cross/cross_distreg_funcwork.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_funcwork.png", height = 3.5, width = 7)


## insurance ----
insurance_results <- dist_reg_boot(data = pass14,
                                  depvar = "d_insurance",
                                  times = 100,
                                  sec_range = seq(quantile(pass14$d_insurance, 0.01, na.rm = TRUE),
                                                  quantile(pass14$d_insurance, 0.975, na.rm = TRUE),
                                                  1))

plot_distreg(insurance_results, "Duration for insurance in seconds")

ggsave("results/cross/cross_distreg_insurance.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_insurance.png", height = 3.5, width = 7)

## satisfaction ----
satisfaction_results <- dist_reg_boot(data = pass14,
                                   depvar = "d_satisfaction",
                                   times = 100,
                                   sec_range = seq(quantile(pass14$d_satisfaction, 0.01, na.rm = TRUE),
                                                   quantile(pass14$d_satisfaction, 0.975, na.rm = TRUE),
                                                   1))

plot_distreg(satisfaction_results, "Duration for life satisfaction in seconds")

ggsave("results/cross/cross_distreg_satisfaction.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/cross/cross_distreg_satisfaction.png", height = 3.5, width = 7)

# plot differences between recorded and nonrecorded interviews for cati --------

# bind results dataframes
cati_diff <- bind_rows(
  mutate(social_trust_results, var = "Social trust"),
  mutate(att_life_results, var = "Attitude to life"),
  mutate(att_self_results, var = "Attitude to self"),
  mutate(role_model_results, var = "Role model"),
  mutate(politics_results, var = "Politics"),
  mutate(democracy_results, var = "Democracy"),
  mutate(leftright_results, var = "Left-Right"),
  mutate(activity_results, var = "Activities"),
  mutate(leisure_results, var = "Leisure"),
  mutate(funcwork_results, var = "Functions of work"),
  mutate(insurance_results, var = "Insurance"),
  mutate(social_part_results, var = "Social participation"),
  mutate(satisfaction_results, var = "Life satisfaction")
)


# plot
cati_diff %>%
  filter(method_covid == "CATI") %>%
  ggplot(aes(y = rearranged, ymin = lower, ymax = upper, x = second, color = cat, fill = cat)) +
  geom_line(size = 0.2) +
  geom_ribbon(alpha = 0.6, size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_fill_grey(start = 0.5, end = 0.2) +
  scale_color_grey(start = 0.5, end = 0.2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Duration in seconds", y = "F(x)") +
  theme(legend.title = element_blank()) +
  facet_wrap(~var, scales = "free_x", nrow = 3)


ggsave("results/cross/cross_distreg_cati.pdf", height = 7, width = 12, device = cairo_pdf)
ggsave("results/cross/cross_distreg_cati.png", height = 7, width = 12)
