# Title: analysis of panel data for outcomes

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

# pass social participation
pass_social <- readRDS("Daten/processed/pass_social.rds")

# pass life satisfaction
pass_sat <- readRDS("Daten/processed/pass_sat.rds")

# exclude durations exceeding 100 seconds per item and missings
df_social <- pass_social %>%
  filter(d_social <= 2*100) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

df_status <- df_social %>%
  filter(d_social <= 2*100, !is.na(status)) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

df_part <- df_social %>%
  filter(d_social <= 2*100, !is.na(participation)) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

df_sat <- pass_sat %>%
  filter(d_satisfaction <= 100, !is.na(satisfaction)) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()


# event study analysis ---------------------------------------------------------

## function to extract coefficients ----

get_coefs_capi <- function(fit){
  as_tibble(coeftable(fit, cluster = ~pnr)) %>%
    mutate(
      lower = confint(fit, cluster = ~pnr)[,1],
      upper = confint(fit, cluster = ~pnr)[,2],
      time_to_treat =c(7, 8, 10)
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

## outcomes ----

### fit models ----
fit_status <- fixest::feols(status ~ i(time_to_treat,  9) | pnr + welle, df_status)
fit_participation <- fixest::feols(participation ~ i(time_to_treat, 9)  | pnr + welle, df_part)
fit_satisfaction <- fixest::feols(satisfaction ~ i(time_to_treat, 9) | pnr + welle, df_sat)


# extract results
results <- bind_rows(
  get_coefs_capi(fit_status) %>% mutate(variable = "Position in society"),
  get_coefs_capi(fit_participation) %>% mutate(variable = "Part of society"),
  get_coefs_capi(fit_satisfaction) %>% mutate(variable = "Life satisfaction")
)

### plot results ----
results %>%
  ggplot(aes(y = estimate, ymin = lower, ymax = upper, x = time_to_treat, color = variable)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_linerange(position = position_dodge2(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(y = "Estimate with 95% CI", x = "Wave", color = "Outcome") +
  theme(legend.position = "bottom")


# export estimation results to latex -------------------------------------------

# set dictionary
setFixest_dict(c(status = "Position in society", 
                 participation = "Part of society", 
                 satisfaction = "Life satisfaction",
                 pnr = "Respondent",
                 welle = "Wave",
                 time_to_treat = " Treatment $\\times$ Wave"))

# set style
style_tex <- style.tex(main = "base",
                       depvar.title = "",
                       model.title = "",
                       stats.title = "\\midrule",
                       line.top = "\\toprule",
                       var.title = "\\midrule")

etable(fit_status, fit_participation, fit_satisfaction, 
       style.tex = style_tex,
       tex = TRUE, 
       digits.stats = 4)

# export tables
etable(fit_status, fit_participation, fit_satisfaction, 
       style.tex = style_tex,
       tex = TRUE, 
       digits.stats = 4,
       title = "Effects of audio recordings on measurement.",
       file = "results/tables/effects_measurement.tex",
       label = "tab:effects_measurement",
       placement = "!ht",
       replace = TRUE)

# clear memory
rm(list = ls())
