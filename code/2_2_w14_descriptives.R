# Title: descriptives for w14 data

# run setup file ---------------------------------------------------------------

source("code/setup.R")


# load data --------------------------------------------------------------------

pass14 <- readRDS("Daten/processed/pass14.rds")


# descriptive analysis ---------------------------------------------------------

# crosstab of mode and recording
pass14_tab <- pass14 %>%
  rename("Mode" = method_covid) %>%
  mutate(
    recording = ifelse(recording == "Yes", "Recorded", "Non-recorded")
  )

datasummary_crosstab(`Mode` ~ recording,
                     data = pass14_tab,
                     fmt = "%.2f",
                     title = "Recorded and non-recorded interviews by mode, PASS W14. \\label{tab:recordings_w14}",
                     output = "results/tables/recordings_w14.tex")


# distribution of durations ----------------------------------------------------

pass14_long <- pass14 %>%
  pivot_longer(
    cols = starts_with("d_"),
    names_to = "section",
    values_to = "duration"
  ) %>%
  filter(section != "d_trust_inst", section != "d_experiment") %>%
  mutate(
    section_lab = case_when(
      section == "d_att_life" ~ "Attitude To Life",
      section == "d_att_self" ~ "Attitude To Self",
      section == "d_social_trust" ~ "Social Trust",
      section == "d_role_model" ~ "Role Model",
      section == "d_social_part" ~ "Social Participation",
      section == "d_politics" ~ "Politics",
      section == "d_democracy" ~ "Democracy",
      section == "d_leftright" ~ "Left-Right",
      section == "d_activity" ~ "Activities",
      section == "d_leisure" ~ "Leisure",
      section == "d_func_work" ~ "Functions Of Work",
      section == "d_insurance" ~ "Insurance",
      section == "d_satisfaction" ~ "Life satisfaction"
    )
  ) %>%
  group_by(section) %>%
  mutate(
    perc_99 = quantile(duration, 0.99, na.rm = TRUE)
  ) %>%
  ungroup()

section_labs <- unique(pass14_long$section_lab)

plot_list <- lapply(section_labs, function(x){
  df_plot <- pass14_long %>%
    filter(section_lab == {{ x }}) 
  
  limit <- quantile(df_plot$duration, 0.95, na.rm = TRUE)
  
  df_plot %>%
    ggplot(aes(x = duration, color = method_covid, linetype = recording)) +
    stat_density(geom = "line", position = "identity", kernel = "epanechnikov", trim = TRUE, adjust = 2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                       limits = c(0, limit)) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = {{ x }}, y = "Density", x = "Duration in seconds", color = "Mode", linetype = "Recorded") +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 8)) 
})

ggarrange(plotlist = plot_list, ncol = 2, nrow = 7, common.legend = TRUE, legend = "bottom", align = "hv")

ggsave("results/distribution/duration_dist_w14.pdf", height = 10, width = 7, device = cairo_pdf)

# plot for disputation

ggarrange(plot_list[[5]], plot_list[[13]], ncol = 2, nrow =1, common.legend = TRUE, legend = "bottom", align = "hv")

ggsave("results/distribution/duration_dist_w14_disputation.pdf", height = 3.5, width = 7, device = cairo_pdf)


# summary statistics of durations ----------------------------------------------

summary_durations <- function(data, variable, label){
  
  data %>%
    summarise(
      N = sum(!is.na({{variable}})),
      median = median({{variable}}, na.rm = TRUE),
      minimum = min({{variable}}, na.rm = TRUE),
      maximum = max({{variable}}, na.rm = TRUE),
      unique = n_distinct({{variable}})
    ) %>%
    mutate(
      section = label, .before = 1
    )
}

rbind(summary_durations(pass14, d_social_trust, "Social trust"),
      summary_durations(pass14, d_att_life, "Attitude to life"),
      summary_durations(pass14, d_att_self, "Attitude to self"),
      summary_durations(pass14, d_role_model, "Role model"),
      summary_durations(pass14, d_social_part, "Social participation"),
      summary_durations(pass14, d_politics, "Interest in politics"),
      summary_durations(pass14, d_democracy, "Democracy"),
      summary_durations(pass14, d_leftright, "Left-right scale"),
      summary_durations(pass14, d_activity, "Activities"),
      summary_durations(pass14, d_leisure, "Leisure"),
      summary_durations(pass14, d_func_work, "Functions of work"),
      summary_durations(pass14, d_insurance, "Insurance"),
      summary_durations(pass14, d_satisfaction, "Life satisfaction"))


# ICCs by mode -----------------------------------------------------------------

# fit multilevel logistic models
fit_cati <- glmer(recording == "Yes" ~ 1 + (1|internr), data = subset(pass14, method_covid == "CATI"), family = binomial())
fit_capi_f2f <- glmer(recording == "Yes" ~ 1 + (1|internr), data = subset(pass14, method_covid == "CAPI, f2f"), family = binomial())
fit_capi_tel <- glmer(recording == "Yes" ~ 1 + (1|internr), data = subset(pass14, method_covid == "CAPI, tel."), family = binomial())

# calculate iccs
icc(fit_cati)
icc(fit_capi_f2f)
icc(fit_capi_tel)

