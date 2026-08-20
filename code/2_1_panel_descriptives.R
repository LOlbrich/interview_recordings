# Title: descriptives for panel data

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

# full pass panel
pass_panel <- readRDS("Daten/processed/pass_panel.rds")

# pass social participation
pass_social <- readRDS("Daten/processed/pass_social.rds")

# pass life satisfaction
pass_sat <- readRDS("Daten/processed/pass_sat.rds")


# consent rates ----------------------------------------------------------------

table(pass_panel$qmitschn, useNA = "always")
table(pass_panel$caticapi, useNA = "always")

pass_panel %>% 
  filter(caticapi == 1) %>%
  group_by(welle) %>% 
  summarize(mean(qmitschn))


# raw development of shares ----------------------------------------------------

# prepare data for plots by recording
pass_summary_rec <- pass_panel %>%
  group_by(caticapi, welle, qmitschn) %>%
  summarise(
    share_sat = mean(threshold_sat, na.rm = TRUE),
    n_sat = sum(threshold_sat == 1, na.rm = TRUE),
    N_sat = sum(!is.na(threshold_sat)),
    lower_sat = binom.test(n_sat,N_sat)$conf.int[1], # Clopper-Pearson CI
    upper_sat = binom.test(n_sat,N_sat)$conf.int[2],
    share_social = mean(threshold_social, na.rm = TRUE),
    n_social = sum(threshold_social == 1, na.rm = TRUE),
    N_social = sum(!is.na(threshold_social)),
    lower_social = binom.test(n_social,N_social)$conf.int[1],
    upper_social = binom.test(n_social,N_social)$conf.int[2]
  )


# prepare data for plots in total
pass_summary <- pass_panel %>%
  group_by(caticapi, welle) %>%
  summarise(
    share_sat = mean(threshold_sat, na.rm = TRUE),
    n_sat = sum(threshold_sat == 1, na.rm = TRUE),
    N_sat = sum(!is.na(threshold_sat)),
    lower_sat = binom.test(n_sat,N_sat)$conf.int[1],
    upper_sat = binom.test(n_sat,N_sat)$conf.int[2],
    share_social = mean(threshold_social, na.rm = TRUE),
    n_social = sum(threshold_social == 1, na.rm = TRUE),
    N_social = sum(!is.na(threshold_social)),
    lower_social = binom.test(n_social,N_social)$conf.int[1],
    upper_social = binom.test(n_social,N_social)$conf.int[2]
  ) %>%
  mutate(
    qmitschn = 2
  )

# bind summary data
pass_summary_all <- rbind(pass_summary, pass_summary_rec) %>%
  mutate(
    recording = case_when(
      qmitschn == 0 ~ "No recording",
      qmitschn == 1 ~ "Recording",
      qmitschn == 2 ~ "Total"
    ),
    mode = ifelse(caticapi == 1, "CAPI", "CATI")
  )


# plot for life satisfaction
plot_sat <- pass_summary_all %>%
  filter(mode == "CAPI") %>%
  ggplot(aes(x = welle, y = share_sat, ymin = lower_sat, ymax = upper_sat, color = recording, linetype = recording, shape = recording)) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "darkgray") +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(show.legend = FALSE, position = position_dodge2(width = 0.3), linetype = "solid") +
  geom_line(position = position_dodge2(width = 0.3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8),
                     labels = scales::percent,
                     limits = c(0, 0.3)) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  labs(title = "Life satisfaction", y = "Duration shorter than 4 WPS limit", x = "Wave") +
  scale_color_grey(start = 0.5, end = 0) +
  theme(legend.position = "bottom",
        legend.title = element_blank())


# plot for social participation
plot_social <- pass_summary_all %>%
  filter(mode == "CAPI") %>%
  ggplot(aes(x = welle, y = share_social, ymin = lower_social, ymax = upper_social, color = recording, linetype = recording, shape = recording)) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "darkgray") +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(show.legend = FALSE, position = position_dodge2(width = 0.3), linetype = "solid") +
  geom_line(position = position_dodge2(width = 0.3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8),
                     labels = scales::percent,
                     limits = c(0, 0.3)) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  labs(title = "Social participation", y = "Duration shorter than 4 WPS limit", x = "Wave") +
  scale_color_grey(start = 0.5, end = 0) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# combine plots
ggarrange(plot_social, plot_sat, common.legend = TRUE, legend = "bottom")

# save plots
ggsave("results/panel/development_both.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/panel/development_both.jpg", height = 3.5, width = 7, dpi = 300)


# clear memory
rm(pass_summary, pass_summary_all, pass_summary_rec, plot_sat, plot_social)


# sample summary ---------------------------------------------------------------

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

# get number of unique respondents and consents 
length(unique(df_social$pnr))
table(df_social$recording)

length(unique(df_sat$pnr))
table(df_sat$recording)


# ICCs --------------------------------------------------------------------

icc(glmer(recording == "Yes" ~ 1 + (1|internr), data = subset(df_social, welle == 10), family = binomial()))
icc(glmer(recording == "Yes" ~ 1 + (1|internr), data = subset(df_sat, welle == 10), family = binomial()))

