# Title: analysis of panel data - wave 11

# run setup file ---------------------------------------------------------------

source("code/setup.R")


# load data --------------------------------------------------------------------

# full pass panel
pass_panel <- readRDS("Daten/processed/pass_panel.rds")

# pass social participation
pass_social <- readRDS("Daten/processed/pass_social.rds")

# pass life satisfaction
pass_sat <- readRDS("Daten/processed/pass_sat.rds")


# exclude durations exceeding 100 seconds per item
pass_social <- pass_social %>%
  filter(d_social <= 2*100) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

pass_sat <- pass_sat %>%
  filter(d_satisfaction <= 100) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

# data preparation and analysis - social ---------------------------------------

pass_w11 <- pass_panel %>%
  filter(welle == 11, pnr %in% unique(pass_social$pnr)) %>%
  select(pnr, welle, internr, int_mode, recording, d_social, threshold_social) %>%
  mutate(welle = as.factor(welle))

pass_w10_social <- pass_social %>%
  filter(welle == 10) %>%
  select(pnr, welle, internr, int_mode, recording, d_social, threshold_social)

pass_w1011 <- bind_rows(pass_w10_social, pass_w11)


pass_w1011 <- pass_w1011 %>%
  group_by(pnr) %>%
  mutate(
    recording_group = case_when(
      sum(recording == "No") == 2 ~ "Not recorded in W10, not recorded in W11",
      sum(recording == "Yes") == 2 ~ "Recorded in W10, recorded in W11",
      sum(recording == "Yes") == 1 & n() == 2 ~ "Switched",
      sum(recording == "Yes", na.rm = TRUE) == 1 ~ "Recorded to nonresponse",
      sum(recording == "No", na.rm = TRUE) == 1 ~ "Not recorded to nonresponse"
    )
  ) %>%
  ungroup() %>%
  mutate(
    recording_group = case_when(
      recording_group == "Switched" & welle == 10 & recording == "No" ~ "Not recorded in W10, recorded in W11",
      recording_group == "Switched" & welle == 10 & recording == "Yes" ~ "Recorded in W10, not recorded in W11",
      recording_group == "Switched" & welle == 11 & recording == "Yes" ~ "Not recorded in W10, recorded in W11",
      recording_group == "Switched" & welle == 11 & recording == "No" ~ "Recorded in W10, not recorded in W11",
      TRUE ~ recording_group
    )
  )

table(pass_w1011$welle, pass_w1011$recording, useNA = "always")

pass_w1011_summary <- pass_w1011 %>%
  group_by(recording_group, welle) %>%
  summarize(
    share_social = mean(threshold_social, na.rm = TRUE),
    n_social = sum(threshold_social == 1, na.rm = TRUE),
    N_social = sum(!is.na(threshold_social)),
    lower_social = binom.test(n_social,N_social)$conf.int[1],
    upper_social = binom.test(n_social,N_social)$conf.int[2]
  )


plot_social <- pass_w1011_summary %>%
  filter(!str_detect(recording_group, "nonresponse")) %>%
  ggplot(aes(x = welle, y = share_social, ymin = lower_social, ymax = upper_social, color = recording_group, group = recording_group)) +
  geom_point(position = position_dodge2(width = 0.2)) +
  geom_linerange(position = position_dodge2(width = 0.2), show.legend = FALSE) +
  geom_line(aes(linetype = recording_group), position = position_dodge2(width = 0.2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8),
                     labels = scales::percent,
                     limits = c(0, NA)) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.2))) +
  labs(title = "Social participation", y = "Duration shorter than 4 WPS limit", x = "Wave") +
  scale_color_manual(values = c("black", "darkgray", "black", "darkgray")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(nrow = 2))

plot_social

# data preparation and analysis - satisfaction ---------------------------------

pass_w11 <- pass_panel %>%
  filter(welle == 11, pnr %in% unique(pass_sat$pnr)) %>%
  select(pnr, welle, internr, int_mode, recording, d_satisfaction, threshold_sat) %>%
  mutate(welle = as.factor(welle))

pass_w10_sat <- pass_sat %>%
  filter(welle == 10) %>%
  select(pnr, welle, internr, int_mode, recording, d_satisfaction, threshold_sat)

pass_w1011 <- bind_rows(pass_w10_sat, pass_w11)


pass_w1011 <- pass_w1011 %>%
  group_by(pnr) %>%
  mutate(
    recording_group = case_when(
      sum(recording == "No") == 2 ~ "Not recorded in W10, not recorded in W11",
      sum(recording == "Yes") == 2 ~ "Recorded in W10, recorded in W11",
      sum(recording == "Yes") == 1 & n() == 2 ~ "Switched",
      sum(recording == "Yes", na.rm = TRUE) == 1 ~ "Recorded to nonresponse",
      sum(recording == "No", na.rm = TRUE) == 1 ~ "Not recorded to nonresponse"
    )
  ) %>%
  ungroup() %>%
  mutate(
    recording_group = case_when(
      recording_group == "Switched" & welle == 10 & recording == "No" ~ "Not recorded in W10, recorded in W11",
      recording_group == "Switched" & welle == 10 & recording == "Yes" ~ "Recorded in W10, not recorded in W11",
      recording_group == "Switched" & welle == 11 & recording == "Yes" ~ "Not recorded in W10, recorded in W11",
      recording_group == "Switched" & welle == 11 & recording == "No" ~ "Recorded in W10, not recorded in W11",
      TRUE ~ recording_group
    )
  )

table(pass_w1011$welle, pass_w1011$recording, useNA = "always")

pass_w1011_summary <- pass_w1011 %>%
  group_by(recording_group, welle) %>%
  summarize(
    share_sat = mean(threshold_sat, na.rm = TRUE),
    n_sat = sum(threshold_sat == 1, na.rm = TRUE),
    N_sat = sum(!is.na(threshold_sat)),
    lower_sat = binom.test(n_sat,N_sat)$conf.int[1],
    upper_sat = binom.test(n_sat,N_sat)$conf.int[2]
  )


plot_sat <- pass_w1011_summary %>%
  filter(!str_detect(recording_group, "nonresponse")) %>%
  ggplot(aes(x = welle, y = share_sat, ymin = lower_sat, ymax = upper_sat, color = recording_group, group = recording_group)) +
  geom_point(position = position_dodge2(width = 0.2)) +
  geom_linerange(position = position_dodge2(width = 0.2), show.legend = FALSE) +
  geom_line(aes(linetype = recording_group), position = position_dodge2(width = 0.2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8),
                     labels = scales::percent,
                     limits = c(0, NA)) +
  scale_x_discrete(expand = expansion(add = c(0.2,0.2))) +
  labs(title = "Life satisfaction", y = "Duration shorter than 4 WPS limit", x = "Wave") +
  scale_color_manual(values = c("black", "darkgray", "black", "darkgray")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(nrow = 2))

plot_sat

# combien plots
ggarrange(plot_social, plot_sat, nrow = 1, common.legend = TRUE, legend = "bottom")

# save plot
ggsave("results/panel/development_w11.pdf", height = 3.5, width = 7, device = cairo_pdf)
ggsave("results/panel/development_w11.jpg", height = 3.5, width = 7, dpi = 300)


# prepare data for assessing mode, nonresponse and recording after w10 ---------

# load full data
pass11_full <- read.dta13("Daten/raw/Welle_11/PASS-W11_P-Sen_Quer_anonym.dta",
                     convert.factors = FALSE)

pass11_full <- pass11_full %>%
  filter(
    !(internr %in% c(300004, 400560)), # drop falsifiers
    is.na(raus), # drop observations that will be dropped from final data
    is.na(raus2) # drop observations that will be dropped from final data
  )

# load data
pass_penddat <-
  read.dta13(
    "Daten/raw/PASS/PENDDAT.dta",
    convert.factors = FALSE
  )

# keep only wave 11
pass_penddat <- pass_penddat %>%
  filter(welle == 11)

pass11_full <- pass11_full %>%
  left_join(pass_penddat, by = c("pnr"))

pass11_full <- pass11_full %>%
  mutate(
    recording_w11 = ifelse(qmitschn == 1, "Yes", "No"),
    int_mode_w11 = case_when(
      pintmod == 0 ~ "Yes",
      pintmod %in% c(1,2) ~ "No"
    ),
    internr_w11 = internr
  )

pass11_full <- pass11_full %>%
  select(pnr, internr_w11, int_mode_w11, recording_w11)


# analysis for social participation ----------------------------------------------------

# join data
pass_w10_social <- pass_w10_social %>%
  left_join(pass11_full, by = c("pnr"))


# nonresponse
pass_w10_social <- pass_w10_social %>%
  mutate(
    response_11 = ifelse(pnr %in% unique(pass_penddat$pnr), "Yes", "No")
  )

prop.table(table(pass_w10_social$response_11, pass_w10_social$recording), margin = 2)


# recording
prop.table(table(pass_w10_social$recording_w11, pass_w10_social$recording), margin = 2)


# change of mode
prop.table(table(pass_w10_social$int_mode_w11, pass_w10_social$recording), margin = 2)


# summarize in table
tab_resp <- datasummary(response_11 ~ recording * ( N + Percent("col")), data = pass_w10_social, output = "data.frame")
tab_mode <- datasummary(int_mode_w11 ~ recording * ( N + Percent("col")), data = subset(pass_w10_social, !is.na(int_mode_w11)), output = "data.frame")
tab_rec <- datasummary(recording_w11 ~ recording * ( N + Percent("col")), data = subset(pass_w10_social, !is.na(recording_w11) & int_mode_w11 != "CATI"), output = "data.frame")


tab_social <- bind_rows(tab_resp, tab_mode, tab_rec) %>%
  add_row(" " = "Participated in W11", .before = 1) %>%
  add_row(" " = "CATI in W11", .before = 4) %>%
  add_row(" " = "Recorded in W11", .before = 7) %>%
  add_row("No N" = "N", "No Percent" = "Percent", "Yes N" = "N", "Yes Percent" = "Percent", .before = 1) %>%
  add_row("No N" = "Not recorded", "Yes N" = "Recorded", .before = 1) 


# analysis for life satisfaction -----------------------------------------------

# join data
pass_w10_sat <- pass_w10_sat %>%
  left_join(pass11_full, by = c("pnr"))


# nonresponse
pass_w10_sat <- pass_w10_sat %>%
  mutate(
    response_11 = ifelse(pnr %in% unique(pass_penddat$pnr), "Yes", "No")
  )

prop.table(table(pass_w10_sat$response_11, pass_w10_sat$recording), margin = 2)


# recording
prop.table(table(pass_w10_sat$recording_w11, pass_w10_sat$recording), margin = 2)


# change of mode
prop.table(table(pass_w10_sat$int_mode_w11, pass_w10_sat$recording), margin = 2)


# summarize in table
tab_resp <- datasummary(response_11 ~ recording * ( N + Percent("col")), data = pass_w10_sat, output = "data.frame")
tab_mode <- datasummary(int_mode_w11 ~ recording * ( N + Percent("col")), data = subset(pass_w10_sat, !is.na(int_mode_w11)), output = "data.frame")
tab_rec <- datasummary(recording_w11 ~ recording * ( N + Percent("col")), data = subset(pass_w10_sat, !is.na(recording_w11) & int_mode_w11 != "CATI"), output = "data.frame")


tab_sat <- bind_rows(tab_resp, tab_mode, tab_rec) %>%
  add_row(" " = "Participated in W11", .before = 1) %>%
  add_row(" " = "CATI in W11", .before = 4) %>%
  add_row(" " = "Recorded in W11", .before = 7) %>%
  add_row("No N" = "N", "No Percent" = "Percent", "Yes N" = "N", "Yes Percent" = "Percent", .before = 1) %>%
  add_row("No N" = "Not recorded", "Yes N" = "Recorded", .before = 1) 


# combine social and sat table
tab_both <- bind_cols(tab_social, tab_sat) %>%
  select(-c(" ...6"))

colnames(tab_both) <- c("", "Social participation sample", rep("", 3), "Life satisfaction sample", rep("", 3))

print(xtable(tab_both,
             caption = "Participation, mode, and audio recording in Wave 11.",
             label = "tab:panel_w11"),
      include.rownames = FALSE,
      booktabs = TRUE,
      hline.after = c(-1, 0, 2, nrow(tab_both)),
      caption.placement = "top",
      table.placement = "!ht",
      file = "results/tables/panel_w11.tex")
