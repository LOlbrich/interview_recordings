# Title: analysis of experiment 

# run setup file ---------------------------------------------------------------

source("code/setup.R")


# data preparation -------------------------------------------------------------

## load data that contains timestamps
pass14 <- read.dta13("Daten/raw/Welle_14/dta/PASS-W14_P-Sen_Quer.dta",
                     convert.factors = FALSE) %>%
  clean_names()


## sample restrictions
pass14 <- pass14 %>%
  filter(sub_r_code != 3, # drop senioren
         !(samplepost %in% c(23, 24, 21, 19, 17, 14)), # drop syrisch/irakisch
         !(is.na(samplepost)), # drop observations with missings in samplepost
         sprache == 0, # only german interviews
         is.na(raus), # exclude cases which will be excluded from the data
         is.na(raus2))

## drop falsifiers
pass14 <- pass14 %>%
  filter(!(internr %in% c(300004, 400560)))


## generate durations for questionnaire sections and items
pass14 <- pass14 %>%
  mutate(
    d_experiment = case_when(
      !is.na(zstp_vx_intro2) ~ zstp_vx_intro2 - zstp_vx_intro1,
      !is.na(zstp_vx_outro2) ~ zstp_vx_outro2 - zstp_vx_intro1
    ),
    d_experiment = ifelse(d_experiment <= 0, NA, d_experiment),
    threshold_experiment = ifelse(d_experiment < 203/4, "Faster than 4 WPS", "4 WPS or slower")
  )

## generate method and- recording variables
pass14 <- pass14 %>%
  mutate(
    method = ifelse(methode == 1, "CATI", "CAPI"),
    recording = ifelse(qmitschn == 1, "Recorded", "Not recorded"),
    experiment = case_when(
      vx_intro1 == 1 ~ "Participation",
      vx_intro1 == 7 ~ "No participation"
    )
  ) %>%
  filter(!is.na(d_experiment))

# descriptives -----------------------------------------------------------------

pass14 %>%
  filter(!is.na(vx_intro1)) %>%
  ggplot(aes(x = d_experiment)) +
  geom_histogram(bins = 70) +
  geom_vline(xintercept = 203/4, linetype = "dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10),
                     limits = c(0, 300)) +
  labs(y = "Frequency", x = "Duration for experiment introduction") +
  facet_wrap(~recording + experiment)

pass14_table <- pass14 %>%
  rename(
    "Participation" = experiment,
    "Duration" = threshold_experiment,
    "Recording" = recording
  )

datasummary(`Participation` + `Duration` ~ `Recording` * ( N + Percent("col")), 
            data = pass14_table, 
            title = "Experiment participation by duration and recording, PASS CAPI sample Wave 14. \\label{tab:experiment_durrec}",
            output = "results/tables/experiment_durrec.tex", 
            fmt = "%.2f",
            align = "llllll")


datasummary_crosstab(`Recording` * `Duration` ~ `Participation`, 
            data = pass14_table, 
            title = "Experiment participation by duration and recording crossings, PASS CAPI sample Wave 14. \\label{tab:experiment_cross}",
            output = "results/tables/experiment_cross.tex",
            fmt = "%.2f",
            align = "llllll")



# plot of responses over durations ---------------------------------------------

boxplot(pass14$d_experiment, outline = FALSE)


values <- seq(1,250,1)

response_by_val <- lapply(values, function(x){
  pass14 %>%
    filter(d_experiment <= {{ x }}) %>%
    group_by(experiment, recording) %>%
    summarize(
      sum_part = n()
    ) %>%
    mutate(
      second = {{ x }}
    )
})

response_by_val_df <- do.call(rbind, response_by_val)

response_by_val_df %>%
  ggplot(aes(fill = experiment, x = second, y = sum_part)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.4) +
  labs(x = "Duration for experiment introduction", y = "Count (Duration <= x-axis value)", fill = "Experiment") +
  facet_wrap(~recording, scales = "free_y", nrow = 2)

ggsave("results/cross/dur_experiment.pdf", height = 6, width = 6, device = cairo_pdf)
