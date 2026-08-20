# Title: assess composition of recorded and nonrecorded interviews in panel data

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

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


# compare composition of samples -----------------------------------------------

pass_social10 <- pass_social %>%
  filter(welle == 10) %>%
  select("Age" = age, "Gender" = gender,  "German" = german, "Education" = education, "Month in field period" = month, "Panel experience" = n_pnr, "Unemployed" = unemployed, recording, recording) %>%
  mutate(
    recording = paste0("Recorded: ", recording)
  )

pass_sat10 <- pass_sat %>%
  filter(welle == 10) %>%
  select("Age" = age, "Gender" = gender,  "German" = german, "Education" = education, "Month in field period" = month, "Panel experience" = n_pnr, "Unemployed" = unemployed, recording, recording) %>%
  mutate(
    recording = paste0("Recorded: ", recording)
  )

datasummary_balance(~recording,
                    data = pass_social10,
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w10_social.tex",
                    title = "Social participation sample characteristics by audio recording, wave 10. \\label{tab:balanced_w10_social}")

datasummary_balance(~recording,
                    data = pass_sat10,
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w10_sat.tex",
                    title = "Life satisfaction sample characteristics by audio recording, wave 10. \\label{tab:balanced_w10_sat}")




# load data --------------------------------------------------------------------

pass14 <- readRDS("Daten/processed/pass14.rds")

## selection analysis ----------------------------------------------------------

pass14 <- pass14 %>%
  select("Age" = age, "Gender" = gender,  "German" = german, "Education" = education, "Month in field period" = month, "Panel experience" = n_pnr, "Unemployed" = unemployed, recording, method_covid) %>%
  mutate(
    recording = paste0("Recorded: ", recording)
  )

datasummary_balance(~ method_covid,
                    data = subset(pass14, select = -c(recording)),
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w14_modes.tex",
                    title = "Covariates by modes, wave 14.")


datasummary_balance(~recording,
                    data = subset(pass14, method_covid == "CATI", select = -c(method_covid)),
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w14_CATI.tex",
                    title = "Covariates by audio recording, CATI, wave 14.")     

datasummary_balance(~recording,
                    data = subset(pass14, method_covid == "CAPI, f2f", select = -c(method_covid)),
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w14_CAPIf2f.tex",
                    title = "Covariates by audio recording, CAPI, f2f, wave 14.")    

datasummary_balance(~recording,
                    data = subset(pass14, method_covid == "CAPI, tel.", select = -c(method_covid)),
                    fmt = "%.2f",
                    dinm = FALSE,
                    output = "results/tables/balanced_w14_CAPItel.tex",
                    title = "Covariates by audio recording, CAPI, tel, wave 14.")    
                
