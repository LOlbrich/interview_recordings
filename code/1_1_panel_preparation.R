# Title: preparation of panel data

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

## wave 7 ----
pass7 <- read.dta13("Daten/raw/Welle_7/PASS-W7_P-Sen_Quer_anonym.dta",
                    convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstp41, zstp42, zstp26, zstp27, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, scrcnt) %>%
  mutate(welle = 7) %>%
  rename(
    sat_before = zstp41,
    sat_after = zstp42,
    social_before = zstp26,
    social_after = zstp27,
    SCRCNT = scrcnt
  )

## wave 8 ----
pass8 <- read.dta13("Daten/raw/Welle_8/PASS-W8_P-Sen_Quer_anonym.dta",
                    convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstp40, zstp41, zstp26, zstp27, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 8) %>%
  rename(
    sat_before = zstp40,
    sat_after = zstp41,
    social_before = zstp26,
    social_after = zstp27
  )

## wave 9 ----
pass9 <- read.dta13("Daten/raw/Welle_9/PASS-W9_P-Sen_Quer_anonym.dta",
                    convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstp46, zstp47, zstp30, zstp30a, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 9) %>%
  rename(
    sat_before = zstp46,
    sat_after = zstp47,
    social_before = zstp30,
    social_after = zstp30a
  )

## wave 10 ----
pass10 <- read.dta13("Daten/raw/Welle_10/PASS-W10_P-Sen_Quer_anonym.dta",
                     convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstpPA1000, zstpend, zstpPML0100, zstpPA0800, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 10) %>%
  rename(
    sat_before = zstpPA1000,
    sat_after = zstpend,
    social_before = zstpPA0800,
    social_after = zstpPML0100
  )

## wave 11 ----
pass11 <- read.dta13("Daten/raw/Welle_11/PASS-W11_P-Sen_Quer_anonym.dta",
                     convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstpPA1000, zstpend, zstpPSK0100, zstpPA0800, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, scrcnt) %>%
  mutate(welle = 11) %>%
  rename(
    sat_before = zstpPA1000,
    sat_after = zstpend,
    social_before = zstpPA0800,
    social_after = zstpPSK0100,
    SCRCNT = scrcnt
  )

## wave 12 ----
pass12 <- read.dta13("Daten/raw/Welle_12/PASS-W12_P-Sen_Quer_anonym.dta",
                     convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstpPA1000, zstpend, zstpPSK0100, zstpPA0800, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 12) %>%
  rename(
    sat_before = zstpPA1000,
    sat_after = zstpend,
    social_before = zstpPA0800,
    social_after = zstpPSK0100
  )

## wave 13 ----
pass13 <- read.dta13("Daten/raw/Welle_13/dta/PASS-W13_P-Sen_Quer_anonym.dta",
                     convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstpPA1000, zstpend, zstpppt0100, zstpPA0800, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 13) %>%
  rename(
    sat_before = zstpPA1000,
    sat_after = zstpend,
    social_before = zstpPA0800,
    social_after = zstpppt0100
  )

## wave 14 ----
pass14 <- read.dta13("Daten/raw/Welle_14/dta/PASS-W14_P-Sen_Quer.dta",
                     convert.factors = FALSE) %>%
  select(pnr, lfd, tsprache, subRCode, caticapi, qmitschn, zstpPA1000, zstpVXIntro1, zstpppt0100, zstpPA0800, internr, raus, raus2, int_talt, int_malt, int_jalt, PA1000, PA0800, PA0900, SCRCNT) %>%
  mutate(welle = 14) %>%
  rename(
    sat_before = zstpPA1000,
    sat_after = zstpVXIntro1,
    social_before = zstpPA0800,
    social_after = zstpppt0100
  )

## PENDDAT ----

# load data
pass_penddat <-
  read.dta13(
    "Daten/raw/PASS/PENDDAT.dta",
    convert.factors = FALSE
  )

# interviewer experience in pass
pass_penddat <- pass_penddat %>%
  group_by(pintnum) %>%
  mutate(
    int_experience = dense_rank(welle)
  ) %>%
  ungroup()


# number of participations
pass_penddat <- pass_penddat %>%
  group_by(pnr) %>%
  mutate(
    n_pnr = sum(welle <= 10)
  ) %>%
  ungroup()

# keep only waves 7 or later
pass_penddat <- pass_penddat %>%
  filter(welle >= 7)

# generate control variables
pass_penddat <- pass_penddat %>%
mutate(
  gender = ifelse(zpsex == 1, "Male", "Female"),
  age = zpalthh,
  age_cat = case_when(
    age >= 15 & age <= 30 ~ "15-30",
    age >= 31 & age <= 40 ~ "31-40",
    age >= 41 & age <= 50 ~ "41-50",
    age >= 51 & age <= 60 ~ "51-60",
    age >= 61 ~ "over 60"
  ),
  german = ifelse(PMI0400 == 1, "Yes", "No"),
  german = ifelse(PMI0400 < 0, NA, german),
  education = bilzeit,
  unemployed = case_when(
    alakt == 1 ~ "Yes",
    alakt == 2 ~ "No",
    alakt == -3 & statakt %in% c(3,4) ~ "No",
    TRUE ~ NA_character_
  ))

# select relevant variables
pass_penddat <- pass_penddat %>%
  select(pnr, welle, pintmod, sample, gender, age, age_cat, german, education, unemployed, n_pnr, RegP0100, int_experience, PET0510, PSK0100, PSK0200, PP0110, PA0100, PA0200, PA0300, PSK0400a, PSK0400b, PSK0400c, PSK0400d, PG0500, PG0100)



# data preparation -------------------------------------------------------------

## binding and merging ----

# bind dataframes containing timestamp data
pass <- bind_rows(pass7, pass8, pass9, pass10, pass11, pass12, pass13, pass14)

# merge with published penddat data
pass <- pass %>%
  left_join(pass_penddat, by = c("welle", "pnr"))

## sample selection ----

# filter observations
pass <- pass %>%
  filter(subRCode != 3, # keep only person interviews
         tsprache == 1, # keep only german interviews
         !(internr %in% c(300004, 400560)), # drop falsifiers
         is.na(raus), # drop observations that will be dropped from final data
         is.na(raus2), # drop observations that will be dropped from final data
         !(sample %in% c(23, 24, 21, 19, 17, 14))) # drop Syrian/Iraqi households

## check mode and recording coding

table(pass$pintmod, useNA = "always")
table(pass$pintmod, pass$caticapi, useNA = "always")

table(pass$qmitschn, useNA = "always")


# pintmod is missing for 53 observations, we exclude those as they are nor part of the published data
pass <- pass %>%
  filter(!is.na(pintmod))


## variable preparation ----


# calculate durations and generate recordings and mode variables
pass <- pass %>%
  mutate(
    d_satisfaction = sat_after - sat_before,
    d_satisfaction = ifelse(d_satisfaction <= 0, NA, d_satisfaction),
    log_satisfaction = log(d_satisfaction),
    d_social = social_after - social_before,
    d_social = ifelse(d_social <= 0, NA, d_social),
    log_social = log(d_social),
    threshold_sat = ifelse(d_satisfaction < 11.5, 1, 0), # 46 words
    threshold_sat = ifelse(is.na(d_satisfaction), NA, threshold_sat),
    threshold_social = case_when(
      d_social < 30.25 & welle >= 12 ~ 1, # 121 words from wave 12 on
      d_social < 33.25 & welle < 12 ~ 1, # 133 before that
      TRUE ~ 0
    ), 
    threshold_social = ifelse(is.na(d_social), NA, threshold_social),
    recording = ifelse(qmitschn == 1, "Yes", "No"),
    int_mode = case_when(
      pintmod == 0 ~ "CATI",
      pintmod %in% c(1,2) ~ "CAPI"
    ) 
  )



# prepare date variables
pass <- pass %>%
  mutate(
    day = int_talt,
    month = int_malt,
    year = int_jalt,
    date = make_date(year = year, month = month, day = day),
    week = week(date)
  ) %>%
  group_by(welle) %>%
  mutate(
    week_rank = as.numeric(factor(rank(week))),
    month = dense_rank(month)
  )


# prepare variable on social participation, status and life satisfaction
pass <- pass %>%
  mutate(
    participation = ifelse(PA0800 < 98, PA0800, NA),
    status = ifelse(PA0900 < 98, PA0900, NA),
    satisfaction = ifelse(PA1000 < 98, PA1000, NA),
    participation_ers = case_when(
      participation %in% c(1,2,9,10) ~ 1,
      participation %in% c(3,4,5,6,7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    status_ers = case_when(
      status %in% c(1,2,9,10) ~ 1,
      status %in% c(3,4,5,6,7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    satisfaction_ers = case_when(
      satisfaction %in% c(0,1,9,10) ~ 1,
      satisfaction %in% c(2,3,4,5,6,7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    participation_mrs = case_when(
      participation %in% c(4,5,6,7) ~ 1,
      participation %in% c(1,2,3,8,9,10) ~ 0,
      TRUE ~ NA_real_
    ),
    status_mrs = case_when(
      status %in% c(4,5,6,7) ~ 1,
      status %in% c(1,2,3,8,9,10) ~ 0,
      TRUE ~ NA_real_
    ),
    satisfaction_mrs = case_when(
      satisfaction %in% c(4,5,6) ~ 1,
      satisfaction %in% c(0,1,2,3,7,8,9,10) ~ 0,
      TRUE ~ NA_real_
    )
  )

# prepare covariates

## recode education variable
table(pass$education, useNA = "always")


# for those who are still in school (-5) we impute age - 6 as years of education
pass <- pass %>%
  mutate(
    education = case_when(
      education == -5 ~ age - 6, # for those who are still in school (-5) we impute age - 6 as years of education
      education %in% c(-4, -2, -1) ~ NA_real_, # recode missings to NA,
      education > 0 ~ education
  ))

# check if everything is fine
table(pass$education)


table(pass$unemployed, useNA = "always")


# clear memory
rm(pass7, pass8, pass9, pass10, pass11, pass12, pass13, pass14, pass_penddat)


# export data ------------------------------------------------------------------

# store full panel df
saveRDS(pass, "Daten/processed/pass_panel.rds")

# prepare and store data for event study analysis

# select capi data for waves 7 to 10
pass7_10 <- pass %>%
  filter(welle < 11, int_mode == "CAPI")

# generate treatment variable (respondents who consented to recording in w10)
pass7_10 <- pass7_10 %>%
  group_by(pnr) %>%
  mutate(
    recorded_w10 = mean(qmitschn)
  ) %>%
  ungroup() %>%
  mutate(
    treatment = ifelse(recorded_w10 > 0, TRUE, FALSE),
    welle = as.factor(welle),
    time_to_treat = case_when(
      welle == 7 & treatment ~ 7,
      welle == 8 & treatment ~ 8,
      welle == 9 & treatment ~ 9,
      welle == 10 & treatment ~ 10,
      TRUE ~ 10 # arbitrary value for control group
    ),
    time_to_treat = as.factor(time_to_treat)
  )

# check coding
table(pass7_10$treatment, pass7_10$time_to_treat)
table(pass7_10$welle)

# check zero durations
pass7_10 %>%
  group_by(welle) %>%
  summarise(
    na_social = sum(social_after - social_before == 0, na.rm = TRUE),
    na_satisfaction = sum(sat_after - sat_before == 0, na.rm = TRUE)
  )

# prepare separate dfs for life satisfaction and social participation
pass_sat <- pass7_10 %>%
  filter(!is.na(d_satisfaction)) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

# keep only cases with no interviewer change
pass_sat <- pass_sat %>%
  group_by(pnr) %>%
  mutate(
    n_interviewers = n_distinct(internr)
  ) %>%
  ungroup() %>%
  filter(n_interviewers == 1)

# german is missing for one observations which responded with "Yes" in all other years
table(is.na(pass_sat$german))
pass_sat <- pass_sat %>%
  mutate(
    german = ifelse(is.na(german), "Yes", german)
  )

table(pass_sat$welle, pass_sat$treatment)

# store satisfaction panel data
saveRDS(pass_sat, "Daten/processed/pass_sat.rds")



pass_social <- pass7_10 %>%
  filter(!is.na(d_social)) %>%
  group_by(pnr) %>%
  filter(n() == 4) %>%
  ungroup()

# keep only cases with no interviewer change
pass_social <- pass_social %>%
  group_by(pnr) %>%
  mutate(
    n_interviewers = n_distinct(internr)
  ) %>%
  ungroup() %>%
  filter(n_interviewers == 1)

# german is missing for one observations which responded with "Yes" in all other years
table(is.na(pass_social$german))
pass_social <- pass_social %>%
  mutate(
    german = ifelse(is.na(german), "Yes", german)
  )

table(pass_social$welle, pass_social$treatment)

# store social participation panel data
saveRDS(pass_social, "Daten/processed/pass_social.rds")

# clear memory
rm(list = ls())

