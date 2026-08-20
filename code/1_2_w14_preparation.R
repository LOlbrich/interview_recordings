# Title: data preparation of w14 data

# run setup file ---------------------------------------------------------------

source("code/setup.R")


# data preparation -------------------------------------------------------------

## load data that contains timestamps
pass14 <- read.dta13("Daten/raw/Welle_14/PASS-W14_P-Sen_Quer.dta",
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

## generate method and- recording variables
pass14 <- pass14 %>%
  mutate(
    method = ifelse(methode == 1, "CATI", "CAPI"),
    recording = ifelse(qmitschn == 1, "Yes", "No"),
    date = make_date(year = int_j, month = int_m, day = int_t),
    week = week(date),
    month = dense_rank(month(date))
  )


## generate durations for questionnaire sections and items
pass14 <- pass14 %>%
  mutate(
    d_social_trust = case_when(
      !is.na(zstp_pcv0100) ~ zstp_pcv0100 - zstp_pa2000,
      is.na(zstp_pcv0100) ~ zstp_pb0100 - zstp_pa2000
    ),
    d_social_trust = ifelse(d_social_trust <= 0, NA, d_social_trust),
    d_att_life = zstp_peo0100 - zstp_pa0100,
    d_att_life = ifelse(d_att_life <= 0, NA, d_att_life),
    d_att_self = zstp_pa2000 - zstp_peo0100,
    d_att_self = ifelse(d_att_self <= 0, NA, d_att_self),
    d_role_model = zstp_ptk0200 - zstp_peo0400,
    d_role_model = ifelse(d_role_model <= 0, NA, d_role_model),
    d_social_part = zstpppt0100 - zstp_pa0800,
    d_social_part = ifelse(d_social_part <= 0, NA, d_social_part),
    d_politics = zstpppt0200 - zstpppt0100,
    d_politics = ifelse(d_politics <= 0, NA, d_politics),
    d_democracy = zstpppt0400 - zstpppt0300,
    d_democracy = ifelse(d_democracy <= 0, NA, d_democracy),
    d_leftright = zstp_psk0100 - zstpppt0400,
    d_leftright = ifelse(d_leftright <= 0, NA, d_leftright),
    d_activity = case_when(
      !is.na(zstp_psk0400) ~ zstp_psk0410 - zstp_psk0400,
      is.na(zstp_psk0400) ~ zstp_psk0410 - zstp_psk0300
    ),
    d_activity = ifelse(d_activity <= 0, NA, d_activity),
    d_leisure = case_when(
      !is.na(zstp_psk0600) ~ zstp_pla0100 - zstp_psk0600,
      is.na(zstp_psk0600) ~ zstp_pla0100 - zstp_psk0410
    ),
    d_leisure = ifelse(d_leisure <= 0, NA, d_leisure),
    d_func_work = zstp_pd0200 - zstp_pla0100,
    d_func_work = ifelse(d_func_work <= 0, NA, d_func_work),
    d_insurance = zstp_pp0110 - zstp_pg1300,
    d_insurance = ifelse(d_insurance <= 0, NA, d_insurance),
    d_satisfaction = zstp_vx_intro1 - zstp_pa1000,
    d_satisfaction = ifelse(d_satisfaction <= 0, NA, d_satisfaction),
    d_experiment = case_when(
      !is.na(zstp_vx_intro2) ~ zstp_vx_intro2 - zstp_vx_intro1,
      !is.na(zstp_vx_outro2) ~ zstp_vx_outro2 - zstp_vx_intro1
    ),
    d_experiment = ifelse(d_experiment <= 0, NA, d_experiment)
  )

# check numer of zero durations for life satisfaction and social participation
# check zero durations
pass14 %>%
  summarise(
    na_social = mean(zstpppt0100 - zstp_pa0800 == 0, na.rm = TRUE),
    na_satisfaction = mean(zstp_vx_intro1 - zstp_pa1000 == 0, na.rm = TRUE)
  )

## prepare nondifferentiation --------------------------------------------------

pass14 <- pass14 %>%
  mutate(
    attitude_1 = ifelse(peo0100a < 8, peo0100a, NA),
    attitude_2 = ifelse(peo0100b < 8, peo0100b, NA),
    attitude_3 = ifelse(peo0100c < 8, peo0100c, NA),
    attitude_4 = ifelse(peo0100d < 8, peo0100d, NA),
    attitude_5 = ifelse(peo0100e < 8, peo0100e, NA),
    role_model_1 = ifelse(peo0400a < 8, peo0400a, NA),
    role_model_2 = ifelse(peo0400b < 8, peo0400b, NA),
    role_model_3 = ifelse(peo0400c < 8, peo0400c, NA),
    role_model_4 = ifelse(peo0400d < 8, peo0400d, NA),
    func_work_1 = ifelse(pla0100a < 98, pla0100a, NA),
    func_work_2 = ifelse(pla0100b < 98, pla0100b, NA),
    func_work_3 = ifelse(pla0100c < 98, pla0100c, NA),
    func_work_4 = ifelse(pla0100d < 98, pla0100d, NA),
    func_work_5 = ifelse(pla0100e < 98, pla0100e, NA),
    func_work_6 = ifelse(pla0100f < 98, pla0100f, NA),
    func_work_7 = ifelse(pla0100g < 98, pla0100g, NA),
    func_work_8 = ifelse(pla0100h < 98, pla0100h, NA),
    func_work_9 = ifelse(pla0100i < 98, pla0100i, NA),
    func_work_10 = ifelse(pla0100j < 98, pla0100j, NA),
    func_work_11 = ifelse(pla0100k < 98, pla0100k, NA),
    func_work_12 = ifelse(pla0100l < 98, pla0100l, NA),
    func_work_13 = ifelse(pla0100m < 98, pla0100m, NA),
    func_work_14 = ifelse(pla0100n < 98, pla0100n, NA),
    func_work_15 = ifelse(pla0100o < 98, pla0100o, NA),
    func_work_16 = ifelse(pla0100p < 98, pla0100p, NA),
    func_work_17 = ifelse(pla0100q < 98, pla0100q, NA),
    func_work_18 = ifelse(pla0100r < 98, pla0100r, NA)
  )

pass14$nd_attitude <- apply(pass14[, c("attitude_1",
                                       "attitude_2",
                                       "attitude_3",
                                       "attitude_4",
                                       "attitude_5")], 1, sd, na.rm = TRUE)

pass14$nd_role_model <- apply(pass14[, c("role_model_1",
                                       "role_model_2",
                                       "role_model_3",
                                       "role_model_4")], 1, sd, na.rm = TRUE)

pass14$nd_func_work <- apply(pass14[, c("func_work_1",
                                        "func_work_2",
                                        "func_work_3",
                                        "func_work_4",
                                        "func_work_5",
                                        "func_work_6",
                                        "func_work_7",
                                        "func_work_8",
                                        "func_work_9",
                                        "func_work_10",
                                        "func_work_11",
                                        "func_work_12",
                                        "func_work_13",
                                        "func_work_14",
                                        "func_work_15",
                                        "func_work_16",
                                        "func_work_17",
                                        "func_work_18")], 1, sd, na.rm = TRUE)
## keep only relevant variables
pass14 <- pass14 %>%
  select(pnr, internr, samplepost, method:nd_func_work, vx_intro1, pa1000, pa0800, pa0900)


## load pass data that contains more info on covariates
if (TRUE) {
  pass_penddat14 <-
    read.dta13(
      "Daten/raw/PASS/PENDDAT.dta",
      convert.factors = FALSE
    ) %>%
    clean_names()
  
  ## generate variable on total participations in PASS
  pass_penddat14 <- pass_penddat14 %>%
    group_by(pnr) %>%
    mutate(n_pnr = n()) %>%
    ungroup() %>%
    filter(welle == 14)
  
  ## generate covariates
  pass_penddat14 <- pass_penddat14 %>%
    mutate(
      gender = as.factor(ifelse(zpsex == 1, "Male", "Female")),
      age = zpalthh,
      age_cat = as.factor(case_when(
        age >= 15 & age <= 30 ~ "15-30",
        age >= 31 & age <= 40 ~ "31-40",
        age >= 41 & age <= 50 ~ "41-50",
        age >= 51 & age <= 60 ~ "51-60",
        age >= 61 ~ "over 60"
      )),
      german = ifelse(pmi0400 == 1, "Yes", "No"),
      german = ifelse(pmi0400 < 0, NA, german),
      education = bilzeit,
      unemployed = as.factor(case_when(
        alakt == 1 ~ "Yes",
        alakt == 2 ~ "No",
        alakt == -3 & statakt %in% c(3,4) ~ "No",
        TRUE ~ NA_character_
      ))
    ) %>%
    select(pnr, gender, age, age_cat, german, education, n_pnr, pintmod, unemployed)
  
  ## save data to speed up loading the data
  saveRDS(pass_penddat14, file = "data/pass_penddat14")
  
}

## load penddat data
pass_penddat14 <- readRDS(file = "data/pass_penddat14")

## merge with data that contains timestamps
pass14 <- pass14 %>%
  left_join(pass_penddat14, by = c("pnr"))

## drop falsifiers
pass14 <- pass14 %>%
  filter(!(internr %in% c(300004, 400560)))

# generate mode variable
pass14 <- pass14 %>%
  mutate(
    method_covid = case_when(
      pintmod == 0 ~ "CATI",
      pintmod == 1 ~ "CAPI, f2f",
      pintmod == 2 ~ "CAPI, tel."
    ),
    method_covid = fct_relevel(method_covid, "CATI", "CAPI, f2f", "CAPI, tel."),
    recording = as.factor(recording)
  )
 
table(pass14$method_covid, pass14$method, useNA = "always")

# 9 missings --> exclude from analysis
pass14 <- pass14 %>%
  filter(!(is.na(method_covid)))

## check for missings in control variables
table(pass14$gender, useNA = "always")
table(pass14$age, useNA = "always")
table(pass14$german, useNA = "always")
table(pass14$month, useNA = "always")
table(pass14$n_pnr, useNA = "always")
table(pass14$unemployed, useNA = "always")

pass14 <- pass14 %>%
  filter(
    !is.na(german), # 4 missings for german citizenship, exclude from analysis
    !is.na(unemployed) # three missings, exclude from analysis
  )

## recode education variable
table(pass14$education, useNA = "always")

# drop if there is no info on years of education (-4 and -2)
pass14 <- pass14 %>%
  filter(!(education %in% c(-4, -2)))

# for those who are still in school (-5) we impute age - 6 as years of education
pass14 <- pass14 %>%
  mutate(
    education = ifelse(education == -5, age - 6, education)
  )

# check if everything is fine
table(pass14$education)


## number of interviews per interviewer
pass14 <- pass14 %>%
  group_by(internr, method_covid) %>%
  mutate(
    int_count = n()
  ) %>%
  ungroup()


# save data
write_dta(pass14, "Daten/processed/pass14.dta")
saveRDS(pass14, file = "Daten/processed/pass14.rds")


