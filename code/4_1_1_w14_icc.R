# Title: calculation of iccs for w14 data

# run setup file ---------------------------------------------------------------

source("code/setup.R")

# load data --------------------------------------------------------------------

# pass wave 14
pass14 <- readRDS("Daten/processed/pass14.rds")


# prepare variables ------------------------------------------------------------

pass14 <- pass14 %>%
  mutate(
    participation = ifelse(pa0800 < 98, pa0800, NA),
    status = ifelse(pa0900 < 98, pa0900, NA),
    satisfaction = ifelse(pa1000 < 98, pa1000, NA),
  )

# subset data ------------------------------------------------------------------

pass_capi_rec <- filter(pass14, method_covid != "CATI" & recording == "Yes")
pass_capi_norec <- filter(pass14, method_covid != "CATI" & recording == "No")
pass_cati_rec <- filter(pass14, method_covid == "CATI" & recording == "Yes")
pass_cati_norec <- filter(pass14, method_covid == "CATI" & recording == "No")

# calculate averages of outcome variables --------------------------------------

vars <- c("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work")

pass_capi_rec %>%
  summarize(across("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work", list(mean)))

pass_capi_rec %>%
  summarize(across(.cols = c("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work"), mean, na.rm = TRUE))
pass_capi_norec %>%
  summarize(across(.cols = c("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work"), mean, na.rm = TRUE))
pass_cati_rec %>%
  summarize(across(.cols = c("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work"), mean, na.rm = TRUE))
pass_cati_norec %>%
  summarize(across(.cols = c("participation", "status", "satisfaction", "nd_attitude", "nd_role_model", "nd_func_work"), mean, na.rm = TRUE))


pass_capi_rec %>%
  summarize(across(.cols = c("nd_attitude", "nd_role_model", "nd_func_work"), function(x) mean(x == 0, na.rm = TRUE)))
pass_capi_norec %>%
  summarize(across(.cols = c("nd_attitude", "nd_role_model", "nd_func_work"), function(x) mean(x == 0, na.rm = TRUE)))
pass_cati_rec %>%
  summarize(across(.cols = c("nd_attitude", "nd_role_model", "nd_func_work"), function(x) mean(x == 0, na.rm = TRUE)))
pass_cati_norec %>%
  summarize(across(.cols = c("nd_attitude", "nd_role_model", "nd_func_work"), function(x) mean(x == 0, na.rm = TRUE)))


# fit models -------------------------------------------------------------------

## participation ----

part_capi_rec <- lmer(participation ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                      data = pass_capi_rec)
part_capi_norec <- lmer(participation ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_capi_norec)
part_cati_rec <- lmer(participation ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                      data = pass_cati_rec)
part_cati_norec <- lmer(participation ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_cati_norec)


## status ----

status_capi_rec <- lmer(status ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                 data = pass_capi_rec)
status_capi_norec <- lmer(status ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                    data = pass_capi_norec)
status_cati_rec <- lmer(status ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                data = pass_cati_rec)
status_cati_norec <- lmer(status ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_cati_norec)


## satisfaction ----

sat_capi_rec <- lmer(satisfaction ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_capi_rec)
sat_capi_norec <- lmer(satisfaction ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                          data = pass_capi_norec)
sat_cati_rec <- lmer(satisfaction ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_cati_rec)
sat_cati_norec <- lmer(satisfaction ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                          data = pass_cati_norec)

## nondifferentiation attitude ----

att_capi_rec <- lmer(nd_attitude ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                     data = pass_capi_rec)
att_capi_norec <- lmer(nd_attitude ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                       data = pass_capi_norec)
att_cati_rec <- lmer(nd_attitude ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                     data = pass_cati_rec)
att_cati_norec <- lmer(nd_attitude ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                       data = pass_cati_norec)

## nondifferentiation role model ----

role_capi_rec <- lmer(nd_role_model ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                     data = pass_capi_rec)
role_capi_norec <- lmer(nd_role_model ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                       data = pass_capi_norec)
role_cati_rec <- lmer(nd_role_model ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                     data = pass_cati_rec)
role_cati_norec <- lmer(nd_role_model ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                       data = pass_cati_norec)

## nondifferentiation functions of work ----

func_capi_rec <- lmer(nd_func_work ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                      data = pass_capi_rec)
func_capi_norec <- lmer(nd_func_work ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_capi_norec)
func_cati_rec <- lmer(nd_func_work ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                      data = pass_cati_rec)
func_cati_norec <- lmer(nd_func_work ~ 1 + age + I((age^2)/100) + gender + german + education + month + n_pnr + unemployed + (1|internr),
                        data = pass_cati_norec)


# generate table of results ----------------------------------------------------

# function to get icc and RLRT statistic
icc_rlrt <- function(fit){
  if(is.na(icc(fit)[[1]])){
    "N/E"
  } else{
    paste0(format(round(icc(fit)[[1]], 3), nsmall = 3), " (", 
           format(round(exactRLRT(fit, seed = 123)$p, 3), nsmall = 3),
           ")")
  }
}

results_icc <- tibble(
  var = c("Part of society", "Position in society", "Life satisfaction", "ND attitude to self", "ND role model", "ND functions of work"),
  cati_rec = c(
    icc_rlrt(part_cati_rec), icc_rlrt(status_cati_rec), icc_rlrt(sat_cati_rec), icc_rlrt(att_cati_rec), icc_rlrt(role_cati_rec), icc_rlrt(func_cati_rec)
  ),
  cati_norec = c(
    icc_rlrt(part_cati_norec), icc_rlrt(status_cati_norec), icc_rlrt(sat_cati_norec), icc_rlrt(att_cati_norec), icc_rlrt(role_cati_norec), icc_rlrt(func_cati_norec)
  ),
  capi_rec = c(
    icc_rlrt(part_capi_rec), icc_rlrt(status_capi_rec), icc_rlrt(sat_capi_rec), icc_rlrt(att_capi_rec), icc_rlrt(role_capi_rec), icc_rlrt(func_capi_rec)
  ),
  capi_norec = c(
    icc_rlrt(part_capi_norec), icc_rlrt(status_capi_norec), icc_rlrt(sat_capi_norec), icc_rlrt(att_capi_norec), icc_rlrt(role_capi_norec), icc_rlrt(func_capi_norec)
  )
)

colnames(results_icc) <- c("Variable", "CATI, recorded", "CATI, not rec.", "CAPI, recorded", "CAPI, not rec.")


print(xtable(results_icc,
             caption = "ICCs by mode and recording.",
             label = "tab:icc_mode_rec"), 
      type = "latex",
      file = "results/tables/icc_mode_rec.tex",
      caption.placement = "top",
      size = "\\fontsize{10pt}{11pt}\\selectfont",
      include.rownames = FALSE,
      booktabs = TRUE,
      hline.after = c(-1,0),
      add.to.row = list(pos = list(nrow(results_icc)),
                        command = paste0("\\hline \n \\multicolumn{5}{l}{Notes: p-values in parentheses are based on restricted likelihood ratio test of interviewer variance.} \\\\ \\multicolumn{5}{l}{ND = Nondifferentiation. N/E = Model did not converge.}")))

