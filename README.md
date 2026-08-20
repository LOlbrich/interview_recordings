# Description
This repository contains the R scripts for the Article "Off the record? Effects of interview audio recordings on interviewer behavior". 
The scripts are stored in the subfolder "code". In the following, brief descriptions of each script are provided.

## setup.R
Script that loads the required R libraries, defines ggplot2-themes, and sets a seed. This files is run before each of the other scripts.

## 1_1_panel_preparation.R
Raw data for each panel wave containing the respective timestamp data are loaded separately and appended to obtain a panel dataset. Data preparations and restrictions as described in the article are implemented. The processed data are stored for further analyses.

## 1_2_w14_preparation
Data preparation of the raw wave 14 data with implementations of the descriped data restrictions. The processed data are stored for further analyses.

## 2_1_panel_descriptives.R
Descriptive statistics of the processed panel data (i.e., audio recording consent rates over time, development of very short durations over time by recording status in wave 10, ICCs of audio recording consent).

## 2_2_w14_descriptives.R
Descriptive statistics for the processed wave 14 data (i.e., crosstabs of audio recording consent and interview mode, distributions of durations, ICCs of audio recording consent).

## 3_1_panel_regressions.R
Regression analysis of duration data with visualization of regression coefficients. Re-running the analysis with different outlier thresholds. Assessing raw development of log durations.

## 3_2_w14_regressions.R
Distribution regressions for various questionnaire section durations. Visualization of the effects.  

## 4_1_1_w14_icc.R
ICCs for various outcomes for the wave 14 data.

## 4_1_2_w14_experiment.R
Analysis of duration data and participation outcomes for experiment implemented in wave 14.

## 4_2_1_panel_outcomes.R
Regression analysis of effect of audio recrding consent on questionnaire items.

## 4_2_2_panel_balance.R
Assessment of the balance of the socio-demographic composition of non-consenting and consenting respondents.

## 4_2_3_panel_followup.R
Descriptive analysis of changes in audio recording consent in the year after the introduction of audio recording consent.
