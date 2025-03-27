
# Load packages and libraries


# Libraries

library(APCG1)
library(APCtools)
library(APCI)
library(apc)
library(bamp)
library(StanMoMo)
library(readxl)
library(summarytools)
library(gtsummary)
library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(ggthemr)
library(readxl)

# 1. Load data bases

# Load main data base

bd_long_dm <- read_excel("bd_long_dm.xlsx")

# General data base

# Lexis table database frequency (Count of Cases)

bd_lexis_f_dm <- read_xlsx("TABLA_LEXIS_FRECUENCIA_DM.xlsx")

# Lexis table database for general population  

bd_lexis_p_dm <- read_xlsx("TABLA_LEXIS_POBLACION_GENERAL.xlsx")

# Lexis table database for diabetes mortality rate

bd_lexis_t_dm <- read_xlsx("TABLA_LEXIS_TASA_DM.xlsx")

################################################################################

# Databases for diabetes mellitus by male sex

# Load main data base 

bd_long_dm_m <- read_xlsx("bd_long_dm_m.xlsx")

# Lexis Table for frequency (Count of Cases)

bd_lexis_f_dm_m <- read_xlsx("TABLA_LEXIS_FRECUENCIA_DM_M.xlsx")

# Lexis table database for general population

bd_lexis_p_dm_m <- read_xlsx("TABLA_LEXIS_POBLACION_DM_M.xlsx")

# Lexis table database for diabetes mortality rate

bd_lexis_t_dm_m <- read_xlsx("TABLA_LEXIS_TASA_DM_M.xlsx")


################################################################################

# Databases for diabetes mellitus by female sex


# Load main data base 

bd_long_dm_m <- read_xlsx("bd_long_dm_m.xlsx")

# Lexis Table for frequency (Count of Cases)

bd_lexis_f_dm_m <- read_xlsx("TABLA_LEXIS_FRECUENCIA_DM_M.xlsx")

# Lexis table database for general population

bd_lexis_p_dm_m <- read_xlsx("TABLA_LEXIS_POBLACION_DM_M.xlsx")

# Lexis table database for diabetes mortality rate

bd_lexis_t_dm_m <- read_xlsx("TABLA_LEXIS_TASA_DM_M.xlsx")


# GENERAL ANALYSIS

# 2. Simple quasi-poisson regression models

## Lexis tables in age and period format with respective labels.

bd_lexis_f_dm_a_p <- apcheader(r=bd_lexis_f_dm, 
                               agestart=0, 
                               yearstart=1983, 
                               agespan=5, 
                               yearspan=5, 
                               head=F)

#

bd_lexis_p_dm_a_p <- apcheader(r=bd_lexis_p_dm, 
                               agestart=0, 
                               yearstart=1983, 
                               agespan=5, 
                               yearspan=5, 
                               head=F)

#
bd_lexis_t_dm_a_p <- apcheader(r=bd_lexis_t_dm, 
                               agestart=0, 
                               yearstart=1983, 
                               agespan=5, 
                               yearspan=5, 
                               head=F)

# 3. Simple models

# Simple model of the effect of AGE, as an independent variable, 
# on the expected cases of diabetes mellitus, as a dependent variable, 
# and the exposed population, as a compensation term:


model_A <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="A", fam="qlik", Plot=T)
model_A
str(model_A)


# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_A <- model_A$parameter[, "Estimate"]
std_errors_model_A <- model_A$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_A <- parameters_model_A - 1.96 * std_errors_model_A
conf_int_upper_model_A <- parameters_model_A + 1.96 * std_errors_model_A

# Show confidence intervals
conf_int_model_A <- data.frame(lower = conf_int_lower_model_A, upper = conf_int_upper_model_A)
conf_int_model_A

# Exponentiate regression coeffcients

coef_exp_model_A <- exp(model_A$parameter[, "Estimate"])
coef_exp_model_A <- round(coef_exp_model_A, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_A <- exp(model_A$parameter[, "Estimate"] - 1.96 * model_A$parameter[, "Std. Error"])
conf_int_lower_exp_model_A <- round(conf_int_lower_exp_model_A, 2)

conf_int_upper_exp_model_A <- exp(model_A$parameter[, "Estimate"] + 1.96 * model_A$parameter[, "Std. Error"])
conf_int_upper_exp_model_A <- round(conf_int_upper_exp_model_A, 2)

# Create a data frame with the exponential results

results_exp_model_A <- data.frame(Rate_Ratio = coef_exp_model_A,
                                  lower = conf_int_lower_exp_model_A,
                                  upper = conf_int_upper_exp_model_A)

# Results

results_exp_model_A

# Simple model of the effect of the PERIOD, as an independent variable, on the expected cases of 
# death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:

model_P <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="P", fam="qlik", Plot=T)
model_P
str(model_P)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_P <- model_P$parameter[, "Estimate"]
std_errors_model_P <- model_P$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_P <- parameters_model_P - 1.96 * std_errors_model_P
conf_int_upper_model_P <- parameters_model_P + 1.96 * std_errors_model_P

# Show confidence intervals

conf_int_model_P <- data.frame(lower = conf_int_lower_model_P, upper = conf_int_upper_model_P)
conf_int_model_P

# Exponentiate regression coeffcients

coef_exp_model_P <- exp(model_P$parameter[, "Estimate"])
coef_exp_model_P <- round(coef_exp_model_P, 2)


# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_P <- exp(model_P$parameter[, "Estimate"] - 1.96 * model_P$parameter[, "Std. Error"])
conf_int_lower_exp_model_P <- round(conf_int_lower_exp_model_P, 2)

conf_int_upper_exp_model_P <- exp(model_P$parameter[, "Estimate"] + 1.96 * model_P$parameter[, "Std. Error"])
conf_int_upper_exp_model_P <- round(conf_int_upper_exp_model_P, 2)

# Create a data frame with the exponential results

results_exp_model_P <- data.frame(Rate_Ratio = coef_exp_model_P,
                                  lower = conf_int_lower_exp_model_P,
                                  upper = conf_int_upper_exp_model_P)

# results

results_exp_model_P


# Simple model of the effect of the COHORT, as an independent variable, on the expected 
# cases of death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:


model_C <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="C", fam="qlik", Plot=T)
model_C
str(model_C)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_C <- model_C$parameter[, "Estimate"]
std_errors_model_C <- model_C$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_C <- parameters_model_C - 1.96 * std_errors_model_C
conf_int_upper_model_C <- parameters_model_C + 1.96 * std_errors_model_C

# Show confidence intervals

conf_int_model_C <- data.frame(lower = conf_int_lower_model_C, upper = conf_int_upper_model_C)
conf_int_model_C

# Exponentiate regression coeffcients

coef_exp_model_C <- exp(model_C$parameter[, "Estimate"])
coef_exp_model_C <- round(coef_exp_model_C, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_C <- exp(model_C$parameter[, "Estimate"] - 1.96 * model_C$parameter[, "Std. Error"])
conf_int_lower_exp_model_C <- round(conf_int_lower_exp_model_C, 2)

conf_int_upper_exp_model_C <- exp(model_C$parameter[, "Estimate"] + 1.96 * model_C$parameter[, "Std. Error"])
conf_int_upper_exp_model_C <- round(conf_int_upper_exp_model_C, 2)

# Create a data frame with exponentiated results

results_exp_model_C <- data.frame(Rate_Ratio = coef_exp_model_C,
                                  lower = conf_int_lower_exp_model_C,
                                  upper = conf_int_upper_exp_model_C)

# Results

results_exp_model_C

# 4. Multiple quasi-Poisson regression models

# Multiple model of the effect of AGE AND PERIOD, as independent variables, on expected cases 
# of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:


model_AP <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="AP", fam="qlik", Plot=T)
model_AP
str(model_AP)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AP <- model_AP$parameter[, "Estimate"]
std_errors_model_AP <- model_AP$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AP <- parameters_model_AP - 1.96 * std_errors_model_AP
conf_int_upper_model_AP <- parameters_model_AP + 1.96 * std_errors_model_AP

# Show confidence intervals

conf_int_model_AP <- data.frame(lower = conf_int_lower_model_AP, upper = conf_int_upper_model_AP)
conf_int_model_AP

# Exponentiate regression coeffcients

coef_exp_model_AP <- exp(model_AP$parameter[, "Estimate"])
coef_exp_model_AP <- round(coef_exp_model_AP, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_AP <- exp(model_AP$parameter[, "Estimate"] - 1.96 * model_AP$parameter[, "Std. Error"])
conf_int_lower_exp_model_AP <- round(conf_int_lower_exp_model_AP, 2)

conf_int_upper_exp_model_AP <- exp(model_AP$parameter[, "Estimate"] + 1.96 * model_AP$parameter[, "Std. Error"])
conf_int_upper_exp_model_AP <- round(conf_int_upper_exp_model_AP, 2)

# Create a data frame with exponentiated results

results_exp_model_AP <- data.frame(Rate_Ratio = coef_exp_model_AP,
                                   lower = conf_int_lower_exp_model_AP,
                                   upper = conf_int_upper_exp_model_AP)

# Results 
results_exp_model_AP

# Multiple model of the effect of AGE and COHORT, as independent variables, on expected cases of 
# death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:


model_AC <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="AC", fam="qlik", Plot=T)
model_AC
str(model_AC)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AC <- model_AC$parameter[, "Estimate"]
std_errors_model_AC <- model_AC$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AC <- parameters_model_AC - 1.96 * std_errors_model_AC
conf_int_upper_model_AC <- parameters_model_AC + 1.96 * std_errors_model_AC

# Show confidence intervals

conf_int_model_AC <- data.frame(lower = conf_int_lower_model_AC, upper = conf_int_upper_model_AC)
conf_int_model_AC

# Exponentiate regression coeffcients

coef_exp_model_AC <- exp(model_AC$parameter[, "Estimate"])
coef_exp_model_AC <- round(coef_exp_model_AC, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_AC <- exp(model_AC$parameter[, "Estimate"] - 1.96 * model_AC$parameter[, "Std. Error"])
conf_int_lower_exp_model_AC <- round(conf_int_lower_exp_model_AC, 2)

conf_int_upper_exp_model_AC <- exp(model_AC$parameter[, "Estimate"] + 1.96 * model_AC$parameter[, "Std. Error"])
conf_int_upper_exp_model_AC <- round(conf_int_upper_exp_model_AC, 2)

# Create a data frame with results exponentiated results

results_exp_model_AC <- data.frame(Rate_Ratio = coef_exp_model_AC,
                                   lower = conf_int_lower_exp_model_AC,
                                   upper = conf_int_upper_exp_model_AC)

# Results
results_exp_model_AC

# 5.  Complete Multiple Quasi-Poisson Regression Model

# Multiple model of the effect of age, period and cohort, as independent variables, on the expected 
# cases of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_APC <- apcglmfit(r=bd_lexis_t_dm_a_p, header=T, n.risk= bd_lexis_p_dm_a_p, Scale=1e-5, apcmodel="APC", fam="qlik", Plot=T, apc = 0)
model_APC
str(model_APC)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_APC <- model_APC$parameter[, "Estimate"]
std_errors_model_APC <- model_APC$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_APC <- parameters_model_APC - 1.96 * std_errors_model_APC
conf_int_upper_model_APC <- parameters_model_APC + 1.96 * std_errors_model_APC

# Show confidence intervals

conf_int_model_APC <- data.frame(lower = conf_int_lower_model_APC, upper = conf_int_upper_model_APC)
conf_int_model_APC

# Exponentiate regression coeffcients

coef_exp_model_APC <- exp(model_APC$parameter[, "Estimate"])
coef_exp_model_APC <- round(coef_exp_model_APC, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_APC <- exp(model_APC$parameter[, "Estimate"] - 1.96 * model_APC$parameter[, "Std. Error"])
conf_int_lower_exp_model_APC <- round(conf_int_lower_exp_model_APC, 2)

conf_int_upper_exp_model_APC <- exp(model_APC$parameter[, "Estimate"] + 1.96 * model_APC$parameter[, "Std. Error"])
conf_int_upper_exp_model_APC <- round(conf_int_upper_exp_model_APC, 2)

# Create a data frame with results exponentiated results

results_exp_model_APC <- data.frame(Rate_Ratio = coef_exp_model_APC,
                                    lower = conf_int_lower_exp_model_APC,
                                    upper = conf_int_upper_exp_model_APC)

# Results
results_exp_model_APC


# ANALYSIS ACCORDING TO MALE SEX

# 6. Simple quasi-poisson regression models

## Lexis tables in age and period format with respective labels.


bd_lexis_f_dm_a_p_h <- apcheader(r=bd_lexis_f_dm_h, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)

bd_lexis_p_dm_a_p_h <- apcheader(r=bd_lexis_p_dm_h, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)

bd_lexis_t_dm_a_p_h <- apcheader(r=bd_lexis_t_dm_h, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)


# Simple model of the effect of AGE, as an independent variable, 
# on the expected cases of diabetes mellitus, as a dependent variable, 
# and the exposed population, as a compensation term:

model_A_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="A", fam="qlik", Plot=T)
model_A_h
str(model_A_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_A_h <- model_A_h$parameter[, "Estimate"]
std_errors_model_A_h <- model_A_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_A_h <- parameters_model_A_h - 1.96 * std_errors_model_A_h
conf_int_upper_model_A_h <- parameters_model_A_h + 1.96 * std_errors_model_A_h

# Show confidence intervals

conf_int_model_A_h <- data.frame(lower = conf_int_lower_model_A_h, upper = conf_int_upper_model_A_h)
conf_int_model_A_h

# Exponentiate regression coeffcients

coef_exp_model_A_h <- exp(model_A_h$parameter[, "Estimate"])
coef_exp_model_A_h <- round(coef_exp_model_A_h, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_A_h <- exp(model_A_h$parameter[, "Estimate"] - 1.96 * model_A_h$parameter[, "Std. Error"])
conf_int_lower_exp_model_A_h <- round(conf_int_lower_exp_model_A_h, 2)

conf_int_upper_exp_model_A_h <- exp(model_A_h$parameter[, "Estimate"] + 1.96 * model_A_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_A_h <- round(conf_int_upper_exp_model_A_h, 2)


#Create a data frame with results exponentiated results

results_exp_model_A_h <- data.frame(Rate_Ratio = coef_exp_model_A_h,
                                    lower = conf_int_lower_exp_model_A_h,
                                    upper = conf_int_upper_exp_model_A_h)

#Results
results_exp_model_A_h


# Simple model of the effect of the PERIOD, as an independent variable, on the expected cases of 
# death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:

model_P_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="P", fam="qlik", Plot=T)
model_P_h
str(model_P_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_P_h <- model_P_h$parameter[, "Estimate"]
std_errors_model_P_h <- model_P_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_P_h <- parameters_model_P_h - 1.96 * std_errors_model_P_h
conf_int_upper_model_P_h <- parameters_model_P_h + 1.96 * std_errors_model_P_h

# Show confidence intervals

conf_int_model_P_h <- data.frame(lower = conf_int_lower_model_P_h, upper = conf_int_upper_model_P_h)
conf_int_model_P_h

# Exponentiate regression coeffcients

coef_exp_model_P_h <- exp(model_P_h$parameter[, "Estimate"])
coef_exp_model_P_h <- round(coef_exp_model_P_h, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_P_h <- exp(model_P_h$parameter[, "Estimate"] - 1.96 * model_P_h$parameter[, "Std. Error"])
conf_int_lower_exp_model_P_h <- round(conf_int_lower_exp_model_P_h, 2)

conf_int_upper_exp_model_P_h <- exp(model_P_h$parameter[, "Estimate"] + 1.96 * model_P_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_P_h <- round(conf_int_upper_exp_model_P_h, 2)

# Create a data frame with results exponentiated results

results_exp_model_P_h <- data.frame(Rate_Ratio = coef_exp_model_P_h,
                                    lower = conf_int_lower_exp_model_P_h,
                                    upper = conf_int_upper_exp_model_P_h)

# Results
results_exp_model_P_h

# Simple model of the effect of the COHORT, as an independent variable, on the expected 
# cases of death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:

model_C_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="C", fam="qlik", Plot=T)
model_C_h
str(model_C_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_C_h <- model_C_h$parameter[, "Estimate"]
std_errors_model_C_h <- model_C_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_C_h <- parameters_model_C_h - 1.96 * std_errors_model_C_h
conf_int_upper_model_C_h <- parameters_model_C_h + 1.96 * std_errors_model_C_h

# Show confidence intervals

conf_int_model_C_h <- data.frame(lower = conf_int_lower_model_C_h, upper = conf_int_upper_model_C_h)
conf_int_model_C_h

# Exponentiate regression coeffcients

coef_exp_model_C_h <- exp(model_C_h$parameter[, "Estimate"])
coef_exp_model_C_h <- round(coef_exp_model_C_h, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_C_h <- exp(model_C_h$parameter[, "Estimate"] - 1.96 * model_C_h$parameter[, "Std. Error"])
conf_int_lower_exp_model_C_h <- round(conf_int_lower_exp_model_C_h, 2)

conf_int_upper_exp_model_C_h <- exp(model_C_h$parameter[, "Estimate"] + 1.96 * model_C_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_C_h <- round(conf_int_upper_exp_model_C_h, 2)

# Create a data frame with results exponentiated results

results_exp_model_C_h <- data.frame(Rate_Ratio = coef_exp_model_C_h,
                                    lower = conf_int_lower_exp_model_C_h,
                                    upper = conf_int_upper_exp_model_C_h)

# Results
results_exp_model_C_h

# 7. Multiple quasi-Poisson regression models

# Multiple model of the effect of AGE AND PERIOD, as independent variables, on expected cases 
# of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_AP_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="AP", fam="qlik", Plot=T)
model_AP_h
str(model_AP_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AP_h <- model_AP_h$parameter[, "Estimate"]
std_errors_model_AP_h <- model_AP_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AP_h <- parameters_model_AP_h - 1.96 * std_errors_model_AP_h
conf_int_upper_model_AP_h <- parameters_model_AP_h + 1.96 * std_errors_model_AP_h

# Show confidence intervals

conf_int_model_AP_h <- data.frame(lower = conf_int_lower_model_AP_h, upper = conf_int_upper_model_AP_h)
conf_int_model_AP_h

# Exponentiate regression coeffcients

coef_exp_model_AP_h <- exp(model_AP_h$parameter[, "Estimate"])

# Exponentiate the limits of confidence intervals

conf_int_lower_exp_model_AP_h <- exp(model_AP_h$parameter[, "Estimate"] - 1.96 * model_AP_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_AP_h <- exp(model_AP_h$parameter[, "Estimate"] + 1.96 * model_AP_h$parameter[, "Std. Error"])

# Create a data frame with results exponentiated results

results_exp_model_AP_h <- data.frame(Rate_Ratio = coef_exp_model_AP_h,
                                     lower = conf_int_lower_exp_model_AP_h,
                                     upper = conf_int_upper_exp_model_AP_h)

# Results
results_exp_model_AP_h


# Multiple model of the effect of AGE and COHORT, as independent variables, on expected cases of 
# death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_AC_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="AC", fam="qlik", Plot=T)
model_AC_h
str(model_AC_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AC_h <- model_AC_h$parameter[, "Estimate"]
std_errors_model_AC_h <- model_AC_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AC_h <- parameters_model_AC_h - 1.96 * std_errors_model_AC_h
conf_int_upper_model_AC_h <- parameters_model_AC_h + 1.96 * std_errors_model_AC_h

# Show confidence intervals

conf_int_model_AC_h <- data.frame(lower = conf_int_lower_model_AC_h, upper = conf_int_upper_model_AC_h)
conf_int_model_AC_h

# Exponentiate regression coeffcients

coef_exp_model_AC_h <- exp(model_AC_h$parameter[, "Estimate"])

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_AC_h <- exp(model_AC_h$parameter[, "Estimate"] - 1.96 * model_AC_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_AC_h <- exp(model_AC_h$parameter[, "Estimate"] + 1.96 * model_AC_h$parameter[, "Std. Error"])

# Create a data frame with results exponentiated results

results_exp_model_AC_h <- data.frame(Rate_Ratio = coef_exp_model_AC_h,
                                     lower = conf_int_lower_exp_model_AC_h,
                                     upper = conf_int_upper_exp_model_AC_h)

# Results
results_exp_model_AC_h

# 8.  Complete Multiple Quasi-Poisson Regression Model

# Multiple model of the effect of AGE, PERIOD and COHORT, as independent variables, on the expected 
# cases of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_APC_h <- apcglmfit(r=bd_lexis_t_dm_a_p_h, header=T, n.risk= bd_lexis_p_dm_a_p_h, Scale=1e-5, apcmodel="APC", fam="qlik", Plot=T)
model_APC_h
str(model_APC_h)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_APC_h <- model_APC_h$parameter[, "Estimate"]
std_errors_model_APC_h <- model_APC_h$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_APC_h <- parameters_model_APC_h - 1.96 * std_errors_model_APC_h
conf_int_upper_model_APC_h <- parameters_model_APC_h + 1.96 * std_errors_model_APC_h

# Show confidence intervals

conf_int_model_APC_h <- data.frame(lower = conf_int_lower_model_APC_h, upper = conf_int_upper_model_APC_h)
conf_int_model_APC_h

# Exponentiate regression coeffcients

coef_exp_model_APC_h <- exp(model_APC_h$parameter[, "Estimate"])
coef_exp_model_APC_h <- round(coef_exp_model_APC_h, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_APC_h <- exp(model_APC_h$parameter[, "Estimate"] - 1.96 * model_APC_h$parameter[, "Std. Error"])
conf_int_lower_exp_model_APC_h <- round(conf_int_lower_exp_model_APC_h, 2)

conf_int_upper_exp_model_APC_h <- exp(model_APC_h$parameter[, "Estimate"] + 1.96 * model_APC_h$parameter[, "Std. Error"])
conf_int_upper_exp_model_APC_h <- round(conf_int_upper_exp_model_APC_h, 2)

# Create a data frame with results exponentiated results

results_exp_model_APC_h <- data.frame(Rate_Ratio = coef_exp_model_APC_h,
                                      lower = conf_int_lower_exp_model_APC_h,
                                      upper = conf_int_upper_exp_model_APC_h)

# Results
results_exp_model_APC_h


# ANALYSIS ACCORDING TO FEMALE SEX

# 9. Simple quasi-poisson regression models

## Lexis tables in age and period format with respective labels.

bd_lexis_f_dm_a_p_m <- apcheader(r=bd_lexis_f_dm_m, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)

bd_lexis_p_dm_a_p_m <- apcheader(r=bd_lexis_p_dm_m, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)

bd_lexis_t_dm_a_p_m <- apcheader(r=bd_lexis_t_dm_m, 
                                 agestart=0, 
                                 yearstart=1983, 
                                 agespan=5, 
                                 yearspan=5, 
                                 head=F)

# Simple model of the effect of AGE, as an independent variable, 
# on the expected cases of diabetes mellitus, as a dependent variable, 
# and the exposed population, as a compensation term:

model_A_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="A", fam="qlik", Plot=T)
model_A_m
str(model_A_m)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_A_m <- model_A_m$parameter[, "Estimate"]
std_errors_model_A_m <- model_A_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_A_m <- parameters_model_A_m - 1.96 * std_errors_model_A_m
conf_int_upper_model_A_m <- parameters_model_A_m + 1.96 * std_errors_model_A_m

# Show confidence intervals

conf_int_model_A_m <- data.frame(lower = conf_int_lower_model_A_m, upper = conf_int_upper_model_A_m)
conf_int_model_A_m

# Exponentiate regression coeffcients

coef_exp_model_A_m <- exp(model_A_m$parameter[, "Estimate"])
coef_exp_model_A_m <- round(coef_exp_model_A_m, 2)

# Exponentiate the limits of confidence intervals

conf_int_lower_exp_model_A_m <- exp(model_A_m$parameter[, "Estimate"] - 1.96 * model_A_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_A_m <- round(conf_int_lower_exp_model_A_m, 2)

conf_int_upper_exp_model_A_m <- exp(model_A_m$parameter[, "Estimate"] + 1.96 * model_A_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_A_m <- round(conf_int_upper_exp_model_A_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_A_m <- data.frame(Rate_Ratio = coef_exp_model_A_m,
                                    lower = conf_int_lower_exp_model_A_m,
                                    upper = conf_int_upper_exp_model_A_m)

# Results
results_exp_model_A_m

# Simple model of the effect of the PERIOD, as an independent variable, on the expected cases of 
# death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:

model_P_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="P", fam="qlik", Plot=T)
model_P_m
str(model_P_m)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_P_m <- model_P_m$parameter[, "Estimate"]
std_errors_model_P_m <- model_P_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_P_m <- parameters_model_P_m - 1.96 * std_errors_model_P_m
conf_int_upper_model_P_m <- parameters_model_P_m + 1.96 * std_errors_model_P_m

# Show confidence intervals

conf_int_model_P_m <- data.frame(lower = conf_int_lower_model_P_m, upper = conf_int_upper_model_P_m)
conf_int_model_P_m

# Exponentiate regression coeffcients

coef_exp_model_P_m <- exp(model_P_m$parameter[, "Estimate"])
coef_exp_model_P_m <- round(coef_exp_model_P_m, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_P_m <- exp(model_P_m$parameter[, "Estimate"] - 1.96 * model_P_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_P_m <- round(conf_int_lower_exp_model_P_m, 2)

conf_int_upper_exp_model_P_m <- exp(model_P_m$parameter[, "Estimate"] + 1.96 * model_P_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_P_m <- round(conf_int_upper_exp_model_P_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_P_m <- data.frame(Rate_Ratio = coef_exp_model_P_m,
                                    lower = conf_int_lower_exp_model_P_m,
                                    upper = conf_int_upper_exp_model_P_m)

# Results
results_exp_model_P_m

# Simple model of the effect of the COHORT, as an independent variable, on the expected 
# cases of death from diabetes mellitus, as a dependent variable, and the exposed 
# population, as a compensation term:

model_C_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="C", fam="qlik", Plot=T)
model_C_m
str(model_C_m)


# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_C_m <- model_C_m$parameter[, "Estimate"]
std_errors_model_C_m <- model_C_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_C_m <- parameters_model_C_m - 1.96 * std_errors_model_C_m
conf_int_upper_model_C_m <- parameters_model_C_m + 1.96 * std_errors_model_C_m

# Show confidence intervals

conf_int_model_C_m <- data.frame(lower = conf_int_lower_model_C_m, upper = conf_int_upper_model_C_m)
conf_int_model_C_m

# Exponentiate regression coeffcients

coef_exp_model_C_m <- exp(model_C_m$parameter[, "Estimate"])
coef_exp_model_C_m <- round(coef_exp_model_C_m, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_C_m <- exp(model_C_m$parameter[, "Estimate"] - 1.96 * model_C_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_C_m <- round(conf_int_lower_exp_model_C_m, 2)

conf_int_upper_exp_model_C_m <- exp(model_C_m$parameter[, "Estimate"] + 1.96 * model_C_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_C_m <- round(conf_int_upper_exp_model_C_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_C_m <- data.frame(Rate_Ratio = coef_exp_model_C_m,
                                    lower = conf_int_lower_exp_model_C_m,
                                    upper = conf_int_upper_exp_model_C_m)

# Results
results_exp_model_C_m

# 10. Multiple quasi-Poisson regression models

# Multiple model of the effect of AGE AND PERIOD, as independent variables, on expected cases 
# of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_AP_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="AP", fam="qlik", Plot=T)
model_AP_m
str(model_AP_m)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AP_m <- model_AP_m$parameter[, "Estimate"]
std_errors_model_AP_m <- model_AP_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AP_m <- parameters_model_AP_m - 1.96 * std_errors_model_AP_m
conf_int_upper_model_AP_m <- parameters_model_AP_m + 1.96 * std_errors_model_AP_m

# Show confidence intervals

conf_int_model_AP_m <- data.frame(lower = conf_int_lower_model_AP_m, upper = conf_int_upper_model_AP_m)
conf_int_model_AP_m

# Exponentiate regression coeffcients

coef_exp_model_AP_m <- exp(model_AP_m$parameter[, "Estimate"])
coef_exp_model_AP_m <- round(coef_exp_model_AP_m, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_AP_m <- exp(model_AP_m$parameter[, "Estimate"] - 1.96 * model_AP_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_AP_m <- round(conf_int_lower_exp_model_AP_m, 2)

conf_int_upper_exp_model_AP_m <- exp(model_AP_m$parameter[, "Estimate"] + 1.96 * model_AP_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_AP_m <- round(conf_int_upper_exp_model_AP_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_AP_m <- data.frame(Rate_Ratio = coef_exp_model_AP_m,
                                     lower = conf_int_lower_exp_model_AP_m,
                                     upper = conf_int_upper_exp_model_AP_m)

# Results
results_exp_model_AP_m

# Multiple model of the effect of AGE and COHORT, as independent variables, on expected cases of 
# death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_AC_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="AC", fam="qlik", Plot=T)
model_AC_m
str(model_AC_m)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_AC_m <- model_AC_m$parameter[, "Estimate"]
std_errors_model_AC_m <- model_AC_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_AC_m <- parameters_model_AC_m - 1.96 * std_errors_model_AC_m
conf_int_upper_model_AC_m <- parameters_model_AC_m + 1.96 * std_errors_model_AC_m

# Show confidence intervals

conf_int_model_AC_m <- data.frame(lower = conf_int_lower_model_AC_m, upper = conf_int_upper_model_AC_m)
conf_int_model_AC_m

# Exponentiate regression coeffcients

coef_exp_model_AC_m <- exp(model_AC_m$parameter[, "Estimate"])
coef_exp_model_AC_m <- round(coef_exp_model_AC_m, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_AC_m <- exp(model_AC_m$parameter[, "Estimate"] - 1.96 * model_AC_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_AC_m <- round(conf_int_lower_exp_model_AC_m, 2)

conf_int_upper_exp_model_AC_m <- exp(model_AC_m$parameter[, "Estimate"] + 1.96 * model_AC_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_AC_m <- round(conf_int_upper_exp_model_AC_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_AC_m <- data.frame(Rate_Ratio = coef_exp_model_AC_m,
                                     lower = conf_int_lower_exp_model_AC_m,
                                     upper = conf_int_upper_exp_model_AC_m)

# Results
results_exp_model_AC_m

# 11.  Complete Multiple Quasi-Poisson Regression Model

# Multiple model of the effect of AGE, PERIOD and COHORT, as independent variables, on the expected 
# cases of death from diabetes mellitus, as dependent variable, and the exposed 
# population, as compensation term:

model_APC_m <- apcglmfit(r=bd_lexis_t_dm_a_p_m, header=T, n.risk= bd_lexis_p_dm_a_p_m, Scale=1e-5, apcmodel="APC", fam="qlik", Plot=T)
model_APC_m
str(model_APC_m)

# 95 % intervals of confidence of regression coefficients

# Extract parameters and standard erros from the model

parameters_model_APC_m <- model_APC_m$parameter[, "Estimate"]
std_errors_model_APC_m <- model_APC_m$parameter[, "Std. Error"]

# Calculate 95% confidence intervals

conf_int_lower_model_APC_m <- parameters_model_APC_m - 1.96 * std_errors_model_APC_m
conf_int_upper_model_APC_m <- parameters_model_APC_m + 1.96 * std_errors_model_APC_m

# Show confidence intervals

conf_int_model_APC_m <- data.frame(lower = conf_int_lower_model_APC_m, upper = conf_int_upper_model_APC_m)
conf_int_model_APC_m

# Exponentiate regression coeffcients

coef_exp_model_APC_m <- exp(model_APC_m$parameter[, "Estimate"])
coef_exp_model_APC_m <- round(coef_exp_model_APC_m, 2)

# Exponentiate the limits of confidence intervals 

conf_int_lower_exp_model_APC_m <- exp(model_APC_m$parameter[, "Estimate"] - 1.96 * model_APC_m$parameter[, "Std. Error"])
conf_int_lower_exp_model_APC_m <- round(conf_int_lower_exp_model_APC_m, 2)

conf_int_upper_exp_model_APC_m <- exp(model_APC_m$parameter[, "Estimate"] + 1.96 * model_APC_m$parameter[, "Std. Error"])
conf_int_upper_exp_model_APC_m <- round(conf_int_upper_exp_model_APC_m, 2)

# Create a data frame with results exponentiated results

results_exp_model_APC_m <- data.frame(Rate_Ratio = coef_exp_model_APC_m,
                                      lower = conf_int_lower_exp_model_APC_m,
                                      upper = conf_int_upper_exp_model_APC_m)

# Results
results_exp_model_APC_m




