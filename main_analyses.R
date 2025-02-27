#Tipping point for OAC medication main script
#by: Aleksi Kristian Winsten
#contact: alkrwi@utu.fi
#date: 19.02.2025
#University of Turku, Finland

library(pacman)

#Load rest of packages with pacman
p_load(ggplot2, ggthemes, tibble, dplyr, showtext, magick,
       tidyr, forcats, circlize, patchwork, gt,
       ggstream, cowplot, pdftools, foreach, doParallel, doRNG, interp)


#######################################################################
## calculate the severity coefficients using linear regression model ##
#######################################################################
severity_matrix <- matrix(c(
  #stroke

  c(0, 0, 0, 1, 0, 0, 0, 0, rep(0,24), 0.55),
  c(0, 0, 0, 0, 1, 1, 1, 0, rep(0,24), 0.38),
  c(0, 0, 0, 0, 0, 0, 0, 1, rep(0,24), 0.62),
  c(0, 1, 1, 0, 0, 1, 1, 0, rep(0,24), 2*0.406),
  c(0, 0, 0, 1, 0, 0, 0, 1, rep(0,24), 2*0.437),
  c(1, 0, 0, 0, 0, 0, 0, 0, rep(0,24), 0.139),
  c(0, 1, 0, 0, 0, 0, 0, 0, rep(0,24), 0.142),
  c(0, 0, 1, 0, 0, 0, 0, 0, rep(0,24), 0.223),
  c(0, 0, 0, 1, 0, 0, 0, 0, rep(0,24), 0.495),
  c(0, 0, 0, 0, 1, 0, 0, 0, rep(0,24), 0.092),
  c(0, 0, 0, 0, 0, 1, 0, 0, rep(0,24), 0.115),
  c(0, 0, 0, 0, 0, 0, 1, 0, rep(0,24), 0.231),
  c(0, 0, 0, 0, 0, 0, 0, 1, rep(0,24), 0.562),
  c(1, 0, 0, 0, 1, 0, 0, 0, rep(0,24), 0.125),
  c(1, 0, 0 , 0, 0, 0, 0, 0, rep(0,24), 0.277),
  c(0, 0, 0, 0, 1, 0, 0, 0, rep(0,24), 0.199),
  c(1, 1, 1, 1, 0, 0, 0, 0, rep(0,24), 1),
  c(0, 0, 0, 0, 1, 1, 1, 1, rep(0,24), 1),
  #new stroke values below 10.9.2024
  c(0.55, 0.55, 0.55, -0.45, 0, 0, 0, 0, rep(0,24), 0),
  c(1, 0, 0, 0, 0, 0, 0, 0, rep(0,24), 0.364),
  c(0, 1, 0, 0, 0, 0, 0, 0, rep(0,24), 0.136),
  c(0, 0, 1, 0, 0, 0, 0, 0, rep(0,24), 0.182),
  c(0, 0, 0, 1, 0, 0, 0, 0, rep(0,24), 0.318),
  c(0, 0, 0, 0, 1, 0, 0, 0, rep(0,24), 0.312),
  c(0, 0, 0, 0, 0, 1, 0, 0, rep(0,24), 0.124),
  c(0, 0, 0, 0, 0, 0, 1, 0, rep(0,24), 0.250),
  c(0, 0, 0, 0, 0, 0, 0, 1, rep(0,24), 0.312),
  c(1, 1, 1, 0, 0, 0, 0, 0, rep(0,24), 0.572),
  c(0, 0, 0, 1, 0, 0, 0, 0, rep(0,24), 0.428),
  c(0, 0, 0, 0, 1, 1, 1, 0, rep(0,24), 0.494),
  c(0, 0, 0, 0, 0, 0, 0, 1, rep(0,24), 0.506),
  c(0, 0, 0, -1.25, 0, 0, 0, 1, rep(0,24), 0),
  #intracerebral
  c(rep(0,8), 1, 0, 0, 0, 1, 0, 0, 0,rep(0,16), 2*0.432),
  c(rep(0,8), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 0.486),
  c(rep(0,8), 0, 0, 0, 0, 0, 1, 0, 0, rep(0,16), 0.34),
  c(rep(0,8), 0, 0, 0, 0, 0, 0, 0, 1, rep(0,16), 0.24),
  c(rep(0,8), 1, 0, 0, 0, 0, 0, 0, 0, rep(0,16), 0.311),
  c(rep(0,8), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 0.456),
  c(rep(0,8), 1, 0, 0, 0, 0, 0, 0, 0, rep(0,16), 0.25),
  c(rep(0,8), 0, 1, 0, 0, 0, 0, 0, 0, rep(0,16), 0.27),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 0, rep(0,16), 0.27),
  c(rep(0,8), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 0.37),
  c(rep(0,8), 0, 0, 0, 0, 0, 1, 0, 0, rep(0,16), 0.22),
  c(rep(0,8), 0, 0, 0, 0, 0, 0, 0, 1, rep(0,16), 0.23),
  c(rep(0,8), -1.62, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 0),
  c(rep(0,8), 1, 0, 0, 0, 0, 0, 0, 0, rep(0,16), 0.225),
  c(rep(0,8), 0, 1, 0, 0, 0, 0, 0, 0, rep(0,16), 0.453),
  c(rep(0,8), 0, 0, 1, 0, 0, 0, 0, 0, rep(0,16), 0.212),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 0, rep(0,16), 0.110),
  c(rep(0,8), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 0.442),
  c(rep(0,8), 0, 0, 0, 0, 0, 1, 0, 0, rep(0,16), 0.418),
  c(rep(0,8), 0, 0, 0, 0, 0, 0, 1, 0, rep(0,16), 0.07),
  c(rep(0,8), 0, 0, 0, 0, 0, 0, 0, 1, rep(0,16), 0.07),
  c(rep(0,8), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 2*0.13),
  c(rep(0,8), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 2*0.14),
  c(rep(0,8), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,16), 2*0.19),
  c(rep(0,8), 0, 1, 0, 0, 0, 1, 0, 0, rep(0,16), 2*0.21),
  c(rep(0,8), 0, 1, 0, 0, 0, 1, 0, 0, rep(0,16), 2*0.16),
  c(rep(0,8), 0, 1, 0, 0, 0, 1, 0, 0, rep(0,16), 2*0.15),
  c(rep(0,8), 0, 1, 0, 0, 0, 1, 0, 0, rep(0,16), 2*0.11),
  c(rep(0,8), 0, 0, 1, 0, 0, 0, 1, 0, rep(0,16), 2*0.39),
  c(rep(0,8), 0, 0, 1, 0, 0, 0, 1, 0, rep(0,16), 2*0.20),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 1, rep(0,16), 2*0.25),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 1, rep(0,16), 2*0.32),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 1, rep(0,16), 2*0.46),
  c(rep(0,8), 0, 0, 0, 1, 0, 0, 0, 1, rep(0,16), 2*0.53),
  c(rep(0,8), 1, 1, 1, 1, 0, 0, 0, 0, rep(0,16), 1),
  c(rep(0,8), 0, 0, 0, 0, 1, 1, 1, 1, rep(0,16), 1),
  #intracranial
  c(rep(0,16), 1, 0, 0, 0, 0, 0, 0, 0, rep(0,8), 0.21),
  c(rep(0,16), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 0.32),
  c(rep(0,16), 1, 0, 0, 0, 0, 0, 0, 0, rep(0,8), 0.30),
  c(rep(0,16), 0, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 0.32),
  c(rep(0,16), -1.5, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 0),
  c(rep(0,16), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 2*0.25),
  c(rep(0,16), 1, 1, 0, 0, 1, 1, 0, 0, rep(0,8), 2*0.50),
  c(rep(0,16), 1, 1, 0, 0, 0, 0, 0, 0, rep(0,8), 0.190),
  c(rep(0,16), 0, 0, 0, 0, 1, 1, 0, 0, rep(0,8), 0.268),
  c(rep(0,16), 0, 0, 1, -1, 0, 0, 0, 0, rep(0,8), 0),
  c(rep(0,16), 0, 0, 0, 0, 0, 0, 1, -1, rep(0,8), 0),
  c(rep(0,16), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 2*0.05),
  c(rep(0,16), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 2*0.131),
  c(rep(0,16), 1, 0, 0, 0, 1, 0, 0, 0, rep(0,8), 2*0.17),
  c(rep(0,16), 0, -75.3/24.7, 1, 1, 0, -75.3/24.7, 1, 1, rep(0,8), 0),
  c(rep(0,16), 1, 1, 1, 1, 0, 0, 0, 0, rep(0,8), 1),
  c(rep(0,16), 0, 0, 0, 0, 1, 1, 1, 1, rep(0,8), 1),
  #extracranial
  c(rep(0,24), -2.15, 0, 0, 0, 1, 0, 0, 0, 0),
  c(rep(0,24), 0, 0, 0, 0, 1, 0, 0, 0, 0.051),
  c(rep(0,24), 0, 0, 0, 0, 0, 1, 0, 0, 0.01),
  c(rep(0,24), 0, 0, 0, 0, 0, 0, 1, 0, 0.06),
  c(rep(0,24), 0, 0, 0, 0, 0, 0, 0, 1, 0.91),
  c(rep(0,24), 0, 0, 0, 0, 1, 0, 0, 0, 0.09),
  c(rep(0,24), 1, 0, 0, 0, 1, 0, 0, 0, 2*0.04),
  c(rep(0,24), 0, 1, 0, 0, 0, -1, 0, 0, 0), 
  c(rep(0,24), 0, 0, 1, 0, 0, 0, -1, 0, 0),
  c(rep(0,24), 0, 0, 0, -1, 0, 0, 0, 1, 0),
  c(rep(0,24), 1, 1, 1, 1, 0, 0, 0, 0, 1),
  c(rep(0,24), 0, 0, 0, 0, 1, 1, 1, 1, 1),
  c(rep(0,24), 1, 0, 0, 0, 1, 0, 0, 0, 0.08),
  c(rep(0,24), 0, 0, 0, 0, 1, 0, 0, 0, 0.022),
  c(rep(0,24), 0, 0, 0, 0, 1, 0, 0, 0, 0.042),
  c(rep(0,24), 0, 0, 0, 0, 1, 0, 0, 0, 0.043),
  c(rep(0,28), 1, 0, 0, 0, 0.022),
  c(rep(0,28), 1, 0, 0, 0, 0.048),
  c(rep(0,28), 1, 0, 0, 0, 0.154),
  #all
  c(rep(0,12), 1, 0, 0, 0, rep(0,4), 1, 0, 0, 0, rep(0,4), 1, 0, 0, 0, 3*0.094)

),
ncol = 33,
byrow = TRUE
)
colnames(severity_matrix) <- c(paste0("StrokeNoNOAC",c("Death", "Severe", "Moderate", "Mild")),
                               paste0("StrokeYesNOAC", c("Death", "Severe", "Moderate", "Mild")),
                               paste0("IchNoNOAC",c("Death", "Severe", "Moderate", "Mild")),
                               paste0("IchYesNOAC", c("Death", "Severe", "Moderate", "Mild")),
                               paste0("SubNoNOAC",c("Death", "Severe", "Moderate", "Mild")),
                               paste0("subYesNOAC", c("Death", "Severe", "Moderate", "Mild")),
                               paste0("OtherNoNOAC",c("Death", "Severe", "Moderate", "Mild")),
                               paste0("OtherYesNOAC", c("Death", "Severe", "Moderate", "Mild")),
                               "Val")

severity_model <- lm(severity_matrix[,33] ~ severity_matrix[,-33] -1)
severity_No_NOAC_coefficients <- c(coefficients(severity_model)[1:4] / sum(coefficients(severity_model)[1:4]),
                                   coefficients(severity_model)[9:12] / sum(coefficients(severity_model)[9:12]),
                                   coefficients(severity_model)[17:20] / sum(coefficients(severity_model)[17:20]),
                                   coefficients(severity_model)[25:28] / sum(coefficients(severity_model)[25:28]))#normalized
severity_Yes_NOAC_coefficients <- c(coefficients(severity_model)[5:8] / sum(coefficients(severity_model)[5:8]),
                                    coefficients(severity_model)[13:16] / sum(coefficients(severity_model)[13:16]),
                                    coefficients(severity_model)[21:24] / sum(coefficients(severity_model)[21:24]),
                                    coefficients(severity_model)[29:32] / sum(coefficients(severity_model)[29:32]))#normalized
severity.df <- data.frame(
  NoNOAC = as.numeric(severity_No_NOAC_coefficients), 
  YesNOAC = as.numeric(severity_Yes_NOAC_coefficients), 
  group_names = c(rep("Stroke", 4), rep("Hemorrhagic stroke", 4), rep("Other intracranial bleeding", 4), rep("Extracranial bleeding", 4)),
  row_names = c(rep(c("Death", "Severe", "Moderate", "Mild"),2), rep(c("Death", "Severe", "Mild", "Nothing"),2)))


#probabilities for severity of observed health state
severity_input <- array(c(severity.df[1,1], severity.df[2,1], severity.df[3,1], severity.df[4,1], 0, 
                          severity.df[5,1], severity.df[6,1], severity.df[7,1], severity.df[8,1], 0, 
                          severity.df[9,1] , severity.df[10,1], 0, severity.df[11,1], severity.df[12,1], 
                          severity.df[13,1], severity.df[14,1], 0, severity.df[15,1], severity.df[16,1],
                          
                          severity.df[1,2], severity.df[2,2], severity.df[3,2], severity.df[4,2], 0, 
                          severity.df[5,2], severity.df[6,2], severity.df[7,2], severity.df[8,2], 0, 
                          severity.df[9,2] , severity.df[10,2], 0, severity.df[11,2], severity.df[12,2], 
                          severity.df[13,2], severity.df[14,2], 0, severity.df[15,2], severity.df[16,2]),
                        dim = c(5,4,2),
                        dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                        c("Ischemic stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding"),
                                        c("No NOAC", "NOAC")))
rm(severity_model, severity_matrix, severity_Yes_NOAC_coefficients, severity_No_NOAC_coefficients)

policy_No_NOAC <- array(c(1, 0,
                          1, 0,
                          1, 0,
                          1, 0,
                          1, 0,
                          1, 0),
                        dim = c(2,6),
                        dimnames = list(c("No NOAC", "NOAC"),
                                        c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding")
                        ))

policy_Yes_NOAC <- array(c(0, 1,
                           0, 1,
                           0, 1,
                           1, 0,
                           1, 0,
                           1, 0),
                         dim = c(2,6),
                         dimnames = list(c("No NOAC", "NOAC"),
                                         c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding")
                         ))

###################################
## Disability coefficients table ##
###################################
library(dplyr)
library(gt)
gt(severity.df, rowname_col = "row_names", groupname_col = "group_names") %>%
  tab_header(title = "Disability coefficients with or without NOAC medication") %>%
  fmt_number(rows = everything(), columns = everything(), decimals = 3) %>%
  tab_stub_indent(rows = everything(), indent = 5) %>%
  cols_label(NoNOAC = "No", YesNOAC = "Yes")  %>% gtsave("NewFixedSeverityVAlues20250219.html")
rm(severity.df)

#################################
## The Decision Maker Function ##
#################################
#by: Aleksi Kristian Winsten
#email: alkrwi@utu.fi
#University of Turku, Finland
decisionMaker <- function(isRisk = 1, policy = NULL, severity = NULL, months = 240, size = 10000, initial_state = NULL, seed = NULL, end.NOAC.after.bleeding = FALSE) {
  #isRisk: stroke risk percents
  #policy: the policy to medicate patients
  #severity: proportions of different severities of the outcome
  #qaly: quality adjusted life years for different observed outcomes
  #months: maximum time in months to simulate
  #size: number of simulations
  #base_qaly: quality adjusted life without any outcomes
  #initial_state: the initial health state to start the simulation
  #seed: integer seed for RNG
  ##################################################################################################################
  #linear functions to calculate bleedin, mortality, and stroke relative risk as a function of ischemic stroke risk#
  ##################################################################################################################
  majorBleeding_coef <- lm(c(1.14, 1.0, 0.7, 0.51, 0.7, 0.7, 2.1, 1.8, 1.4, 2.0) ~ c(1.05, 3, 0.7, 0.18, 0.44, 0.7, 4.9, 3.4, 4.9, 3.7))$coefficients
  majorBleed <- function(x) {
    as.numeric(majorBleeding_coef[1] + x * majorBleeding_coef[2])/100
  }
  mortality_coef <- lm(c(3.75, 3.7, 1.2, 3.2, 8.0, 13.8, 3.0, 4.4, 4.4, 3.7) ~ c(1.05, 1.1, 0.4, 0.8, 1.7, 3.1, 1.49, 1.83, 1.95, 4))$coefficients
  mortality <- function(x) {
    as.numeric(mortality_coef[1] + x * mortality_coef[2])/100
  }
  #medication effect on stroke relative to strokerisk
  noacRR_coef <- c(intercept = sum(1/5 * c(6, -1) * c(0.68, 0.32)), 
                   beta = sum(1/5 * c(-1, 1) * c(0.68, 0.32)))
  noacRR <- function(x) {
    if (x <= 6){
      as.numeric(noacRR_coef[1] + noacRR_coef[2] * x)
    } else {
      0.32
    }
  }
  #death relative risk if medicated
  if (policy[2,2] == 1 & isRisk <= 1) {
    deathRR <- 1
  } else if (policy[2,2] == 1 & isRisk <= 6) {
    deathRR <- ((6-0.57) + (0.57-1)*isRisk) / 5
  } else if (policy[2,2] == 1 & isRisk > 6) {
    deathRR <- 0.57
  } else {
    deathRR <- 1
  }
  ###########################################
  # use linear functions to calculate rates #
  ###########################################
  effectCoefficients <- c(Stroke = noacRR(isRisk), Bleed = 1.91) #medication effect for stroke and bleed
  total_bleed_rate <- majorBleed(isRisk)
  bleed_proportions <- c(0.11, 0.07, 0.82) #c(0.0018, 0.0015, 0.0073)/total_bleed_rate
  total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
  stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients["Stroke"])*(isRisk/100) #medication effect on stroke with base rate 0.0105
  #bleed rates according to different bleeding proportions with different treatments
  #The base bleeding rate is
  bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate, #bleeding without medication.
                         c(0.1, 0.1, 0.8)*total_bleed_rate_difference + bleed_proportions * total_bleed_rate), #bleeding rates with medication
                       ncol = 3,
                       byrow = TRUE,
                       dimnames = list(c("No NOAC", "NOAC"),
                                       c("ICH","Subdural","Other major bleed")))
  death_rate <- mortality(isRisk)*deathRR #base death rate
  
  rates_without_noac <- c(death_rate/12, stroke_rate[1]/12, bleed_rate[1,]/12) #rates without noac treatment
  rates_with_noac <- c(death_rate/12, stroke_rate[2]/12, bleed_rate[2,]/12) #rates with noac treatment
  #################################################
  ## build the rates input for the decisionMaker ##
  #################################################
  rate <- array(c(rates_without_noac[1], 1- sum(rates_without_noac), rates_without_noac[2:5], #monthly rates without OAC
                  rates_with_noac[1], 1- sum(rates_with_noac), rates_with_noac[2:5]), #monthly rates with OAC
                dim = c(6,2),
                dimnames = list(c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding"),
                                c("No NOAC", "NOAC")))
  ######################
  # allocate variables #
  ######################
  severity_qaly <- array(c(0, 0.16, 0.60, 0.88, 1,
                           0, 0.45, 0.73, 0.89, 1),
                         dim = c(5,2),
                         dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                         c("until 6 months after event", "from 6 months after event")))
  base_qaly <- c(rep(0.794,120), rep(0.733, 240-120))
  qaly <- severity_qaly%o%rep(1,241) #outer product to define life long qalys
  states <- 1:(dim(rate)[1]) # get health states
  actions <- 1:(dim(policy)[1])# get actions (No NOAC treatment, Yes NOAC treatment)
  observed_state <- initial_state
  Time <- 1:months #timeline to simulate over to
  output <- list() #temporal working variable
  death_rates_coefficients_after_disability <- c(rate[1,1], rate[1,1], -log(1-0.14)/12, -log(1-0.16)/12, -log(1-0.16)/12, rate[1,1]) / rate[1,1]
  severity <- array(c(
    cbind( matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,1]),
    cbind(matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,2])
  ),
  dim = c(5,6,2),
  )
  
  ###########################################################
  #---------------simulation loop starts--------------------#
  ###########################################################
  
  #Monthly loop for clinical outcomes
  #set seed for reproducibility
  #uses parallel computing. Requires foreach, doParallel, and doRNG packages
  cores <- detectCores()
  cl <- makeCluster(cores[1]-1)
  registerDoParallel(cl)
  set.seed(seed)
  #the main loop. Loop over simulated individuals
  output <- foreach (i = 1:size, .errorhandling = "remove") %dorng% {
    observed_state <- initial_state
    observation <- data.frame(state = observed_state, morbidity = 5, qalm = base_qaly[1]) #observed state, disability (5 is no disability), base qaly 0.794
    #a hypothetical lifeLine of the patient
    lifeLine <- rep(1,months)
    probLine <- array(rep(rate,months), dim = c(dim(rate), months)) #time dependent probabilities
    #this is the time loop. Loops over the follow up time for individuals
    for (n in 1:months){
      #sample a random action according to the policy
      action <- sample(actions, 1, prob = policy[,observed_state])
      #sample a random health state that depends from the action
      observed_state <- sample(states, 1, prob = probLine[, action, n]) # the next observed state
      ##########################################################################################
      ## Update the patients lifeLine and probLine according to the observed health state ######
      ##########################################################################################
      morbidity <- sample(1:5, 1, prob = severity[,observed_state,action]) # random morbidity from observed health state
      lifeLine[n:(n+5)] <- qaly[morbidity, 1,n]*lifeLine[n:(n+5)] #qaly until 6 months after event
      lifeLine[(n+6):months] <- qaly[morbidity,2,n]*lifeLine[(n+6):months] #qaly from 6 months after event
      #geometrically increase death rate for a year after the disability
      temprate <- 1-colSums(rbind( probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], rate[3:6,] ))
      probLine[c(1,2),,(n):min((n+12), months)] <- rbind(Death = probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], Susceptible = temprate) #increased base rate to die for a year after event
      if (end.NOAC.after.bleeding & observed_state %in% c(4,5)) {
        #end medication if hemorrhagic stroke or other intracranial bleeding
        policy <- array(c(1, 0,
                          1, 0,
                          1, 0,
                          1, 0,
                          1, 0,
                          1, 0),
                        dim = c(2,6),
                        dimnames = list(c("No NOAC", "NOAC"),
                                        c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding")
                        ))
      }
      if (end.NOAC.after.bleeding & observed_state == 3) {
        #start medication if stroke
        policy <- array(c(0, 1,
                          0, 1,
                          0, 1,
                          1, 0,
                          1, 0,
                          1, 0),
                        dim = c(2,6),
                        dimnames = list(c("No NOAC", "NOAC"),
                                        c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding")
                        ))
      }
      #build the sample path
      observation <- rbind(observation, c(observed_state, morbidity, lifeLine[n]*base_qaly[n]))
      #death by stroke or bleed or accident breaks the loop
      if (lifeLine[n] == 0) {break}
    }
    #collect the results and build the random path of the health states for the patient
    observation <- cbind(observation, cumsum(observation[,3]) / 12)
    try(colnames(observation) <- c("Observation", "Morbidity", "QALM", "Cumulative QALY"))
    observation
  }
  #########################################################################
  #------------------------simulation loop ends---------------------------#
  #########################################################################
  stopCluster(cl)
  return(output)
}

#################################
## The Markov Decision Process ##
#################################
#set simulation size
sim <- 10000
#set global seed for the random number generator
seed <- 46692
#set policy to keep NOAC medication after hemorrhagic stroke and other
#intracranial bleeding or not
end.noac.after.bleeding <- FALSE

#simulate
simulated_cumqalys_without_noac <- lapply(seq(0,10, by = 0.1), function(isRisk) {
  lapply(decisionMaker(
    isRisk = isRisk, 
    policy = policy_No_NOAC, 
    severity = severity_input, 
    months = 240, 
    size = sim, 
    initial_state = 2, #inital state 2 is susceptible
    seed = seed,
    end.NOAC.after.bleeding = end.noac.after.bleeding
  ), 
  function(x) {
    temp.table <- table(x[["Observation"]])[c("1", "2", "3", "4", "5", "6")]
    temp.table[is.na(temp.table)] <- 0
    months.before.severe.disability <- which(x[["Morbidity"]] == 2)[1] #first severe disability event time in months
    list(Observations = temp.table,
         lifeTime = sum(temp.table),
         goodLife = months.before.severe.disability,
         QALY = x[["Cumulative QALY"]][length(x[["Cumulative QALY"]])])
  }
  )
})

#iterate over ischemic stroke risk between 0 and 10
simulated_cumqalys_with_noac <- lapply(seq(0,10, by = 0.1), function(isRisk) {
  #simulate all individuals life events and their qalys
  lapply(decisionMaker(
    isRisk = isRisk, 
    policy = policy_Yes_NOAC, 
    severity = severity_input, 
    months = 240, #240
    size = sim, 
    initial_state = 2, #inital state 2 is susceptible
    seed = seed,
    end.NOAC.after.bleeding = end.noac.after.bleeding
  ), 
  #make a list of observations and QALY in the end of followup
  function(x) {
    temp.table <- table(x[["Observation"]])[c("1", "2", "3", "4", "5", "6")]
    temp.table[is.na(temp.table)] <- 0
    months.before.severe.disability <- which(x[["Morbidity"]]  == 2)[1]
    list(Observations = temp.table, 
         lifeTime = sum(temp.table),
         goodLife = months.before.severe.disability,
         QALY = x[["Cumulative QALY"]][length(x[["Cumulative QALY"]])])
  }
  )
})

#
for (i in 1:101){
  for (j in 1:sim) {
    simulated_cumqalys_with_noac[[i]][[j]]$goodLife <- if_else(is.na(simulated_cumqalys_with_noac[[i]][[j]]$goodLife), 
                                                               simulated_cumqalys_with_noac[[i]][[j]]$lifeTime, 
                                                               simulated_cumqalys_with_noac[[i]][[j]]$goodLife
    )
  }
}
for (i in 1:101){
  for (j in 1:sim) {
    simulated_cumqalys_without_noac[[i]][[j]]$goodLife <- if_else(is.na(simulated_cumqalys_without_noac[[i]][[j]]$goodLife), 
                                                                  simulated_cumqalys_without_noac[[i]][[j]]$lifeTime, 
                                                                  simulated_cumqalys_without_noac[[i]][[j]]$goodLife
    )
  }
}
#


##########################
## Mean QALY difference ##
##########################

#mean QALY difference at the end of follow up
#stroke risk 0.5%
mean(sapply(simulated_cumqalys_with_noac[[6]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[6]], function(x) x$QALY))
#stroke risk 1%
mean(sapply(simulated_cumqalys_with_noac[[11]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[11]], function(x) x$QALY))
#stroke risk 2%
mean(sapply(simulated_cumqalys_with_noac[[21]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[21]], function(x) x$QALY))
#stroke risk 5%
mean(sapply(simulated_cumqalys_with_noac[[51]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[51]], function(x) x$QALY))


