#### Analysis code for
#### Garke et al. 2024
#### "Improvements in Emotion Regulation During Cognitive Behavior Therapy 
#### Predict Subsequent Social Anxiety Reductions"
#### Cognitive Behavior Therapy

# this is R code for the analyses from the study which is based on the
# preregistration "Emotion Regulation Improvements as a 
# Predictor of Symptom Change after Cognitive-Behavioural Therapy for Social Anxiety Disorder: 
# A longitudinal study with a 2-year follow-up". Code developed by Maria Åbonde Garke
# email: maria.garke@ki.se

#### PACKAGES LOADED ####
library(plyr)
library(readxl)
library(nlme)
library(psych)
library(irr)
library(jtools)
library(piecewiseSEM)
library(Rmisc)
library(car)
library(BSDA)
library(ggpubr)
library(gghighlight)
library(viridis)
library(MuMIn)
library(effsize)
library(tidyverse)


#### DATA IMPORT ####

#### DATA CLEANING ####

# make vox_sud_patients tidy
str(vox_sud_patients) #only variable sex that need to be transformed into factor
vox_sud_patients$sex <- as.factor(vox_sud_patients$sex)

# make vox_sud_controls tidy
str(vox_sud_controls) #only variable sex that need to be transformed into factor
vox_sud_controls$sex <- as.factor(vox_sud_controls$sex)
# now female = 1, male = 2

# make vox_surveys_patients tidy
str(vox_surveys_patients) #sex needs to be transformed into factor
vox_surveys_patients$sex <- as.factor(vox_surveys_patients$sex)
# now female = 1, male = 2

# make vox_surveys_controls tidy
str(vox_surveys_controls) #sex needs to be renamed and transformed into factor
vox_surveys_controls$sex <- as.factor(vox_surveys_controls$sex)


## join vox_sud with vox_surveys for both patients and controls
vox_patients <- bind_cols(vox_surveys_patients, vox_sud_patients)
vox_controls <- bind_cols(vox_surveys_controls, vox_sud_controls)

## remove three control cases due to LSAS-SR ratings above clinical cut-offs
vox_controls <- vox_controls[!(vox_controls$ID...1 == "" | vox_controls$ID...1 == "" | vox_controls$ID...1 == ""),]

# join both patients and controls in one df
vox <- bind_rows(vox_patients, vox_controls)
# make group variable into factor
vox$group <- as.factor(vox$group)

## compute total scores for all time points

# LSAS total scores
vox$T0_lsas.tot <- rowSums(vox[, 5:52])
vox$T1_lsas.tot <- rowSums(vox[, 93:140])
vox$T2_lsas.tot <- rowSums(vox[, 177:224])
vox$T3_lsas.tot <- rowSums(vox[, 297:344])
vox$T4_lsas.tot <- rowSums(vox[, 345:392])
vox$T5_lsas.tot <- rowSums(vox[, 469:516])
vox$T6_lsas.tot <- rowSums(vox[, 553:600])
vox$T7_lsas.tot <- rowSums(vox[, 637:684])

# LSAS subscale scores
# anxiety
vox$T0_lsas.anx <- vox$`T0_LSAS-1.r` + vox$`T0_LSAS-2.r` + vox$`T0_LSAS-3.r` + vox$`T0_LSAS-4.r` +
  vox$`T0_LSAS-5.r` + vox$`T0_LSAS-6.r` + vox$`T0_LSAS-7.r` + vox$`T0_LSAS-8.r` + vox$`T0_LSAS-9.r` + 
  vox$`T0_LSAS-10.r` + vox$`T0_LSAS-11.r` + vox$`T0_LSAS-12.r` + vox$`T0_LSAS-13.r` + vox$`T0_LSAS-14.r` + 
  vox$`T0_LSAS-15.r` + vox$`T0_LSAS-16.r` + vox$`T0_LSAS-17.r` + vox$`T0_LSAS-18.r` + vox$`T0_LSAS-19.r` +
  vox$`T0_LSAS-20.r` + vox$`T0_LSAS-21.r` + vox$`T0_LSAS-22.r` + vox$`T0_LSAS-23.r` + vox$`T0_LSAS-24.r`

vox$T1_lsas.anx <- vox$`T1_LSAS-1.r` + vox$`T1_LSAS-2.r` + vox$`T1_LSAS-3.r` + vox$`T1_LSAS-4.r` +
  vox$`T1_LSAS-5.r` + vox$`T1_LSAS-6.r` + vox$`T1_LSAS-7.r` + vox$`T1_LSAS-8.r` + vox$`T1_LSAS-9.r` + 
  vox$`T1_LSAS-10.r` + vox$`T1_LSAS-11.r` + vox$`T1_LSAS-12.r` + vox$`T1_LSAS-13.r` + vox$`T1_LSAS-14.r` + 
  vox$`T1_LSAS-15.r` + vox$`T1_LSAS-16.r` + vox$`T1_LSAS-17.r` + vox$`T1_LSAS-18.r` + vox$`T1_LSAS-19.r` +
  vox$`T1_LSAS-20.r` + vox$`T1_LSAS-21.r` + vox$`T1_LSAS-22.r` + vox$`T1_LSAS-23.r` + vox$`T1_LSAS-24.r`

vox$T2_lsas.anx <- vox$`T2_LSAS-1.r` + vox$`T2_LSAS-2.r` + vox$`T2_LSAS-3.r` + vox$`T2_LSAS-4.r` +
  vox$`T2_LSAS-5.r` + vox$`T2_LSAS-6.r` + vox$`T2_LSAS-7.r` + vox$`T2_LSAS-8.r` + vox$`T2_LSAS-9.r` + 
  vox$`T2_LSAS-10.r` + vox$`T2_LSAS-11.r` + vox$`T2_LSAS-12.r` + vox$`T2_LSAS-13.r` + vox$`T2_LSAS-14.r` + 
  vox$`T2_LSAS-15.r` + vox$`T2_LSAS-16.r` + vox$`T2_LSAS-17.r` + vox$`T2_LSAS-18.r` + vox$`T2_LSAS-19.r` +
  vox$`T2_LSAS-20.r` + vox$`T2_LSAS-21.r` + vox$`T2_LSAS-22.r` + vox$`T2_LSAS-23.r` + vox$`T2_LSAS-24.r`

vox$T3_lsas.anx <- vox$`T3_LSAS-1.r` + vox$`T3_LSAS-2.r` + vox$`T3_LSAS-3.r` + vox$`T3_LSAS-4.r` +
  vox$`T3_LSAS-5.r` + vox$`T3_LSAS-6.r` + vox$`T3_LSAS-7.r` + vox$`T3_LSAS-8.r` + vox$`T3_LSAS-9.r` + 
  vox$`T3_LSAS-10.r` + vox$`T3_LSAS-11.r` + vox$`T3_LSAS-12.r` + vox$`T3_LSAS-13.r` + vox$`T3_LSAS-14.r` + 
  vox$`T3_LSAS-15.r` + vox$`T3_LSAS-16.r` + vox$`T3_LSAS-17.r` + vox$`T3_LSAS-18.r` + vox$`T3_LSAS-19.r` +
  vox$`T3_LSAS-20.r` + vox$`T3_LSAS-21.r` + vox$`T3_LSAS-22.r` + vox$`T3_LSAS-23.r` + vox$`T3_LSAS-24.r`

vox$T4_lsas.anx <- vox$`T4_LSAS-1.r` + vox$`T4_LSAS-2.r` + vox$`T4_LSAS-3.r` + vox$`T4_LSAS-4.r` +
  vox$`T4_LSAS-5.r` + vox$`T4_LSAS-6.r` + vox$`T4_LSAS-7.r` + vox$`T4_LSAS-8.r` + vox$`T4_LSAS-9.r` + 
  vox$`T4_LSAS-10.r` + vox$`T4_LSAS-11.r` + vox$`T4_LSAS-12.r` + vox$`T4_LSAS-13.r` + vox$`T4_LSAS-14.r` + 
  vox$`T4_LSAS-15.r` + vox$`T4_LSAS-16.r` + vox$`T4_LSAS-17.r` + vox$`T4_LSAS-18.r` + vox$`T4_LSAS-19.r` +
  vox$`T4_LSAS-20.r` + vox$`T4_LSAS-21.r` + vox$`T4_LSAS-22.r` + vox$`T4_LSAS-23.r` + vox$`T4_LSAS-24.r`

vox$T5_lsas.anx <- vox$`T5_LSAS-1.r` + vox$`T5_LSAS-2.r` + vox$`T5_LSAS-3.r` + vox$`T5_LSAS-4.r` +
  vox$`T5_LSAS-5.r` + vox$`T5_LSAS-6.r` + vox$`T5_LSAS-7.r` + vox$`T5_LSAS-8.r` + vox$`T5_LSAS-9.r` + 
  vox$`T5_LSAS-10.r` + vox$`T5_LSAS-11.r` + vox$`T5_LSAS-12.r` + vox$`T5_LSAS-13.r` + vox$`T5_LSAS-14.r` + 
  vox$`T5_LSAS-15.r` + vox$`T5_LSAS-16.r` + vox$`T5_LSAS-17.r` + vox$`T5_LSAS-18.r` + vox$`T5_LSAS-19.r` +
  vox$`T5_LSAS-20.r` + vox$`T5_LSAS-21.r` + vox$`T5_LSAS-22.r` + vox$`T5_LSAS-23.r` + vox$`T5_LSAS-24.r`

vox$T6_lsas.anx <- vox$`T6_LSAS-1.r` + vox$`T6_LSAS-2.r` + vox$`T6_LSAS-3.r` + vox$`T6_LSAS-4.r` +
  vox$`T6_LSAS-5.r` + vox$`T6_LSAS-6.r` + vox$`T6_LSAS-7.r` + vox$`T6_LSAS-8.r` + vox$`T6_LSAS-9.r` + 
  vox$`T6_LSAS-10.r` + vox$`T6_LSAS-11.r` + vox$`T6_LSAS-12.r` + vox$`T6_LSAS-13.r` + vox$`T6_LSAS-14.r` + 
  vox$`T6_LSAS-15.r` + vox$`T6_LSAS-16.r` + vox$`T6_LSAS-17.r` + vox$`T6_LSAS-18.r` + vox$`T6_LSAS-19.r` +
  vox$`T6_LSAS-20.r` + vox$`T6_LSAS-21.r` + vox$`T6_LSAS-22.r` + vox$`T6_LSAS-23.r` + vox$`T6_LSAS-24.r`

vox$T7_lsas.anx <- vox$`T7_LSAS-1.r` + vox$`T7_LSAS-2.r` + vox$`T7_LSAS-3.r` + vox$`T7_LSAS-4.r` +
  vox$`T7_LSAS-5.r` + vox$`T7_LSAS-6.r` + vox$`T7_LSAS-7.r` + vox$`T7_LSAS-8.r` + vox$`T7_LSAS-9.r` + 
  vox$`T7_LSAS-10.r` + vox$`T7_LSAS-11.r` + vox$`T7_LSAS-12.r` + vox$`T7_LSAS-13.r` + vox$`T7_LSAS-14.r` + 
  vox$`T7_LSAS-15.r` + vox$`T7_LSAS-16.r` + vox$`T7_LSAS-17.r` + vox$`T7_LSAS-18.r` + vox$`T7_LSAS-19.r` +
  vox$`T7_LSAS-20.r` + vox$`T7_LSAS-21.r` + vox$`T7_LSAS-22.r` + vox$`T7_LSAS-23.r` + vox$`T7_LSAS-24.r`



# avoidance
vox$T0_lsas.avo <- vox$`T0_LSAS-1.u` + vox$`T0_LSAS-2.u` + vox$`T0_LSAS-3.u` + vox$`T0_LSAS-4.u` +
  vox$`T0_LSAS-5.u` + vox$`T0_LSAS-6.u` + vox$`T0_LSAS-7.u` + vox$`T0_LSAS-8.u` + vox$`T0_LSAS-9.u` + 
  vox$`T0_LSAS-10.u` + vox$`T0_LSAS-11.u` + vox$`T0_LSAS-12.u` + vox$`T0_LSAS-13.u` + vox$`T0_LSAS-14.u` + 
  vox$`T0_LSAS-15.u` + vox$`T0_LSAS-16.u` + vox$`T0_LSAS-17.u` + vox$`T0_LSAS-18.u` + vox$`T0_LSAS-19.u` +
  vox$`T0_LSAS-20.u` + vox$`T0_LSAS-21.u` + vox$`T0_LSAS-22.u` + vox$`T0_LSAS-23.u` + vox$`T0_LSAS-24.u`

vox$T1_lsas.avo <- vox$`T1_LSAS-1.u` + vox$`T1_LSAS-2.u` + vox$`T1_LSAS-3.u` + vox$`T1_LSAS-4.u` +
  vox$`T1_LSAS-5.u` + vox$`T1_LSAS-6.u` + vox$`T1_LSAS-7.u` + vox$`T1_LSAS-8.u` + vox$`T1_LSAS-9.u` + 
  vox$`T1_LSAS-10.u` + vox$`T1_LSAS-11.u` + vox$`T1_LSAS-12.u` + vox$`T1_LSAS-13.u` + vox$`T1_LSAS-14.u` + 
  vox$`T1_LSAS-15.u` + vox$`T1_LSAS-16.u` + vox$`T1_LSAS-17.u` + vox$`T1_LSAS-18.u` + vox$`T1_LSAS-19.u` +
  vox$`T1_LSAS-20.u` + vox$`T1_LSAS-21.u` + vox$`T1_LSAS-22.u` + vox$`T1_LSAS-23.u` + vox$`T1_LSAS-24.u`

vox$T2_lsas.avo <- vox$`T2_LSAS-1.u` + vox$`T2_LSAS-2.u` + vox$`T2_LSAS-3.u` + vox$`T2_LSAS-4.u` +
  vox$`T2_LSAS-5.u` + vox$`T2_LSAS-6.u` + vox$`T2_LSAS-7.u` + vox$`T2_LSAS-8.u` + vox$`T2_LSAS-9.u` + 
  vox$`T2_LSAS-10.u` + vox$`T2_LSAS-11.u` + vox$`T2_LSAS-12.u` + vox$`T2_LSAS-13.u` + vox$`T2_LSAS-14.u` + 
  vox$`T2_LSAS-15.u` + vox$`T2_LSAS-16.u` + vox$`T2_LSAS-17.u` + vox$`T2_LSAS-18.u` + vox$`T2_LSAS-19.u` +
  vox$`T2_LSAS-20.u` + vox$`T2_LSAS-21.u` + vox$`T2_LSAS-22.u` + vox$`T2_LSAS-23.u` + vox$`T2_LSAS-24.u`

vox$T3_lsas.avo <- vox$`T3_LSAS-1.u` + vox$`T3_LSAS-2.u` + vox$`T3_LSAS-3.u` + vox$`T3_LSAS-4.u` +
  vox$`T3_LSAS-5.u` + vox$`T3_LSAS-6.u` + vox$`T3_LSAS-7.u` + vox$`T3_LSAS-8.u` + vox$`T3_LSAS-9.u` + 
  vox$`T3_LSAS-10.u` + vox$`T3_LSAS-11.u` + vox$`T3_LSAS-12.u` + vox$`T3_LSAS-13.u` + vox$`T3_LSAS-14.u` + 
  vox$`T3_LSAS-15.u` + vox$`T3_LSAS-16.u` + vox$`T3_LSAS-17.u` + vox$`T3_LSAS-18.u` + vox$`T3_LSAS-19.u` +
  vox$`T3_LSAS-20.u` + vox$`T3_LSAS-21.u` + vox$`T3_LSAS-22.u` + vox$`T3_LSAS-23.u` + vox$`T3_LSAS-24.u`

vox$T4_lsas.avo <- vox$`T4_LSAS-1.u` + vox$`T4_LSAS-2.u` + vox$`T4_LSAS-3.u` + vox$`T4_LSAS-4.u` +
  vox$`T4_LSAS-5.u` + vox$`T4_LSAS-6.u` + vox$`T4_LSAS-7.u` + vox$`T4_LSAS-8.u` + vox$`T4_LSAS-9.u` + 
  vox$`T4_LSAS-10.u` + vox$`T4_LSAS-11.u` + vox$`T4_LSAS-12.u` + vox$`T4_LSAS-13.u` + vox$`T4_LSAS-14.u` + 
  vox$`T4_LSAS-15.u` + vox$`T4_LSAS-16.u` + vox$`T4_LSAS-17.u` + vox$`T4_LSAS-18.u` + vox$`T4_LSAS-19.u` +
  vox$`T4_LSAS-20.u` + vox$`T4_LSAS-21.u` + vox$`T4_LSAS-22.u` + vox$`T4_LSAS-23.u` + vox$`T4_LSAS-24.u`

vox$T5_lsas.avo <- vox$`T5_LSAS-1.u` + vox$`T5_LSAS-2.u` + vox$`T5_LSAS-3.u` + vox$`T5_LSAS-4.u` +
  vox$`T5_LSAS-5.u` + vox$`T5_LSAS-6.u` + vox$`T5_LSAS-7.u` + vox$`T5_LSAS-8.u` + vox$`T5_LSAS-9.u` + 
  vox$`T5_LSAS-10.u` + vox$`T5_LSAS-11.u` + vox$`T5_LSAS-12.u` + vox$`T5_LSAS-13.u` + vox$`T5_LSAS-14.u` + 
  vox$`T5_LSAS-15.u` + vox$`T5_LSAS-16.u` + vox$`T5_LSAS-17.u` + vox$`T5_LSAS-18.u` + vox$`T5_LSAS-19.u` +
  vox$`T5_LSAS-20.u` + vox$`T5_LSAS-21.u` + vox$`T5_LSAS-22.u` + vox$`T5_LSAS-23.u` + vox$`T5_LSAS-24.u`

vox$T6_lsas.avo <- vox$`T6_LSAS-1.u` + vox$`T6_LSAS-2.u` + vox$`T6_LSAS-3.u` + vox$`T6_LSAS-4.u` +
  vox$`T6_LSAS-5.u` + vox$`T6_LSAS-6.u` + vox$`T6_LSAS-7.u` + vox$`T6_LSAS-8.u` + vox$`T6_LSAS-9.u` + 
  vox$`T6_LSAS-10.u` + vox$`T6_LSAS-11.u` + vox$`T6_LSAS-12.u` + vox$`T6_LSAS-13.u` + vox$`T6_LSAS-14.u` + 
  vox$`T6_LSAS-15.u` + vox$`T6_LSAS-16.u` + vox$`T6_LSAS-17.u` + vox$`T6_LSAS-18.u` + vox$`T6_LSAS-19.u` +
  vox$`T6_LSAS-20.u` + vox$`T6_LSAS-21.u` + vox$`T6_LSAS-22.u` + vox$`T6_LSAS-23.u` + vox$`T6_LSAS-24.u`

vox$T7_lsas.avo <- vox$`T7_LSAS-1.u` + vox$`T7_LSAS-2.u` + vox$`T7_LSAS-3.u` + vox$`T7_LSAS-4.u` +
  vox$`T7_LSAS-5.u` + vox$`T7_LSAS-6.u` + vox$`T7_LSAS-7.u` + vox$`T7_LSAS-8.u` + vox$`T7_LSAS-9.u` + 
  vox$`T7_LSAS-10.u` + vox$`T7_LSAS-11.u` + vox$`T7_LSAS-12.u` + vox$`T7_LSAS-13.u` + vox$`T7_LSAS-14.u` + 
  vox$`T7_LSAS-15.u` + vox$`T7_LSAS-16.u` + vox$`T7_LSAS-17.u` + vox$`T7_LSAS-18.u` + vox$`T7_LSAS-19.u` +
  vox$`T7_LSAS-20.u` + vox$`T7_LSAS-21.u` + vox$`T7_LSAS-22.u` + vox$`T7_LSAS-23.u` + vox$`T7_LSAS-24.u`


# DERS total scores (items already reversed in data file)
vox$T1_ders.tot <- vox$T1_DERS.1 + vox$T1_DERS.2 + vox$T1_DERS.3 + vox$T1_DERS.4 + vox$T1_DERS.5 + 
  vox$T1_DERS.6 + vox$T1_DERS.7 + vox$T1_DERS.8 + vox$T1_DERS.9 + vox$T1_DERS.10 + 
  vox$T1_DERS.11 + vox$T1_DERS.12 + vox$T1_DERS.13 + vox$T1_DERS.14 + vox$T1_DERS.15 + vox$T1_DERS.16 + 
  vox$T1_DERS.17 + vox$T1_DERS.18 + vox$T1_DERS.19 + vox$T1_DERS.20 + vox$T1_DERS.21 + 
  vox$T1_DERS.22 + vox$T1_DERS.23 + vox$T1_DERS.24 + vox$T1_DERS.25 + vox$T1_DERS.26 + 
  vox$T1_DERS.27 + vox$T1_DERS.28 + vox$T1_DERS.29 + vox$T1_DERS.30 + vox$T1_DERS.31 + vox$T1_DERS.32 + 
  vox$T1_DERS.33 + vox$T1_DERS.34 + vox$T1_DERS.35 + vox$T1_DERS.36

vox$T2_ders.tot <- vox$T2_DERS.1 + vox$T2_DERS.2 + vox$T2_DERS.3 + vox$T2_DERS.4 + vox$T2_DERS.5 + 
  vox$T2_DERS.6 + vox$T2_DERS.7 + vox$T2_DERS.8 + vox$T2_DERS.9 + vox$T2_DERS.10 + 
  vox$T2_DERS.11 + vox$T2_DERS.12 + vox$T2_DERS.13 + vox$T2_DERS.14 + vox$T2_DERS.15 + vox$T2_DERS.16 + 
  vox$T2_DERS.17 + vox$T2_DERS.18 + vox$T2_DERS.19 + vox$T2_DERS.20 + vox$T2_DERS.21 + 
  vox$T2_DERS.22 + vox$T2_DERS.23 + vox$T2_DERS.24 + vox$T2_DERS.25 + vox$T2_DERS.26 + 
  vox$T2_DERS.27 + vox$T2_DERS.28 + vox$T2_DERS.29 + vox$T2_DERS.30 + vox$T2_DERS.31 + vox$T2_DERS.32 + 
  vox$T2_DERS.33 + vox$T2_DERS.34 + vox$T2_DERS.35 + vox$T2_DERS.36

vox$T3_ders.tot <- vox$T3_DERS.1 + vox$T3_DERS.2 + vox$T3_DERS.3 + vox$T3_DERS.4 + vox$T3_DERS.5 + 
  vox$T3_DERS.6 + vox$T3_DERS.7 + vox$T3_DERS.8 + vox$T3_DERS.9 + vox$T3_DERS.10 + 
  vox$T3_DERS.11 + vox$T3_DERS.12 + vox$T3_DERS.13 + vox$T3_DERS.14 + vox$T3_DERS.15 + vox$T3_DERS.16 + 
  vox$T3_DERS.17 + vox$T3_DERS.18 + vox$T3_DERS.19 + vox$T3_DERS.20 + vox$T3_DERS.21 + 
  vox$T3_DERS.22 + vox$T3_DERS.23 + vox$T3_DERS.24 + vox$T3_DERS.25 + vox$T3_DERS.26 + 
  vox$T3_DERS.27 + vox$T3_DERS.28 + vox$T3_DERS.29 + vox$T3_DERS.30 + vox$T3_DERS.31 + vox$T3_DERS.32 + 
  vox$T3_DERS.33 + vox$T3_DERS.34 + vox$T3_DERS.35 + vox$T3_DERS.36

vox$T4_ders.tot <- vox$T4_DERS.1 + vox$T4_DERS.2 + vox$T4_DERS.3 + vox$T4_DERS.4 + vox$T4_DERS.5 + 
  vox$T4_DERS.6 + vox$T4_DERS.7 + vox$T4_DERS.8 + vox$T4_DERS.9 + vox$T4_DERS.10 + 
  vox$T4_DERS.11 + vox$T4_DERS.12 + vox$T4_DERS.13 + vox$T4_DERS.14 + vox$T4_DERS.15 + vox$T4_DERS.16 + 
  vox$T4_DERS.17 + vox$T4_DERS.18 + vox$T4_DERS.19 + vox$T4_DERS.20 + vox$T4_DERS.21 + 
  vox$T4_DERS.22 + vox$T4_DERS.23 + vox$T4_DERS.24 + vox$T4_DERS.25 + vox$T4_DERS.26 + 
  vox$T4_DERS.27 + vox$T4_DERS.28 + vox$T4_DERS.29 + vox$T4_DERS.30 + vox$T4_DERS.31 + vox$T4_DERS.32 + 
  vox$T4_DERS.33 + vox$T4_DERS.34 + vox$T4_DERS.35 + vox$T4_DERS.36

vox$T5_ders.tot <- vox$T5_DERS.1 + vox$T5_DERS.2 + vox$T5_DERS.3 + vox$T5_DERS.4 + vox$T5_DERS.5 + 
  vox$T5_DERS.6 + vox$T5_DERS.7 + vox$T5_DERS.8 + vox$T5_DERS.9 + vox$T5_DERS.10 + 
  vox$T5_DERS.11 + vox$T5_DERS.12 + vox$T5_DERS.13 + vox$T5_DERS.14 + vox$T5_DERS.15 + vox$T5_DERS.16 + 
  vox$T5_DERS.17 + vox$T5_DERS.18 + vox$T5_DERS.19 + vox$T5_DERS.20 + vox$T5_DERS.21 + 
  vox$T5_DERS.22 + vox$T5_DERS.23 + vox$T5_DERS.24 + vox$T5_DERS.25 + vox$T5_DERS.26 + 
  vox$T5_DERS.27 + vox$T5_DERS.28 + vox$T5_DERS.29 + vox$T5_DERS.30 + vox$T5_DERS.31 + vox$T5_DERS.32 + 
  vox$T5_DERS.33 + vox$T5_DERS.34 + vox$T5_DERS.35 + vox$T5_DERS.36

vox$T6_ders.tot <- vox$T6_DERS.1 + vox$T6_DERS.2 + vox$T6_DERS.3 + vox$T6_DERS.4 + vox$T6_DERS.5 + 
  vox$T6_DERS.6 + vox$T6_DERS.7 + vox$T6_DERS.8 + vox$T6_DERS.9 + vox$T6_DERS.10 + 
  vox$T6_DERS.11 + vox$T6_DERS.12 + vox$T6_DERS.13 + vox$T6_DERS.14 + vox$T6_DERS.15 + vox$T6_DERS.16 + 
  vox$T6_DERS.17 + vox$T6_DERS.18 + vox$T6_DERS.19 + vox$T6_DERS.20 + vox$T6_DERS.21 + 
  vox$T6_DERS.22 + vox$T6_DERS.23 + vox$T6_DERS.24 + vox$T6_DERS.25 + vox$T6_DERS.26 + 
  vox$T6_DERS.27 + vox$T6_DERS.28 + vox$T6_DERS.29 + vox$T6_DERS.30 + vox$T6_DERS.31 + vox$T6_DERS.32 + 
  vox$T6_DERS.33 + vox$T6_DERS.34 + vox$T6_DERS.35 + vox$T6_DERS.36

vox$T7_ders.tot <- vox$T7_DERS.1 + vox$T7_DERS.2 + vox$T7_DERS.3 + vox$T7_DERS.4 + vox$T7_DERS.5 + 
  vox$T7_DERS.6 + vox$T7_DERS.7 + vox$T7_DERS.8 + vox$T7_DERS.9 + vox$T7_DERS.10 + 
  vox$T7_DERS.11 + vox$T7_DERS.12 + vox$T7_DERS.13 + vox$T7_DERS.14 + vox$T7_DERS.15 + vox$T7_DERS.16 + 
  vox$T7_DERS.17 + vox$T7_DERS.18 + vox$T7_DERS.19 + vox$T7_DERS.20 + vox$T7_DERS.21 + 
  vox$T7_DERS.22 + vox$T7_DERS.23 + vox$T7_DERS.24 + vox$T7_DERS.25 + vox$T7_DERS.26 + 
  vox$T7_DERS.27 + vox$T7_DERS.28 + vox$T7_DERS.29 + vox$T7_DERS.30 + vox$T7_DERS.31 + vox$T7_DERS.32 + 
  vox$T7_DERS.33 + vox$T7_DERS.34 + vox$T7_DERS.35 + vox$T7_DERS.36

# DERS subscale scores (items already reversed)
# nonacceptance
vox$T1_ders.nonaccept <- vox$T1_DERS.11 + vox$T1_DERS.12 + vox$T1_DERS.21 + vox$T1_DERS.23 + vox$T1_DERS.25 +
  vox$T1_DERS.29

vox$T2_ders.nonaccept <- vox$T2_DERS.11 + vox$T2_DERS.12 + vox$T2_DERS.21 + vox$T2_DERS.23 + vox$T2_DERS.25 +
  vox$T2_DERS.29

vox$T3_ders.nonaccept <- vox$T3_DERS.11 + vox$T3_DERS.12 + vox$T3_DERS.21 + vox$T3_DERS.23 + vox$T3_DERS.25 +
  vox$T3_DERS.29

vox$T4_ders.nonaccept <- vox$T4_DERS.11 + vox$T4_DERS.12 + vox$T4_DERS.21 + vox$T4_DERS.23 + vox$T4_DERS.25 +
  vox$T4_DERS.29

vox$T5_ders.nonaccept <- vox$T5_DERS.11 + vox$T5_DERS.12 + vox$T5_DERS.21 + vox$T5_DERS.23 + vox$T5_DERS.25 +
  vox$T5_DERS.29

vox$T6_ders.nonaccept <- vox$T6_DERS.11 + vox$T6_DERS.12 + vox$T6_DERS.21 + vox$T6_DERS.23 + vox$T6_DERS.25 +
  vox$T6_DERS.29

vox$T7_ders.nonaccept <- vox$T7_DERS.11 + vox$T7_DERS.12 + vox$T7_DERS.21 + vox$T7_DERS.23 + vox$T7_DERS.25 +
  vox$T7_DERS.29

# goals
vox$T1_ders.goals <- vox$T1_DERS.13 + vox$T1_DERS.18 + vox$T1_DERS.20 + vox$T1_DERS.26 + vox$T1_DERS.33

vox$T2_ders.goals <- vox$T2_DERS.13 + vox$T2_DERS.18 + vox$T2_DERS.20 + vox$T2_DERS.26 + vox$T2_DERS.33

vox$T3_ders.goals <- vox$T3_DERS.13 + vox$T3_DERS.18 + vox$T3_DERS.20 + vox$T3_DERS.26 + vox$T3_DERS.33

vox$T4_ders.goals <- vox$T4_DERS.13 + vox$T4_DERS.18 + vox$T4_DERS.20 + vox$T4_DERS.26 + vox$T4_DERS.33

vox$T5_ders.goals <- vox$T5_DERS.13 + vox$T5_DERS.18 + vox$T5_DERS.20 + vox$T5_DERS.26 + vox$T5_DERS.33

vox$T6_ders.goals <- vox$T6_DERS.13 + vox$T6_DERS.18 + vox$T6_DERS.20 + vox$T6_DERS.26 + vox$T6_DERS.33

vox$T7_ders.goals <- vox$T7_DERS.13 + vox$T7_DERS.18 + vox$T7_DERS.20 + vox$T7_DERS.26 + vox$T7_DERS.33

# impulse
vox$T1_ders.impulse <- vox$T1_DERS.3 + vox$T1_DERS.14 + vox$T1_DERS.19 + vox$T1_DERS.24 + vox$T1_DERS.27 + 
  vox$T1_DERS.32

vox$T2_ders.impulse <- vox$T2_DERS.3 + vox$T2_DERS.14 + vox$T2_DERS.19 + vox$T2_DERS.24 + vox$T2_DERS.27 + 
  vox$T2_DERS.32

vox$T3_ders.impulse <- vox$T3_DERS.3 + vox$T3_DERS.14 + vox$T3_DERS.19 + vox$T3_DERS.24 + vox$T3_DERS.27 + 
  vox$T3_DERS.32

vox$T4_ders.impulse <- vox$T4_DERS.3 + vox$T4_DERS.14 + vox$T4_DERS.19 + vox$T4_DERS.24 + vox$T4_DERS.27 + 
  vox$T4_DERS.32

vox$T5_ders.impulse <- vox$T5_DERS.3 + vox$T5_DERS.14 + vox$T5_DERS.19 + vox$T5_DERS.24 + vox$T5_DERS.27 + 
  vox$T5_DERS.32

vox$T6_ders.impulse <- vox$T6_DERS.3 + vox$T6_DERS.14 + vox$T6_DERS.19 + vox$T6_DERS.24 + vox$T6_DERS.27 + 
  vox$T6_DERS.32

vox$T7_ders.impulse <- vox$T7_DERS.3 + vox$T7_DERS.14 + vox$T7_DERS.19 + vox$T7_DERS.24 + vox$T7_DERS.27 + 
  vox$T7_DERS.32

# awareness
vox$T1_ders.awareness <- vox$T1_DERS.2 + vox$T1_DERS.6 + vox$T1_DERS.8 + vox$T1_DERS.10 +
  vox$T1_DERS.17 + vox$T1_DERS.34

vox$T2_ders.awareness <- vox$T2_DERS.2 + vox$T2_DERS.6 + vox$T2_DERS.8 + vox$T2_DERS.10 +
  vox$T2_DERS.17 + vox$T2_DERS.34

vox$T3_ders.awareness <- vox$T3_DERS.2 + vox$T3_DERS.6 + vox$T3_DERS.8 + vox$T3_DERS.10 +
  vox$T3_DERS.17 + vox$T3_DERS.34

vox$T4_ders.awareness <- vox$T4_DERS.2 + vox$T4_DERS.6 + vox$T4_DERS.8 + vox$T4_DERS.10 +
  vox$T4_DERS.17 + vox$T4_DERS.34

vox$T5_ders.awareness <- vox$T5_DERS.2 + vox$T5_DERS.6 + vox$T5_DERS.8 + vox$T5_DERS.10 +
  vox$T5_DERS.17 + vox$T5_DERS.34

vox$T6_ders.awareness <- vox$T6_DERS.2 + vox$T6_DERS.6 + vox$T6_DERS.8 + vox$T6_DERS.10 +
  vox$T6_DERS.17 + vox$T6_DERS.34

vox$T7_ders.awareness <- vox$T7_DERS.2 + vox$T7_DERS.6 +vox$T7_DERS.8 + vox$T7_DERS.10 +
  vox$T7_DERS.17 + vox$T7_DERS.34

# strategies
vox$T1_ders.strategies <- vox$T1_DERS.15 + vox$T1_DERS.16 + vox$T1_DERS.22 + vox$T1_DERS.28 + vox$T1_DERS.30 +
  vox$T1_DERS.31 + vox$T1_DERS.35 + vox$T1_DERS.36

vox$T2_ders.strategies <- vox$T2_DERS.15 + vox$T2_DERS.16 + vox$T2_DERS.22 + vox$T2_DERS.28 + vox$T2_DERS.30 +
  vox$T2_DERS.31 + vox$T2_DERS.35 + vox$T2_DERS.36

vox$T3_ders.strategies <- vox$T3_DERS.15 + vox$T3_DERS.16 + vox$T3_DERS.22 + vox$T3_DERS.28 + vox$T3_DERS.30 +
  vox$T3_DERS.31 + vox$T3_DERS.35 + vox$T3_DERS.36

vox$T4_ders.strategies <- vox$T4_DERS.15 + vox$T4_DERS.16 + vox$T4_DERS.22 + vox$T4_DERS.28 + vox$T4_DERS.30 +
  vox$T4_DERS.31 + vox$T4_DERS.35 + vox$T4_DERS.36

vox$T5_ders.strategies <- vox$T5_DERS.15 + vox$T5_DERS.16 + vox$T5_DERS.22 + vox$T5_DERS.28 + vox$T5_DERS.30 +
  vox$T5_DERS.31 + vox$T5_DERS.35 + vox$T5_DERS.36

vox$T6_ders.strategies <- vox$T6_DERS.15 + vox$T6_DERS.16 + vox$T6_DERS.22 + vox$T6_DERS.28 + vox$T6_DERS.30 +
  vox$T6_DERS.31 + vox$T6_DERS.35 + vox$T6_DERS.36

vox$T7_ders.strategies <- vox$T7_DERS.15 + vox$T7_DERS.16 + vox$T7_DERS.22 + vox$T7_DERS.28 + vox$T7_DERS.30 +
  vox$T7_DERS.31 + vox$T7_DERS.35 + vox$T7_DERS.36

# clarity
vox$T1_ders.clarity <- vox$T1_DERS.1 + vox$T1_DERS.4 + vox$T1_DERS.5 + vox$T1_DERS.7 + vox$T1_DERS.9

vox$T2_ders.clarity <- vox$T2_DERS.1 + vox$T2_DERS.4 + vox$T2_DERS.5 + vox$T2_DERS.7 + vox$T2_DERS.9

vox$T3_ders.clarity <- vox$T3_DERS.1 + vox$T3_DERS.4 + vox$T3_DERS.5 + vox$T3_DERS.7 + vox$T3_DERS.9

vox$T4_ders.clarity <- vox$T4_DERS.1 + vox$T4_DERS.4 + vox$T4_DERS.5 + vox$T4_DERS.7 + vox$T4_DERS.9

vox$T5_ders.clarity <- vox$T5_DERS.1 + vox$T5_DERS.4 + vox$T5_DERS.5 + vox$T5_DERS.7 + vox$T5_DERS.9

vox$T6_ders.clarity <- vox$T6_DERS.1 + vox$T6_DERS.4 + vox$T6_DERS.5 + vox$T6_DERS.7 + vox$T6_DERS.9

vox$T7_ders.clarity <- vox$T7_DERS.1 + vox$T7_DERS.4 + vox$T7_DERS.5 + vox$T7_DERS.7 + vox$T7_DERS.9


# SPS total scores
vox$T0_sps.tot <- rowSums(vox[, 53:72])
vox$T2_sps.tot <- rowSums(vox[, 854:873])
vox$T4_sps.tot <- rowSums(vox[, 393:412])
vox$T7_sps.tot <- rowSums(vox[, 705:724])

# SIAS total scores (items already reversed)
vox$T0_sias.tot <- vox$T0_SIAS.1 + vox$T0_SIAS.2 + vox$T0_SIAS.3 + vox$T0_SIAS.4 + vox$T0_SIAS.5 + 
  vox$T0_SIAS.6 + vox$T0_SIAS.7 + vox$T0_SIAS.8 + vox$T0_SIAS.9 + vox$T0_SIAS.10 + 
  vox$T0_SIAS.11 + vox$T0_SIAS.12 + vox$T0_SIAS.13 + vox$T0_SIAS.14 + vox$T0_SIAS.15 + vox$T0_SIAS.16 + 
  vox$T0_SIAS.17 + vox$T0_SIAS.18 + vox$T0_SIAS.19 + vox$T0_SIAS.20

vox$T2_sias.tot <- vox$T2_SIAS.1 + vox$T2_SIAS.2 + vox$T2_SIAS.3 + vox$T2_SIAS.4 + vox$T2_SIAS.5 + 
  vox$T2_SIAS.6 + vox$T2_SIAS.7 + vox$T2_SIAS.8 + vox$T2_SIAS.9 + vox$T2_SIAS.10 + 
  vox$T2_SIAS.11 + vox$T2_SIAS.12 + vox$T2_SIAS.13 + vox$T2_SIAS.14 + vox$T2_SIAS.15 + vox$T2_SIAS.16 + 
  vox$T2_SIAS.17 + vox$T2_SIAS.18 + vox$T2_SIAS.19 + vox$T2_SIAS.20

vox$T4_sias.tot <- vox$T4_SIAS.1 + vox$T4_SIAS.2 + vox$T4_SIAS.3 + vox$T4_SIAS.4 + vox$T4_SIAS.5 + 
  vox$T4_SIAS.6 + vox$T4_SIAS.7 + vox$T4_SIAS.8 + vox$T4_SIAS.9 + vox$T4_SIAS.10 + 
  vox$T4_SIAS.11 + vox$T4_SIAS.12 + vox$T4_SIAS.13 + vox$T4_SIAS.14 + vox$T4_SIAS.15 + vox$T4_SIAS.16 + 
  vox$T4_SIAS.17 + vox$T4_SIAS.18 + vox$T4_SIAS.19 + vox$T4_SIAS.20

vox$T7_sias.tot <- vox$T7_SIAS.1 + vox$T7_SIAS.2 + vox$T7_SIAS.3 + vox$T7_SIAS.4 + vox$T7_SIAS.5 + 
  vox$T7_SIAS.6 + vox$T7_SIAS.7 + vox$T7_SIAS.8 + vox$T7_SIAS.9 + vox$T7_SIAS.10 + 
  vox$T7_SIAS.11 + vox$T7_SIAS.12 + vox$T7_SIAS.13 + vox$T7_SIAS.14 + vox$T7_SIAS.15 + vox$T7_SIAS.16 + 
  vox$T7_SIAS.17 + vox$T7_SIAS.18 + vox$T7_SIAS.19 + vox$T7_SIAS.20

# state anxiety derived by taking the mean of TX__sud_distress_X_7 and TX__sud_fear_X_7 (7 = anxiety before speech)
vox$T1_sud7 <- (vox$T1_sud.fear1.7 + vox$T1_sud.distress1.7) / 2
vox$T2_sud7 <- (vox$T2_sud.fear9.7 + vox$T2_sud.distress9.7) / 2
vox$T3_sud7 <- (vox$T3_sud.fear13.7 + vox$T3_sud.distress13.7) / 2
vox$T4_sud7 <- (vox$T4_sud.fear18.7 + vox$T4_sud.distress18.7) / 2
vox$T7_sud7 <- (vox$T7_sud.fear112.7 + vox$T7_sud.distress112.7) / 2

# all measures of state anxiety (means)
vox$T1_sud1 <- (vox$T1_sud.fear1.1 + vox$T1_sud.distress1.1) / 2
vox$T2_sud1 <- (vox$T2_sud.fear9.1 + vox$T2_sud.distress9.1) / 2
vox$T3_sud1 <- (vox$T3_sud.fear13.1 + vox$T3_sud.distress13.1) / 2
vox$T4_sud1 <- (vox$T4_sud.fear18.1 + vox$T4_sud.distress18.1) / 2
vox$T7_sud1 <- (vox$T7_sud.fear112.1 + vox$T7_sud.distress112.1) / 2

vox$T1_sud2 <- (vox$T1_sud.fear1.2 + vox$T1_sud.distress1.2) / 2
vox$T2_sud2 <- (vox$T2_sud.fear9.2 + vox$T2_sud.distress9.2) / 2
vox$T3_sud2 <- (vox$T3_sud.fear13.2 + vox$T3_sud.distress13.2) / 2
vox$T4_sud2 <- (vox$T4_sud.fear18.2 + vox$T4_sud.distress18.2) / 2
vox$T7_sud2 <- (vox$T7_sud.fear112.2 + vox$T7_sud.distress112.2) / 2

vox$T1_sud3 <- (vox$T1_sud.fear1.3 + vox$T1_sud.distress1.3) / 2
vox$T2_sud3 <- (vox$T2_sud.fear9.3 + vox$T2_sud.distress9.3) / 2
vox$T3_sud3 <- (vox$T3_sud.fear13.3 + vox$T3_sud.distress13.3) / 2
vox$T4_sud3 <- (vox$T4_sud.fear18.3 + vox$T4_sud.distress18.3) / 2
vox$T7_sud3 <- (vox$T7_sud.fear112.3 + vox$T7_sud.distress112.3) / 2

vox$T1_sud4 <- (vox$T1_sud.fear1.4 + vox$T1_sud.distress1.4) / 2
vox$T2_sud4 <- (vox$T2_sud.fear9.4 + vox$T2_sud.distress9.4) / 2
vox$T3_sud4 <- (vox$T3_sud.fear13.4 + vox$T3_sud.distress13.4) / 2
vox$T4_sud4 <- (vox$T4_sud.fear18.4 + vox$T4_sud.distress18.4) / 2
vox$T7_sud4 <- (vox$T7_sud.fear112.4 + vox$T7_sud.distress112.4) / 2

vox$T1_sud5 <- (vox$T1_sud.fear1.5 + vox$T1_sud.distress1.5) / 2
vox$T2_sud5 <- (vox$T2_sud.fear9.5 + vox$T2_sud.distress9.5) / 2
vox$T3_sud5 <- (vox$T3_sud.fear13.5 + vox$T3_sud.distress13.5) / 2
vox$T4_sud5 <- (vox$T4_sud.fear18.5 + vox$T4_sud.distress18.5) / 2
vox$T7_sud5 <- (vox$T7_sud.fear112.5 + vox$T7_sud.distress112.5) / 2

vox$T1_sud6 <- (vox$T1_sud.fear1.6 + vox$T1_sud.distress1.6) / 2
vox$T2_sud6 <- (vox$T2_sud.fear9.6 + vox$T2_sud.distress9.6) / 2
vox$T3_sud6 <- (vox$T3_sud.fear13.6 + vox$T3_sud.distress13.6) / 2
vox$T4_sud6 <- (vox$T4_sud.fear18.6 + vox$T4_sud.distress18.6) / 2
vox$T7_sud6 <- (vox$T7_sud.fear112.6 + vox$T7_sud.distress112.6) / 2

vox$T1_sud8 <- (vox$T1_sud.fear1.8 + vox$T1_sud.distress1.8) / 2
vox$T2_sud8 <- (vox$T2_sud.fear9.8 + vox$T2_sud.distress9.8) / 2
vox$T3_sud8 <- (vox$T3_sud.fear13.8 + vox$T3_sud.distress13.8) / 2
vox$T4_sud8 <- (vox$T4_sud.fear18.8 + vox$T4_sud.distress18.8) / 2
vox$T7_sud8 <- (vox$T7_sud.fear112.8 + vox$T7_sud.distress112.8) / 2

vox$T1_sud9 <- (vox$T1_sud.fear1.9 + vox$T1_sud.distress1.9) / 2
vox$T2_sud9 <- (vox$T2_sud.fear9.9 + vox$T2_sud.distress9.9) / 2
vox$T3_sud9 <- (vox$T3_sud.fear13.9 + vox$T3_sud.distress13.9) / 2
vox$T4_sud9 <- (vox$T4_sud.fear18.9 + vox$T4_sud.distress18.9) / 2
vox$T7_sud9 <- (vox$T7_sud.fear112.9 + vox$T7_sud.distress112.9) / 2


#remove extra sets of ID, sex and group
vox <- vox[-c(761:763)]
vox <- vox[-c(891:893)]


colnames(vox)[1:3] <- c("ID","age","sex")

## transforming df into a new df in long format for later visualization and analysis
vox_long <- gather(vox, key = "occasion", value = "value", -ID, -age, -sex, -group)
vox_long$value <- as.numeric(vox_long$value)
# separating time (TX) from the variable (self-report instrument)
vox_long <- separate(vox_long, occasion, into = c("occasion", "variable"), sep = "_")

# transforming occasion variable from chr to num
vox_long$occasion <- str_replace(vox_long$occasion, "T", "")

vox_long$occasion <- as.numeric(vox_long$occasion)

# making new long data without the screening measurement (occasion = 0)
vox_long_2 <- filter(vox_long, occasion >= 1)


# transforming occasion variable into month units corresponding to the actual time period between measurements
# 0 (baseline 1), 2.25 (baseline 2,pretreatment), 3.25 (midtreatment), 4.5 (posttreatment), 10.5 (6mfu), 16.5 (1yfu), 28 (2yfu)
vox_long_2$occasion <- dplyr::recode(vox_long_2$occasion, `1` = 0, `2` = 2.25, `3` = 3.25, `4` = 4.5, `5` = 10.5, `6` = 16.5, `7` = 28)


# make df that includes patients, but excludes 9 patient cases which initiated treatment
# during the follow-up phase (to confirm our results still stand despite this interference)
vox_robust <- vox %>%
  filter(group == "Patient")

vox_robust <- vox_robust[!(vox_robust$ID == "1030wljg" | vox_robust$ID == "1061nzmt" 
                           | vox_robust$ID == "1201flck" | vox_robust$ID == "1220krwj"
                           | vox_robust$ID == "1231pwjg"| vox_robust$ID == "1234mzgm"
                           | vox_robust$ID == "1013tcjp" | vox_robust$ID == "1240hxdr"
                           | vox_robust$ID == "1136xdkb"),]


#### DESCRIPTIVES-DEMOGRAPHICS ####

## age, total and by group
describeBy(vox$age, vox$group)

## sex, total and by group
table(vox$sex, vox$group)

## DERS all timepoints
describeBy(vox$T1_ders.tot, vox$group)
describeBy(vox$T2_ders.tot, vox$group)
describeBy(vox$T3_ders.tot, vox$group)
describeBy(vox$T4_ders.tot, vox$group)
describeBy(vox$T5_ders.tot, vox$group)
describeBy(vox$T6_ders.tot, vox$group)
describeBy(vox$T7_ders.tot, vox$group)

## LSAS all timepoints
describeBy(vox$T0_lsas.tot, vox$group)
describeBy(vox$T1_lsas.tot, vox$group)
describeBy(vox$T2_lsas.tot, vox$group)
describeBy(vox$T3_lsas.tot, vox$group)
describeBy(vox$T4_lsas.tot, vox$group)
describeBy(vox$T5_lsas.tot, vox$group)
describeBy(vox$T6_lsas.tot, vox$group)
describeBy(vox$T7_lsas.tot, vox$group)

## SUD all timepoints
describeBy(vox$T1_sud7, vox$group)
describeBy(vox$T2_sud7, vox$group)
describeBy(vox$T3_sud7, vox$group)
describeBy(vox$T4_sud7, vox$group)
describeBy(vox$T7_sud7, vox$group)


## Descriptives in sample excluding participants who had treatment during follow-up
## DERS
describe(vox_robust$T1_ders.tot)
describe(vox_robust$T2_ders.tot)
describe(vox_robust$T3_ders.tot)
describe(vox_robust$T4_ders.tot)
describe(vox_robust$T5_ders.tot)
describe(vox_robust$T6_ders.tot)
describe(vox_robust$T7_ders.tot)

## LSAS
describe(vox_robust$T0_lsas.tot)
describe(vox_robust$T1_lsas.tot)
describe(vox_robust$T2_lsas.tot)
describe(vox_robust$T3_lsas.tot)
describe(vox_robust$T4_lsas.tot)
describe(vox_robust$T5_lsas.tot)
describe(vox_robust$T6_lsas.tot)
describe(vox_robust$T7_lsas.tot)

## SUD
describe(vox_robust$T1_sud7)
describe(vox_robust$T2_sud7)
describe(vox_robust$T3_sud7)
describe(vox_robust$T4_sud7)
describe(vox_robust$T7_sud7)

## t-test post-treatment DERS total score of patients vs normal population
# from Gratz & Roemer 2004

# mean DERS total score normal population = 79.33 (sd = 19.76), n = 357

tsum.test(mean.x=80.07,   s.x=21.98, n.x=46,
          mean.y=79.33, s.y=19.76, n.y=357)


#### INTERNAL CONSISTENCY ####

## Cronbach's alpha for patients at T1 (lsas and ders) and T0 (sias and sps)
names(vox_patients)

# lsas
psych::alpha(vox_patients[,c(93:140)])$total$std.alpha
# warning: the function applies smoothing because matrix not positive definite

# ders
psych::alpha(vox_patients[,c(141:176)])$total$std.alpha

# sias
psych::alpha(vox_patients[,c(73:92)])$total$std.alpha

# sps
psych::alpha(vox_patients[,c(53:72)])$total$std.alpha


## Cronbach's alpha for controls at T1 (lsas and ders) and T0 (sias and sps)
names(vox_controls)

# lsas
psych::alpha(vox_controls[,c(93:140)])$total$std.alpha
# warning: the function applies smoothing because matrix not positive definite

# ders
psych::alpha(vox_controls[,c(141:176)])$total$std.alpha

# sias
psych::alpha(vox_controls[,c(73:92)])$total$std.alpha

# checking also for T2
psych::alpha(vox_controls[,c(281:300)])$total$std.alpha

# sps
psych::alpha(vox_controls[,c(53:72)])$total$std.alpha


#### ANALYSIS H1 ####

#### H1.1 lsas piecewise growth curve ####
# investigating changes in LSAS and DERS over time (effect of ER on SAD)
# the assumption is that both intercept and slopes will vary
# unstructured covariance matrix structure

## filter LSAS and DERS tot for all occasions into new df
vox_df_H1_pw <- filter(vox_long_2, variable %in% c("lsas.tot", "ders.tot"), group == "Patient") %>%
  spread(key = variable, value)

# adding time-varying person-mean centered predictor (within-individual invariant) and 
# the ders time-invariant person-specific predictor mean (between individual time-invariant)
vox_df_H1_pw <- vox_df_H1_pw %>%
  mutate(ders.tot.mean = mean(ders.tot, na.rm = TRUE)) %>%
  group_by(ID) %>%
  mutate(ders.tot.w = ders.tot - mean(ders.tot, na.rm = TRUE)) %>%
  mutate(ders.tot.b = mean(ders.tot, na.rm = TRUE) - ders.tot.mean) %>%
  mutate(ders.tot.lag.w = lag(ders.tot.w)) %>%
  ungroup() %>%
  mutate(o1 = dplyr::recode(occasion, `0` = 0, `2.25` = 2.25, `3.25` = 3.25, `4.5` = 4.5, `10.5` = 4.5, `16.5` = 4.5, `28` = 4.5)) %>%
  mutate(o2 = dplyr::recode(occasion, `0` = 0, `2.25` = 0, `3.25` = 0, `4.5` = 0, `10.5` = 6, `16.5` = 12, `28` = 23.5))


## basic piecewise growth curve model with outcome + adding the predictor to the lagged outcome
H1mod_1_pw <- lme(lsas.tot ~ (o1 + o2), data = vox_df_H1_pw, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_1_pw)

H1mod_2_pw <- lme(lsas.tot ~ (o1+o2) + ders.tot.b + ders.tot.lag.w, data = vox_df_H1_pw, random = list(ID = pdBlocked(list(~o1 + ders.tot.lag.w, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_2_pw)

H1mod_3_pw <- lme(lsas.tot ~ (o1+o2)*ders.tot.b + (o1+o2)*ders.tot.lag.w, data = vox_df_H1_pw, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_3_pw)
intervals(H1mod_3_pw, which = "all") #confidence intervals for estimates and sd
VarCorr(H1mod_3_pw) #variance and correlation components
rsquared(nlme::lme(lsas.tot ~ (o1+o2)*ders.tot.b + (o1+o2)*ders.tot.lag.w, data = vox_df_H1_pw, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude), method = "nagelkerke")
#pseudo-rsquared for model (fixed) using nagelkerke method


## checking model assumptions
qqnorm(resid(H1mod_3_pw))

hist(resid(H1mod_3_pw))

plot(fitted(H1mod_3_pw), resid(H1mod_3_pw))

#calculating pseudo-R2 by comparing models with and without the within-person predictor
#(resid var model without predictor - resid var model with predictor) / resid var model without predictor

#model without predictor
H1mod_5_pw <- lme(lsas.tot ~ (o1+o2)*ders.tot.b, data = vox_df_H1_pw, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude)
VarCorr(H1mod_5_pw)
#residual variance = 183.03

#model with predictor
VarCorr(H1mod_3_pw)
#residual variance = 104.36

(183.03-104.36)/183.03
#gives 0.43


#### H1.2 lsas REVERSE piecewise growth curve ####
# investigating changes in LSAS and DERS over time (effect of SAD on ER)
# the assumption is that both intercept and slopes will vary
# unstructured covariance matrix structure

## filter LSAS and DERS tot for all occasions into new df
vox_df_H1_pw_2 <- filter(vox_long_2, variable %in% c("lsas.tot", "ders.tot"), group == "Patient") %>%
  spread(key = variable, value)

# adding time-varying person-mean centered predictor (within-individual invariant) and 
# the ders time-invariant person-specific predictor mean (between individual time-invariant)
vox_df_H1_pw_2 <- vox_df_H1_pw_2 %>%
  mutate(lsas.tot.mean = mean(lsas.tot, na.rm = TRUE)) %>%
  group_by(ID) %>%
  mutate(lsas.tot.w = lsas.tot - mean(lsas.tot, na.rm = TRUE)) %>%
  mutate(lsas.tot.b = mean(lsas.tot, na.rm = TRUE) - lsas.tot.mean) %>%
  mutate(lsas.tot.lag.w = lag(lsas.tot.w)) %>%
  ungroup() %>%
  mutate(o1 = dplyr::recode(occasion, `0` = 0, `2.25` = 2.25, `3.25` = 3.25, `4.5` = 4.5, `10.5` = 4.5, `16.5` = 4.5, `28` = 4.5)) %>%
  mutate(o2 = dplyr::recode(occasion, `0` = 0, `2.25` = 0, `3.25` = 0, `4.5` = 0, `10.5` = 6, `16.5` = 12, `28` = 23.5))


## basic piecewise growth curve model with outcome + adding the predictor to the lagged outcome
H1mod_1_pw_2 <- lme(ders.tot ~ (o1 + o2), data = vox_df_H1_pw_2, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_1_pw_2)

H1mod_2_pw_2 <- lme(ders.tot ~ (o1+o2) + lsas.tot.b + lsas.tot.lag.w, data = vox_df_H1_pw_2, random = list(ID = pdBlocked(list(~o1 + lsas.tot.lag.w, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_2_pw_2)

H1mod_3_pw_2 <- lme(ders.tot ~ (o1+o2)*lsas.tot.b + (o1+o2)*lsas.tot.lag.w, data = vox_df_H1_pw_2, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude)
summary(H1mod_3_pw_2)
intervals(H1mod_3_pw_2, which = "all") #confidence intervals for estimates and sd
VarCorr(H1mod_3_pw_2) #variance and correlation components
rsquared(nlme::lme(ders.tot ~ (o1+o2)*lsas.tot.b + (o1+o2)*lsas.tot.lag.w, data = vox_df_H1_pw_2, random = list(ID = pdBlocked(list(~o1, ~0 + o2))), method = "REML", na.action = na.exclude))
#pseudo-rsquared for model (fixed) using nagelkerke method
#FINAL MODEL = H1mod_3_pw_2 

## checking model assumptions
qqnorm(resid(H1mod_3_pw_2))

hist(resid(H1mod_3_pw_2))

plot(fitted(H1mod_3_pw_2), resid(H1mod_3_pw_2))


#### H1.3 sud piecewise growth curve ####
# investigating changes in state anxiety (SUD) and DERS over time (effect of ER on SUD)
# the assumption is that both intercept and slopes will vary
# unstructured covariance matrix structure

## filter SUD and DERS tot for all occasions into new df
vox_df_H1_sud <- filter(vox_long_2, variable %in% c("sud7", "ders.tot"), group == "Patient") %>%
  spread(key = variable, value) %>%
  filter(occasion %in% c(0, 2.25, 3.25, 4.5, 28))

# adding time-varying person-mean centered predictor (within-individual invariant) and 
# the ders time-invariant person-specific predictor mean (between individual time-invariant)
vox_df_H1_sud <- vox_df_H1_sud %>%
  mutate(ders.tot.mean = mean(ders.tot, na.rm = TRUE)) %>%
  group_by(ID) %>%
  mutate(ders.tot.w = ders.tot - mean(ders.tot, na.rm = TRUE)) %>%
  mutate(ders.tot.b = mean(ders.tot, na.rm = TRUE) - ders.tot.mean) %>%
  mutate(ders.tot.lag.w = lag(ders.tot.w)) %>%
  ungroup() %>%
  mutate(o1 = dplyr::recode(occasion, `0` = 0, `2.25` = 2.25, `3.25` = 3.25, `4.5` = 4.5, `28` = 4.5)) %>%
  mutate(o2 = dplyr::recode(occasion, `0` = 0, `2.25` = 0, `3.25` = 0, `4.5` = 0, `28` = 23.5))


# basic linear models with all timepoints and lagged DERS
H1mod_1_sud <- lme(sud7 ~ (o1+o2) + ders.tot.b + ders.tot.lag.w, data = vox_df_H1_sud, random = ~ o1 | ID, method = "REML", na.action = na.exclude)
summary(H1mod_1_sud)

H1mod_2_sud <- lme(sud7 ~ (o1+o2)*ders.tot.b + (o1+o2)*ders.tot.lag.w, data = vox_df_H1_sud, random = ~ o1 | ID, method = "REML", na.action = na.exclude)
summary(H1mod_2_sud)
intervals(H1mod_2_sud, which = "all") #confidence intervals for estimates and sd
VarCorr(H1mod_2_sud) #variance and correlation components
rsquared(nlme::lme(sud7 ~ (o1+o2)*ders.tot.b + (o1+o2)*ders.tot.lag.w, data = vox_df_H1_sud, random = ~ o1 | ID, method = "REML", na.action = na.exclude))
#pseudo-rsquared for model (fixed) using nagelkerke method
#FINAL MODEL = H1mod_2_sud

#calculating pseudo-R2 by comparing models with and without the within-person predictor
#(resid var model without predictor - resid var model with predictor) / resid var model without predictor

#model without predictor
H1mod_3_sud <- lme(sud7 ~ (o1+o2)*ders.tot.lag.w, data = vox_df_H1_sud, random = ~ o1 | ID, method = "REML", na.action = na.exclude)
VarCorr(H1mod_3_sud)
#residual variance = 158.50

#model with predictor
VarCorr(H1mod_2_sud)
#residual variance = 159.95

(158.50-159.95)/158.50
#gives -0.01


#### H1.4 sud REVERSE piecewise growth curve #### 
# investigating changes in state anxiety (SUD) and DERS over time (effect of SUD on ER)
# the assumption is that both intercept and slopes will vary
# unstructured covariance matrix structure

## filter SUD and DERS tot for all occasions into new df
vox_df_H1_sud_2 <- filter(vox_long_2, variable %in% c("sud7", "ders.tot"), group == "Patient") %>%
  spread(key = variable, value) %>%
  filter(occasion %in% c(0, 2.25, 3.25, 4.5, 28))

# adding time-varying person-mean centered predictor (within-individual invariant) and 
# the sud7 time-invariant person-specific predictor mean (between individual time-invariant)
vox_df_H1_sud_2 <- vox_df_H1_sud_2 %>%
  mutate(sud7.mean = mean(sud7, na.rm = TRUE)) %>%
  group_by(ID) %>%
  mutate(sud7.w = sud7 - mean(sud7, na.rm = TRUE)) %>%
  mutate(sud7.b = mean(sud7, na.rm = TRUE) - sud7.mean) %>%
  mutate(sud7.lag.w = lag(sud7.w)) %>%
  ungroup() %>%
  mutate(o1 = dplyr::recode(occasion, `0` = 0, `2.25` = 2.25, `3.25` = 3.25, `4.5` = 4.5, `28` = 4.5)) %>%
  mutate(o2 = dplyr::recode(occasion, `0` = 0, `2.25` = 0, `3.25` = 0, `4.5` = 0, `28` = 23.5))


# basic linear models with all timepoints and lagged DERS
H1mod_1_sud_2 <- lme(ders.tot ~ (o1+o2) + sud7.b + sud7.lag.w, data = vox_df_H1_sud_2, random = ~ o1 | ID, method = "REML", na.action = na.exclude)
summary(H1mod_1_sud)

H1mod_2_sud_2 <- lme(ders.tot ~ (o1+o2)*sud7.b + (o1+o2)*sud7.lag.w, data = vox_df_H1_sud_2, random = ~ o1 | ID, method = "REML", na.action = na.exclude)
summary(H1mod_2_sud_2)
intervals(H1mod_2_sud_2, which = "all") #confidence intervals for estimates and sd
VarCorr(H1mod_2_sud_2) #variance and correlation components
rsquared(nlme::lme(ders.tot ~ (o1+o2)*sud7.b + (o1+o2)*sud7.lag.w, data = vox_df_H1_sud_2, random = ~ o1 | ID, method = "REML", na.action = na.exclude))
#pseudo-rsquared for model (fixed) using nagelkerke method
#FINAL MODEL = H1mod_2_sud_2


#### ANALYSIS H2 ####

## linear regression analysis testing for group differences in DERS between baseline 1 and 2

# make new df with one time variable dummy coded as 0 and 1
vox_df_H2 <- vox_long_2

vox_df_H2$group <- as.factor(vox_df_H2$group)
vox_df_H2$group <- as.numeric(vox_df_H2$group)

vox_df_H2 <- vox_df_H2 %>%
  filter(occasion <= 2.25) %>% #keep timepoints baseline 1 & 2
  mutate(occasion = replace(occasion, occasion == 2.25, 1)) %>% # dummy code occasion so that baseline1=0 and baseline2=1
  mutate(group = ifelse(group == 2, 1, 0)) %>% #recode group variable, 0 = control, 1 = patient
  filter(variable %in% c("ders.tot", "lsas.tot", "sud7")) %>% #keep ders.tot, lsas.tot and sud7 as variable
  spread(key = variable, value)

H2mod1 <- lme(ders.tot ~ occasion  + group + occasion*group, random = ~ occasion | ID , data = vox_df_H2, na.action = na.omit, method = "REML")
summary(H2mod1)
#FINAL MODEL FOR DERS
r.squaredGLMM(H2mod1)

H2mod2 <- lme(lsas.tot ~ occasion  + group + occasion*group, random = ~ occasion | ID , data = vox_df_H2, na.action = na.omit, method = "REML")
summary(H2mod2)
#FINAL MODEL FOR LSAS
r.squaredGLMM(H2mod2)

H2mod3 <- lme(sud7 ~ occasion  + group + occasion*group, random = ~ occasion | ID , data = vox_df_H2, na.action = na.omit, method = "REML")
summary(H2mod3)
#FINAL MODEL FOR SUD
r.squaredGLMM(H2mod3)

#### ANALYSIS H3 ####

## test-retest reliability of ders.tot baseline 1 and 2 using ICC

# patients
vox_df_H3_pat <- filter(vox, group == "Patient") %>%
  select(T1_ders.tot, T2_ders.tot)

icc(vox_df_H3_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_H3_con <- filter(vox, group == "Control") %>%
  select(T1_ders.tot, T2_ders.tot)

icc(vox_df_H3_con, model = "twoway", type = "agreement", unit = "single")


## test-retest reliability of lsas.tot baseline 1 and 2 using ICC

# patients
vox_df_H3_lsas_pat <- filter(vox, group == "Patient") %>%
  select(T1_lsas.tot, T2_lsas.tot)

icc(vox_df_H3_lsas_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_H3_lsas_con <- filter(vox, group == "Control") %>%
  select(T1_lsas.tot, T2_lsas.tot)

icc(vox_df_H3_lsas_con, model = "twoway", type = "agreement", unit = "single")


## test-retest reliability of DERS item 1-36 baseline 1 and 2 using ICC
#patients

vox_H3_ders1 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.1, T2_DERS.1)
icc(vox_H3_ders1, model = "twoway", type = "agreement", unit = "single")
#0.69

vox_H3_ders2 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.2, T2_DERS.2)
icc(vox_H3_ders2, model = "twoway", type = "agreement", unit = "single")
#0.43

vox_H3_ders3 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.3, T2_DERS.3)
icc(vox_H3_ders3, model = "twoway", type = "agreement", unit = "single")
#0.44

vox_H3_ders4 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.4, T2_DERS.4)
icc(vox_H3_ders4, model = "twoway", type = "agreement", unit = "single")
#0.71

vox_H3_ders5 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.5, T2_DERS.5)
icc(vox_H3_ders5, model = "twoway", type = "agreement", unit = "single")
#0.79

vox_H3_ders6 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.6, T2_DERS.6)
icc(vox_H3_ders6, model = "twoway", type = "agreement", unit = "single")
#0.54

vox_H3_ders7 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.7, T2_DERS.7)
icc(vox_H3_ders7, model = "twoway", type = "agreement", unit = "single")
#0.73

vox_H3_ders8 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.8, T2_DERS.8)
icc(vox_H3_ders8, model = "twoway", type = "agreement", unit = "single")
#0.67

vox_H3_ders9 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.9, T2_DERS.9)
icc(vox_H3_ders9, model = "twoway", type = "agreement", unit = "single")
#0.37

vox_H3_ders10 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.10, T2_DERS.10)
icc(vox_H3_ders10, model = "twoway", type = "agreement", unit = "single")
#0.40

vox_H3_ders11 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.11, T2_DERS.11)
icc(vox_H3_ders11, model = "twoway", type = "agreement", unit = "single")
#0.40

vox_H3_ders12 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.12, T2_DERS.12)
icc(vox_H3_ders12, model = "twoway", type = "agreement", unit = "single")
#0.33

vox_H3_ders13 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.13, T2_DERS.13)
icc(vox_H3_ders13, model = "twoway", type = "agreement", unit = "single")
#0.51

vox_H3_ders14 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.14, T2_DERS.14)
icc(vox_H3_ders14, model = "twoway", type = "agreement", unit = "single")
#0.39

vox_H3_ders15 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.15, T2_DERS.15)
icc(vox_H3_ders15, model = "twoway", type = "agreement", unit = "single")
#0.55

vox_H3_ders16 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.16, T2_DERS.16)
icc(vox_H3_ders16, model = "twoway", type = "agreement", unit = "single")
#0.62

vox_H3_ders17 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.17, T2_DERS.17)
icc(vox_H3_ders17, model = "twoway", type = "agreement", unit = "single")
#0.44

vox_H3_ders18 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.18, T2_DERS.18)
icc(vox_H3_ders18, model = "twoway", type = "agreement", unit = "single")
#0.51

vox_H3_ders19 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.19, T2_DERS.19)
icc(vox_H3_ders19, model = "twoway", type = "agreement", unit = "single")
#0.53

vox_H3_ders20 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.20, T2_DERS.20)
icc(vox_H3_ders20, model = "twoway", type = "agreement", unit = "single")
#0.41

vox_H3_ders21 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.21, T2_DERS.21)
icc(vox_H3_ders21, model = "twoway", type = "agreement", unit = "single")
#0.47

vox_H3_ders22 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.22, T2_DERS.22)
icc(vox_H3_ders22, model = "twoway", type = "agreement", unit = "single")
#0.66

vox_H3_ders23 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.23, T2_DERS.23)
icc(vox_H3_ders23, model = "twoway", type = "agreement", unit = "single")
#0.65

vox_H3_ders24 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.24, T2_DERS.24)
icc(vox_H3_ders24, model = "twoway", type = "agreement", unit = "single")
#0.42

vox_H3_ders25 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.25, T2_DERS.25)
icc(vox_H3_ders25, model = "twoway", type = "agreement", unit = "single")
#0.44

vox_H3_ders26 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.26, T2_DERS.26)
icc(vox_H3_ders26, model = "twoway", type = "agreement", unit = "single")
#0.53

vox_H3_ders27 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.27, T2_DERS.27)
icc(vox_H3_ders27, model = "twoway", type = "agreement", unit = "single")
#0.61

vox_H3_ders28 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.28, T2_DERS.28)
icc(vox_H3_ders28, model = "twoway", type = "agreement", unit = "single")
#0.56

vox_H3_ders29 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.29, T2_DERS.29)
icc(vox_H3_ders29, model = "twoway", type = "agreement", unit = "single")
#0.35

vox_H3_ders30 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.30, T2_DERS.30)
icc(vox_H3_ders30, model = "twoway", type = "agreement", unit = "single")
#0.28

vox_H3_ders31 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.31, T2_DERS.31)
icc(vox_H3_ders31, model = "twoway", type = "agreement", unit = "single")
#0.48

vox_H3_ders32 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.32, T2_DERS.32)
icc(vox_H3_ders32, model = "twoway", type = "agreement", unit = "single")
#0.63

vox_H3_ders33 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.33, T2_DERS.33)
icc(vox_H3_ders33, model = "twoway", type = "agreement", unit = "single")
#0.53

vox_H3_ders34 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.34, T2_DERS.34)
icc(vox_H3_ders34, model = "twoway", type = "agreement", unit = "single")
#0.20

vox_H3_ders35 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.35, T2_DERS.35)
icc(vox_H3_ders35, model = "twoway", type = "agreement", unit = "single")
#0.34

vox_H3_ders36 <- filter(vox, group == "Patient") %>%
  select(T1_DERS.36, T2_DERS.36)
icc(vox_H3_ders36, model = "twoway", type = "agreement", unit = "single")
#0.49

## average ICC from all item level DERS for patients
(0.69 + 0.43 + 0.44 + 0.71 + 0.79 + 
    0.54 + 0.73 + 0.67 + 0.37 + 0.40 + 
    0.40 + 0.33 + 0.51 + 0.39 + 0.55 + 
    0.62 + 0.44 + 0.51 + 0.53 + 0.41 + 
    0.47 + 0.66 + 0.65 + 0.42 + 0.44 + 
    0.53 + 0.61 + 0.56 + 0.35 + 0.28 + 
    0.48 + 0.63 + 0.53 + 0.20 + 0.34 + 0.49) / 36
# = 0.50


## test-retest reliability of ders item 1-36 baseline 1 and 2 using ICC
#controls

vox_H3_ders1_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.1, T2_DERS.1)
icc(vox_H3_ders1_c, model = "twoway", type = "agreement", unit = "single")
#0.50

vox_H3_ders2_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.2, T2_DERS.2)
icc(vox_H3_ders2_c, model = "twoway", type = "agreement", unit = "single")
#0.66

vox_H3_ders3_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.3, T2_DERS.3)
icc(vox_H3_ders3_c, model = "twoway", type = "agreement", unit = "single")
#0.28

vox_H3_ders4_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.4, T2_DERS.4)
icc(vox_H3_ders4_c, model = "twoway", type = "agreement", unit = "single")
#0.35

vox_H3_ders5_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.5, T2_DERS.5)
icc(vox_H3_ders5_c, model = "twoway", type = "agreement", unit = "single")
#0.67

vox_H3_ders6_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.6, T2_DERS.6)
icc(vox_H3_ders6_c, model = "twoway", type = "agreement", unit = "single")
#0.33

vox_H3_ders7_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.7, T2_DERS.7)
icc(vox_H3_ders7_c, model = "twoway", type = "agreement", unit = "single")
#0.48

vox_H3_ders8_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.8, T2_DERS.8)
icc(vox_H3_ders8_c, model = "twoway", type = "agreement", unit = "single")
#0.56

vox_H3_ders9_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.9, T2_DERS.9)
icc(vox_H3_ders9_c, model = "twoway", type = "agreement", unit = "single")
#0.36

vox_H3_ders10_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.10, T2_DERS.10)
icc(vox_H3_ders10_c, model = "twoway", type = "agreement", unit = "single")
#0.36

vox_H3_ders11_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.11, T2_DERS.11)
icc(vox_H3_ders11_c, model = "twoway", type = "agreement", unit = "single")
#0.32

vox_H3_ders12_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.12, T2_DERS.12)
icc(vox_H3_ders12_c, model = "twoway", type = "agreement", unit = "single")
#0.21

vox_H3_ders13_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.13, T2_DERS.13)
icc(vox_H3_ders13_c, model = "twoway", type = "agreement", unit = "single")
#0.50

vox_H3_ders14_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.14, T2_DERS.14)
icc(vox_H3_ders14_c, model = "twoway", type = "agreement", unit = "single")
#0.40

vox_H3_ders15_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.15, T2_DERS.15)
icc(vox_H3_ders15_c, model = "twoway", type = "agreement", unit = "single")
#-0.02

vox_H3_ders16_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.16, T2_DERS.16)
icc(vox_H3_ders16_c, model = "twoway", type = "agreement", unit = "single")
#0.20

vox_H3_ders17_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.17, T2_DERS.17)
icc(vox_H3_ders17_c, model = "twoway", type = "agreement", unit = "single")
#0.58

vox_H3_ders18_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.18, T2_DERS.18)
icc(vox_H3_ders18_c, model = "twoway", type = "agreement", unit = "single")
#0.57

vox_H3_ders19_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.19, T2_DERS.19)
icc(vox_H3_ders19_c, model = "twoway", type = "agreement", unit = "single")
#0.30

vox_H3_ders20_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.20, T2_DERS.20)
icc(vox_H3_ders20_c, model = "twoway", type = "agreement", unit = "single")
#0.55

vox_H3_ders21_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.21, T2_DERS.21)
icc(vox_H3_ders21_c, model = "twoway", type = "agreement", unit = "single")
#0.54

vox_H3_ders22_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.22, T2_DERS.22)
icc(vox_H3_ders22_c, model = "twoway", type = "agreement", unit = "single")
#0.42

vox_H3_ders23_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.23, T2_DERS.23)
icc(vox_H3_ders23_c, model = "twoway", type = "agreement", unit = "single")
#0.24

vox_H3_ders24_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.24, T2_DERS.24)
icc(vox_H3_ders24_c, model = "twoway", type = "agreement", unit = "single")
#0.26

vox_H3_ders25_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.25, T2_DERS.25)
icc(vox_H3_ders25_c, model = "twoway", type = "agreement", unit = "single")
#0.33

vox_H3_ders26_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.26, T2_DERS.26)
icc(vox_H3_ders26_c, model = "twoway", type = "agreement", unit = "single")
#0.60

vox_H3_ders27_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.27, T2_DERS.27)
icc(vox_H3_ders27_c, model = "twoway", type = "agreement", unit = "single")
#0.56

vox_H3_ders28_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.28, T2_DERS.28)
icc(vox_H3_ders28_c, model = "twoway", type = "agreement", unit = "single")
#0.16

vox_H3_ders29_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.29, T2_DERS.29)
icc(vox_H3_ders29_c, model = "twoway", type = "agreement", unit = "single")
#0.57

vox_H3_ders30_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.30, T2_DERS.30)
icc(vox_H3_ders30_c, model = "twoway", type = "agreement", unit = "single")
#0.71

vox_H3_ders31_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.31, T2_DERS.31)
icc(vox_H3_ders31_c, model = "twoway", type = "agreement", unit = "single")
#0.30

vox_H3_ders32_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.32, T2_DERS.32)
icc(vox_H3_ders32_c, model = "twoway", type = "agreement", unit = "single")
#0.46

vox_H3_ders33_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.33, T2_DERS.33)
icc(vox_H3_ders33_c, model = "twoway", type = "agreement", unit = "single")
#0.50

vox_H3_ders34_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.34, T2_DERS.34)
icc(vox_H3_ders34_c, model = "twoway", type = "agreement", unit = "single")
#0.51

vox_H3_ders35_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.35, T2_DERS.35)
icc(vox_H3_ders35_c, model = "twoway", type = "agreement", unit = "single")
#0.17

vox_H3_ders36_c <- filter(vox, group == "Control") %>%
  select(T1_DERS.36, T2_DERS.36)
icc(vox_H3_ders36_c, model = "twoway", type = "agreement", unit = "single")
#0.30


## average ICC from all item level DERS for controls
(0.50 + 0.66 + 0.28 + 0.35 + 0.67 + 
    0.33 + 0.48 + 0.56 + 0.36 + 0.36 + 
    0.32 + 0.21 + 0.50 + 0.43 + (-0.02) + 
    0.20 + 0.58 + 0.57 + 0.30 + 0.55 + 
    0.54 + 0.42 + 0.24 + 0.26 + 0.33 + 
    0.60 + 0.56 + 0.16 + 0.57 + 0.71 + 
    0.30 + 0.46 + 0.50 + 0.51 + 0.17 + 0.30) / 36
# = 0.41


## test-retest reliability of LSAS subscales baseline 1 and 2 using ICC

# patients
vox_df_H3_lsasavo_pat <- filter(vox, group == "Patient") %>%
  select(T1_lsas.avo, T2_lsas.avo)
icc(vox_df_H3_lsasavo_pat, model = "twoway", type = "agreement", unit = "single")
#0.78

vox_df_H3_lsasanx_pat <- filter(vox, group == "Patient") %>%
  select(T1_lsas.anx, T2_lsas.anx)
icc(vox_df_H3_lsasanx_pat, model = "twoway", type = "agreement", unit = "single")
#0.84


# controls
vox_df_H3_lsasavo_con <- filter(vox, group == "Control") %>%
  select(T1_lsas.avo, T2_lsas.avo)
icc(vox_df_H3_lsasavo_con, model = "twoway", type = "agreement", unit = "single")
#0.66

vox_df_H3_lsasanx_con <- filter(vox, group == "Control") %>%
  select(T1_lsas.anx, T2_lsas.anx)
icc(vox_df_H3_lsasanx_con, model = "twoway", type = "agreement", unit = "single")
#0.75


## test-retest reliability of LSAS items by subscale baseline 1 and 2 using ICC

## avoidance subscale
# patients

vox_df_H3_lsas1avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-1.u", "T2_LSAS-1.u")
icc(vox_df_H3_lsas1avo, model = "twoway", type = "agreement", unit = "single")
# 0.67

vox_df_H3_lsas2avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-2.u", "T2_LSAS-2.u")
icc(vox_df_H3_lsas2avo, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas3avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-3.u", "T2_LSAS-3.u")
icc(vox_df_H3_lsas3avo, model = "twoway", type = "agreement", unit = "single")
# 0.59

vox_df_H3_lsas4avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-4.u", "T2_LSAS-4.u")
icc(vox_df_H3_lsas4avo, model = "twoway", type = "agreement", unit = "single")
# 0.50

vox_df_H3_lsas5avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-5.u", "T2_LSAS-5.u")
icc(vox_df_H3_lsas5avo, model = "twoway", type = "agreement", unit = "single")
# 0.61

vox_df_H3_lsas6avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-6.u", "T2_LSAS-6.u")
icc(vox_df_H3_lsas6avo, model = "twoway", type = "agreement", unit = "single")
# 0.61

vox_df_H3_lsas7avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-7.u", "T2_LSAS-7.u")
icc(vox_df_H3_lsas7avo, model = "twoway", type = "agreement", unit = "single")
# 0.79

vox_df_H3_lsas8avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-8.u", "T2_LSAS-8.u")
icc(vox_df_H3_lsas8avo, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas9avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-9.u", "T2_LSAS-9.u")
icc(vox_df_H3_lsas9avo, model = "twoway", type = "agreement", unit = "single")
# 0.68

vox_df_H3_lsas10avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-10.u", "T2_LSAS-10.u")
icc(vox_df_H3_lsas10avo, model = "twoway", type = "agreement", unit = "single")
# 0.45

vox_df_H3_lsas11avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-11.u", "T2_LSAS-11.u")
icc(vox_df_H3_lsas11avo, model = "twoway", type = "agreement", unit = "single")
# 0.47

vox_df_H3_lsas12avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-12.u", "T2_LSAS-12.u")
icc(vox_df_H3_lsas12avo, model = "twoway", type = "agreement", unit = "single")
# 0.68

vox_df_H3_lsas13avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-13.u", "T2_LSAS-13.u")
icc(vox_df_H3_lsas13avo, model = "twoway", type = "agreement", unit = "single")
# 0.90

vox_df_H3_lsas14avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-14.u", "T2_LSAS-14.u")
icc(vox_df_H3_lsas14avo, model = "twoway", type = "agreement", unit = "single")
# 0.70

vox_df_H3_lsas15avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-15.u", "T2_LSAS-15.u")
icc(vox_df_H3_lsas15avo, model = "twoway", type = "agreement", unit = "single")
# 0.46

vox_df_H3_lsas16avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-16.u", "T2_LSAS-16.u")
icc(vox_df_H3_lsas16avo, model = "twoway", type = "agreement", unit = "single")
# 0.70

vox_df_H3_lsas17avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-17.u", "T2_LSAS-17.u")
icc(vox_df_H3_lsas17avo, model = "twoway", type = "agreement", unit = "single")
# 0.69

vox_df_H3_lsas18avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-18.u", "T2_LSAS-18.u")
icc(vox_df_H3_lsas18avo, model = "twoway", type = "agreement", unit = "single")
# 0.47

vox_df_H3_lsas19avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-19.u", "T2_LSAS-19.u")
icc(vox_df_H3_lsas19avo, model = "twoway", type = "agreement", unit = "single")
# 0.55

vox_df_H3_lsas20avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-20.u", "T2_LSAS-20.u")
icc(vox_df_H3_lsas20avo, model = "twoway", type = "agreement", unit = "single")
# 0.60

vox_df_H3_lsas21avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-21.u", "T2_LSAS-21.u")
icc(vox_df_H3_lsas21avo, model = "twoway", type = "agreement", unit = "single")
# 0.60

vox_df_H3_lsas22avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-22.u", "T2_LSAS-22.u")
icc(vox_df_H3_lsas22avo, model = "twoway", type = "agreement", unit = "single")
# 0.55

vox_df_H3_lsas23avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-23.u", "T2_LSAS-23.u")
icc(vox_df_H3_lsas23avo, model = "twoway", type = "agreement", unit = "single")
# 0.68

vox_df_H3_lsas24avo <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-24.u", "T2_LSAS-24.u")
icc(vox_df_H3_lsas24avo, model = "twoway", type = "agreement", unit = "single")
# 0.73

## average ICC from all avo item level LSAS for patients
(0.67 + 0.64 + 0.59 + 0.50 + 0.61 + 
    0.61 + 0.79 + 0.64 + 0.68 + 0.45 + 
    0.47 + 0.68 + 0.90 + 0.70 + 0.46 + 
    0.70 + 0.69 + 0.47 + 0.55 + 0.60 + 
    0.60 + 0.55 + 0.68 + 0.73) / 24
# = 0.62


# controls

vox_df_H3_lsas1avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-1.u", "T2_LSAS-1.u")
icc(vox_df_H3_lsas1avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.58

vox_df_H3_lsas2avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-2.u", "T2_LSAS-2.u")
icc(vox_df_H3_lsas2avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.66

vox_df_H3_lsas3avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-3.u", "T2_LSAS-3.u")
icc(vox_df_H3_lsas3avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.92

vox_df_H3_lsas4avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-4.u", "T2_LSAS-4.u")
icc(vox_df_H3_lsas4avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.48

vox_df_H3_lsas5avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-5.u", "T2_LSAS-5.u")
icc(vox_df_H3_lsas5avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.69

vox_df_H3_lsas6avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-6.u", "T2_LSAS-6.u")
icc(vox_df_H3_lsas6avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.53

vox_df_H3_lsas7avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-7.u", "T2_LSAS-7.u")
icc(vox_df_H3_lsas7avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.23

vox_df_H3_lsas8avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-8.u", "T2_LSAS-8.u")
icc(vox_df_H3_lsas8avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.65

vox_df_H3_lsas9avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-9.u", "T2_LSAS-9.u")
icc(vox_df_H3_lsas9avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.49

vox_df_H3_lsas10avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-10.u", "T2_LSAS-10.u")
icc(vox_df_H3_lsas10avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.51

vox_df_H3_lsas11avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-11.u", "T2_LSAS-11.u")
icc(vox_df_H3_lsas11avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.44

vox_df_H3_lsas12avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-12.u", "T2_LSAS-12.u")
icc(vox_df_H3_lsas12avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.58

vox_df_H3_lsas13avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-13.u", "T2_LSAS-13.u")
icc(vox_df_H3_lsas13avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.50

vox_df_H3_lsas14avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-14.u", "T2_LSAS-14.u")
icc(vox_df_H3_lsas14avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.86

vox_df_H3_lsas15avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-15.u", "T2_LSAS-15.u")
icc(vox_df_H3_lsas15avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.68

vox_df_H3_lsas16avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-16.u", "T2_LSAS-16.u")
icc(vox_df_H3_lsas16avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.55

vox_df_H3_lsas17avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-17.u", "T2_LSAS-17.u")
icc(vox_df_H3_lsas17avo_c, model = "twoway", type = "agreement", unit = "single")
# -0.06

vox_df_H3_lsas18avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-18.u", "T2_LSAS-18.u")
icc(vox_df_H3_lsas18avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.61

vox_df_H3_lsas19avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-19.u", "T2_LSAS-19.u")
icc(vox_df_H3_lsas19avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.56

vox_df_H3_lsas20avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-20.u", "T2_LSAS-20.u")
icc(vox_df_H3_lsas20avo_c, model = "twoway", type = "agreement", unit = "single")
# -0.04

vox_df_H3_lsas21avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-21.u", "T2_LSAS-21.u")
icc(vox_df_H3_lsas21avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.48

vox_df_H3_lsas22avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-22.u", "T2_LSAS-22.u")
icc(vox_df_H3_lsas22avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.83

vox_df_H3_lsas23avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-23.u", "T2_LSAS-23.u")
icc(vox_df_H3_lsas23avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.50

vox_df_H3_lsas24avo_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-24.u", "T2_LSAS-24.u")
icc(vox_df_H3_lsas24avo_c, model = "twoway", type = "agreement", unit = "single")
# 0.29

## average ICC from all avo item level LSAS for patients
(0.58 + 0.66 + 0.92 + 0.48 + 0.69 + 
    0.53 + 0.23 + 0.65 + 0.49 + 0.51 + 
    0.44 + 0.58 + 0.50 + 0.86 + 0.68 + 
    0.55 + -0.06 + 0.61 + 0.56 + -0.04 + 
    0.48 + 0.83 + 0.50 + 0.29) / 24
# = 0.52



## anxiety subscale
# patients

vox_df_H3_lsas1anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-1.r", "T2_LSAS-1.r")
icc(vox_df_H3_lsas1anx, model = "twoway", type = "agreement", unit = "single")
# 0.73

vox_df_H3_lsas2anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-2.r", "T2_LSAS-2.r")
icc(vox_df_H3_lsas2anx, model = "twoway", type = "agreement", unit = "single")
# 0.55

vox_df_H3_lsas3anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-3.r", "T2_LSAS-3.r")
icc(vox_df_H3_lsas3anx, model = "twoway", type = "agreement", unit = "single")
# 0.66

vox_df_H3_lsas4anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-4.r", "T2_LSAS-4.r")
icc(vox_df_H3_lsas4anx, model = "twoway", type = "agreement", unit = "single")
# 0.69

vox_df_H3_lsas5anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-5.r", "T2_LSAS-5.r")
icc(vox_df_H3_lsas5anx, model = "twoway", type = "agreement", unit = "single")
# 0.71

vox_df_H3_lsas6anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-6.r", "T2_LSAS-6.r")
icc(vox_df_H3_lsas6anx, model = "twoway", type = "agreement", unit = "single")
# 0.21

vox_df_H3_lsas7anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-7.r", "T2_LSAS-7.r")
icc(vox_df_H3_lsas7anx, model = "twoway", type = "agreement", unit = "single")
# 0.67

vox_df_H3_lsas8anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-8.r", "T2_LSAS-8.r")
icc(vox_df_H3_lsas8anx, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas9anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-9.r", "T2_LSAS-9.r")
icc(vox_df_H3_lsas9anx, model = "twoway", type = "agreement", unit = "single")
# 0.73

vox_df_H3_lsas10anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-10.r", "T2_LSAS-10.r")
icc(vox_df_H3_lsas10anx, model = "twoway", type = "agreement", unit = "single")
# 0.60

vox_df_H3_lsas11anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-11.r", "T2_LSAS-11.r")
icc(vox_df_H3_lsas11anx, model = "twoway", type = "agreement", unit = "single")
# 0.55

vox_df_H3_lsas12anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-12.r", "T2_LSAS-12.r")
icc(vox_df_H3_lsas12anx, model = "twoway", type = "agreement", unit = "single")
# 0.67

vox_df_H3_lsas13anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-13.r", "T2_LSAS-13.r")
icc(vox_df_H3_lsas13anx, model = "twoway", type = "agreement", unit = "single")
# 0.83

vox_df_H3_lsas14anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-14.r", "T2_LSAS-14.r")
icc(vox_df_H3_lsas14anx, model = "twoway", type = "agreement", unit = "single")
# 0.71

vox_df_H3_lsas15anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-15.r", "T2_LSAS-15.r")
icc(vox_df_H3_lsas15anx, model = "twoway", type = "agreement", unit = "single")
# 0.43

vox_df_H3_lsas16anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-16.r", "T2_LSAS-16.r")
icc(vox_df_H3_lsas16anx, model = "twoway", type = "agreement", unit = "single")
# 0.61

vox_df_H3_lsas17anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-17.r", "T2_LSAS-17.r")
icc(vox_df_H3_lsas17anx, model = "twoway", type = "agreement", unit = "single")
# 0.65

vox_df_H3_lsas18anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-18.r", "T2_LSAS-18.r")
icc(vox_df_H3_lsas18anx, model = "twoway", type = "agreement", unit = "single")
# 0.69

vox_df_H3_lsas19anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-19.r", "T2_LSAS-19.r")
icc(vox_df_H3_lsas19anx, model = "twoway", type = "agreement", unit = "single")
# 0.63

vox_df_H3_lsas20anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-20.r", "T2_LSAS-20.r")
icc(vox_df_H3_lsas20anx, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas21anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-21.r", "T2_LSAS-21.r")
icc(vox_df_H3_lsas21anx, model = "twoway", type = "agreement", unit = "single")
# 0.71

vox_df_H3_lsas22anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-22.r", "T2_LSAS-22.r")
icc(vox_df_H3_lsas22anx, model = "twoway", type = "agreement", unit = "single")
# 0.70

vox_df_H3_lsas23anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-23.r", "T2_LSAS-23.r")
icc(vox_df_H3_lsas23anx, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas24anx <- filter(vox, group == "Patient") %>%
  select("T1_LSAS-24.r", "T2_LSAS-24.r")
icc(vox_df_H3_lsas24anx, model = "twoway", type = "agreement", unit = "single")
# 0.77

## average ICC from all anx item level LSAS for patients
(0.73 + 0.55 + 0.66 + 0.69 + 0.71 + 
    0.21 + 0.67 + 0.64 + 0.73 + 0.60 + 
    0.55 + 0.67 + 0.83 + 0.71 + 0.43 + 
    0.61 + 0.65 + 0.69 + 0.63 + 0.64 + 
    0.71 + 0.70 + 0.64 + 0.77) / 24
# = 0.64


# controls

vox_df_H3_lsas1anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-1.r", "T2_LSAS-1.r")
icc(vox_df_H3_lsas1anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.22

vox_df_H3_lsas2anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-2.r", "T2_LSAS-2.r")
icc(vox_df_H3_lsas2anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.65

vox_df_H3_lsas3anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-3.r", "T2_LSAS-3.r")
icc(vox_df_H3_lsas3anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.92

vox_df_H3_lsas4anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-4.r", "T2_LSAS-4.r")
icc(vox_df_H3_lsas4anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.66

vox_df_H3_lsas5anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-5.r", "T2_LSAS-5.r")
icc(vox_df_H3_lsas5anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.47

vox_df_H3_lsas6anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-6.r", "T2_LSAS-6.r")
icc(vox_df_H3_lsas6anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.65

vox_df_H3_lsas7anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-7.r", "T2_LSAS-7.r")
icc(vox_df_H3_lsas7anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.23

vox_df_H3_lsas8anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-8.r", "T2_LSAS-8.r")
icc(vox_df_H3_lsas8anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.78

vox_df_H3_lsas9anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-9.r", "T2_LSAS-9.r")
icc(vox_df_H3_lsas9anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.78

vox_df_H3_lsas10anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-10.r", "T2_LSAS-10.r")
icc(vox_df_H3_lsas10anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.74

vox_df_H3_lsas11anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-11.r", "T2_LSAS-11.r")
icc(vox_df_H3_lsas11anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.63

vox_df_H3_lsas12anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-12.r", "T2_LSAS-12.r")
icc(vox_df_H3_lsas12anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.62

vox_df_H3_lsas13anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-13.r", "T2_LSAS-13.r")
icc(vox_df_H3_lsas13anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.83

vox_df_H3_lsas14anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-14.r", "T2_LSAS-14.r")
icc(vox_df_H3_lsas14anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.64

vox_df_H3_lsas15anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-15.r", "T2_LSAS-15.r")
icc(vox_df_H3_lsas15anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.50

vox_df_H3_lsas16anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-16.r", "T2_LSAS-16.r")
icc(vox_df_H3_lsas16anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.67

vox_df_H3_lsas17anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-17.r", "T2_LSAS-17.r")
icc(vox_df_H3_lsas17anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.57

vox_df_H3_lsas18anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-18.r", "T2_LSAS-18.r")
icc(vox_df_H3_lsas18anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.54

vox_df_H3_lsas19anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-19.r", "T2_LSAS-19.r")
icc(vox_df_H3_lsas19anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.78

vox_df_H3_lsas20anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-20.r", "T2_LSAS-20.r")
icc(vox_df_H3_lsas20anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.18

vox_df_H3_lsas21anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-21.r", "T2_LSAS-21.r")
icc(vox_df_H3_lsas21anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.72

vox_df_H3_lsas22anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-22.r", "T2_LSAS-22.r")
icc(vox_df_H3_lsas22anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.66

vox_df_H3_lsas23anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-23.r", "T2_LSAS-23.r")
icc(vox_df_H3_lsas23anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.62

vox_df_H3_lsas24anx_c <- filter(vox, group == "Control") %>%
  select("T1_LSAS-24.r", "T2_LSAS-24.r")
icc(vox_df_H3_lsas24anx_c, model = "twoway", type = "agreement", unit = "single")
# 0.34

## average ICC from all anx item level LSAS for patients
(0.22 + 0.65 + 0.92 + 0.66 + 0.47 + 
    0.65 + 0.23 + 0.78 + 0.78 + 0.74 + 
    0.63 + 0.62 + 0.83 + 0.64 + 0.50 + 
    0.67 + 0.57 + 0.54 + 0.78 + 0.18 + 
    0.72 + 0.66 + 0.62 + 0.34) / 24
# = 0.60


## test-retest reliability of sud7 baseline 1 and 2 using ICC

# patients
vox_df_H3_sud7_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud7, T2_sud7)

icc(vox_df_H3_sud7_pat, model = "twoway", type = "agreement", unit = "single")
# 0.59


# controls
vox_df_H3_sud7_con <- filter(vox, group == "Control") %>%
  select(T1_sud7, T2_sud7)

icc(vox_df_H3_sud7_con, model = "twoway", type = "agreement", unit = "single")
# 0.44


#### VISUALIZATION ####

#visualizing mean sud change by occasion for all, Supplementary
mssupplot <- ggplot(data = vox_df_completesudgraphall, aes(x = variable, y = mean, group = interaction(occasion, group))) +
  geom_point(aes(color = occasion, shape = group), size = 2.5) +
  geom_line(aes(color = occasion), size = 1) +
  scale_shape_manual(values=c(17, 16)) +
  ylim(0, 80) +
  theme_apa() +
  xlab("SUD rating occasion") +
  ylab("Mean SUD") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(0.4)) +
  scale_color_viridis_d(name  ="Time point",
                        breaks=c("1", "2", "3", "4", "7"),
                        labels=c("Baseline 1", "Baseline 2", "Mid-treatment", "Post-treatment", "2-year follow-up")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("sud1","sud2","sud3","sud4","sud5","sud6","sud7","sud8","sud9"),
                   labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme(legend.position = "bottom")


## visualizing mean ders change by occasion for all
vox_df_dersgraphpatcon <- vox_long_2 %>%
  filter(variable == "ders.tot") %>%
  group_by(occasion, variable, group) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(length(value)), n = sum(!is.na(value))) %>%
  mutate(lwr = mean - 1.96*(sd/sqrt(n)), upr = mean + 1.96*(sd/sqrt(n)))


msplot1 <- ggplot(data = vox_df_dersgraphpatcon, aes(x = occasion, y = mean)) +
  geom_point(aes(shape = group), size = 2.5, show.legend = FALSE) +
  scale_shape_manual(values=c(17, 16)) +
  #geom_line(aes(group = group), size = 1) +
  #geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(0.9)) +
  ylim(40, 100) +
  theme_apa() +
  xlab("Time in months") +
  ylab("Mean DERS") +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  annotate(geom = "text", x = 12, y = 91, label = expression("Dotted line:"~italic(d)~" = 0.30")) +
  annotate(geom = "text", x = 12.25, y = 86, label = expression("Dashed line:"~italic(d)~" = 0.15")) +
  annotate(geom = "segment", x = 0, xend = 2.25, y = 91.52, yend = 89.63, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 0, xend = 2.25, y = 50.15, yend = 53.95, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 2.25, xend = 3.25, y = 89.63, yend = 83.22, linetype = 3, size = 1) +
  annotate(geom = "segment", x = 3.25, xend = 4.5, y = 83.22, yend = 80.07, linetype = 2, size = 1) +
  annotate(geom = "segment", x = 4.5, xend = 10.5, y = 80.07, yend = 74.61, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 10.5, xend = 16.5, y = 74.61, yend = 69.54, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 16.5, xend = 28, y = 69.54, yend = 72.39, linetype = 1, size = 1)


## visualizing mean lsas change by occasion for all
vox_df_lsasgraphpatcon <- vox_long_2 %>%
  filter(variable == "lsas.tot") %>%
  group_by(occasion, variable, group) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(length(value)), n = sum(!is.na(value))) %>%
  mutate(lwr = mean - 1.96*(sd/sqrt(n)), upr = mean + 1.96*(sd/sqrt(n)))

msplot2 <- ggplot(data = vox_df_lsasgraphpatcon, aes(x = occasion, y = mean)) +
  geom_point(aes(shape = group), size = 2.5, show.legend = FALSE) +
  scale_shape_manual(values=c(17, 16)) +
  #geom_line(aes(group = group), size = 1) +
  #geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(0.9)) +
  ylim(0, 90) +
  theme_apa() +
  xlab("Time in months") +
  ylab("Mean LSAS-SR") +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  annotate(geom = "text", x = 12, y = 73, label = expression("Dotted line:"~italic(d)~" = 0.28")) +
  annotate(geom = "text", x = 12.25, y = 65, label = expression("Dashed line:"~italic(d)~" = 1.05")) +
  annotate(geom = "segment", x = 0, xend = 2.25, y = 76.00, yend = 73.37, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 0, xend = 2.25, y = 7.69, yend = 9.03, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 2.25, xend = 3.25, y = 73.37, yend = 67.7, linetype = 3, size = 1) +
  annotate(geom = "segment", x = 3.25, xend = 4.5, y = 67.7, yend = 44.52, linetype = 2, size = 1) +
  annotate(geom = "segment", x = 4.5, xend = 10.5, y = 44.52, yend = 38.39, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 10.5, xend = 16.5, y = 38.39, yend = 33.56, linetype = 1, size = 1) +
  annotate(geom = "segment", x = 16.5, xend = 28, y = 33.56, yend = 38.44, linetype = 1, size = 1)

## visualizing mean sud7 change by occasion for all
vox_df_sudgraphpatcon <- vox_long_2 %>%
  filter(variable == "sud7") %>%
  group_by(occasion, variable, group) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(length(value)), n = sum(!is.na(value)))  %>%
  mutate(lwr = mean - 1.96*(sd/sqrt(n)), upr = mean + 1.96*(sd/sqrt(n)))

msplot3 <- ggplot(data = vox_df_sudgraphpatcon, aes(x = occasion, y = mean)) +
  geom_point(aes(shape = group), size = 2.5, show.legend = FALSE) +
  scale_shape_manual(values=c(17, 16)) +
  geom_line(aes(group = group), size = 1) +
  #geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(0.9)) +
  ylim(0, 80) +
  theme_apa() +
  xlab("Time in months") +
  ylab("Mean SUD rating (speech anxiety)") +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  annotate(geom = "point", x = 0, y = 62.12, color = "#440154FF", size = 2.5) +
  annotate(geom = "point", x = 2.25, y = 46.03, color = "#414487FF", size = 2.5) +
  annotate(geom = "point", x = 3.25, y = 40.5, color = "#2A788EFF", size = 2.5) +
  annotate(geom = "point", x = 4.5, y = 27.55, color = "#7AD151FF", size = 2.5) +
  annotate(geom = "point", x = 28, y = 23.63, color = "#FDE725FF", size = 2.5) +
  annotate(geom = "point", x = 0, y = 4.8, color = "#440154FF", size = 2.5, shape = 17) +
  annotate(geom = "point", x = 2.25, y = 2.4, color = "#414487FF", size = 2.5, shape = 17) 

## visualizing mean all sud change by occasion for all
vox_df_completesudgraphall <- vox_long %>%
  filter(variable %in% c("sud1", "sud2", "sud3","sud4", "sud5", "sud6", "sud7", "sud8", "sud9")) %>%
  group_by(occasion, variable, group) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(length(value)), n = sum(!is.na(value))) %>%
  mutate(lwr = mean - 1.96*(sd/sqrt(n)), upr = mean + 1.96*(sd/sqrt(n)))

vox_df_completesudgraphall$variable <- as.factor(vox_df_completesudgraphall$variable)
vox_df_completesudgraphall$occasion <- as.factor(vox_df_completesudgraphall$occasion)

msplot4 <- ggplot(data = vox_df_completesudgraphall, aes(x = variable, y = mean, group = interaction(occasion, group))) +
  geom_point(aes(color = occasion, shape = group), size = 2.5) +
  scale_shape_manual(values=c(17, 16)) +
  geom_line(aes(color = occasion), size = 1) +
  gghighlight(variable == "sud7", keep_scales = TRUE) +
  ylim(0, 80) +
  theme_apa() +
  xlab("SUD rating occasion") +
  ylab("Mean SUD") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(0.4), alpha = 0.85) +
  scale_color_viridis_d(name  ="Time point",
                        breaks=c("1", "2", "3", "4", "7"),
                        labels=c("Baseline 1", "Baseline 2", "Mid-treatment", "Post-treatment", "28-month follow-up")) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_discrete(breaks=c("sud1","sud2","sud3","sud4","sud5","sud6","sud7","sud8","sud9"),
                   labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme(legend.position = "bottom")


# arranging these 4 plots for the manuscript into one figure
msplotfinal <- ggarrange(msplot1, msplot2, msplot3, msplot4, labels=c("a)", "b)", "c)", "d)"), align = c("hv"), common.legend = TRUE, legend = "bottom")


#### ANALYSIS EXPLORATORY/OTHER ####

## split of data with regards to LSAS and DERS into two sets: treatment and follow-up
# to do two separate analyses to try this out, testing what it looks like

# take df from analysis H1
vox_df_H1_treat <- vox_df_H1 %>%
  filter(occasion %in% c(0, 2.25, 3.25, 4.5))

vox_df_H1_fu <- vox_df_H1 %>%
  filter(occasion %in% c(10.5, 16.5, 28))

H1mod_treat_1 <- lme(lsas.tot ~ occasion*ders.tot.b + occasion*ders.tot.lag.w, data = vox_df_H1_treat, random = ~ occasion | ID, method = "REML", na.action = na.exclude)
summary(H1mod_treat_1)

ggplot(data = vox_df_H1_treat, aes(x = occasion, y = lsas.tot, group = group)) +
  geom_line(aes(group = ID, color = ID)) +
  ylim(0, 144) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm")

H1mod_fu_1 <- lme(lsas.tot ~ occasion*ders.tot.b + occasion*ders.tot.lag.w, data = vox_df_H1_fu, random = ~ 1 | ID, method = "REML", na.action = na.exclude)
summary(H1mod_fu_1)

ggplot(data = vox_df_H1_fu, aes(x = occasion, y = lsas.tot, group = group)) +
  geom_line(aes(group = ID, color = ID)) +
  ylim(0, 144) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm")



## analysis to look at group-level changes in patients between pre-mid and mid-post
# this is done to investigate the magnitude of change in LSAS and DERS between these timepoints

vox_df_premid <- vox %>%
  filter(group == "Patient") %>%
  select("ID", "T2_ders.tot", "T3_ders.tot", "T4_ders.tot", "T2_lsas.tot", "T3_lsas.tot", "T4_lsas.tot")

describe(vox_df_premid$T2_ders.tot)
describe(vox_df_premid$T3_ders.tot)
describe(vox_df_premid$T4_ders.tot)

t.test(vox_df_premid$T2_ders.tot, vox_df_premid$T3_ders.tot, paired = TRUE)
t.test(vox_df_premid$T3_ders.tot, vox_df_premid$T4_ders.tot, paired = TRUE)

describe(vox_df_premid$T2_lsas.tot)
describe(vox_df_premid$T3_lsas.tot)
describe(vox_df_premid$T4_lsas.tot)

t.test(vox_df_premid$T2_lsas.tot, vox_df_premid$T3_lsas.tot, paired = TRUE)
t.test(vox_df_premid$T3_lsas.tot, vox_df_premid$T4_lsas.tot, paired = TRUE)


#cohens d for paired measurements
cohen.d(vox_df_premid$T2_ders.tot, vox_df_premid$T3_ders.tot, paired = TRUE)
cohen.d(vox_df_premid$T3_ders.tot, vox_df_premid$T4_ders.tot, paired = TRUE)

cohen.d(vox_df_premid$T2_lsas.tot, vox_df_premid$T3_lsas.tot, paired = TRUE)
cohen.d(vox_df_premid$T3_lsas.tot, vox_df_premid$T4_lsas.tot, paired = TRUE)


## analysis of test-retest reliability with ICC for all Subjective Units of Distress ratings
#sud1-sud9

## ICC for sud1 between baseline 1 and 2

# patients
vox_df_sud1_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud1, T2_sud1)

icc(vox_df_sud1_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud1_con <- filter(vox, group == "Control") %>%
  select(T1_sud1, T2_sud1)

icc(vox_df_sud1_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud2 between baseline 1 and 2

# patients
vox_df_sud2_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud2, T2_sud2)

icc(vox_df_sud2_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud2_con <- filter(vox, group == "Control") %>%
  select(T1_sud2, T2_sud2)

icc(vox_df_sud2_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud3 between baseline 1 and 2

# patients
vox_df_sud3_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud3, T2_sud3)

icc(vox_df_sud3_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud3_con <- filter(vox, group == "Control") %>%
  select(T1_sud3, T2_sud3)

icc(vox_df_sud3_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud4 between baseline 1 and 2

# patients
vox_df_sud4_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud4, T2_sud4)

icc(vox_df_sud4_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud4_con <- filter(vox, group == "Control") %>%
  select(T1_sud4, T2_sud4)

icc(vox_df_sud4_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud5 between baseline 1 and 2

# patients
vox_df_sud5_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud5, T2_sud5)

icc(vox_df_sud5_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud5_con <- filter(vox, group == "Control") %>%
  select(T1_sud5, T2_sud5)

icc(vox_df_sud5_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud6 between baseline 1 and 2

# patients
vox_df_sud6_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud6, T2_sud6)

icc(vox_df_sud6_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud6_con <- filter(vox, group == "Control") %>%
  select(T1_sud6, T2_sud6)

icc(vox_df_sud6_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud7 between baseline 1 and 2

# patients
vox_df_sud7_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud7, T2_sud7)

icc(vox_df_sud7_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud7_con <- filter(vox, group == "Control") %>%
  select(T1_sud7, T2_sud7)

icc(vox_df_sud7_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud8 between baseline 1 and 2

# patients
vox_df_sud8_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud8, T2_sud8)

icc(vox_df_sud8_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud8_con <- filter(vox, group == "Control") %>%
  select(T1_sud8, T2_sud8)

icc(vox_df_sud8_con, model = "twoway", type = "agreement", unit = "single")



## ICC for sud9 between baseline 1 and 2

# patients
vox_df_sud9_pat <- filter(vox, group == "Patient") %>%
  select(T1_sud9, T2_sud9)

icc(vox_df_sud9_pat, model = "twoway", type = "agreement", unit = "single")

# controls
vox_df_sud9_con <- filter(vox, group == "Control") %>%
  select(T1_sud9, T2_sud9)

icc(vox_df_sud9_con, model = "twoway", type = "agreement", unit = "single")


