## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----library-------------------------------------------------------------
library(languageR)
library(convenience)
library(ggplot2)
library(dplyr)

## ----lexdec--------------------------------------------------------------
data(lexdec)

## ----dataprep------------------------------------------------------------
lexdec.rt <- lexdec[lexdec$Correct=="correct",]
lexdec.acc <- lexdec

lexdec.acc$accuracy<-ifelse(lexdec.acc$Correct == "correct",1, 0)

## ----sem-----------------------------------------------------------------
sem_summary.rt <- sem(data = lexdec.rt, dv = RT, id = Subject, Class, Complex, NativeLanguage)
print(sem_summary.rt)

sem_summary.acc <- sem(data = lexdec.acc, dv = accuracy, id = Subject, Class, Complex,NativeLanguage)
print(sem_summary.acc)

## ----plot----------------------------------------------------------------
ggplot(sem_summary.rt, aes(x = Class, color=NativeLanguage, y = mean_RT, ymin = mean_RT - 1.96 * SEM, ymax = mean_RT + 1.96 * SEM)) + facet_wrap(~Complex) + geom_pointrange(position = position_dodge(.9))

ggplot(sem_summary.acc, aes(x = Class, color=NativeLanguage, y = mean_accuracy, ymin = mean_accuracy - 1.96 * SEM, ymax = mean_accuracy + 1.96 * SEM)) + facet_wrap(~Complex) + geom_pointrange(position = position_dodge(.9))


## ----plot_1sem-----------------------------------------------------------
ggplot(sem_summary.rt, aes(x = Class, color=NativeLanguage, y = mean_RT, ymin = lower, ymax = upper)) + facet_wrap(~Complex) + geom_pointrange(position = position_dodge(.9))

ggplot(sem_summary.acc, aes(x = Class, color=NativeLanguage, y = mean_accuracy, ymin = lower, ymax = upper)) + facet_wrap(~Complex) + geom_pointrange(position = position_dodge(.9))


