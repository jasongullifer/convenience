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

## ----serr----------------------------------------------------------------
serr_summary.rt <- serr(data = lexdec.rt, dv = RT, id = Subject, Class, Complex, NativeLanguage)
print(serr_summary.rt)

serr_summary.acc <- serr(data = lexdec.acc, dv = accuracy, id = Subject, Class, Complex,NativeLanguage)
print(serr_summary.acc)

## ----plot----------------------------------------------------------------
ggplot(serr_summary.rt, aes(x = Class, y = mean_RT, ymin = mean_RT - 1.96 * SERR, ymax = mean_RT + 1.96 * SERR)) + 
  facet_wrap(~Complex) + geom_pointrange()

ggplot(serr_summary.acc, aes(x = Class, y = mean_accuracy, ymin = mean_accuracy - 1.96 * SERR, ymax = mean_accuracy + 1.96 * SERR)) + 
  facet_wrap(~Complex) + geom_pointrange()


## ----plot_1serr----------------------------------------------------------
ggplot(serr_summary.rt, aes(x = Class, y = mean_RT, ymin = lower, ymax = upper)) + 
  facet_wrap(~Complex) + geom_pointrange()

ggplot(serr_summary.acc, aes(x = Class, y = mean_accuracy, ymin = lower, ymax = upper)) + 
  facet_wrap(~Complex) + geom_pointrange()


