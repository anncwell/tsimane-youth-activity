################################
# tables and figures for annie #
################################
rm(list=ls())
library(openxlsx)
library(lavaan)
library(semPlot)
library(data.table)
library(purrr)
library(DiagrammeR)
library(ggplot2)

source('./code/project_funcitons.r')

################
# data munging #
################
d<-read.csv('./data/analysis_table.csv')

.c<-function(x) {foo<-(x - mean(x,na.rm=T)); return(foo)}
.std<-function(x) {foo<-(x-mean(x,na.rm=T))/sd(x,na.rm=T); return(foo)}

my.vars<-c("Edad","SumMinsSed", "SumMinsLight", "SumMinsMod", "SumMinsVig", "SumMinsMVPA", "steps_per_day", "SumMinsSed_full_day", "SumMinsLight_full_day", "SumMinsMod_full_day", "SumMinsVig_full_day", "SumMinsMVPA_full_day", "steps_per_day_full_day", "ht_velocity")
d.c<-d
apply(d.c[,my.vars], 2, function(x) .c(x))->d.c[,my.vars]
d.stnd<-d
apply(d.stnd[,my.vars], 2, function(x) .std(x))->d.stnd[,my.vars]


##############################################
# Table regressions with age*sex interaction #
##############################################


sed.mod<-lm(SumMinsSed ~ Edad + Sexo, data=d)
lig.mod<-lm(SumMinsLight_full_day ~ Edad + Sexo, data=d)
mod.mod<-lm(SumMinsMod_full_day ~ Edad + Sexo, data=d)
vig.mod<-lm(SumMinsVig_full_day ~ Edad + Sexo, data=d)
mvpa.mod<-lm(SumMinsMVPA_full_day ~ Edad + Sexo, data=d)
step.mod<-lm(steps_per_day_full_day ~ Edad + Sexo, data=d)


sed.int.mod<-lm(SumMinsSed ~ Edad * Sexo, data=d)
lig.int.mod<-lm(SumMinsLight_full_day ~ Edad * Sexo, data=d)
mod.int.mod<-lm(SumMinsMod_full_day ~ Edad * Sexo, data=d)
vig.int.mod<-lm(SumMinsVig_full_day ~ Edad * Sexo, data=d)
mvpa.int.mod<-lm(SumMinsMVPA_full_day ~ Edad * Sexo, data=d)
step.int.mod<-lm(steps_per_day_full_day ~ Edad * Sexo, data=d)

################################
#sem models on the full sample #
################################

source('./code/tanner_mediator_model.r')

summary(fitsed_t,fit.measures = TRUE)
summary(fitlig_t,fit.measures = TRUE)
summary(fitmvpa_t,fit.measures = TRUE)
summary(fitstep_t,fit.measures = TRUE)