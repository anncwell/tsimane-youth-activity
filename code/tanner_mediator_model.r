###########################
# tanner mediation models #
###########################

if(!dir.exists('./output/path_models/tanner_mediator')) dir.create('./output/path_models/tanner_mediator')
# sedentary

pathmodelLV<-'
#mediation 
Tanner~a*Edad
SumMinsSed~b*Tanner
SumMinsSed~c*Edad
#indirecct effect
ab:=a*b
#total effect
total:=c+a*b'
fitsed_t<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd)
path_model(fitsed_t, './output/path_models/tanner_mediator/Sedentary.png')

summary(fitsed_t,fit.measures = TRUE)



#sex compare
fitsed_t_male<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Male'),])
#path_model(fitsed_t_male, './output/path_models/tanner_mediator/male_Sed.png')
fitsed_t_female<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Female'),])
#path_model(fitsed_t_female, './output/path_models/tanner_mediator/female_Sed.png')
# light


pathmodelLV<-'
#mediation 
Tanner~a*Edad
SumMinsLight_full_day~b*Tanner
SumMinsLight_full_day~c*Edad
#indirecct effect
ab:=a*b
#total effect
total:=c+a*b'
fitlig_t<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd)
path_model(fitlig_t, './output/path_models/tanner_mediator/Light.png')

summary(fitlig_t,fit.measures = TRUE)



#sex compare
fitlig_t_male<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Male'),])
#path_model(fitlig_t_male, './output/path_models/tanner_mediator/male_Light.png')
fitlig_t_female<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Female'),])
#path_model(fitlig_t_female, './output/path_models/tanner_mediator/female_Light.png')



# mvpa


pathmodelLV<-'
#mediation 
Tanner~a*Edad
SumMinsMVPA_full_day~b*Tanner
SumMinsMVPA_full_day~c*Edad
#indirecct effect
ab:=a*b
#total effect
total:=c+a*b'
fitmvpa_t<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd)
path_model(fitmvpa_t, './output/path_models/tanner_mediator/MVPA.png')

summary(fitmvpa_t,fit.measures = TRUE)



#sex compare
fitmvpa_t_male<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Male'),])
#path_model(fitmvpa_t_male, './output/path_models/tanner_mediator/male_MVPA.png')
fitmvpa_t_female<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Female'),])
#path_model(fitmvpa_t_female, './output/path_models/tanner_mediator/female_MVPA.png')


# steps


pathmodelLV<-'
#mediation 
Tanner~a*Edad
steps_per_day_full_day~b*Tanner
steps_per_day_full_day~c*Edad
#indirecct effect
ab:=a*b
#total effect
total:=c+a*b'
fitstep_t<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd)
path_model(fitstep_t, './output/path_models/tanner_mediator/Steps.png')
summary(fitstep_t,fit.measures = TRUE)



#sex compare
fitstep_t_male<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Male'),])
#path_model(fitstep_t_male, './output/path_models/tanner_mediator/male_Steps.png')
fitstep_t_female<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd[which(d.stnd$Sexo=='Female'),])
#path_model(fitstep_t_female, './output/path_models/tanner_mediator/female_Steps.png')

pathmodelLV<-'
#mediation 
Tanner~a*Edad
SumMinsSed_full_day~b*Tanner
SumMinsSed_full_day~c*Edad
#indirecct effect
ab:=a*b
#total effect
total:=c+a*b'
fitsed_t_full_day<-sem(pathmodelLV, ordered = c("Tanner"),data=d.stnd)
path_model(fitsed_t_full_day, './output/path_models/tanner_mediator/Sedentary_full_day.png')

save(list=ls(), file='./output/path_models/tanner_mediator/models.RData')
