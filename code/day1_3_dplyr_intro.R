library(ggplot2)
library(dplyr)
library(sf)
library(zoo)



#importer la base de donnees qui contient les cas de la maladie X
data<-read.csv("code/dataset1.csv")



##### dplyr basics


## filtrer les donnees: exemple on ne veut examiner que les donnees des cas de sexe masculin ou les donnees de la region Melaky

#la fonction filter() permet de voir les donnees en fonction de certaines valeurs
#le premier argument est la base de donnee, les arguments suivants sont les expressions qui filtrent la base de donnees

filter(data, region=="Melaky")
filter(data, region=="Analamanga",age<2)

#dplyr ne modifie pas la base donnee initiale, mais on peut creer un nouvel objet en utilisant <-
enfants_AN<-filter(data, region=="Analamanga",age<2)

# trier les donnees
## trier les cas de la region Melaky, et uniquement la region Melaky en fonction de l'age des cas

MK<-filter(data, region=="Melaky")
MKage<-arrange(MK,age)

######################################################
###############

#piping with dplyr "%>%"

MKage2<-data%>%
  filter(region=="Melaky")%>%
  arrange(age)

##############################################################
## Summary statistics
##############################################################

# count number of cases
data%>%
  summarise(cas=n())

#compter le nombre de cas par jour
(day<-data%>%
  group_by(date)%>%
  summarise(cas=n()))

# count number of cases per region
(region<-data%>%
  group_by(region)%>%
  summarise(cas=n()))


#on peut donner plusieurs statistiques et grouper selon plusieurs facteurs 
data%>%
  group_by(region,sexe)%>%
  summarise(cas=n(),
            m_age=mean(age,na.rm=TRUE),
            md_age=median(age,na.rm=TRUE),
            sd=sd(age,na.rm = TRUE)
            )




# count number of cases per region per day
data%>%
  group_by(region,date)%>%
  summarise(cas=n())
  


## ajouter une nouvelle colonne (mutate)
###supposons que nous souhaiterions savoir quelle proportion des cas totaux ont eu lieu a quelle date et
### quelle est la moyenne (mobile) quotidienne des cas
data%>%
  group_by(date)%>%
  summarise(cas=n())%>%
  mutate(prop_pct=cas/sum(cas)*100)%>%
  mutate(rollmean=rollmeanr(cas,7,fill=NA))

###############################
##############

##grouper deux bases de donnees en fonction d'une colonne commune
tests<-read.csv("code/tests.csv")



day<-day%>%
  right_join(tests)%>% #joindre le tableau day (nombre de cas par jour avec le tableau contenant le nombre de tests effectues par jour
  filter(date<max(data$date))%>%#filtrer le tableau pour ne pas depasser la date maximale du tableau de cas
  arrange(date) 

day<-day%>%select(-X)
day

day$cas<-ifelse(is.na(day$cas),0,day$cas)
day



