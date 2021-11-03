library(ggplot2)
library(dplyr)
library(sf)
#importer la base de donnees qui contient les cas de la maladie X
data<-read.csv("code/dataset1.csv")
table(as.factor(data$sexe))

#####dplyr basics


## filtrer les donnees: exemple on ne veut examiner que les donnees des cas de sexe masculin

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

#piping with dplyr "%>%"

MKage2<-data%>%
  filter(region=="Melaky")%>%
  arrange(age)


## Summary statistics

# count number of cases
data%>%
  summarise(cas=n())

# count number of cases per region
data%>%
  group_by(region)%>%
  summarise(cas=n())



# count number of cases per region per day
data%>%
  group_by(region,date)%>%
  summarise(cas=n())


## ajouter une nouvelle colonne (mutate)

data%>%
  group_by(region,date)%>%
  summarise(cas=n())%>%
  mutate(rollmean=rollmeanr(cas,7,fill=NA))->moyenne_region
