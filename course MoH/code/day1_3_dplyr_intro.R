library(ggplot2)
library(dplyr)
library(sf)
#importer la base de donnees qui contient les cas de la maladie X
data<-read.csv("data/dataset1.csv")
table(as.factor(data$sexe))

#####dplyr basics
## filtrer les donnees: exemple on ne veut examiner que les donnees des cas de sexe masculin

#la fonction filter() permet de voir les donnees en fonction de certaines valeurs
#le premier argument est la base de donnee, les arguments suivants sont les expressions qui filtrent la base de donnees

filter(data, region=="Melaky")
filter(data, region=="Analamanga",age<2)

#dplyr ne modifie pas la base donnee initiale, mais on peut creer un nouvel objet en utilisant <-
enfants_AN<-filter(data, region=="Analamanga",age<2)


