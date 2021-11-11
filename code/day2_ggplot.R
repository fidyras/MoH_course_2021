# day_2 ggplot2 and maps

library(ggplot2)
library(dplyr)
library(zoo)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(scales)

setwd("/Users/tanjona/Box Sync/misc/course/dashboard/MoH_course_2021")

## importer les données csv

dataset1 <- read.csv("code/dataset1.csv")

str(dataset1) # aperçu des données

# On remarque que les colonnes (variables) date et dob sont de type chr (character). On veut que le logiciel les identifie comme date. 
dataset1$date <- as.Date(dataset1$date)
dataset1$dob <- as.Date(dataset1$dob)

str(dataset1) # tout est en ordre

################################################
###### Section 1: figure de base minimale ######
################################################

#############################################
## 1 - Construire un barplot ################
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() 

#############################################
## 2 - Construire une histogram #############
ggplot(dataset1, aes(x = age)) +
  geom_histogram()

### a - Modifier l'orrientation
ggplot(dataset1, aes(y  = age)) +
  geom_histogram()

### b- Changer l' interval de groupement
ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 1)

###  Comme exercice, varier le binwidth  

###############################################
## 3- Construire une boxplot (box-whisker chart)
ggplot(dataset1, aes(y = age)) +
  geom_boxplot()

## l'axe des x n'a pas vraiment de sense. On va séparer les ages en fonction du sexe
ggplot(dataset1, aes(x = sexe, y = age)) +
  geom_boxplot()

#############################################
## 4- Construire une violin plot ############
ggplot(dataset1, aes(x = sexe, y = age)) +
  geom_violin()

#############################################
## 5 - Construire un scatter plot ###########

## L'ancien jeu de donnée ne contient pas d'information pour faire un scatter plot, on va importer un nouveau donnée qui contient la taille de la population par région et visualiser le nombre de cas en fonction de la 

## Le donnée brute est par classe d'age, on va importer ce donnée et ensuite calculer la population par region en faisant la somme des individus par classe d'age
pop0 <- read.csv("code/age_dist.csv") 
pop <- pop0 %>% 
  group_by(Region, key) %>% 
  summarise(pop_size = sum(sum_pop)) %>% 
  rename(region = Region) # pour uniformiser le nom du colonne region

## Calculer le nombre de cas par region
cas <- dataset1 %>% 
  group_by(region) %>% 
  summarise(n_cas = n())

## Combiner les deux données
dataset2 <- merge(cas, pop, by = "region") 

## on peut finalement faire la figure 
ggplot(dataset2, aes(x = pop_size, y = n_cas)) +
  geom_point()

##  Echanger les axes
ggplot(dataset2, aes(y = pop_size, x = n_cas)) +
  geom_point()

#############################################
## 6 - Construire un line plot ##############
ggplot(dataset2, aes(x = pop_size, y = n_cas)) +
  geom_line()

## On peut rajouter les points originaux
ggplot(dataset2, aes(x = pop_size, y = n_cas)) +
  geom_line() +
  geom_point()

#############################################
## 7- Serie temporelle ######################
## Une serie temporelle est juste un barplot, scatter plot ou line plot mais la variable sur l'axe des x est une date.

### a - construire un barplot. 
ggplot(dataset1, aes(x = date)) +
  geom_bar()

## Par défault, "il n'y a pas de y" car geom_bar() calcul directement le nombre de cas en comptant le nombre de fois qu'une certaine date apprait (i.e. le nombre de ligne). Pour geom_line() ou geom_point()., il faut définir y. Dans notre cas, le plus simple est de créer un nouveau jeu donnée. 

# On va calculer le nombre de cas par jour par la fonction count(), le nombre de cas cumulé pas la fonction cumsum(), et le nombre de cas moyen journalier sur une fenetre de 7 jours.

dataset1b <- dataset1 %>% 
  select(date) %>% 
  count(date) %>% 
  mutate(sumn = cumsum(n), ma = rollmean(n, 7, align = "right", fill = NA)) %>% 
  rename(n_cas = n)

### b- construire un lineplot
ggplot(dataset1b, aes(x = date, y = n_cas)) +
  geom_line()

### voir le nombre de cas cumulé
ggplot(dataset1b, aes(x = date, y = sumn)) +
  geom_line()

### c- construire un scatter plot
ggplot(dataset1b, aes(x = date, y = n_cas)) +
  geom_point()

### d - combiner bar et ligne
ggplot(dataset1b, aes(x = date)) +
  geom_bar(aes(y = n_cas), stat = "identity") +
  geom_line(aes(y = ma))

################################################
######### Section 2: controler les axes ########
################################################

#############################################
## 1- Renomer les axes ######################

### On recommande de toujours renomer les axes sur la figure pour ameliorer la lisibilité et comprehension. 
ggplot(dataset1b, aes(x = date, y = sumn)) +
  geom_line() +
  labs(x = "Date", y = "Cas cumulés")

## Si besoin, on peut aussi ajouter un titre et sous-titre
ggplot(dataset1b, aes(x = date, y = sumn)) +
  geom_line() +
  labs(x = "Date", y = "Cas cumulés", title = "un titre", subtitle = "un sous-titre")

#############################################
## 2- Echelle sur les axes ##################

ggplot(dataset2, aes(x = pop_size, y = n_cas)) +
  geom_line() +
  geom_point()

## La figure ci-dessus n'est pas pratique, sauf si on veut montrer que la region d'Analamanga est vraiment différent. Autrement, on peut transformer les axes. Le plus simple c'est d'utilisé une echelle logarithmique

ggplot(dataset2, aes(x = pop_size, y = n_cas)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

#############################################
## 3- Utiliser des proportions ou pourcentage ou densité sur l'axe des y

## Voici la figure simple 
ggplot(dataset1, aes(x = age))+
  geom_histogram(binwidth = 10)

## a - Histogramme avec proportion 

ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 10, aes(y = stat(count) / sum(count)))

## remarquer que les valeurs sur y change quand on change le binwidth. Maintenant, on va renomer les axes 

ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 10, aes(y = stat(count) / sum(count))) +
  labs(x = "Age", y = "Proportion")

## b - Histogramme avec pourentage

ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 10, aes(y = stat(count) / sum(count))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age", y = "Pourcentage")

## c- Histogramme avec densité

ggplot(dataset1, aes(x = age))+
  geom_histogram(aes(y = ..density..), binwidth = 10) +
  labs(x = "Age", y = "Densité")

################################################
########### Section 3: Groupement ##############
################################################

#############################################
## 1 - faceting #############################

## Il se peut que plusieurs facteurs sont en jeu. On avait crée une histogramme de tous les ages, mais on peut aussi créer deux histogrammes pour les deux sexes

ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~sexe)

# On peut aussi grouper en fonction de la region mais....une figure avec 22 paneaux peut etre trop encombrant. 

ggplot(dataset1, aes(x = age)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~region)

#############################################
## 2 - par fill #############################

# Autrement, on peut visualizer sur la même figure les deux sexes 
ggplot(dataset1, aes(x = age, fill = sexe)) +
  geom_histogram(binwidth = 10) 

## Ci dessus, les bars sont placées les unes sur les autres (stacked en anglais). Cela ne permet pas de vraiment comparer les classes d'age. On peut les placées l'une à coté de l'autre avec: position = "dodge"

ggplot(dataset1, aes(x = age, fill = sexe)) +
  geom_histogram(binwidth = 10, position = "dodge") 

#############################################
## 3 - par couleur  #########################

## On va d'abord créer deux nouvelles variables catégoriques/qualitatives: si la nombre de cas est supérieur ou non à la médiane et si la taille de la population est supérieur ou non à la médiane.

m1 <- median(dataset2$n_cas) ## calculer la moyenne des nombres de cas
m2 <- median(dataset2$pop_size) ## calculer la moyenne de la population

## Créer les variables (colonnes)
dataset2b <- dataset2 %>% 
  mutate(cas_bas = (n_cas < m1), pop_bas = (pop_size < m2))

### a-  Grouper par nombre de cas
ggplot(dataset2b, aes(x = pop_size, y = n_cas, color = cas_bas)) +
  geom_point()+
  scale_x_log10() +
  scale_y_log10()

### b-  Grouper par taille population
ggplot(dataset2b, aes(x = pop_size, y = n_cas, color = pop_bas)) +
  geom_point()+
  scale_x_log10() +
  scale_y_log10()

#############################################
## 4 - par symbole ###########################

ggplot(dataset2b, aes(x = pop_size, y = n_cas, shape = pop_bas)) +
  geom_point(size = 2)+
  scale_x_log10() +
  scale_y_log10()

#############################################
## 4 - par taille ###########################

## On peut aussi changer la taille des points en fonction d'autre variable
ggplot(dataset2b, aes(x = pop_size, y = n_cas, size = pop_size)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

#############################################
## 5 - Combiner couleur, forme, taille ######

ggplot(dataset2b, aes(x = pop_size, y = n_cas, color = cas_bas, shape = pop_bas, size = pop_size)) +
  geom_point()+
  scale_x_log10() +
  scale_y_log10()

################################################
############# Section 4: Filirony ##############
################################################

#############################################
## 1- Changer la couleur ####################

### a - changer la couleur des contours du rectangle
ggplot(dataset1, aes(x = age)) +
  geom_histogram(color = "blue")

### b - changer la couleur des points
ggplot(dataset2, aes(x = pop_size, y = n_cas))+
  geom_point(color = "blue")

### c- Changer la couleur de l'interieur du rectangle
ggplot(dataset1, aes(x = age)) +
  geom_histogram(fill = "blue", color = "white")

### d- Utiliser des valeurs personaliser
ggplot(dataset2b, aes(x = pop_size, y = n_cas, color = pop_bas)) +
  geom_point() +
  scale_y_log10() +
  scale_color_manual(values = c("#999999", "#E69F00"))

#############################################
## 2- Changer les labels ####################
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) # l'axe des x est une variable discrete/qualitative

ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_y_continuous(breaks = c(0, 12345)) # l'axe des y est une variable continue/quantitative 

ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_y_continuous(breaks = c(0, 12345), labels = c("zero", "example")) # l'axe des y est une variable continue/quantitative 

#############################################
## 3 - le puissant theme() ##################

## Cette fonction contient des "dizaines" d'arguement qui permet de controller l'esthetique de la figure. 

### a - changer la taille des polices
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) +
  theme(text = element_text(size = 22))

### on peut aussi specificer les textes sur les axes
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) +
  theme(axis.text.x = element_text(size = 22))

### en italique
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) +
  theme(axis.text.x = element_text(size = 22, face = "italic"))

### b - rotation des labels des axes
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) +
  theme(axis.text.x = element_text(angle = 35))

### Et aussi controler la position
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  scale_x_discrete(labels = c("Ampela", "Lahy")) +
  theme(axis.text.x = element_text(angle = 35, hjust = -0.5, vjust = -1))

### c - Changer le fond

ggplot(dataset1, aes(x = sexe)) +
  geom_bar()+
  theme(panel.background = element_rect(fill = "pink"))

### d- changer les lignes
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  theme(panel.grid.major = element_line(size = 1.5, color = "red"))

### et les lines mineurs

ggplot(dataset1, aes(x = sexe)) +
  geom_bar() +
  theme(panel.grid.major = element_line(size = 1.5, color = "red"),
        panel.grid.minor = element_line(size = 0.25, color = "black", linetype = "dashed"))

### On donne juste quelques example ici, veuillez vous référer au manuel pour plus d'information. Il exist des themes prédéfini dans ggplot. Taper theme_ dans RStudio et vous verrez les différentes options.

### VOICI UN EXAMPLE COMPLET ###
display.brewer.all() ## voir les palettes disponible avec brewer

col1 <- brewer.pal(4, "Set3")[4] # choisir dans une palette de couleur 

ggplot(dataset1, aes(x = sexe)) +
  geom_bar(fill = col1, col = "black") +
  labs(x = "Sexe", y = "Nombre", title = "Total covid-19 infection par sexe")+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "Black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray")) +
  scale_x_discrete(labels = c("Femelle", "Male")) 

################################################
############### Section 5: Carte ###############
################################################

# Le concept est la même pour créer une carte mais on a besoin d'une autre source de donnée pour la carte. En général, c'est un shapefile.

# faire une carte
library(sf)
library(scales)

## importer le delimitation administrative de Madagascar 
mdg0 <- read_sf("code/shapefile/MDG_ADM1.shp")
mdg1 <- rmapshaper::ms_simplify(mdg0, keep = 0.15, keep_shapes = T) #pour alleger la carte
mdg1 <- st_as_sf(mdg1)

## nombre de cas par region
region <- dataset1 %>%
  group_by(region) %>%
  summarise(n_cas=n())

## Age moyen par region
mean_age <- dataset1 %>% 
  group_by(region) %>% 
  summarise(mean_age = mean(age, na.rm = T))

## combiner les données
mdg2 <- mdg1 %>%
  left_join(region, by=c("NAME"="region")) %>% 
  left_join(pop, by = c("NAME" = "region"))

#############################################
## 1- nombre de cas par region ##############

ggplot(mdg2, aes(fill = n_cas)) +
  geom_sf(color = "yellow") 

#############################################
## 2 - changer la couleur ###################
ggplot(mdg2, aes(fill = n_cas)) +
  geom_sf(color = "yellow") +
  scale_fill_gradient(low = "white",
                      high = "black")

## une autre exemple
ggplot(mdg2, aes(fill = n_cas)) +
  geom_sf(color = "yellow") +
  scale_fill_viridis_b()

##  Le nombre de cas élévé à Analamanga domine la figure, on peut utiliser une echelle logarithmique comme précédemment. Mais ici, on 

ggplot(mdg2, aes(fill = n_cas)) +
  geom_sf(color = "yellow") +
  scale_fill_gradient(low = "white",
                      high = "black",
                      trans = "log10")

#############################################
## 3 - Categoriser les cas ##################

## Il n'est pas recommender d'avoir trop de couleur sur une carte. On peut définir manuellement des catégories  

grad <- c(100, 1000, 10000) # par example diviser en 4 catégories

ggplot(mdg2, aes(fill = n_cas)) +
  geom_sf(color = "yellow") +
  scale_fill_viridis_b(breaks =  grad,
                      trans = "log10")

#############################################
## 4 - et pour la route: des labels #########
ggplot(mdg2) + 
  geom_sf(fill = "white", color = "black") +
  geom_sf_text(aes(label = key), size = 2, color = "blue") +
  theme_void() # pour se focaliser sur la carte


# UNE EXEMPLE COMPLETE

ggplot(mdg2) +
  geom_sf(aes(fill = n_cas), color = "white") +
  geom_sf_text(aes(label = key), size = 2) +
  scale_fill_viridis_b(breaks = grad,
                       trans = "log10",
                       name = "Nombre de cas",
                       direction = -1)+ ## renverse la couleur
  theme_void()


#################################################
################# FINAL FIGURES #################
#################################################

dataset1b <- dataset1 %>% 
  select(date) %>% 
  count(date) %>% 
  mutate(sumn = cumsum(n), ma = rollmean(n, 7, align = "right", fill = NA))

## Cumulative case
ggplot(dataset1b, aes(x = date, y = sumn)) +
  geom_area() +
  theme_bw()+
  labs(x = "Date", y = "Nombre cas cumulés")

## Daily cases
ggplot(dataset1b, aes(x = date)) +
  geom_col(aes(y = n))+
  geom_line(aes(y = ma), col = "red", size = 1) +
  theme_bw() +
  labs(x = "Date", y = "Nombre cas par jour")

## Cases per region
mdg0 <- read_sf("code/shapefile/MDG_ADM1.shp")
mdg1 <- rmapshaper::ms_simplify(mdg0, keep = 0.15, keep_shapes = T) #pour alleger la carte
mdg1 <- st_as_sf(mdg1)

region <- dataset1 %>%
  group_by(region) %>%
  summarise(cas=n())

pop0 <- read.csv("code/age_dist.csv")
pop <- pop0 %>% 
  group_by(Region, key) %>% 
  summarise(pop_size = sum(sum_pop))

mean_age <- dataset1 %>% 
  group_by(region) %>% 
  summarise(mean_age = mean(age, na.rm = T))

mdg2 <- mdg1 %>%
  left_join(region,  by=c("NAME"="region")) %>% 
  left_join(pop, by = c("NAME" = "Region")) %>% 
  left_join(mean_age, by = c("NAME" = "region"))

breaks_cases = c(100, 1000, 10000)

ggplot(mdg2)+
  geom_sf(aes(fill = cas), color = "white")+
  geom_sf_text(aes(label = key), size = 2) +
  scale_fill_viridis_b(breaks = breaks_cases,
                       trans = "log10",
                       name = "Nombre de cas",
                       option = "A",
                       direction = -1)+
  theme_void()

## Scatter plot 
ggplot(mdg2, aes(x = pop_size, y = cas, color = mean_age, label = key)) +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_y_log10() +
  theme_bw() +
  geom_text(size = 1.75, vjust = -1.5, color = "black") +
  labs(x = "Population size (millions)", y = "Nombre de cas", color = "Age\nmoyen")

## Pyramide des ages
ageF <- dataset1 %>% 
  filter(!is.na(age) & sexe  == "FEMININ") %>%
  group_by(age)%>%
  summarise(cas=n())%>%
  mutate(age_cat=cut(age,breaks=c(seq(0,80,5),90,120),include.lowest = TRUE))%>%
  select("age"=age,"age_cat"=age_cat,cas)%>%
  group_by(age_cat)%>%
  summarise(cas=sum(cas))
ageF$sexe <- "FEMININ"

ageM <- dataset1 %>% 
  filter(!is.na(age) & sexe  == "MASCULIN") %>%
  group_by(age)%>%
  summarise(cas=n())%>%
  mutate(age_cat=cut(age,breaks=c(seq(0,80,5),90,120),include.lowest = TRUE))%>%
  select("age"=age,"age_cat"=age_cat,cas)%>%
  group_by(age_cat)%>%
  summarise(cas=sum(cas))
ageM$sexe <- "MASCULIN"

ageD <- rbind(ageF, ageM) %>% 
  group_by(sexe) %>% 
  mutate(prop = cas/sum(cas))

ggplot(ageD,aes(x=age_cat,y=prop,fill=sexe))+
  geom_col(data=subset(ageD,sexe == "MASCULIN"))+
  geom_col(data=subset(ageD,sexe == "FEMININ"),aes(y=prop*(-1)))+
  coord_flip()+
  theme(legend.position = "right",
          text = element_text(size = 12),
          panel.background=element_blank())+
  scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(breaks = c(-0.1, -0.05, 0, 0.05, 0.1), label= c("10%", "5%", "0%", "5%", "10%"))+
  labs(x="Categorie d'age",y="Proportion" , fill = "Sexe")





