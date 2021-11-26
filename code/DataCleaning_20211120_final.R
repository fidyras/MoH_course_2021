
################################################################################
### MODULE 3: Data Cleaning                                                  ### 
################################################################################

################################################################################
### MODULE 3 : Nettoyage des données                                         ### 
################################################################################

## OBJECTIVE:
## The goal of this exercise is to become accustomed to using R to perform data cleaning. While we will not show all possible ways to clean data, we will focus on an example data set and highlight common areas for data cleaning.

## OBJECTIF:
## Le but de cet exercice est de s'habituer à utiliser R pour effectuer le nettoyage des données. Bien que nous ne montrons pas toutes les manières possibles de nettoyer les données, nous nous concentrerons sur un exemple d'ensemble de données et soulignerons les domaines communs pour le nettoyage des données.

## just as a reminder the general steps of data cleaning include: 
# 1. Remove duplicate or irrelevant observations
# 2. Fix structural errors
# 3. Filter unwanted outliers
# 4. Handle missing data
# 5. Validate and quality assurance 

#  pour vous rappeler des étapes générales du nettoyage des données, elles incluent: 
# 1. Supprimer les observations en double ou non pertinentes
# 2. Corriger les erreurs structurelles
# 3. Filtrer les valeurs aberrantes indésirables
# 4. Gérer les données manquantes
# 5. Validation et assurance de qualité

### One of the most important aspects is validating and quality assurance -- effectively checking our data!
# Useful things to think about are: 
#   Do the data make sense?
#   What are some metrics we can use to determine if the data is ‘appropriate’? 
#   What are you assumptions about how the data should be? Your hypotheses? Your theories?
#   Are there preliminary checks of the data should make? 
#   Are there data quality issues? 
# 
### Un des aspects les plus importants est la validation et l'assurance de la qualité-- effectivement de vérifier nos données!
# Validation et Assurance de Qualité
# 
# Les données ont-elles un sens ?
#   Quelles métriques pouvons-nous utiliser pour déterminer si les données sont «appropriées»?
#   Comment supposez vous que les données devraient être? Quelles sont vos hypothèses ? Vos théories ?
#   Y a-t-il des vérifications préliminaires des données à effectuer?
#   Y a-t-il des problèmes de qualité des données?

### There is no one way to perform data cleaning and this will depend on the data set and question of interest -- but there are general things you may want to check. Effectively, you want to develop a pipeline to perform your own data quality/cleaning checks so you can develop a system of making sure your data is clean so you can perform analyses.  

### Il n'y a pas qu'une seule façon d'effectuer le nettoyage 
##des données et votre processus dépendra de l'ensemble de données et 
##de la question d'intérêt - mais il y a des choses générales à vérifier. 
##En effet, vous souhaitez développer un processus pour effectuer 
##vos propres contrôles de qualité/nettoyage des données afin de pouvoir développer 
##un système permettant de s'assurer que vos données sont propres afin de pouvoir 
##effectuer des analyses solides.

### A. Let's read in some data ##################################

# Here we are going to read in a data sample of surveillance data from the MOH in Madagascar. We are going to first start by reading in the data set. 
# Ici, nous allons lire un échantillon de 
##données de surveillance du MS au Madagascar. 
##Nous allons d'abord commencer par lire l'ensemble des données.

library(tidyverse)
setwd('~/Desktop/MoH_course_2021-main/DataCleaning/')
data_file <- read.csv('DataSample_final.csv', header = TRUE)

# We see that there is a problem with an error message showing up in red. We cannot clean our data unless we can read in our data. We can fix this by allowing for French accents. 
# Nous voyons qu'il y a un problème avec un message d'erreur qui s'affiche en rouge. 
# Nous ne pouvons pas nettoyer nos données à moins de pouvoir lire nos données. 
# Nous pouvons résoudre ce problème en autorisant les accents en français.

#data_file <- read.csv('DataSample.csv', header = TRUE, fileEncoding = "Latin1", check.names = F)
# Si la ligne ci-dessus génère des avertissement, vous pouvez essayer la suivante:
data_file <- read.csv("DataSample_final.csv", header=TRUE, encoding = "UTF-8", check.names = F)

# Now what should we check in the data first, we see that there are different columns that represent different pathogens -- to make it easier we are going to relabel the date variable as 'date'; Suspicion d'arbovirose as 'sus_arb'; Diarrh\u008ees aigues chez moins 5ans as 'diarr_u5', and Toxi Infection Alimentaire collective (TIAC) as 'tiac'
# Maintenant, que devons-nous vérifier  en premier, nous voyons qu'il y a différentes colonnes qui représentent des agents pathogènes -- 
# pour rendre le processus plus facile, nous allons renommer 
##la variable que donne la date avec l'étiquete "date"; 
# Suspicion d'arbovirose comme «sus_arb»; 
# Diarrhées aigues chez moins 5ans comme 'diarr_u5', 
#et Toxi Infection Alimentaire collective (TIAC) comme 'tiac'

colnames(data_file) ## will display the column names of our data set
nrow(data_file) ## will count the number of rows in the data set
head(data_file) ## will display the first few rows of the data set

# In these data, we want to make some summary plots of suspected arbovirus cases; diarrhea in children under 5; and toxic foods -- we are going to focus on these column names but since we are going to be using them often, we will first shorten these names to something easier. 
# Dans ces données, nous voulons créer des figures qui récapitulent les cas suspects d'arbovirus; diarrhée chez les enfants de moins de 5 ans; et les aliments toxiques -- nous allons nous concentrer sur ces données, mais puisque nous allons les utiliser souvent, nous allons d'abord raccourcir ces noms pour faciliter l'analyse .
# Arbovirus suspision 
# Diarrhées aigues chez moins 5ans - O 
# Toxi Infection Alimentaire collective (TIAC)

data_file$date <- data_file$`Date SE` 
data_file$sus_arb <- data_file$`Suspicion d'arbovirose`
data_file$diarr_u5 <- data_file$"Diarrh\xe9es aigues chez moins 5ans"  
data_file$tiac <- data_file$`Toxi Infection Alimentaire collective (TIAC)`

## and we can check how well this worked -- 
## et nous pouvons vérifier à quel point cela a fonctionné --
colnames(data_file)

## Often there are issues with names and dates, so we will check those first. 
# Checking dates ############################################
## Souvent, il y a des problèmes avec les noms et les dates, nous allons donc les vérifier en premier.
# Vérification des dates ############################################

table(data_file$date)

# We can see that there are two dates that seem to be incorrect -- the day is coming before the month -- 26/10/2020  29/6/2020 -- we can use a command 'str_replace' to fix these. 
# Nous pouvons voir qu'il y a deux dates qui semblent être incorrectes -- le jour vient avant le mois -- 26/10/2020 29/6/2020 -- nous pouvons utiliser une commande 'str_replace' pour les corriger.
data_file$date <- str_replace(data_file$date, '26/10/2020', '10/26/20')
data_file$date <- str_replace(data_file$date, '29/6/2020', '6/29/20')
## note: after we replace something or change something to clean the data - we are going to keep checking back on these data to see if there may be other errors we have missed 
## Remarque: après avoir remplacé quelque chose ou modifié quelque chose pour nettoyer les données, nous allons continuer de vérifier ces données pour voir s'il peut y avoir d'autres erreurs que nous avons manquées
table(data_file$date)

# we can also see that there is one date where there is only 1 entry -- this may also be an error that we can correct -- 5/10/20
# nous pouvons également voir qu'il y a une date où il n'y a qu'une seule entrée -- cela peut aussi être une erreur que nous pouvons corriger -- 5/10/20
data_file$date <- str_replace(data_file$date, '5/10/20', '10/5/20')
table(data_file$date)

# We can see that each there may be a few other dates that look strange in our data -- 1/20/19; 1/27/19 have very few recorded observations. But these may be true things in the data -- it could be that there are certain parts of our data where few people were reporting and so these errors, while strange may not be errors. What are some reasons why these data may be different? Are there ways we can check? 
# Nous pouvons voir qu'il peut y avoir quelques autres dates 
#qui semblent étranges dans nos données -- 1/20/19 ; 27/01/19,
#ces dates ont très peu d'observations enregistrées. 
#Mais ce sont peut-être des choses vraies dans les données - 
#il se peut qu'il y ait certaines parties de nos données où peu de 
#personnes ont signalé et donc ces données, bien qu'étranges, 
#pourraient être correctes. Quelles sont les raisons pour 
# lesquelles ces données pourraient être différentes? 
# Existe-t-il des moyens de les vérifier?
plot(table(data_file$date)) ## plot the total number of instances of each date in the data set 

data_file[which(data_file$date == '1/20/19'),] ## show me the row where the date is equal to 1/20/19
data_file$date <- str_replace(data_file$date, '1/20/19', '1/20/20') ## replace 1/20/19 with the correct year of 1/20/20

data_file[which(data_file$date == '1/27/19'),] ## show me the row where the date is equal to 1/27/19
data_file$date <- str_replace(data_file$date, '1/27/19', '1/27/20') ## replace 1/27/19 with the correct year of 1/27/20

table(data_file$date)
# and we can make it a date variable
# et nous pouvons en faire une variable en format d'une date
# load library for managing dates (lubridate)
# on charge la bibliothèque pour la géstion des dates
library(lubridate)
data_file$date <- mdy(data_file$date) ## put my date in a correct date format 

# Checking location information ############################################
# Now let's check the locations -- how many districts and regions should there be in the data? First, let's clean the region variable. 
# Vérification des informations de localisation ############################################
# Maintenant, vérifions les emplacements -- 
# combien de districts et de régions devons nous avoir dans les données? 
#Tout d'abord, nettoyons la variable <<region>>.

sort(unique(data_file$region)) ## the unique regions that are listed in the data - shown alphabetically

## First, we can see that there are too many regions -- why are some of additional regions?
## Premièrement, nous pouvons voir qu'il y a trop de régions -- 
#pourquoi y a-t-il des régions supplémentaires?

data_file$region <- str_replace(data_file$region, 'BOENY', 'Boeny')
data_file$region <- str_replace(data_file$region, 'boeny', 'Boeny')
sort(unique(data_file$region))

## there are still too many -- we can see that there is a 'Boeny' and a ' Boeny' too
## il y en a encore trop -- on voit qu'il y a un 'Boeny' et un 
#' Boeny' aussi --espace en surplus. 

data_file$region <- str_replace(data_file$region, ' Boeny', 'Boeny')
sort(unique(data_file$region))

## there are still too many -- there are also two Melaky
## il y en a encore trop -- il y a aussi deux Melaky
data_file$region <- str_replace(data_file$region, 'MELAKY', 'Melaky')
sort(unique(data_file$region))

data_file$region <- str_replace(data_file$region, 'Mangoro Alaotra', 'Alaotra Mangoro')
sort(unique(data_file$region))

## for now we will just focus on the regions and work on cleaning the districts later. 
## pour l'instant nous allons nous concentrer uniquement sur les 
# régions et travailler sur le nettoyage des quartiers plus tard.


### START - FRIDAY ACTIVITY: 
# Fix district names ############################################
# Now we can check on the districts and clean them up similar to what we did for regions

sort(unique(data_file$district))
## in total we should have 114, but we see that we have 116 which means we have at least 2 errors (assuming every district reports data)

## we can see that two districts have an error with an extra space before the name
data_file$district <- str_replace(data_file$district, ' Ambalavao', 'Ambalavao')
data_file$district <- str_replace(data_file$district, ' Ambanja', 'Ambanja')

sort(unique(data_file$district))

# Checking for duplicate rows ############################################
# Now let's check to see if there are duplicate rows, first we'll check it using the date variable and see if there are dates where there more than 114 districts reporting

dates_to_check <- names(table(data_file$date))[which(table(data_file$date)>114)]
dates_to_check
# There are 3 dates where there are more than 114 districts reporting data. We can check the data for those dates. 

## check the first date identified
check_1 <- data_file %>% filter(date == '2019-12-30')
nrow(check_1) ## there are 116 rows in the data, but only 114 unique district names 
length(unique(check_1$district)) ## 114 unique district names 
## we can look at how many instances of each district occur in these data and see that 'Bealanana' has 2 rows of data
sort(table(check_1$district))
## we can look at these data and we see that data was entered 2 times and the rows of these data do match (i.e. it is a clear duplicate of this row)
check_1 %>% filter(district == 'Bealanana')
row_to_remove <- rownames(data_file[which(data_file$date == '2019-12-30' & data_file$district == 'Bealanana' & data_file[,1] == 2),])

## we can remove one row of these data from our data set then
data_file <- data_file[-as.numeric(row_to_remove),]
table(data_file$date)

## check the second date identified
check_2 <- data_file %>% filter(date == '2020-01-27')
nrow(check_2) ## there are 116 rows in the data, but only 114 unique district names 
length(unique(check_2$district)) ## 114 unique district names 
## we can look at how many instances of each district occur in these data and see that 'Bealanana' has 3 rows of data
sort(table(check_2$district))
check_2 %>% filter(district == 'Bealanana')

### These all suggest that there is an issue with reporting from Bealanana where there are multiple reports in a given month with different values in that month. This should warrant further investigation in trying to figure out why this particular district is reporting multiple reports in certain months. 

## for now, we will just remove this date and district data since we are not sure which row to check 

row_to_remove_2 <- rownames(data_file[which(data_file$date == '2020-01-27' & data_file$district == 'Bealanana'),])
data_file <- data_file[-as.numeric(row_to_remove_2),]

### END - FRIDAY ACTIVITY

# Finally plotting a time series ############################################
# Now let's finally plot how many suspected arbovirus cases there are by day
# Enfin fair un figure d'une série temporelle ############################################
# Maintenant, calculons enfin le nombre de cas suspects 
#d'arbovirus par jour
plot(data_file$date, data_file$sus_arb, xlab = 'Date', ylab = 'Susc. Arb cases') 

# Does this plot look reasonable to you? do we think there may be some errors? 
# First, it looks like there were a huge number of cases two days -- let's investigate these cases -- 
# Cette figure vous semble-t-elle raisonnable? 
#pensons-nous qu'il peut y avoir des erreurs?
# Premièrement, il semble qu'il y a un grand nombre de cas en
#deux jours -- enquêtons sur ces cas --

data_file[which(data_file$sus_arb == max(data_file$sus_arb, na.rm=T)),] ## look at the rows where the number of reportes suscp arb cases are the highest -- we want to look at the two very high outliers 

# were there any outbreaks during this time? or is this a data error? We can first look at the other cases that were reported in this region 
# y a-t-il eu des épidémies pendant cette période? 
#ou est-ce une erreur dans les données ? 
#Nous pouvons d'abord regarder les autres cas qui ont été signalés 
#dans cette région

very_high_sus_arb <- filter(data_file, region == 'Amoron i Mania') ## look at just that region's data
plot(very_high_sus_arb$date, very_high_sus_arb$sus_arb)

## These are the only 2 missing values -- which makes it suspicious that there really were 10,000 cases reported on two very close days to one another. But this is not definitive proof that these are errors. We can also consult other reports, historical information, the initial data entry forms, databases, etc. to check these data since they look very suspicious. It turns out that these were both data entry errors -- for these two days there was only 1 case not 10,000 cases reported. 
## Ce sont les 2 seules valeurs manquantes - 
#ce qui rend suspect qu'il y ait vraiment eu 10 000 
#cas signalés au cours de deux jours très proches l'un de l'autre.
#Mais ce n'est pas une preuve définitive qu'il s'agit d'erreurs. 
#Nous pouvons également consulter d'autres rapports, 
#des informations historiques, les formulaires de données initiales, 
#des bases de données, etc. pour vérifier ces données car 
#elles semblent très bizarre. Après une invenstigation nous découvrons qu'il s'agissait de deux erreurs 
#de l'entrée des données dans la base de données - pour ces deux jours, en réalité il n'y a eu qu'un seul cas 
# de signalé.

## we can correct these data now. 
## nous pouvons corriger ces données maintenant.
row_index_sus_arb_error <- rownames(data_file[which(data_file$sus_arb == max(data_file$sus_arb, na.rm=T)),])
data_file[row_index_sus_arb_error,'sus_arb'] <- c(1,1)

## now let's look at the plot again -- we also have a lot of missing data -- we would want to double check that there weren't issues with the data entry. Do we think that there should be a large number of arborvirus cases reported in 2019? 
## Maintenant, regardons à nouveau la figure -- 
#nous avons également beaucoup de données manquantes --
#nous voudrions vérifier qu'il n'y a pas eu de problèmes avec 
#l'entrée des données. 
#Pensons-nous qu'il devrait y avoir un grand nombre de cas
#d'arborvirus signalés en 2019 ? 
plot(data_file$date, data_file$sus_arb, xlab = 'Date', ylab = 'Susc. Arb cases') 

## but this still isn't the plot we wanted -- we wanted the total number per day
## Mais cette figures n'est pas tout a fait correcte-- nous voulons le nobre totale de cas par jour. 
total_sus_arb_per_day <- data_file %>% group_by(date) %>% summarise(total_arb_cases = sum(sus_arb, na.rm=T))

plot(total_sus_arb_per_day$date, total_sus_arb_per_day$total_arb_cases, xlab = 'Date', ylab = 'Total susp arbovirus cases')

## there was an outbreak in 2020 ->2021, now lets see if these cases were in particular locations (regions) or not
## il y a eu une épidémie en 2020 ->2021, 
#voyons maintenant si ces cas se trouvaient 
#dans des endroits particuliers (régions) ou non

sus_arb_cases_2020_region <- data_file %>% filter(date > '2020-01-01') %>% group_by(date, region) %>% summarise(total_arb_cases = sum(sus_arb, na.rm=T))

ggplot(sus_arb_cases_2020_region, aes(x = date, y = total_arb_cases, color = region)) + geom_line() + xlab('Date') + ylab('Total Susp Arb Cases')

## FRIDAY ACTIVITY 
# Plotting the time series of TIAC data per day ############################################

plot(total_tiac_per_day$date, total_tiac_per_day$total_tiac_cases, xlab = 'Date')

total_tiac_per_day <- data_file %>% group_by(date) %>% summarise(total_tiac_cases = sum(tiac, na.rm=T))
plot(total_tiac_per_day$date, total_tiac_per_day$total_tiac_cases, xlab = 'Date', ylab = 'Total tiac cases', type = 'b')

### and we can plot tiac by district 

tiac_cases_2020_district <- data_file %>% filter(date > '2020-01-01') %>% group_by(date, district) %>% summarise(total_tiac_cases = sum(tiac, na.rm=T))

ggplot(tiac_cases_2020_district, aes(x = date, y = total_tiac_cases, color = district)) + geom_line() + xlab('Date') + ylab('Total TIAC Cases')



