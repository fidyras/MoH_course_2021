# day_2 ggplot2 and maps

library(ggplot2)
library(dplyr)
library(zoo)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(scales)

# import csv

## Construire un barplot
ggplot(dataset1, aes(x = sexe)) +
  geom_bar() 


# to do: change axes label, y-scale, color bar and size text

# creer une histograme de la distribution des ages par sexe
ggplot(dataset1, aes(x = age, fill = sexe))+
  geom_histogram(binwidth = 10, position = "dodge")+
  facet_wrap(~sexe)


p1 <- dataset1 %>% 
  filter(sexe == "FEMININ") %>% 
  ggplot(aes(y = age))+
  geom_histogram(binwidth = 10, position = "dodge", col = 1)+
  labs(y  = "")

p2 <- dataset1 %>% 
  filter(sexe == "MASCULIN") %>% 
  ggplot(aes(y = age))+
    geom_histogram(binwidth = 10, position = "dodge", col = 1, fill = 3) +
    scale_x_reverse() +
  theme(axis.text.y = element_blank()) +
  labs(y = "")

ggarrange(p2, p1,  ncol = 2)

group by facet or color or shape

### Change le binwidth pour voir les difference
### remplacer x par y 

## Changer les valeurs sur y
dataset1%>%group_by(sexe) %>% 
  ggplot( aes(x = age))+
  geom_histogram( aes(y = stat(count) / sum(count),fill=sexe), binwidth = 10) +
  labs(y = "Percentage") +
  scale_y_continuous(labels = scales::percent)


ggplot(dataset1, aes(x = age))+
  geom_histogram( aes(y = ..density..), binwidth = 10) +
  labs(y = "Density")


# construire une boxplot
ggplot(dataset1, aes(x = sexe, y = age))+
  geom_boxplot()

ggplot(dataset1, aes(x = sexe, y = age))+
  geom_violin()

# nombre de nouveau cas
ggplot(dataset1, aes(x = date)) +
  geom_bar()

# une maniere de visioner le nombre de cas cumulé
ggplot(dataset1, aes(x = date)) +
  stat_bin(aes(y=cumsum(..count..)), geom= "bar", binwidth  = 1)

# creer un nouveau jeu de donner 
dataset1b <- dataset1 %>% 
  select(date) %>% 
  count(date) %>% 
  mutate(sumn = cumsum(n)) %>% 
  mutate(ma = rollmean(n, 7, align = "right", fill = NA))

dataset1b <- dataset1 %>% 
  select(date) %>% 
  count(date) %>% 
  mutate(sumn = cumsum(n),ma = rollmean(n, 7, align = "right", fill = NA))

# une autre maniere de visioner le nombre de nouveau cas avec moving average
ggplot(dataset1b, aes(x = date)) +
  geom_col(aes(y = n))+
  geom_line(aes(y = ma), col = 2)

ggplot(dataset1b, aes(x = date)) +
  geom_line(aes(y = sumn), col = 2)

add pyramid des cas

# customization
display.brewer.all()

col1 <- brewer.pal(4, "Set3")[4]

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
  scale_x_discrete(labels = c("Feminin", "Masculin")) 
  # scale_y_continuous(breaks = c(0 ,2000,15000), labels = c("ZX", "Y", "Z"))

# creer un nouveau jeu de donnée pour afficher les pourcentages
dataset1 %>% 
  group_by(sexe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count)) %>% 
  ggplot( aes(x = sexe, y = prop)) +
  geom_col(fill = col1, col = 1) +
  labs(x = "Sexe",
       y = "Proportion",
       title = "Total covid-19 infection par sexe")+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "Black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray")) +
  scale_x_discrete(labels = c("Feminin", "Masculin"))+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(ylim = c(0, 0.6))


# faire une carte
library(sf)
library(scales)

data<-read.csv("code/dataset1.csv")
data$date<-as.Date(data$date)
data$dob<-as.Date(data$dob)

mdg2 <- read_sf("code/shapefile/MDG_ADM1.shp")
mdg2<-rmapshaper::ms_simplify(mdg2, keep = 0.15, keep_shapes = T) #pour alleger la carte
mdg2<-st_as_sf(mdg2)


region <- dataset1 %>%
  group_by(region) %>%
  summarise(cas=n())

mdg2<-mdg2%>%
  left_join(region,by=c("NAME"="region"))

breaks_cases=c(100,500,1000,5000,10000)
ggplot(mdg2)+
  geom_sf(aes(fill=cas))+
  scale_fill_viridis_b(breaks=breaks_cases,
                       trans="log10",
                       name="Nombre de cas",
                       option="A",
                       direction = -1)+
  theme_void()

ggplot(mdg2)+
  geom_sf(aes(fill=cas))+
  geom_sf_label(aes(label = feature_id))+
  scale_fill_gradient(breaks=breaks_cases,
                      low = "white",
                      high = "black",
                      trans  = "log10")+
  theme_void()




#################################################
############# FINAL FIGURES #####################
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





