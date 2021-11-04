# day_2 ggplot2 and maps

library(ggplot2)
library(dplyr)
library(zoo)
library(RColorBrewer)

## Construire un barplot
ggplot(dataset1, aes(x = sexe)) +
  geom_bar()

# to do: change axes label, y-scale, color bar and size text

# creer une histograme de la distribution des ages par sexe
ggplot(dataset1, aes(x = age))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~sexe)

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
  mutate(sumn = cumsum(n), ma = rollmean(dataset1b$n, 7, align = "right", fill = NA) )

# une autre maniere de visioner le nombre de nouveau cas avec moving average
ggplot(dataset1b, aes(x = date)) +
  geom_col(aes(y = n))+
  geom_line(aes(y = ma), col = 2)

ggplot(dataset1b, aes(x = date)) +
  geom_line(aes(y = sumn), col = 2)

# customization
display.brewer.all()

col1 <- brewer.pal(4, "Set3")[4]

ggplot(dataset1, aes(x = sexe)) +
  geom_bar(fill = col1, col = 1) +
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

