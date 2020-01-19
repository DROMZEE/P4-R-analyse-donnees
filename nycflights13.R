#install.packages("nycflights13")
library(ggplot2)
library(dplyr)
library(nycflights13)
library(knitr)


#data(nycflights13)

mode(nycflights13)
class(nycflights13)
str(flights)

flights
print(nycflights13::flights)
print(nycflights13::flights)

View(flights)
glimpse(flights)

#ggsave

airlines
kable(airlines)
airlines$name

glimpse(airports)

?nycflights13::flights
?flights

# exemple de filtre

filter(flights, month==1, day ==1)
filter(flights, month==11 | month==12)

########################## 1.2 

flights_NA_cols <- sapply(flights, function(x) sum(is.na(x)))
flights_NA_cols

#distance

na <- filter(flights, is.na(flights$distance) == TRUE)

distdf <- is.na(flights$distance)
distdf == TRUE

filter(flights, distdf == TRUE)

is.na(flights$distance)



air_time_na <- flights %>% 
  filter(is.na(air_time))

air_time_na

###########################1.3

# moyenne, écart-type, min, max)


################### Rapprochement avec des données météo
############### 2.1. De la même manière, parcourez la base weather et proposez un traitement des valeurs manquantes.

print(nycflights13::weather)

############### 2.2. Sortez des statistiques sur les variables qui vous semblent pouvoir impacter le retard des avions, sur toute la base puis selon l’aéroport.

# retartd au depart utiliser scheduler
# pour supprimer na.rm = TRUE
# ! valeurs manquantes pas simple a remplacer continuer exercice

flights %>%
  select(day, arr_delay, dep_delay) %>% 
  group_by(day) %>% 
  summarise(avg_delay =  mean(arr_delay, na.rm = TRUE) + 
              mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(-avg_delay)


View(weather)
# nb de departs annules

sum(is.na(flights$dep_time))

#Filtering only delayed flights from all airports
flights_delayed <-filter(flights,dep_delay>0)
flights_not_delayed <-filter(flights,dep_delay<=0)

flights_delayed

by_time_hour_airport =group_by(flights_delayed,origin,time_hour)
by_time_hour_airport

# https://books.google.fr/books?id=Ru9oDwAAQBAJ&pg=PA56&dq=nycflights13&hl=fr&sa=X&ved=0ahUKEwjf2Krb6oXnAhX2A2MBHbXLBccQ6AEIQDAC#v=onepage&q=nycflights13&f=false

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, count = n(), 
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dest!= "HNL")
delay

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) + 
  geom_smooth(se = FALSE)


delays <- flights %>%
  group_by(dest) %>% 
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest!= "HNL")

delays

filter(airports, faa %in%c('ALB','BDL','BTV'))

airportcounts <- flights %>%
  filter(dest %in%c('ALB','BDL','BTV')) %>%
  group_by(year, month, dest) %>%
  summarise(count =n()) %>%
  collect()

airportcounts

head(weather)

avgdelay <- flights %>%
  group_by(month, day) %>%
  filter(month < 13) %>%
  summarise(avgdelay =mean(arr_delay, na.rm=TRUE))
precip <- weather %>%
  group_by(month, day) %>%
  filter(month < 13) %>%
  summarise(totprecip =sum(precip), maxwind =max(wind_speed))
precip <-mutate(precip, anyprecip =ifelse(totprecip==0, "No", "Yes"))
merged <-left_join(avgdelay, precip, by=c("day", "month"))
head(merged)

favstats(~ maxwind, data=merged)
favstats(~ maxwind, data=merg
         
         
# grouper les vols par heure (moyenne)  ajouter au tableau  
# merger sur time_hour (grouper ou merger) sumerize
# et origin


# corelation des parametre entre eux : plus il pleu plus i lfait humide moins il y a de visibilité ?

