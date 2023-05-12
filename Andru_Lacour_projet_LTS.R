
###Production industrielle mensuelle de glaces et sorbets (1990-2023) ###

# ANDRU Kilian - LACOUR Xavier
  
  
  
## Téléchargement et formatage du fichier
  

rm(list = objects())
# path <- "W:/Bureau/S4_ENSAE/STL"     # Xavier - Ordinateur ENSAE
path <- "D:/Etudes/ENSAE/S4/Séries temporelles linéaires/Projet"    # Xavier - Ordinateur perso
path <- "C:/Users/Kilian/Desktop/ENSAE/S4/LTS/Projet---S-rie-temporelle-lin-aire"    # Kilian - Ordinateur perso
setwd(path) # Definit l'espace de travail
library(readr)
donnees_choco <- read.csv("valeurs_mensuelles_chocolat.csv", sep = ";", col.names = c("Periode", "Indice", "Code"))

# On supprime les trois premieres lignes inutiles
donnees_choco = donnees_choco[-c(1,2,3), ]

# On vérifie que la colonne Code ne prend que la valeur "A" et que donc l'ensemble des valeurs sont normales
if(any(donnees_choco["Code"] != "A") == F){
  donnees_choco = subset(donnees_choco, select = c("Periode", "Indice"))
}

# On transforme la colonne Indice en valeurs numériques
donnees_choco["Indice"] = as.numeric(unlist(donnees_choco["Indice"]))


# Possibilité d'enlever les données d'après mars 2020 pour ne pas subir les perturbation du Covid-19
covid_garde = FALSE
if(covid_garde == FALSE){donnees_choco = donnees_choco[-c(363:length(donnees_choco$Indice)), ]}


#Convertion en format zoo (Si besoin : "install.packages('zoo')" et "install.packages('tseries')")
require(zoo)
require(tseries)

choco <- zoo(donnees_choco["Indice"]) #convertit le 1er ´el´ement de data en "zoo"
T <- length(choco)

### Partie I

## Question 1

#Voir rapport de projet.


### Question 2

#Différenciation

plot(choco, xaxt="n")

acf(choco)

d1choco <- diff(choco,1)

plot(d1choco, xaxt="n")

d2choco <- diff(d1choco,1)

plot(d2choco, xaxt="n")

acf(d2choco)


#Test moindre carrés si composante déterministe

library(dplyr)
donnees_choco2 <-  select(donnees_choco, "Indice")
donnees_choco2["temps"] <-  0:(T-1)
donnees_choco2["temps_carr"] <- donnees_choco2$temps^2
lm <- lm(Indice~., data = donnees_choco2)
summary(lm)


