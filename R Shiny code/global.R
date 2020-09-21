library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(stringr)
library(lubridate)
library(ggpubr)
library(bigQueryR)
library(glmnet)


# # connexion à Google Cloud Storage et téléchargement des données
# CloudStorageConnect<-function(path_log, bucket, project){
#   # connect to cs
#   Sys.setenv("GCS_AUTH_FILE" = path_log)
#   library(googleCloudStorageR)
# 
#   # buckets
#   gcs_get_bucket(bucket)
#   Sys.setenv("GCS_DEFAULT_BUCKET" = bucket)
#   gcs_global_bucket(bucket)
# }
# 
# 
# # connexion à GCS
# emplacement_racine0 <- "/home/rstudio/dashboard/app"
# entity <- "entity"
# projet <- "project"
# path_log<-paste(emplacement_racine0,"auth","auth.json",sep="/")
# CloudStorageConnect(path_log,bucket=entity,project=projet)
# 
# # téléchargement des données, puis suppression des fichiers csv sur la VM
# gcs_get_object("data_nps_ca_alea.csv", saveToDisk = "/home/rstudio/dashboard/app/data_nps_ca_alea.csv", overwrite=T)
# gcs_get_object("data_ca_alea.csv", saveToDisk = "/home/rstudio/dashboard/app/data_ca_alea.csv", overwrite=T)

data_nps_ca_init <- read.csv("/Users/Alodie/Documents/aaPolytechnique/3A/Stage/Dashboard NPS/data_nps_ca_alea.csv", sep=";")
data_ca_extended_init <- read.csv("/Users/Alodie/Documents/aaPolytechnique/3A/Stage/Dashboard NPS/data_ca_alea.csv", sep=";")
# file.remove("/home/rstudio/dashboard_nps/app/data_nps_ca_alea.csv")
# file.remove("/home/rstudio/dashboard_nps/app/data_ca_alea.csv")



data_nps_ca <- data_nps_ca_init
data_ca_extended <- data_ca_extended_init

# mise en forme de la table data_ca pour avoir un format similaire à celui de data_nps_ca
names(data_ca_extended) <- c("DATEenvoi", "CA")
data_ca_extended$DATEenvoi <- as.Date.character(as.character(data_ca_extended$DATEenvoi), format="%d/%m/%Y")
date_week <- unique(data_ca_extended$DATEenvoi)

# mise en forme des dates
data_nps_ca$DATEenvoi <- paste(substr(data_nps_ca$DATEenvoi, 1,4), "-", substr(data_nps_ca$DATEenvoi, 6, 7), "-01", sep="")
data_nps_ca$DATEenvoi <- as.Date.character(data_nps_ca$DATEenvoi)
data_nps_ca$week <- as.Date.character(as.character(data_nps_ca$week), format="%d/%m/%Y")
data_nps_ca <- data_nps_ca[is.na(data_nps_ca$week) | (year(data_nps_ca$week) == year(data_nps_ca$DATEenvoi) & (month(data_nps_ca$week) == month(data_nps_ca$DATEenvoi) | month(data_nps_ca$week-6) == month(data_nps_ca$DATEenvoi))),]
data_nps_ca$cardkey <- as.factor(data_nps_ca$cardkey)

# changement d'échelle pour uniformiser les échelles de notation de la satisfaction
data_nps_ca[data_nps_ca$Typquest != 9,"Satisfaction"] <- data_nps_ca[data_nps_ca$Typquest != 9,"Satisfaction"] * -1 + 5   # pour les clients post-appel de gestion, qui ne sont plus dans le dernier fichier de données
data_nps_ca[data_nps_ca$Typquest == 9,"Satisfaction"] <- round(data_nps_ca[data_nps_ca$Typquest == 9,"Satisfaction"] * (3/10) + 1,0)

# suppression des clients qui n'ont pas généré de CA sur le lot 1
data_nps_ca <- data_nps_ca[!(data_nps_ca$Lot == 1  & data_nps_ca$CA == 0),]

# data_nps_ca par semaine
data_nps_ca_semaine <- data_nps_ca %>% group_by(week, Lot, Typquest, Satisfaction, Recommandation, Detracteur, Promoteur, Neutre, cardkey) %>% summarise(CA = sum(CA))
names(data_nps_ca_semaine) <- c("DATEenvoi", "Lot", "Typquest", "Satisfaction", "Recommandation", "Detracteur", "Promoteur", "Neutre", "cardkey", "CA")
data_nps_ca_semaine <- data_nps_ca_semaine[!is.na(data_nps_ca_semaine$DATEenvoi),]

# décompte du nombre de clients par lot et produit sur l'étude
typquest_count <- count(unique(data_nps_ca[data_nps_ca$Typquest != 0,! names(data_nps_ca) %in% c("CA", "week")]), Lot, Typquest)


# somme du CA par mois : les semaines à cheval sur 2 mois sont réparties sur les deux mois qu'elles chevauchent
fin_mois <- unique(round_date(date_week, unit="month")) - 1

data_ca_extended <- data_ca_extended %>% group_by(DATEenvoi) %>% summarise(CA = sum(CA))
data_ca_extended_semaine <- data_ca_extended

data_ca_new <- as_tibble(structure(list(DATEenvoi = as_date(1), CA = 1), class = "data.frame"))
for (date in unique(data_ca_extended$DATEenvoi)){
  argmin <- which.min(abs(as.numeric(fin_mois - date)))
  if ((fin_mois[argmin] - date) < 6 & (fin_mois[argmin] - date) >= 0){
    ligne <- data_ca_extended[data_ca_extended$DATEenvoi == date,]
    ligne$DATEenvoi <- as_date(date + 7)
    ligne$CA <- as.numeric(ligne$CA * (6 - min(abs(as.numeric(fin_mois - date))))/7)
    data_ca_extended[data_ca_extended$DATEenvoi == date,]$CA <- data_ca_extended[data_ca_extended$DATEenvoi == date,]$CA * (1 + min(abs(as.numeric(fin_mois - date))))/7
    data_ca_new <- rbind(data_ca_new, as_tibble(ligne))
  }
}
data_ca_new$DATEenvoi <- as_date(data_ca_new$DATEenvoi)
data_ca_extended <- bind_rows(data_ca_extended, data_ca_new[data_ca_new$DATEenvoi != round_date(max(data_nps_ca$DATEenvoi) + 15, unit = "month"),])

# mise en forme des dates
data_ca_extended$DATEenvoi <- paste(substr(as.character(data_ca_extended$DATEenvoi),1,8),"01", sep="")
data_ca_extended$DATEenvoi <- as.character(data_ca_extended$DATEenvoi)
data_ca_extended$DATEenvoi <- as.Date.character(data_ca_extended$DATEenvoi)

data_ca_extended <- data_ca_extended %>% group_by(DATEenvoi) %>% summarise(CA = sum(CA))
data_ca <- data_ca_extended[data_ca_extended$DATEenvoi >= min(data_nps_ca$DATEenvoi) & data_ca_extended$DATEenvoi <= max(data_nps_ca$DATEenvoi),]


# NPS et CA par mois et par client
data_nps_ca <- data_nps_ca %>% group_by(DATEenvoi, Lot, Typquest, Satisfaction, Recommandation, Detracteur, Promoteur, Neutre, cardkey) %>% summarise(panier = mean(CA), frequence = case_when(panier != 0 ~ round(sum(CA)/mean(CA),0), TRUE ~ 0), CA = sum(CA))


# Nombre de clients promoteurs, neutres et détracteurs, avec le CA moyen par groupe de clients, au cours du temps 
data_nps_ca_count <- data_nps_ca %>% group_by(DATEenvoi, Lot, Typquest) %>% summarise(Detracteur_count = sum(Detracteur), Neutre_count = sum(Neutre), Promoteur_count = sum(Promoteur), CA_detracteur = sum(case_when(Detracteur == 1 ~ CA, TRUE ~ 0)), CA_neutre = sum(case_when(Neutre == 1 ~ CA, TRUE ~ 0)), CA_promoteur = sum(case_when(Promoteur == 1 ~ CA, TRUE ~ 0)), CA = CA_detracteur + CA_neutre + CA_promoteur)
data_nps_ca_count <- data_nps_ca_count[!is.na(data_nps_ca_count$Lot),]
data_nps_ca_semaine_count <- data_nps_ca_semaine %>% group_by(DATEenvoi, Lot, Typquest) %>% summarise(Detracteur_count = sum(Detracteur), Neutre_count = sum(Neutre), Promoteur_count = sum(Promoteur), CA_detracteur = sum(case_when(Detracteur == 1 ~ CA, TRUE ~ 0)), CA_neutre = sum(case_when(Neutre == 1 ~ CA, TRUE ~ 0)), CA_promoteur = sum(case_when(Promoteur == 1 ~ CA, TRUE ~ 0)), CA = CA_detracteur + CA_neutre + CA_promoteur)


# Pourcentage de clients promoteurs, neutres et détracteurs, avec le CA moyen par groupe de clients et le NPS, au cours du temps 
data_nps_ca_percentage <- data_nps_ca %>% group_by(DATEenvoi, Lot, Typquest) %>% summarise(Detracteur_count = sum(Detracteur) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), Neutre_count = sum(Neutre) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), Promoteur_count = sum(Promoteur) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), NPS = Promoteur_count - Detracteur_count, CA_detracteur = sum(case_when(Detracteur == 1 ~ CA, TRUE ~ 0)), CA_neutre = sum(case_when(Neutre == 1 ~ CA, TRUE ~ 0)), CA_promoteur = sum(case_when(Promoteur == 1 ~ CA, TRUE ~ 0)), CA = CA_detracteur + CA_neutre + CA_promoteur, CA_client = (CA_detracteur + CA_neutre + CA_promoteur) / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)))
data_nps_ca_percentage <- data_nps_ca_percentage[!is.na(data_nps_ca_percentage$Lot),]
data_nps_ca_semaine_percentage <- data_nps_ca_semaine %>% group_by(DATEenvoi, Lot, Typquest) %>% summarise(Detracteur_count = sum(Detracteur) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), Neutre_count = sum(Neutre) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), Promoteur_count = sum(Promoteur) * 100 / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)), NPS = Promoteur_count - Detracteur_count, CA_detracteur = sum(case_when(Detracteur == 1 ~ CA, TRUE ~ 0)), CA_neutre = sum(case_when(Neutre == 1 ~ CA, TRUE ~ 0)), CA_promoteur = sum(case_when(Promoteur == 1 ~ CA, TRUE ~ 0)), CA = CA_detracteur + CA_neutre + CA_promoteur, CA_client = (CA_detracteur + CA_neutre + CA_promoteur) / (sum(Detracteur) + sum(Neutre) + sum(Promoteur)))


# interpolation du NPS par semaine
data_nps_ca_semaine_2 <- as_tibble(structure(list(DATEenvoi = as_date(1), Lot = 1, Typquest = 1, NPS = 1), class = "data.frame"))
for (i in unique(typquest_count$Lot)){
  for (j in unique(typquest_count[typquest_count$Lot == i,]$Typquest)){
    data <- data_nps_ca_percentage[data_nps_ca_percentage$Lot == i & data_nps_ca_percentage$Typquest == j,]
    if (dim(data)[1] != 0){
      lignes <- as_tibble(approx(x=julian(data$DATEenvoi)[1:length(julian(data$DATEenvoi))], y=data$NPS, xout = julian(as.Date.character(date_week[date_week >= as.Date.character("2017-12-01")]))[1:104], rule=2))
      names(lignes) <- c("DATEenvoi","NPS")
      lignes$DATEenvoi <- as_date(lignes$DATEenvoi)
      lignes <- lignes %>% mutate(Lot = i, Typquest = j)
      data_nps_ca_semaine_2 <- rbind(data_nps_ca_semaine_2,lignes)
    }
  }
}
data_nps_ca_semaine_2 <- left_join(data_nps_ca_semaine_2, data_ca_extended_semaine, by="DATEenvoi")


# pivot de la table data_nps_ca, avec le nombres de clients détracteurs, neutres et promoteurs pour chaque Typquest
data_nps_ca_dnp <- data_nps_ca  %>% group_by(DATEenvoi, Lot, Typquest, cardkey, Detracteur, Neutre, Promoteur) %>% summarise(CA_client = sum(CA), panier = mean(panier), frequence = mean(frequence))
data_nps_ca_dnp <- gather(data_nps_ca_dnp, "Client_group", "count", -DATEenvoi, -Lot, -Typquest, -CA_client, -cardkey, -panier, -frequence)
data_nps_ca_dnp <- data_nps_ca_dnp[data_nps_ca_dnp$count != 0,] %>% group_by(DATEenvoi, Lot, Typquest, Client_group) %>% summarise(CA_client = mean(CA_client), count = as.double(sum(count)), panier = mean(panier), frequence = mean(frequence))


# table avec le CA et les valeurs du NPS sur les 50 semaines précentes pour construire le modèle de régression linéaire
names(data_nps_ca_semaine_percentage) <- c("DATEenvoi", "Lot", "Typquest", "Detracteur_count", "Neutre_count", "Promoteur_count", "NPS_0", "CA_detracteur", "CA_neutre", "CA_promoteur", "CA", "CA_client")
for (i in c(1:50)){
  data <- data_nps_ca_semaine_percentage[,c("DATEenvoi", "Lot", "Typquest", paste("NPS_",(i-1), sep=""))]
  data$DATEenvoi <- data$DATEenvoi + 7
  names(data) <- c("DATEenvoi", "Lot", "Typquest", paste("NPS_",i,sep=""))
  data_nps_ca_semaine_percentage <- left_join(data_nps_ca_semaine_percentage, data, by = c("DATEenvoi", "Lot", "Typquest"))
}


# coefficient R2 ajusté du modèle linéaire de chaque lot
data_r2 <- as_tibble(structure(list(Lot = 1, R2 = 1), class = "data.frame"))
for (i in unique(typquest_count$Lot)){
  
  data <- data_nps_ca_semaine_percentage[data_nps_ca_semaine_percentage$Lot == i,!names(data_nps_ca_semaine_percentage) %in% c("Detracteur_count","Neutre_count","Promoteur_count","CA_detracteur","CA_neutre","CA_promoteur")]
  
  for (j in unique(typquest_count[typquest_count$Lot == i,]$Typquest)){
    data[data$Lot == i & data$Typquest == j,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] <- data[data$Lot == i & data$Typquest == j,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] * typquest_count[typquest_count$Lot == i & typquest_count$Typquest == j,]$n
  }
  
  data[,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] <- data[,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] / sum(typquest_count[typquest_count$Lot == i,]$n)
  
  data <- data %>% group_by(DATEenvoi) %>% summarise(CA_client = sum(CA_client, na.rm = TRUE), NPS_0 = sum(NPS_0, na.rm = TRUE), NPS_1 = sum(NPS_1, na.rm = TRUE), NPS_2 = sum(NPS_2, na.rm = TRUE), NPS_3 = sum(NPS_3, na.rm = TRUE), NPS_4 = sum(NPS_4, na.rm = TRUE), NPS_5 = sum(NPS_5, na.rm = TRUE), NPS_6 = sum(NPS_6, na.rm = TRUE), NPS_7 = sum(NPS_7, na.rm = TRUE), NPS_8 = sum(NPS_8, na.rm = TRUE),
                                                     NPS_9 = sum(NPS_9, na.rm = TRUE), NPS_10 = sum(NPS_10, na.rm = TRUE), NPS_11 = sum(NPS_11, na.rm = TRUE), NPS_12 = sum(NPS_12, na.rm = TRUE), NPS_13 = sum(NPS_13, na.rm = TRUE), NPS_14 = sum(NPS_14, na.rm = TRUE), NPS_15 = sum(NPS_15, na.rm = TRUE), NPS_16 = sum(NPS_16, na.rm = TRUE), NPS_17 = sum(NPS_17, na.rm = TRUE),
                                                     NPS_18 = sum(NPS_18, na.rm = TRUE), NPS_19 = sum(NPS_19, na.rm = TRUE), NPS_20 = sum(NPS_20, na.rm = TRUE), NPS_21 = sum(NPS_21, na.rm = TRUE), NPS_22 = sum(NPS_22, na.rm = TRUE), NPS_23 = sum(NPS_23, na.rm = TRUE), NPS_24 = sum(NPS_24, na.rm = TRUE), NPS_25 = sum(NPS_25, na.rm = TRUE), NPS_26 = sum(NPS_26, na.rm = TRUE))
  data <- data[27:nrow(data),]
  data <- data[,!names(data) %in% c("DATEenvoi")]
  
  model_lm <- lm(CA_client ~ ., data = data)
  
  r2_adj <- summary(model_lm)$adj.r.squared
  
  data_r2 <- rbind(data_r2, tibble(Lot = i, R2 = r2_adj))
}



# NPS et pourcentages de clients promoteurs, neutres et détracteurs pour les valueBox
nps_lot_1 <- (sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 1 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Promoteur",]$count) - sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 1 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Detracteur",]$count))*100/sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 1 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi),]$count)
nps_lot_2 <- (sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 2 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Promoteur",]$count) - sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 2 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Detracteur",]$count))*100/sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 2 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi),]$count)
nps_lot_3 <- (sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 3 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Promoteur",]$count) - sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 3 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi) & data_nps_ca_dnp$Client_group == "Detracteur",]$count))*100/sum(data_nps_ca_dnp[data_nps_ca_dnp$Lot == 3 & data_nps_ca_dnp$DATEenvoi == max(data_nps_ca_dnp$DATEenvoi),]$count)


