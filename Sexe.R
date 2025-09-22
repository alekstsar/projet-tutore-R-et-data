# Usage des ANXIOLYTIQUES DE CLASS ATC3 N05B en France

# ------------------------------------------------------------
# Librairies and datas
# ------------------------------------------------------------

library(tidyverse)
library(gtsummary)
library(gt)

Tot_2018_2023 <- read.csv("./sorted data/Tot_2018_2023.csv")

summary(Tot_2018_2023)

# where 
# AGE- Age au moment des soins
# SEXE - Sexe
# BEN_REG	- Région de Résidence du Bénéficiaire
# REG_NOM - Nom de la région
# Year - Année


# ATC_code - Code ATC
# BOITES/BOITES2	- Nombre de boîtes délivrées
# NBC	- Nombre de consommants (disponible uniquement dans les bases type NB_)


# ------------------------------------------------------------
# Data management
# ------------------------------------------------------------
# we calculate the intensity of consumption (ICi,t,s,a) by region i, year t
# using the data on consumers available from OpenMedic 

sales_summary <- Tot_2018_2023 %>%
  filter(!BEN_REG %in% c(0, 99, 5), sexe %in% c(1,2)) %>%  # clean invalids
  group_by(Year, BEN_REG, REG_NOM, sexe, age) %>%
  summarise(
    bottles = sum(BOITES2, na.rm = TRUE),
    consumers = sum(nbc, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(IC = bottles / consumers) # Intensity of consumption

by_region <- sales_summary %>%
  group_by(Year, BEN_REG, REG_NOM, sexe) %>%
  summarise(
    mean_IC = mean(IC, na.rm = TRUE),
    sd_IC   = sd(IC, na.rm = TRUE),
    .groups = "drop"
  )

# add sex

Tot_2018_2023_filt_sex <- Tot_2018_2023 %>% 
  select(Year, REG_NOM, BEN_REG, BOITES2, sexe, nbc) %>% 
  filter(BEN_REG != 0 & BEN_REG != 99 & BEN_REG != 5 ) %>% 
  droplevels() %>% 
  filter(sexe != 9) %>% 
  droplevels() %>%
  group_by(Year, BEN_REG, REG_NOM, sexe, age) %>% 
  summarise(Sold_bysexe = sum(BOITES2),
            nb_cons = sum(nbc)) %>% 
  print(n=nrow(.))

summary(Tot_2018_2023_filt_sex)

Tot_2018_2023_filt_sex <- Tot_2018_2023_filt_sex %>% 
  mutate(IC=Sold_bysexe/nb_cons) #where Intensity of consumption is ICi,t,s = Average consumption per person in region i, year t, for sex s.

# ------------------------------------------------------------


Tot_2018_2023_filt_sex %>% 
  group_by(sexe, Year, BEN_REG) %>%
  summarise(mean_IC = mean(IC, na.rm = TRUE), 
            sd_IC = sd(IC, na.rm = TRUE)) %>% 
  print()

# summary table sex
# That is some bullshit table. Need to revise what I want to get here
Tot_2018_2023_filt_sex %>% 
  select(Year, REG_NOM, Sold_bysexe, sexe) %>% 
  transform(Year = as.factor(Year)) %>% 
  group_by(Year, REG_NOM, BEN_REG, sexe) %>% 
  tbl_summary(
    by = sexe,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 2,
    missing = "no",
    label = Sold_bysexe ~"Nombre de boîtes des ANXIOLYTIQUES délivrées par sexe"
  ) %>%
  modify_header(label ~ "**Variable**") %>% 
  modify_caption("**Tableau 2. Usage des ANXIOLYTIQUES par region en France 2018-2023**") %>%
  bold_labels()



# ------------------------------------------------------------
# Line graphs
# ------------------------------------------------------------

# Spaghetti Graphs IC

IC_femmes <- Tot_2018_2023_filt_sex %>%
  filter(sexe == 2) %>% 
  select(Year, REG_NOM, IC) %>% 
  group_by(Year, REG_NOM) %>% 
  ggplot(aes(
    x = Year, y = IC, 
    group = REG_NOM, color = REG_NOM
  )) +
  geom_line() +
  geom_point() +
  ylim (4.5, 8.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Année") + ylab("Mesure de la consommation (IC)") + 
  labs(color = "Régione", subtitle = "Femmes", 
       #title = "Intensité de la consommation d'anxiolytiques par les femmes en France métropolitaine entre 2018 et 2023"
       ) +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_minimal()


IC_hommes <- Tot_2018_2023_filt_sex %>%
  filter(sexe == 1) %>% 
  select(Year, REG_NOM, IC) %>% 
  group_by(Year, REG_NOM) %>% 
  ggplot(aes(
    x = Year, y = IC, 
    group = REG_NOM, color = REG_NOM
  )) +
  geom_line() +
  geom_point() +
  #ylim (4.5, 8.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Année") + ylab("Mesure de la consommation (IC)") + 
  labs(color = "Régione", subtitle = "Hommes", 
       #title = "Intensité de la consommation d'anxiolytiques par les hommes en France métropolitaine entre 2018 et 2023"
  ) +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_minimal()

(IC_femmes + IC_hommes) +
  plot_layout(guides = "collect") + 
  plot_annotation("Intensité de la consommation d'anxiolytiques par sexe en France métropolitaine entre 2018 et 2023") & 
  theme(legend.position = 'right')

## Calcul des indicateurs
------------------------------------------------------------
## Gestion des données de l'INSEE
------------------------------------------------------------
  library(readxl)
estim_pop_nreg_sexe_gca_2019 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2019")
estim_pop_nreg_sexe_gca_2018 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2018")
estim_pop_nreg_sexe_gca_2020 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2020")
estim_pop_nreg_sexe_gca_2021 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2021")
estim_pop_nreg_sexe_gca_2022 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2022")
estim_pop_nreg_sexe_gca_2023 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", sheet = "2023")

# Define the function to process population data for a single year
process_population_data <- function(data_input, year_val) {
#Création d'une data frame qui garde uniquement les colonnes indiquées dans le vecteur.
Data_2019_filtrage <- estim_pop_nreg_sexe_gca_2019[, c("Estimation de population au 1er janvier, par région, sexe et grande classe d'âge", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19")]
#Supprime les lignes du vecteur, afin de garder seulement celles qui sont utiles et qui nous intéressent dans notre analyse.
Data_2019_filtrage <- Data_2019_filtrage[-c(1,2,3,18,19,20,21,22,23,24,25,26,27), ]
#On rename les noms de colonne pour que ce soit plus facile à utiliser
Data_2019_filtrage <- Data_2019_filtrage %>%
  rename(REG_NOM = `Estimation de population au 1er janvier, par région, sexe et grande classe d'âge`)
Data_2019_filtrage <- Data_2019_filtrage %>%
  rename(POP_REG_HOM = `...13`, POP_REG_FEM = `...19`)

#Met la variable POP_REG en numérique
Data_2019_filtrage <- Data_2019_filtrage %>% 
  mutate(POP_REG = as.numeric(POP_REG))
#Changement des noms de variables pour qu'ils correspondent à la version OpenMedic
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Île-de-France", "Ile-de-France", `REG_NOM`))
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Centre-Val-de-Loire", "Centre-Val de Loire", `REG_NOM`))
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Hauts-de-France", "Nord-Pas-de-Calais-Picardie", `REG_NOM`))
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Grand Est", "Alsace-Champagne-Ardenne-Lorraine", `REG_NOM`))
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Nouvelle-Aquitaine", "Aquitaine-Limousin-Poitou-Charentes", `REG_NOM`))
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(`REG_NOM` = ifelse(`REG_NOM` == "Occitanie", "Languedoc-Roussillon-Midi-Pyrénées", `REG_NOM`))
#Fusion des lignes Corse et Provence-Alpes-Côte d'Azur afin que ça corresponde aux régions renseignées sur OPEN MEDIC
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(REG_NOM = ifelse(REG_NOM %in% c("Corse", "Provence-Alpes-Côte d'Azur"), "Provence-Alpes-Côte d'Azur et Corse", REG_NOM)) %>%
  group_by(REG_NOM) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

#On rajoute une variable intitulée "Year" qui rajoute à chaque ligne l'année correspondante à la table afin de préparer la fusion avec la table Tot_2018_2023_filtrage
Data_2019_filtrage <- Data_2019_filtrage %>%
  mutate(Year = "2019")  