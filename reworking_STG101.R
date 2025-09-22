

NB_2018_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2018_atc5_age_sexe_reg_spe.CSV")
NB_2019_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2019_atc5_age_sexe_reg_spe.CSV")
NB_2020_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2020_atc5_age_sexe_reg_spe.CSV")
NB_2021_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2021_atc5_age_sexe_reg_spe.CSV")
NB_2022_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2022_atc5_age_sexe_reg_spe.CSV")
NB_2023_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2023_atc5_age_sexe_reg_spe.CSV")


Buspirone_2018 <- NB_2018_atc5_age_sexe_reg_spe %>%
  filter(L_ATC5 == "BUSPIRONE")

Buspirone_2019 <- NB_2019_atc5_age_sexe_reg_spe %>%
  filter(L_ATC5 == "BUSPIRONE")

Buspirone_2020 <- NB_2020_atc5_age_sexe_reg_spe %>%
  filter(L_ATC5 == "BUSPIRONE")

Buspirone_2021 <- NB_2021_atc5_age_sexe_reg_spe %>%
  filter(L_ATC5 == "BUSPIRONE")

Buspirone_2022 <- NB_2022_atc5_age_sexe_reg_spe %>%
  filter(L_ATC5 == "BUSPIRONE")

Buspirone_2023 <- NB_2023_atc5_age_sexe_reg_spe %>%
  filter(l_atc5 == "BUSPIRONE")

summary(Buspirone_2018)

rm(list = ls(pattern = "atc5_age_sexe_reg_spe")) # Remove old data in pattern in order to save some space

Buspirone_2018_2023 <- bind_rows(
  "2018" = Buspirone_2018,
  "2019" = Buspirone_2019,
  "2020" = Buspirone_2020,
  "2021" = Buspirone_2021,
  "2022" = Buspirone_2022,
  "2023" = Buspirone_2023,
  .id = "Year"
)

summary(Buspirone_2018_2023)

Buspirone_2018_2023$Year <- as.factor(Buspirone_2018_2023$Year)

tbl_summary(Buspirone_2018_2023) # in this format it is not ready for publication. 

popData <- tibble(
  Year = c("2018", "2019", "2020", "2021", "2022", "2023"),
  Population_an = c(64844037, 65096768, 65269154, 65505213, 65721831, 65925961)
)

somme_boites <- Buspirone_2018_2023 %>%
  group_by(Year) %>%
  reframe(BOITES_tot = sum(BOITES, na.rm = TRUE)) %>%
  left_join(popData, by = "Year") %>%
  mutate(distribution  = (BOITES_tot / Population_an) * 100000)


rm(list = ls(pattern = "Buspirone_20")) # Remove old data in pattern in order to save some space

write.csv(x = somme_boites, file = "N05BE01_YEARLY_SUM.csv", row.names=FALSE)

# ------------------------------------------------------------
# Statistics
# ------------------------------------------------------------

#Summary table. Still don't understand how to build a normal tableaux.

somme_boites %>%
  select(Year, BOITES_tot, distribution) %>%
  gt() %>% # make it a gt object
  # reformat numbers (~ digits from gtsummary)
  fmt_number(
    columns = c(BOITES_tot, distribution),
    decimals = 0,
    sep_mark = " "
  ) %>%
  tab_header(  # table title
    title    = "Évolution de la consommation de Buspirone (N05BE01)",
    subtitle = "France, 2018-2023"
  ) %>%
  cols_label( # cols titles
    Year              = "Année",
    BOITES_tot        = "Total boîtes",
    distribution = "Boîtes / 100k hab"
  ) 


# ------------------------------------------------------------
# Line graphs
# ------------------------------------------------------------


somme_boites %>% 
  ggplot(aes(x = as.numeric(Year), y = distribution, group = 1, color = 'red')) +
  geom_point() +
  geom_line() +
  labs(title = "Distribution du nombre de boites achetées pour chaque année, pour 100000 habitants", x = "Années", y = "Distribution boites") +
  theme_minimal()

colnames(somme_boites)

str(somme_boites)





# ------------------------------------------------------------
# Combined data for all drugs
# ------------------------------------------------------------


# import and summaries data
NB_2018_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2018_atc5_age_sexe_reg_spe.CSV")
NB_2019_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2019_atc5_age_sexe_reg_spe.CSV")
NB_2020_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2020_atc5_age_sexe_reg_spe.CSV")
NB_2021_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2021_atc5_age_sexe_reg_spe.CSV")
NB_2022_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2022_atc5_age_sexe_reg_spe.CSV")
NB_2023_atc5_age_sexe_reg_spe <- read.csv2("./data/ATC5_sexe_age/NB_2023_atc5_age_sexe_reg_spe.CSV")





# ------------------------------------------------------------
# Data management
# ------------------------------------------------------------
NB_2019_atc5_age_sexe_reg_spe<- NB_2019_atc5_age_sexe_reg_spe %>% 
  rename(sexe = SEXE)

#Create new tables containing the data pertinent to Medication N05BA

filterN05B <- function(data) {
  filteredData <- data %>%
    dplyr::select(ATC5, BEN_REG, BOITES, sexe, age, nbc) %>%
    filter(ATC5 == "N05BX03"| ATC5 == "N05BE01" | ATC5 == "N05BB01" )
  return(filteredData)
}



ATC5_2018_N05B <- filterN05B(NB_2018_atc5_age_sexe_reg_spe)
ATC5_2019_N05B <- filterN05B(NB_2019_atc5_age_sexe_reg_spe)
ATC5_2020_N05B <- filterN05B(NB_2020_atc5_age_sexe_reg_spe)
ATC5_2021_N05B <- filterN05B(NB_2021_atc5_age_sexe_reg_spe)
ATC5_2022_N05B <- filterN05B(NB_2022_atc5_age_sexe_reg_spe)
ATC5_2023_N05B <- filterN05B(NB_2023_atc5_age_sexe_reg_spe)

# Remove old data in pattern in order to save some space
rm(list = ls(pattern = "atc5_age_sexe_reg_spe"))


#Combine into 1 table
N05B_2018_2023 <- bind_rows(
  "2018" = ATC5_2018_N05B,
  "2019" = ATC5_2019_N05B,
  "2020" = ATC5_2020_N05B,
  "2021" = ATC5_2021_N05B,
  "2022" = ATC5_2022_N05B,
  "2023" = ATC5_2023_N05B,
  .id = "Year"
)

summary(N05B_2018_2023) # there are some aberrant values in BIOTES, need to clean up! Why min -13???? What to do about it?
N05B_2018_2023 <- N05B_2018_2023 %>%
  mutate(BOITES2 = if_else(BOITES < 0, NA_real_, BOITES))
view(N05B_2018_2023)

summary(N05B_2018_2023)# seems ok now!

# Remove old data in pattern in order to save some space
rm(list = ls(pattern = "ATC5_20"))

#added total number of bottles of benzodiazepine derivatives ATC4-N05BA sold in France per year, including the data for unknown regions (BEN_REG "0" and "99" in the OpenMedic dictionary).

N05B_2018_2023 %>% 
  group_by(Year) %>% 
  reframe(BOITES_tot = sum(BOITES2, na.rm = TRUE))

# Standardised by population. Imported dataset from INCEE (https://www.insee.fr/fr/statistiques/7746154?sommaire=7746197) 
# "Bilan démographique 2023". Table "Composantes de la croissance démographique, France métropolitaine" 
# variable "Population au 1er janvier" of each year. 

popData <- tibble(
  Year = c("2018", "2019", "2020", "2021", "2022", "2023"),
  POP_JAN = c(64844037, 65096768, 65269154, 65505213, 65721831, 65925961)
)

N05B_YEARLY_SUM <- N05B_2018_2023 %>%
  group_by(Year) %>%
  reframe(BOITES_tot = sum(BOITES2, na.rm = TRUE)) %>%
  left_join(popData, by = "Year") %>%
  mutate(BOITES_stand  = (BOITES_tot / POP_JAN) * 100000) # calculate consumption of N05BA per 100000 pop

N05B_YEARLY_SUM

write.csv(x = N05B_YEARLY_SUM, file = "N05B_YEARLY_SUM.csv", row.names=FALSE)

# ------------------------------------------------------------
# Data management sex
# ------------------------------------------------------------

library(readxl)
library(dplyr)

library(readxl)
extract_pop_data_sexe_gca_2019 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2019")
extract_pop_data_sexe_gca_2018 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2018")
extract_pop_data_sexe_gca_2020 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2020")
extract_pop_data_sexe_gca_2021 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2021")
extract_pop_data_sexe_gca_2022 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2022")
extract_pop_data_sexe_gca_2023 <- read_excel("data/estim-pop-nreg-sexe-gca-1975-2025.xlsx", 
                                             sheet = "2023")


extract_population_data <- function(data, rows_to_remove) {
  data_filtrage <- data[, c(1, 13, 19)]
  data_filtrage <- data_filtrage[-c(1,2,3,4,18,19,20,21,22,23,24,25,26,27), ]
  data_filtrage <- data_filtrage %>%
    rename(REG_NOM = 1, 
           POP_H = 2, # Assuming the 13th column is population men and now the 2nd after extraction
           POP_F = 3) # Assuming the 19th column is population women and now the 3rd after extraction
  
  # Convertir les colonnes de population en numérique
  data_filtrage <- data_filtrage %>%
    mutate(POP_H = as.numeric(POP_H),
           POP_F = as.numeric(POP_F))
  
  # Correction des noms de régions (comme dans votre script)
  data_filtrage <- data_filtrage %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Île-de-France", "Ile-de-France", `REG_NOM`)) %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Centre-Val-de-Loire", "Centre-Val de Loire", `REG_NOM`)) %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Hauts-de-France", "Nord-Pas-de-Calais-Picardie", `REG_NOM`)) %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Grand Est", "Alsace-Champagne-Ardenne-Lorraine", `REG_NOM`)) %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Nouvelle-Aquitaine", "Aquitaine-Limousin-Poitou-Charentes", `REG_NOM`)) %>%
    mutate(`REG_NOM` = ifelse(`REG_NOM` == "Occitanie", "Languedoc-Roussillon-Midi-Pyrénées", `REG_NOM`))
  
  # Fusion des régions Corse et Provence-Alpes-Côte d'Azur
  data_filtrage <- data_filtrage %>%
    mutate(REG_NOM = ifelse(REG_NOM %in% c("Corse", "Provence-Alpes-Côte d'Azur"), "Provence-Alpes-Côte d'Azur et Corse", REG_NOM)) %>%
    group_by(REG_NOM) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE))
  
  # Ajouter la colonne "Year"
  data_filtrage <- data_filtrage %>%
    mutate(Year = "2019")
  
  return(data_filtrage)
}


# ------------------------------------------------------------
# interactive maps
# ------------------------------------------------------------

rdbu_base_colors <- brewer.pal(11, "RdBu")
rdbu_base_colors_reversed <- rev(rdbu_base_colors)
# 2. Create a color ramp function based on RdBu
rdbu_color_ramp <- colorRampPalette(rdbu_base_colors_reversed)

# 3. Generate 12 colors from the RdBu color ramp
rdbu_12_colors <- rdbu_color_ramp(12)


# Barplot 
Tot_2018_2023_filtrage %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2018") %>% 
  group_by(Year, REG_NOM) %>% 
  ungroup() %>%
  arrange(TCS) %>% 
  mutate(REG_NOM = factor(REG_NOM, levels = REG_NOM)) %>%
  ggplot(aes(x=TCS, y=REG_NOM, fill = REG_NOM)) + 
  geom_bar(stat = "identity", width=0.5) +
  scale_fill_manual(values = rdbu_12_colors) +
  theme_minimal() +
  guides(fill = FALSE)

Tot_2018_2023_filtrage %>%
  select(Year, REG_NOM, TCS) %>% 
  group_by(Year, REG_NOM) %>% 
  ungroup() %>%
  arrange(TCS) %>% 
  ggplot(aes(
    x = Year, y = TCS, 
    group = REG_NOM, color = REG_NOM
  )) +
  geom_line() +
  geom_point() +
  ggtitle("Benzodiazepines and Non-Benzodiazepines anxiolytics 
          consumption in the metropolitan regions of France from 2018 to 2023") +
  xlab("Year") + ylab("Consumption metric") + labs(color = "Régione") +
  scale_color_manual(values = rdbu_12_colors) +
  theme_minimal()




