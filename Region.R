# Usage des ANXIOLYTIQUES DE TYPE BENZODIAZEPINE N05BA par region en France 2018-2023

# ------------------------------------------------------------
# Librairies and datas
# ------------------------------------------------------------
library(tidyverse)
library(gtsummary)
library(gt)

# read and summaries data

View(N05BA_2018_2023)
View(N05B_2018_2023)

summary(N05B_2018_2023)
summary(N05BA_2018_2023)

# ------------------------------------------------------------
# Data management
# ------------------------------------------------------------
N05BA_2018_2023 <- N05BA_2018_2023 %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(ATC_code = ATC4)


N05B_2018_2023 <- N05B_2018_2023 %>% 
  rename(ATC_code = ATC5)

#Combine into 1 table
Tot_2018_2023 <- bind_rows(
  N05B_2018_2023,
  N05BA_2018_2023,
  .id = NULL
)

summary(Tot_2018_2023)

Tot_2018_2023 <- Tot_2018_2023 %>% 
  transform(Year = as.factor(Year),
            BEN_REG = as.factor(BEN_REG),
            sexe = as.factor(sexe),
            age = as.factor(age)) %>% 
  mutate(REG_NOM = (case_when(BEN_REG == 0 ~ "Inconnu",
                              BEN_REG == 99 ~ "Inconnu",
                              BEN_REG == 5 ~ "Régions et Départements d'outre-mer",
                              BEN_REG == 11 ~ "Ile-de-France",
                              BEN_REG == 24 ~ "Centre-Val de Loire",
                              BEN_REG == 27 ~ "Bourgogne-Franche-Comté",
                              BEN_REG == 28 ~ "Normandie",
                              BEN_REG == 32 ~ "Nord-Pas-de-Calais-Picardie",
                              BEN_REG == 44 ~ "Alsace-Champagne-Ardenne-Lorraine",
                              BEN_REG == 52 ~ "Pays de la Loire",
                              BEN_REG == 53 ~ "Bretagne",
                              BEN_REG == 75 ~ "Aquitaine-Limousin-Poitou-Charentes",
                              BEN_REG == 76 ~ "Languedoc-Roussillon-Midi-Pyrénées",
                              BEN_REG == 84 ~ "Auvergne-Rhône-Alpes",
                              BEN_REG == 93 ~ "Provence-Alpes-Côte d'Azur et Corse")
  )) 



Tot_2018_2023 <- Tot_2018_2023 %>%
  mutate(PERIODE = case_when( Year == 2018 | Year == 2019 ~ "Pré-COVID",
                              Year == 2020 | Year == 2021 ~ "COVID", 
                              Year == 2022 | Year == 2023 ~ "Post-COVID")
  )





# ------------------------------------------------------------
# Statistics
# ------------------------------------------------------------

# Count number of bottles sold per year in each region (exclude 0 and 99 et Region 5 - outre-mer).

Tot_2018_2023 %>% 
  select(Year, BEN_REG, BOITES2) %>% 
  filter(BEN_REG != 0 & BEN_REG != 99 & BEN_REG != 5 ) %>% 
  group_by(Year, BEN_REG) %>% 
  summarise(Sold = sum(BOITES2)) %>% 
  print(n=nrow(.))

# Count number of bottles sold per period in each region (exclude 0 and 99 and 5).

Tot_2018_2023 %>% 
  select(PERIODE, BEN_REG, BOITES2) %>% 
  filter(BEN_REG != 0 & BEN_REG != 99 & BEN_REG != 5 ) %>% 
  group_by(PERIODE, BEN_REG) %>% 
  summarise(Sold = sum(BOITES2)) %>% 
  print(n=nrow(.))

# summary table

Tot_2018_2023 %>% 
  select(Year, REG_NOM, BOITES2) %>% 
  transform(Year = as.factor(Year)) %>% 
  filter(REG_NOM != "Inconnu" & REG_NOM != "Régions et Départements d'outre-mer") %>%
  droplevels() %>%
  group_by(Year, REG_NOM) %>%
  summarise(Sold = sum(BOITES2)) %>%
  tbl_summary(
    by = Year,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 2,
    missing = "no",
    label = BOITES2 ~"Nombre de boîtes des ANXIOLYTIQUES délivrées"
    ) %>%
    modify_header(label ~ "**Variable**") %>% 
    modify_caption("**Tableau 1. Usage des ANXIOLYTIQUES par region en France 2018-2023**") %>%
    bold_labels()


# ------------------------------------------------------------
# Graphiques
# ------------------------------------------------------------
library(viridis)
library(paletteer)
library(RColorBrewer)


# Color scheme creation
#--------------------------------------------------------
library(jcolors)

rdbu_base_colors <- brewer.pal(11, "RdBu")
rdbu_base_colors_reversed <- rev(rdbu_base_colors)
# 2. Create a color ramp function based on RdBu
rdbu_color_ramp <- colorRampPalette(rdbu_base_colors_reversed)

# 3. Generate 12 colors from the RdBu color ramp
rdbu_12_colors <- rdbu_color_ramp(12)


scale_fill_manual(values = c("#641220","#A11D33","#ca0101","#e35053","#f1a7a9","#f9dcde","#bbdefb","#64b5f6","#2196f3","#2a6f97","#014f86","#051923"),
                  breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Centre-Val de Loire","Alsace-Champagne-Ardenne-Lorraine","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                  drop=FALSE)


scale_fill_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                  breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                  drop=FALSE)

# Heatmaps
#--------------------------------------------------------
# Heatmap by year

Tot_2018_2023 %>% 
  select(Year, REG_NOM, BOITES2) %>% 
  filter(REG_NOM != "Inconnu" & REG_NOM != "Régions et Départements d'outre-mer") %>% 
  group_by(Year, REG_NOM) %>% 
  summarise(Sold = sum(BOITES2)) %>% 
  ggplot(aes(x= Year, y = REG_NOM, fill= Sold)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  #scale_fill_manual(values = rdbu_12_colors) +
  theme_minimal()

# Heatmap by period

Tot_2018_2023 %>% 
  select(PERIODE, REG_NOM, BOITES2) %>% 
  filter(REG_NOM != "Inconnu" & REG_NOM != "Régions et Départements d'outre-mer") %>% 
  group_by(PERIODE, REG_NOM) %>% 
  summarise(Sold = sum(BOITES2)) %>% 
  ggplot(aes(x= PERIODE, y = REG_NOM, fill= Sold)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal()


# Line graphs
#--------------------------------------------------------
# Line graph total by year

Tot_2018_2023 %>%
  select(Year, REG_NOM, BOITES2) %>% 
  filter(REG_NOM != "Inconnu" & REG_NOM != "Régions et Départements d'outre-mer") %>% 
  group_by(Year) %>% 
  summarise(Sold = sum(BOITES2)) %>%
  ggplot(
    aes(x = as.numeric(Year), y = Sold
        )) +
  geom_line() +
  geom_point() +
  ggtitle("Total Benzodiazepines and Non-Benzodiazepines anxiolytics 
          consumption in Metropolitan France from 2018 to 2023") +
  ylab("Rate of psychotropic drugs consumption (in bottles) ") +
  xlab("Year") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_minimal(labels = as.character(Year))



# EXPERIMENTS with color palettes for line graphs

Tot_2018_2023_filtrage %>%
  select(Year, REG_NOM, TCS) %>% 
  group_by(Year, REG_NOM) %>% 
  ggplot(aes(
    x = Year, y = TCS, 
    group = REG_NOM, color = REG_NOM
  )) +
  geom_line() +
  geom_point() +
  #scale_y_log10() +
  #ggtitle("A") +
  xlab("Année") + ylab("Mesure de la consommation (TCS)") + labs(color = "Régione", title = "A" ,subtitle = "Utilisation d'anxiolytiques pour 1000 habitants en France métropolitaine entre 2018 et 2023") +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_minimal()

#scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
#name = "Taux de consommation standardisé 2018",
#limits = c(global_min, global_max)


# Spaghetti graph of absolute consumption of the investigated anxiolitics by region by year

library(patchwork)

boites_spg <- Tot_2018_2023_filtrage %>%
  select(Year, REG_NOM, Sold) %>% 
  group_by(Year, REG_NOM) %>% 
  ggplot(aes(
    x = Year, y = Sold, 
    group = REG_NOM, color = REG_NOM
  )) +
  geom_line() +
  geom_point() +
  #scale_y_log10() +
  #ggtitle("Usage d'anxiolytiques \nen France métropolitaine \nentre 2018 et 2023") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("Année") + ylab("Mesure de la délivrance (boites)") + labs(color = "Régione", title = "A" ,subtitle = "Usage d'anxiolytiques \nen France métropolitaine \nentre 2018 et 2023") +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_minimal()
 
# Spaghetti graph of standardised consumption of the investigated anxiolitics by region by year

TCS_spg <- Tot_2018_2023_filtrage %>%
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
  #ggtitle("Usage d'anxiolytiques pour \n1000 habitants en France métropolitaine \nentre 2018 et 2023") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("Année") + ylab("Mesure de la délivrance (TDS)") + labs(color = "Régione", title = "B" ,subtitle = "Usage d'anxiolytiques pour \n1000 habitants en France métropolitaine \nentre 2018 et 2023") +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_minimal()


(boites_spg + TCS_spg) +
  plot_layout(guides = "collect") & theme(legend.position = 'right')




# Barplots
#--------------------------------------------------------
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


# ------------------------------------------------------------
# Maps
# ------------------------------------------------------------

library(rmapshaper)
library(sf)
library(plotly)


# Open region France geojson file source (https://github.com/gregoiredavid/france-geojson/blob/master/regions.geojson)
# THAT'S THE ONE WE WILL USE!!!

FranceMet <- read_sf(file.path(getwd(), "/regions.geojson"))

# VISUALISATION TEST
ggplot(FranceMet) +
  geom_sf(fill = "red", color = "blue") +
  theme_minimal()

# Agreger regions 93 et 94

print(unique(FranceMet$code))  # Adjust column name if necessary
print(unique(FranceMet$nom))


# Modify region names: Assign the same name to the two regions you want to merge
FranceMet$code[FranceMet$code %in% c("93", "94")] <- "93"
FranceMet$nom[FranceMet$nom %in% c("Provence-Alpes-Côte d'Azur", "Corse")] <- "Provence-Alpes-Côte d'Azur et Corse"

# Try dissolving everything based on region names
FranceMet12 <- ms_dissolve(FranceMet, field = "code")

print(unique(FranceMet12$code))  # Adjust column name if necessary

#
ggplot(FranceMet12) +
  geom_sf(fill = "white", color = "black", linewidth = 0.2) +
  theme_minimal()

Tot_2018_2023_filtrage <- Tot_2018_2023_filtrage %>% 
  mutate(BEN_REG = as.factor(BEN_REG))

summary(Tot_2018_2023_filtrage)
# Make the merge
FranceBoites <- FranceMet12 %>%
  left_join(Tot_2018_2023_filtrage, by = c("code" = "BEN_REG"))

quantile(FranceBoites$TCS)
breaks = round(unique(quantile(FranceBoites$TCS, prob = seq(0, 1, length = 5))), digits = 0)
FranceBoites$deciles = cut(FranceBoites$TCS,breaks=breaks,include.lowest=TRUE)

# Calculate global min and max values of the dataset to anchor the color gradient

global_min <- min(FranceBoites$TCS, na.rm = TRUE) # na.rm = TRUE handles potential NA values
global_max <- max(FranceBoites$TCS, na.rm = TRUE)
median_tcs <- median(FranceBoites$TCS, na.rm = TRUE)

# Print them to check (optional)
print(paste("Global Min:", global_min))
print(paste("Global Max:", global_max))


# Calculate centroids to add Region labels
FranceBoites_centroids <- FranceBoites %>%
  st_centroid()

FranceBoites_multipolygon <- st_cast(FranceBoites, "MULTIPOLYGON")

summary(FranceBoites$TCS)

# EXPERIMENTAL MAP WITH colors and TCS fill label

library(colorBlindness)

FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2018") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  geom_sf_text(data = FranceBoites_centroids %>% filter(Year == "2018"),
               aes(label = round(TCS, 0)),
               size = 4,
               vjust = 0.2) +
theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_gradientn(colors = paletteer_d("colorBlindness::Blue2DarkRed12Steps"), #alpha = 0.5, 
                     n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2018",
                       limits = c( global_min, global_max),  # Set the fixed limits here
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  )

# EXPERIMENTAL MAP avec Nom de la region and multipolygon
FranceBoites_multipolygon %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2021") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  geom_sf_text(data = FranceBoites_centroids %>% filter(Year == "2021"),
               aes(label = REG_NOM),
               size = 4,
               vjust = 0.2) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),breaks = breaks,
                       name = "Taux de consommation standardisé 2021",
                       limits = c(global_min, global_max),  # Set the fixed limits here
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  )

# EXPERIMENTAL MAP gradient
# Function to generate the choropleth for a specific year

library(ggplot2)
library(dplyr)
library(patchwork)

years_to_plot <- unique(FranceBoites$Year) # Get unique years in your data

create_choropleth <- function(year_val) {
  FranceBoites %>%
    select(Year, REG_NOM, TCS) %>% 
    filter(Year == year_val) %>% 
    ggplot(aes(fill = TCS)) +
    ggtitle(year_val) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_sf() +
    theme_void() +
    scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                         labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                         name = paste("Taux de consommation standardisé", year_val),
                         limits = c(global_min, global_max) # Set the fixed limits here
    ) + 
    labs(
      title = year_val
    )
}

# Generate the plots using lapply
plots <- lapply(years_to_plot, create_choropleth)
names(plots) <- years_to_plot

# Arrange the plots in a grid
comb_6 <- ((plots[["2018"]] + plots[["2019"]] + plots[["2020"]]) /(plots[["2021"]] + plots[["2022"]] + plots[["2023"]])) 

final_6 <- comb_6 +
  plot_layout(guides = "collect") & theme(legend.position = "right") + 
  plot_annotation(
    title = "Évolution de l'utilisation d'anxiolytiques standardisée pour 1 000 habitants \nen France Métropolitaine (2018-2023)",
    subtitle = "Comparaison régionale des tendances de consommation sur six ans",
    caption = "Source de données: OpenMedic et INSEE",
    theme = theme(plot.title = element_text())
  )

#theme = theme(plot.title = element_text(hjust = 0.5)

# separate choropleth plots

# choropleth 2018 
FrTC2018 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2018") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", #n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2018",
                       limits = c(global_min, global_max),  # Set the fixed limits here
                       guide = guide_legend(
                         #keyheight = unit(4, units = "mm"),
                         #keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans \n13 régions de France Métropolitaine en 2018",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )

# choropleth 2019 

FrTC2019 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2019") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme( legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2019",
                       limits = c(global_min, global_max),
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2019",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )


# choropleth 2020 

FrTC2020 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2020") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme( legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2020",
                       limits = c(global_min, global_max),
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2020",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )


# choropleth 2021 

FrTC2021 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2021") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme( legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2021",
                       limits = c(global_min, global_max),
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2021",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )


# choropleth 2022 

FrTC2022 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2022") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme( legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2022",
                       limits = c(global_min, global_max),
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2022",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )

# choropleth 2023 

FrTC2023 <- FranceBoites %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2023") %>% 
  ggplot(aes(fill = TCS)) +
  geom_sf() +
  theme_void() +
  theme( legend.position = "bottom") +
  scale_fill_distiller(palette = "RdBu", n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2023",
                       limits = c(global_min, global_max),
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2023",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )

grid.arrange(FrTC2018, FrTC2019, FrTC2020, FrTC2021, FrTC2022, FrTC2023, 
             nrow = 3, 
             top = "Consommation de médicaments anxiolytiques dans 
    les régions de France Métropolitaine entre 2018 et 2023")


#scale_fill_distiller(palette = "YlGnBu", trans = "reverse",
                     #n=nlevels(FranceBoites$deciles), limits = c(global_min, global_max),

# Creation of Function to make several maps. 

years_to_plot <- unique(FranceBoites$Year) # Get unique years in your data

for (year_val in years_to_plot) {
  plot_title <- paste("Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en", year_val)
  
  p <- FranceBoites %>%
    filter(Year == year_val) %>%
    ggplot(aes(fill = TCS)) +
    geom_sf() +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + # Center title
    scale_fill_distiller(
      palette = "RdBu",
      n = nlevels(FranceBoites$deciles),
      name = paste("Taux de consommation standardisé", year_val),
      limits = c(global_min, global_max),
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        keywidth = unit(12, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) +
    ggtitle(plot_title) # Add title to the plot
  
  print(p) # Print the plot for each year
}


# ------------------------------------------------------------
# Maps interactive
# ------------------------------------------------------------

# EXPERIMENT interactive map with plotly
library(plotly)


FrTC2018 <- FranceBoites_multipolygon %>%
  select(Year, REG_NOM, TCS, deciles) %>% 
  filter(Year=="2018") %>% 
  ggplot(aes(fill = TCS,
             text = paste0("Région: ", REG_NOM, "<br>",
                           "TCS: ", round(TCS, 0)))) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_gradientn(colors = rdbu_colors, n=nlevels(FranceBoites$deciles),
                       name = "Taux de consommation standardisé 2018",
                       limits = c(global_min, global_max),  # Set the fixed limits here
                       guide = guide_legend(
                         keyheight = unit(4, units = "mm"),
                         keywidth = unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = "top",
                         nrow = 1)
  ) + labs(
    title = "Consommation de médicaments anxiolytiques dans 
    13 régions de France Métropolitaine en 2018",
    subtitle = "Nombre de boites livrées par 1000 habitants de la région",
    caption = "Data: OpenMedic et INSEE"
  )

interactive_map_2018 <- ggplotly(FrTC2018, tooltip = "text") # Convert ggplot to plotly, specify tooltip

print(interactive_map_2018)

# interactive map loop

years_to_plot <- unique(FranceBoites$Year) # Get unique years in the data

for (year_val in years_to_plot) {
  plot_title <- paste("Consommation de médicaments anxiolytiques dans
    13 régions de France Métropolitaine en", year_val)
  
  p <- FranceBoites %>%
    filter(Year == year_val) %>%
    ggplot(aes(fill = TCS,
               text = paste0("Région: ", REG_NOM, "<br>", # Add 'text' aesthetic for tooltip
                             "TCS: ", round(TCS, 2)))) + # Customize tooltip text
    geom_sf() +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + # Center title
    scale_fill_distiller(
      palette = "RdBu",
      n = nlevels(FranceBoites$deciles),
      name = plot_title,
      limits = c(global_min, global_max),
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        keywidth = unit(12, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) +
    ggtitle(plot_title)
  
  interactive_plot <- ggplotly(p, tooltip = "text") # Convert ggplot to plotly, specify tooltip
  
  print(interactive_plot)
  
# ------------------------------------------------------------
# Maps. animation
# ------------------------------------------------------------
  

  
  library(gganimate)
  
  p_choropleth <- FranceBoites %>% # Utiliser l'intégralité du jeu de données FranceBoites
    ggplot(aes(fill = TCS)) + # 'fill = TCS' pour choroplèthe
    geom_sf() +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + # Centrer le titre et le sous-titre
    scale_fill_distiller(
      palette = "RdBu",
      n = nlevels(FranceBoites$deciles), # Gardez nlevels(FranceBoites$deciles) tel qu'il était dans le boucle
      name = paste(year_val),
      limits = c(global_min, global_max),
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        keywidth = unit(12, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) +
    labs(
      title = "Tendance animée de l'usage des anxiolytiques standardisés \n pour 1 000 habitants en France métropolitaine \n(2018-2023)" # General main title
      #subtitle = "Année: {closest_state}" # Dynamic subtitle to show the year
    )
  
  # 3. Add animation layers using gganimate - transition_states on 'Year'
  anim_choropleth <- p_choropleth +
    transition_states(Year, # Animate by 'Year' column
                      transition_length = 1,
                      state_length = 2) +
    enter_fade() +
    exit_fade()
  
  
  # 4. Save the animation as a GIF (or other format)
  animate(anim_choropleth,
          duration = 30, # Adjust duration as needed
          fps = 20,      # Adjust fps as needed
          renderer = gifski_renderer("choropleth_animation.gif"))
}


# ------------------------------------------------------------
# that's what she said