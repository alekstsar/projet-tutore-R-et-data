# Presentation
#--------------------------------------------------------
scale_fill_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                  breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                  drop=FALSE)



scale_fill_manual(values = c("#641220","#A11D33","#ca0101","#e35053","#f1a7a9","#f9dcde","#bbdefb","#64b5f6","#2196f3","#2a6f97","#014f86","#051923"),
                  breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Centre-Val de Loire","Alsace-Champagne-Ardenne-Lorraine","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                  drop=FALSE)

# Line graphs
#--------------------------------------------------------
# Line graph total by year

library(patchwork)

boites_sp <- Tot_2018_2023_filtrage %>%
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
  xlab("Année") + 
  ylab("Mesure de la délivrance (boites)") + 
  labs(color = "Région", 
       #title = "A" ,
       subtitle = "Usage d'anxiolytiques \nen France métropolitaine \nentre 2018 et 2023") +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_classic()

# Spaghetti graph of standardised consumption of the investigated anxiolitics by region by year

TCS_sp <- Tot_2018_2023_filtrage %>%
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
  xlab("Année") + 
  ylab("Mesure de la délivrance (TDS)") + 
  labs(color = "Région", 
       #title = "B" ,
       subtitle = "Usage d'anxiolytiques pour 1000 habitants \nen France métropolitaine \nentre 2018 et 2023") +
  scale_color_manual(values = c("#A11D33","#ca0101","#e35053","#f1a7a9","#eed800","#fff23e","#ffe130","#ffb700","#64b5f6","#2196f3","#014f86","#051923"),
                     breaks = c("Nord-Pas-de-Calais-Picardie","Bretagne","Normandie","Aquitaine-Limousin-Poitou-Charentes","Provence-Alpes-Côte d'Azur et Corse","Bourgogne-Franche-Comté","Languedoc-Roussillon-Midi-Pyrénées","Alsace-Champagne-Ardenne-Lorraine", "Centre-Val de Loire","Auvergne-Rhône-Alpes","Pays de la Loire","Ile-de-France"),
                     drop=FALSE) +
  theme_classic()


(boites_sp + TCS_sp) +
  plot_layout(guides = "collect") & theme(legend.position = 'right')





# Cartes
#--------------------------------------------------------

# choropleth 2018 
FrTC2018 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2018") %>% 
  ggplot(aes(fill = TCS)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Pré-COVID \nAnnée: 2018" )


# choropleth 2019 

FrTC2019 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2019") %>% 
  ggplot(aes(fill = TCS)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Année: 2019")

# choropleth 2020 

FrTC2020 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2020") %>% 
  ggplot(aes(fill = TCS)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Pandémie \nAnnée: 2020")

# choropleth 2021 

FrTC2021 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2021") %>% 
  ggplot(aes(fill = TCS)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Année: 2021")


# choropleth 2022 

FrTC2022 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2022") %>% 
  ggplot(aes(fill = TCS)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Post-COVID \nAnnée: 2022")

# choropleth 2023 

FrTC2023 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2023") %>% 
  ggplot(aes(fill = TCS)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", 
                       #breaks = c(global_min, median_tcs, global_max),
                       #labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux \nde délivrance \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "Année: 2023")

  
FrTC2018 + plot_spacer() + FrTC2020 + plot_spacer() + FrTC2022 + FrTC2019 + plot_spacer() + FrTC2021 + plot_spacer() + FrTC2023 +
  #FrTCTot_p <- FrTCTot +
  plot_layout(guides = "collect", ncol = 5, widths = c(7, 1, 7, 1, 7)) + 
  plot_annotation(
    #title = "Évolution d'usage d'anxiolytiques standardisée pour 1 000 habitants en France Métropolitaine (2018-2023)", 
    subtitle = "Comparaison régionale des tendances de délivrance des médicaments",
    caption = "Source de données: OpenMedic et INSEE") & theme(legend.position = 'right')






#main
((FrTC2018 + FrTC2020 + FrTC2022) / (FrTC2019 + FrTC2021 + FrTC2023)) +
#FrTCTot_p <- FrTCTot +
  plot_layout(guides = "collect") + 
  plot_annotation(
    #title = "Évolution d'usage d'anxiolytiques standardisée pour 1 000 habitants en France Métropolitaine (2018-2023)", 
    subtitle = "Comparaison régionale des tendances de délivrance des médicaments",
    caption = "Source de données: OpenMedic et INSEE") & theme(legend.position = 'right')
