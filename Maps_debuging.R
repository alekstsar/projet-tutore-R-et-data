# EXPERIMENTAL MAP gradient
# Function to generate the choropleth for a specific year

# Calculate global min and max values of the dataset to anchor the color gradient

global_min <- min(FranceBoites$TCS, na.rm = TRUE) # na.rm = TRUE handles potential NA values
global_max <- max(FranceBoites$TCS, na.rm = TRUE)
median_tcs <- median(FranceBoites$TCS, na.rm = TRUE)

# Print them to check (optional)
print(paste("Global Min:", global_min))
print(paste("Global Max:", global_max))



library(ggplot2)
library(dplyr)
library(patchwork)

library(paletteer)
library(RColorBrewer)

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
  plot_layout(guides = "collect") + 
  patchwork::plot_annotation(
    title = "Évolution de l'utilisation d'anxiolytiques standardisée pour 1 000 habitants \nen France Métropolitaine (2018-2022)", subtitle = "Comparaison régionale des tendances de consommation sur six ans",
    caption = "Source de données: OpenMedic et INSEE") 







# separate plots to combine into a sigle image with a common legend and common title.
# the title doesnt work

# choropleth 2018 
FrTC2018 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2018") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2018") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  )+ labs(title = "2018")


# choropleth 2019 

FrTC2019 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2019") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2019") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "2019")

# choropleth 2020 

FrTC2020 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2020") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2020") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "2020")

# choropleth 2021 

FrTC2021 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2021") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2021") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "2021")


# choropleth 2022 

FrTC2022 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2022") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2022") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  )+ labs(title = "2022")

# choropleth 2023 

FrTC2023 <- FranceBoites %>%
  select(Year, REG_NOM, TCS) %>% 
  filter(Year == "2023") %>% 
  ggplot(aes(fill = TCS)) +
  #ggtitle("2023") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf() +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", breaks = c(global_min, median_tcs, global_max),
                       labels = c(paste0(round(global_min, digits = 0)), paste0(round(median_tcs, digits = 0)), paste0(round(global_max, digits = 0))),
                       name = paste("Taux de consommation \nstandardisé"),
                       limits = c(global_min, global_max) # Set the fixed limits here
  ) + labs(title = "2023")


FrTCTot <- ((FrTC2018 + FrTC2019 + FrTC2020)/ (FrTC2021 + FrTC2022 + FrTC2023)) 


FrTCTot +
  plot_layout(guides = "collect") + 
  patchwork::plot_annotation(
    title = "Évolution de l'utilisation d'anxiolytiques standardisée pour 1 000 habitants \nen France Métropolitaine (2018-2023)", subtitle = "Comparaison régionale des tendances de consommation",
    caption = "Source de données: OpenMedic et INSEE") 




# Animation GIF

library(gganimate)

p_choropleth_gr <- FranceBoites %>% # Utiliser l'intégralité du jeu de données FranceBoites
  ggplot(aes(fill = TCS)) + # 'fill = TCS' pour choroplèthe
  geom_sf() +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, vjust=0.5, face='bold'), plot.subtitle = element_text(hjust = 0.5)) + # Centrer le titre et le sous-titre
  scale_fill_distiller(
    palette = "RdBu",
    breaks = c(global_min, global_max),
    labels = c(paste0(round(global_min, digits = 0)), paste0(round(global_max, digits = 0))),
    name = paste("Taux de consommation \nstandardisé"),
    limits = c(global_min, global_max) # Keep global limits
  ) +
  labs(
    title = "Évolution de l'utilisation d'anxiolytiques standardisée \npour 1 000 habitants \nen France Métropolitaine (2018-2023)", # General main title
    subtitle = "Année: {closest_state}" # Dynamic subtitle to show the year
  )

# 3. Add animation layers using gganimate - transition_states on 'Year'
anim_choropleth_gr <- p_choropleth_gr +
  transition_states(Year, # Animate by 'Year' column
                    transition_length = 1,
                    state_length = 2) +
  enter_fade() +
  exit_fade()

# 4. Save the animation as a GIF (or other format)
animate(anim_choropleth_gr,
        duration = 10, # Adjust duration as needed
        fps = 20,      # Adjust fps as needed
        renderer = gifski_renderer("choropleth_animation_grad.gif")) 