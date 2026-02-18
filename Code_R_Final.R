library(tidyverse)
library(countrycode)
library(ggplot2)
library(zoo)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)


setwd("C:/Users/Belar/OneDrive/Bureau/Documents/projet_tut_m1_m2")

# 1. Engrais (Source WDI)
climate_panel <- read_csv("conso_engrais.csv", skip = 4) %>%
  pivot_longer(cols = `1990`:`2022`, names_to = "Year", values_to = "Fertilizer_Usage") %>%
  mutate(Year = as.numeric(Year)) %>%
  rename(ISO_Code = `Country Code`) %>%
  select(ISO_Code, Year, Fertilizer_Usage)

# 2. Température Moyenne (Surface)
temp_surface <- read_csv("temp_surf.csv") %>%
  pivot_longer(cols = `2000`:`2022`, names_to = "Year", values_to = "Temp_Mean") %>%
  mutate(Year = as.numeric(Year)) %>%
  select(ISO_Code, Year, Temp_Mean)

# ---DONNÉES FAOSTAT (Production & Capital) ---

# 3. Indice de Production (On garde le Total "Crops" pour la productivité globale)
# ---PRODUCTION (Filtrage spécifique)---
cultures_cibles <- c("Wheat", "Barley", "Maize (corn)")
prod_index <- read_csv("Gross Production Index Number (2014-2016 = 100).csv", show_col_types = FALSE) %>%
  
  # On garde que l'indice de production et nos 3 céréales
  filter(Element == "Gross Production Index Number (2014-2016 = 100)",
         Item %in% cultures_cibles) %>%
  
  # On transforme les noms de pays en codes ISO (ex: Spain -> ESP)
  mutate(ISO_Code = countrycode(Area, origin = "country.name", destination = "iso3c")) %>%
  select(ISO_Code, Year, Item, Value) %>%
  rename(Y_Productivity = Value, Crop_Type = Item)


# --- D. IRRIGATION / EAU (AQUASTAT) ---
water_data <- read_delim("Irrigated_Agriculture_Water_Use_Efficiency_USD_m3.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

view(water_data)

# --- E. FUSION FINALE ---
final_data <- climate_panel %>%
  
  # On fusionne avec la production (ceci va créer les 3 lignes par pays/année)
  left_join(prod_index, by = c("ISO_Code", "Year")) %>%
  
  # On ajoute le reste des variables
  left_join(temp_surface, by = c("ISO_Code", "Year")) %>%
  left_join(water_data, by = c("ISO_Code", "Year"))

# --- F. NETTOYAGE FINAL ---
# On enlève les lignes où la production est vide (pays non ciblés)
final_data <- final_data %>% filter(!is.na(Y_Productivity))

# On sauvegarde le résultat
write_csv(final_data, "AgriYield_Final_Panel.csv")

# Vérification
print(nrow(final_data))
head(final_data)


# ---1. Chargement et Nettoyage des Précipitations---
precip_data <- read_csv("precipitation_data.csv") %>%
  
  # On passe toutes les colonnes années en long
  pivot_longer(
    cols = matches("^19\\d{2}$|^20\\d{2}$"),  # 1990–2022
    names_to  = "Year",
    values_to = "Precip_Annual"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Precip_Annual = as.numeric(Precip_Annual)
  )

precip_data <- precip_data[, -c(1, 2, 4, 5, 6)]
precip_data

# ---2. Fusion et Création des variables quadratiques---
final_data <- read_csv("AgriYield_Final_Panel.csv", show_col_types = FALSE) %>%
  # ajout des précipitations
  left_join(precip_data, by = c("ISO_Code", "Year")) %>%
  # création des variables au carré et des logs
  mutate(
    Temp_Mean_Sq = Temp_Mean^2,           # Température au carré
    log_Yield = log(Y_Productivity)        # Log du rendement pour l'élasticité
  )

# Ajout de la température au carré
final_data <- final_data %>%
  mutate(Temp_Mean_Sq = Temp_Mean^2)


# Nécessaire pour l'interpolation
# --- BLOC A : Taux d'irrigation (%) ---
Irrig_Rate <- read_delim("irrgation_rate_in_percentage.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  # ÉTAPE CRUCIALE :
  # Les colonnes d'années peuvent contenir ":" / suffixes / texte.
  # On force en character pour pouvoir ensuite extraire la partie numérique proprement.
  mutate(across(matches("^20\\d{2}$"), as.character)) %>%
  
  # Filtrage :zone + pays échantillon (noms OU codes)
  filter(Pays %in% c("France","Italy","Greece","Romania","Portugal","Spain")) %>%
  
  mutate(
    Year = as.numeric(Year),
    
    # Nettoyage : extrait uniquement le nombre (":" -> NA)
    Irrig_Rate = as.numeric(str_extract(Irrig_Rate, "[0-9.]+")),
    
    # Harmonisation Eurostat -> ISO3 (accepte noms OU codes)
    ISO_Code = case_when(
      Pays %in% c("FR", "France")   ~ "FRA",
      Pays %in% c("IT", "Italy")    ~ "ITA",
      Pays %in% c("EL", "Greece")   ~ "GRC",
      Pays %in% c("RO", "Romania")  ~ "ROU",
      Pays %in% c("PT", "Portugal") ~ "PRT",
      Pays %in% c("ES", "Spain")    ~ "ESP",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # On garde uniquement les variables utiles pour la fusion avec le panel final
  select(ISO_Code, Year, Irrig_Rate)

# Après ton select(ISO_Code, Year, Irrig_Rate)
Irrig_Rate <- Irrig_Rate %>%
  filter(!is.na(ISO_Code), !is.na(Year)) %>%
  group_by(ISO_Code, Year) %>%
  summarise(
    Irrig_Rate = mean(Irrig_Rate, na.rm = TRUE),  # ou first(Irrig_Rate)
    .groups = "drop"
  )

# Vérif : doit être vide
Irrig_Rate %>% count(ISO_Code, Year) %>% filter(n > 1)


# Ajout de la colonne irrig rate
final_data <- final_data %>%
  left_join(Irrig_Rate, by = c("ISO_Code", "Year")) %>%
  group_by(ISO_Code) %>%
  arrange(Year) %>%
  
  # Interpolation pour boucher les trous (ex: entre 2016 et 2020)
  mutate(
    Irrig_Rate = na.approx(Irrig_Rate, na.rm = FALSE)) %>%
  
  # On prolonge les valeurs pour 2021 et 2022 qui sont souvent vides
  fill(Irrig_Rate, .direction = "downup") %>%
  ungroup()

# Sauvegarde
write_csv(final_data, "AgriYield_Final_v2.csv")


# Aperçu du résultat
print(head(select(final_data, ISO_Code, Year, Temp_Mean, Temp_Mean_Sq, Precip_Annual)))


# petit graphique sympatique
irrig_graphe <- Irrig_Rate %>%
  filter(!is.na(ISO_Code), !is.na(Year)) %>%
  group_by(ISO_Code, Year) %>%
  summarise(Irrig_Rate = mean(Irrig_Rate, na.rm = TRUE), .groups = "drop") %>%
  arrange(ISO_Code, Year) %>%
  group_by(ISO_Code) %>%
  mutate(Irrig_Rate = na.approx(Irrig_Rate, x = Year, na.rm = FALSE)) %>%
  fill(Irrig_Rate, .direction = "downup") %>%
  ungroup()

final_data_graphe <- final_data %>%
  select(-Irrig_Rate) %>%   # évite d’avoir deux colonnes Irrig_Rate si elle existe déjà
  left_join(irrig_graphe, by = c("ISO_Code", "Year"))


ggplot(final_data_graphe, aes(x = Year, y = Irrig_Rate)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~ISO_Code) +
  labs(#title = "Évolution du taux d'irrigation après interpolation",
    subtitle = "Source: Eurostat",
    y = "% de la surface irriguée", x = "Année") +
  theme_minimal()



###################################################################
################## PASSONS AU MODELE ! ############################
###################################################################



# On s'assure que la base est bien chargée et on filtre les NA sur les variables clés
data_inf <- read_csv("AgriYield_Final_v2.csv") %>%
  filter(!is.na(Temp_Mean), !is.na(Y_Productivity), !is.na(Irrig_Rate))

# --- PARTIE MAÏS ---
data_maize <- final_data %>% 
  filter(Crop_Type == "Maize (corn)", !is.na(Temp_Mean), !is.na(log_Yield))


model_maize <- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
    Fertilizer_Usage,      
  data = data_maize,
  model = "within",
  effect = "twoways",   
  index = c("ISO_Code", "Year")
)


model_maize_pooling <- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
                             Fertilizer_Usage,     
                           data = data_maize,
                           model = "pooling",
                           effect = "twoways",    
                           index = c("ISO_Code", "Year")
)

summary(model_maize)
pFtest(model_maize, model_maize_pooling)
pbgtest(model_maize)
bptest(model_maize) ##### Le test Breusch–Pagan et le test Breusch–Godfrey/Wooldridge rejettent, respectivement, l’hypothèse d’homoscédasticité 
##### et mettent en évidence une autocorrélation des erreurs. Par conséquent, afin d’obtenir une inférence valide,
##### les résultats sont présentés avec des erreurs standards robustes de type Arellano, clusterisées par individu, robustes à l’hétéroscédasticité 
##### et à la corrélation sérielle intra-panel


coeftest(model_maize, vcov = vcovHC(model_maize, type = "HC1", cluster = "group"))


# --- PARTIE BLÉ ---
data_ble <- final_data %>% 
  filter(Crop_Type == "Wheat", !is.na(Temp_Mean), !is.na(log_Yield))


model_ble <- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
                   Fertilizer_Usage,      
                 data = data_maize,
                 model = "within",
                 effect = "twoways",       
                 index = c("ISO_Code", "Year")
)

model_ble_pooling<- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
                          Fertilizer_Usage,     
                        data = data_maize,
                        model = "pooling",
                        effect = "twoways",       
                        index = c("ISO_Code", "Year")
)
summary(model_ble)
pFtest(model_ble, model_ble_pooling)
pbgtest(model_ble)
bptest(model_ble) 

##### Le test Breusch–Pagan et le test Breusch–Godfrey/Wooldridge rejettent, respectivement, l’hypothèse d’homoscédasticité 
##### et mettent en évidence une autocorrélation des erreurs. Par conséquent, afin d’obtenir une inférence valide,
##### les résultats sont présentés avec des erreurs standards robustes de type Arellano, clusterisées par individu, robustes à l’hétéroscédasticité 
##### et à la corrélation sérielle intra-panel

coeftest(model_ble, vcov = vcovHC(model_ble, method = "arellano",type = "HC1", cluster = "group"))

# --- PARTIE ORGE ---
data_barley <- final_data %>% 
  filter(Crop_Type == "Barley", !is.na(Temp_Mean), !is.na(log_Yield))

model_barley <- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
                      Fertilizer_Usage,     
                    data = data_maize,
                    model = "within",
                    effect = "twoways",     
                    index = c("ISO_Code", "Year")
)

model_barley_pooling <- plm(log_Yield ~ Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
                              Fertilizer_Usage, 
                            data = data_maize,
                            model = "pooling",
                            effect = "twoways",       
                            index = c("ISO_Code", "Year")
)##### Modèle pooling servant de référence pour le test des effets fixes

summary(model_barley)
pFtest(model_barley, model_barley_pooling)
pbgtest(model_barley)
bptest(model_barley)

##### Le test Breusch–Pagan et le test Breusch–Godfrey/Wooldridge rejettent, respectivement, l’hypothèse d’homoscédasticité 
##### et mettent en évidence une autocorrélation des erreurs. Par conséquent, afin d’obtenir une inférence valide,
##### les résultats sont présentés avec des erreurs standards robustes de type Arellano, clusterisées par individu, robustes à l’hétéroscédasticité 
##### et à la corrélation sérielle intra-panel

coeftest(model_barley, vcov = vcovHC(model_barley, method = "arellano", type = "HC1", cluster = "group"))

##### Partie All

model_all <- lm(
  log_Yield ~  Temp_Mean_Sq + Precip_Annual + Irrig_Rate + Irrigation_Proxy +
    Fertilizer_Usage +
    Temp_Mean_Sq * Crop_Type +
    Precip_Annual * Crop_Type +
    Irrig_Rate * Crop_Type +
    Fertilizer_Usage * Crop_Type +
    Irrigation_Proxy +
    factor(ISO_Code) +   # effet fixe pays
    factor(Year),        # effet fixe temps
  data = final_data
)


summary(model_all)
bptest(model_all)

coeftest(model_all, vcov = vcovHC(model_all, method = "arellano", type = "HC1", cluster = "group"))


# ============================================
# BLOC 1 : STATISTIQUES DESCRIPTIVES
# ============================================

# Sélection des variables d'intérêt
vars_interest <- final_data %>%
  select(ISO_Code, Year, Crop_Type, Y_Productivity, Temp_Mean, Precip_Annual,
         Temp_Mean_Sq, Irrig_Rate, Irrigation_Proxy,
         Fertilizer_Usage)


# Statistiques par pays
stats_by_country <- vars_interest %>%
  group_by(ISO_Code) %>%
  summarise(
    Rendement_moyen = mean(Y_Productivity, na.rm = TRUE),
    Temp_moyenne = mean(Temp_Mean, na.rm = TRUE),
    Precip_moyenne = mean(Precip_Annual, na.rm = TRUE),
    Taux_irrigation = mean(Irrig_Rate, na.rm = TRUE),
    Proxy_irrgation = mean(Irrigation_Proxy, na.rm = TRUE),
    Fertilisation = mean(Fertilizer_Usage, na.rm = TRUE),
    N_obs = n()
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(stats_by_country)
write.csv(stats_by_country, "stats_par_pays.csv", row.names = FALSE)



# ============================================
# BLOC 2 : SORTIE POUR LATEX
# ============================================

# Préparation des erreurs-types robustes pour stargazer
se_maize_robust <- sqrt(diag(vcovHC(model_maize, method = "arellano", type = "HC1", cluster = "group")))
se_ble_robust <- sqrt(diag(vcovHC(model_ble, method = "arellano", type = "HC1", cluster = "group")))
se_barley_robust <- sqrt(diag(vcovHC(model_barley, method = "arellano", type = "HC1", cluster = "group")))
se_all_robust <- sqrt(diag(vcovHC(model_all, method = "arellano", type = "HC1", cluster = "group")))


# Génération du tableau LaTeX
stargazer(model_maize, model_ble, model_barley,
          type = "latex",  # Pour Overleaf*
          digits = 10,
          se = list(se_maize_robust, se_ble_robust, se_barley_robust),
          title = "Déterminants de la productivité céréalière en Europe du Sud (2003-2022)",
          column.labels = c("Maïs", "Blé", "Orge"),
          dep.var.labels = "Log(Rendement)",
          covariate.labels = c("Température moyenne au carré (°C)", 
                               "Précipitations annuelles (m/an)",
                               "Taux d'irrigation",
                               "Proxy irrigation",
                               "Usage d'engrais (kg/ha)"),
          add.lines = list(
            c("Effets fixes pays", "Oui", "Oui", "Oui"),
            c("Erreurs-types", "Robustes", "Robustes", "Robustes")
          ),
          omit.stat = c("ser", "f"),
          notes = c("Erreurs-types robustes clustérisées par pays entre parenthèses.",
                    "* p<0.01, ** p<0.05, * p<0.1"),
          notes.align = "l",
          out = "tableau_resultats.tex")  # Fichier de sortie




stargazer(model_all,
          type = "latex",
          se = list(se_all_robust),
          title = "Modèle global : effets fixes pays et temps + interactions par culture",
          dep.var.labels = "log(Rendement)",
          omit.stat = c("f", "ser"),
          notes = c("Erreurs standards robustes (HC1) clusterisées par pays entre parenthèses.",
                    "* p<0.1, ** p<0.05, *** p<0.01"),
          notes.align = "l",
          float.env = "table*",
          out = "modele_global_FE_interactions.tex")



# Affichage à l'écran
stargazer(model_maize, model_ble, model_barley,
          type = "text",  # Pour voir dans R
          se = list(se_maize_robust, se_ble_robust, se_barley_robust),
          column.labels = c("Maïs", "Blé", "Orge"))

# Affichage à l'écran
stargazer(model_all,
          type = "text",  # Pour voir dans R
          se = list(se_all_robust),
          column.labels = c("log_rendement"))


# ============================================
# BLOC 3 : GRAPHIQUES POUR LE RAPPORT
# ============================================


# GRAPHIQUE 1: Évolution des rendements pour les 6 pays
p1 <- ggplot(final_data, aes(x = Year, y = Y_Productivity, color = Crop_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  # Création de sous-graphiques (facets) pour chaque pays (ISO_Code).
  # scales = "free_y" permet à l'axe des ordonnées de s'adapter à l'échelle de production de chaque pays (évite d'écraser les courbes des petits producteurs).
  # ncol = 3 organise les graphiques sur 3 colonnes, créant ainsi une grille de 2 lignes, 3 colonnes.
  facet_wrap(~ISO_Code, scales = "free_y", ncol = 3) +
  labs(title = "Évolution des indices de production céréalière (2003-2022)",
       subtitle = "Dynamiques nationales par type de culture (Base 100 en 2014-2016)",
       x = "Année", 
       y = "Indice de production",
       color = "Culture") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11) # Met en gras les noms des pays
  )

# Sauvegarde
ggsave("evolution_rendements_par_pays.png", p1, width = 12, height = 8, dpi = 300)
print(p1)



# ---- GRAPHIQUE 2 : Boxplot température par pays ----
p2 <- ggplot(final_data %>% filter(is.finite(Temp_Mean)),
             aes(x = ISO_Code, y = Temp_Mean, fill = ISO_Code)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution des températures moyennes par pays (2003-2022)",
       x = "Pays", y = "Température moyenne (°C)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("boxplot_temp_pays.png", p2, width = 8, height = 5, dpi = 300)

print(p2)


# ==============================================================================
# GRAPHIQUE 3 : SCATTER PLOT (Effet Chaleur)
# ==============================================================================

# On ne garde que les lignes où le rendement ET la température sont renseignés (pas de NA)
simple_data <- final_data %>%
  filter(!is.na(Y_Productivity), !is.na(Temp_Mean)) 

p_scatter <- ggplot(simple_data, aes(x = Temp_Mean, y = Y_Productivity)) +
  geom_point(alpha = 0.5, color = "grey50") +
  # L'objectif est de visualiser la direction de la corrélation (montante ou descendante)
  geom_smooth(method = "lm",
              #effet quadratique Temp_Mean dans le modèle économétrique
              formula = y ~ poly(x, 2), 
              color = "blue", size = 1.5) +
  facet_wrap(~Crop_Type, scales = "free") + # Séparé par culture
  labs(title = "Impact de la température sur le rendement",
       subtitle = "Ligne tendancière illustrant que la chaleur nuit à la culture",
       x = "Température Moyenne (°C)",
       y = "Indice de Production") +
  theme_minimal()

ggsave("Graph_Simple_Scatter.png", p_scatter, width = 9, height = 5)
print(p_scatter)

