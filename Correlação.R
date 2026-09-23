# Travessias da Saúde: possíveis relações entre a incidência de câncer e fatores territoriais 


library(ggspatial)
library(sf)
library(geobr)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readxl)


dados <- read_excel("Travessias_BD.xlsx")


#Análise de correlação 

str(dados)

shapiro.test(dados$Inc_mama)
shapiro.test(dados$Inc_colo)
shapiro.test(dados$CNES)
shapiro.test(dados$OBITO_MAMA)
shapiro.test(dados$OBITO_COLO)
shapiro.test(dados$TRAT_MAMA)
shapiro.test(dados$TRAT_COLO)
shapiro.test(dados$IPEA)
shapiro.test(dados$Km_capital)
shapiro.test(dados$Cober_APS)
shapiro.test(dados$Cobertura_citologia)   ##normalidade 
shapiro.test(dados$Cobertura_mamografia)
shapiro.test(dados$DensidadeUF_estab)
shapiro.test(dados$Insatisfatorio_colo)
shapiro.test(dados$ins)





# 1) entre incidências *
cor.test(dados$Inc_mama, dados$Inc_colo, method = "spearman")
                                                                    # >CA MAMA = >CA COLO

# 2) mulheres em tto e óbitos **
cor.test(dados$TRAT_MAMA, dados$OBITO_MAMA, method = "spearman")
                                                                    #Mais mulheres em TTO, mais óbitos 
cor.test(dados$TRAT_COLO, dados$OBITO_COLO, method = "spearman")


# 3) distância e óbitos 
cor.test(dados$Km_capital, dados$OBITO_MAMA, method = "spearman")
                                                                      # negativa, não significativa 
cor.test(dados$Km_capital, dados$OBITO_COLO, method = "spearman")


# 4) tempo de deslocamento e óbitos 
cor.test(dados$Temp_capital, dados$OBITO_MAMA, method = "spearman")
                                                                        # ausência de correlação
cor.test(dados$Temp_capital, dados$OBITO_COLO, method = "spearman")


# 5) Àrea plantada e incidências  *
cor.test(dados$IPEA, dados$Inc_mama, method = "spearman")
                                                                  # positiva fraca
cor.test(dados$IPEA, dados$Inc_colo, method = "spearman")


# 6) Incidências e cobertura APS 
cor.test(dados$Inc_mama, dados$Cober_APS, method = "spearman")
                                                                  # ausência de correlação
cor.test(dados$Inc_colo, dados$Cober_APS, method = "spearman")


# 7) Óbitos e cobertura APS ***
cor.test(dados$OBITO_MAMA, dados$Cober_APS, method = "spearman")
                                                                     #maior cobertura da APS, menores taxas de mortalidade 
cor.test(dados$OBITO_COLO, dados$Cober_APS, method = "spearman")


# 8) Incidências e cobertura citologia/ mamografia 
cor.test(dados$Inc_colo, dados$Cobertura_citologia, method = "spearman")
cor.test(dados$Inc_colo, dados$Cobertura_citologia, method = "pearson")

cor.test(dados$Inc_mama, dados$Cobertura_mamografia, method = "spearman")


# 9) Incidências e densidade de estabelecimentos 
cor.test(dados$Inc_colo, dados$DensidadeUF_estab, method = "spearman")

cor.test(dados$Inc_mama, dados$DensidadeUF_estab, method = "spearman")


# 10) Incidências e proporção de insatisfatórios
cor.test(dados$Inc_colo, dados$Insatisfatorio_colo, method = "spearman")


# 11)Incidências e taxas de positividade/ detecção

cor.test(dados$Inc_mama, dados$Detecçao_mama, method = "spearman")

cor.test(dados$Inc_colo, dados$Positividade_colo, method = "spearman")


## ANÁLISE EXPLORATÓRIA 

library(sf)
library(dplyr)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)


inc_palette <- c("#F2F5E6", "#E6EAD0", "#C8C9A8", "#888B6F", "#4A524A")
cinza_fora <- "#f4f4f4"
  
# Geometrias dos estados brasileiros

br_estados <- ne_states(country = "Brazil", returnclass = "sf")
br_estados <- st_make_valid(br_estados)
br_estados <- st_simplify(br_estados, dTolerance = 0.001)


if ("name_pt" %in% names(br_estados)) {
  br_estados$nome <- br_estados$name_pt
} else {
  br_estados$nome <- br_estados$name
}


#  Estados da amostra (18 estados)

estados_amostra <- c(
  "Amazonas","Pará","Tocantins","Maranhão","Ceará",
  "Rio Grande do Norte","Pernambuco","Bahia","Goiás",
  "Mato Grosso","Mato Grosso do Sul","Minas Gerais",
  "Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
  "Rio Grande do Sul"
)


# Link entre  dados e o shapefile

br_estados_inc <- br_estados %>%
  left_join(dados, by = c("nome" = "Estado"))

br_estados_inc$Inc_colo[!br_estados_inc$nome %in% estados_amostra] <- NA


#Incidências
inc_breaks <- c(-Inf, 0, 50, 100, 150, 200,250, 300, 350,400)
inc_labels <- c(
  "0 casos",
  "1 |-- 50 casos",
  "51 |-- 100 casos",
  "101 |-- 150 casos",
  "151 |-- 200 casos",
  "201 |-- 250 casos", 
  "251 |-- 300 casos", 
  "301 |-- 350 casos",
  "351 |-- 400 casos"
)


map_inc <- 
  tm_shape(br_estados_inc) +
  tm_polygons(
    "Inc_colo",
    palette = inc_palette,
    style = "fixed",
    breaks = inc_breaks,
    labels = inc_labels,
    title = "Incidência/100.000 habitantes",
    border.col = "gray60",
    lwd = 0.6,
    colorNA = cinza_fora,  # Estados fora da amostra
    textNA = ""
  ) +
  tm_title("Incidência acumulada, por 100 mil habitantes, de Cancêr de Colo do Útero entre 2019 e 2024", size = 1.1) +
  tm_layout(
    frame = TRUE, 
    bg.color = "white",
    legend.outside = TRUE,
    legend.title.size = 1,
    legend.text.size = 0.8,
    frame.lwd = 1 
  ) +
  tm_layout(
    frame = TRUE,
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.9,
    bg.color = "white"
  ) +
  tm_compass(
    type = "8star",
    size = 3,
    position = c("left", "bottom")
  ) +
  tm_scalebar(
    text.size = 0.7,
    position = c("right","bottom")
  ) +
  tm_graticules(
    col = "gray90",
    lwd = 0.4,
    labels.show = TRUE,
    zindex=-1
  )

map_inc



#MAMA

map_inc2 <- 
  tm_shape(br_estados_inc) +
  tm_polygons(
    "Inc_mama",
    palette = inc_palette,
    style = "fixed",
    breaks = inc_breaks,
    labels = inc_labels,
    title = "Incidência/100.000 habitantes",
    border.col = "gray60",
    lwd = 0.6,
    colorNA = cinza_fora,  # Estados fora da amostra
    textNA = ""
  )+
  tm_title("Incidência acumulada, por 100 mil habitantes, de Cancêr de Mama entre 2019 e 2024", size = 1.1) +
  tm_layout(
    frame = TRUE, 
    bg.color = "white",
    legend.outside = TRUE,
    legend.title.size = 1,
    legend.text.size = 0.8,
    frame.lwd = 1 
  ) +
  tm_layout(
    frame = TRUE,
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.9,
    bg.color = "white"
  ) +
  tm_compass(
    type = "8star",
    size = 3,
    position = c("left", "bottom")
  ) +
  tm_scalebar(
    text.size = 0.7,
    position = c("right","bottom")
  ) +
  tm_graticules(
    col = "gray90",
    lwd = 0.4,
    labels.show = TRUE,
    zindex=-1
  )

map_inc2


