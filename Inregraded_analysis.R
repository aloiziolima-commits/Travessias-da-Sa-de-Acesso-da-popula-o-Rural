#### Índice Sintético de Prioridade Sanitária ####

# 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Manipulação de dados e gráficos
  GGally     # Matriz de correlação multivariada
)

# 2. LEITURA E CONSOLIDAÇÃO DOS DADOS ANUAIS
# ------------------------------------------------------------------------------
dados_brutos <- read_csv2("dados_municipios_colo.csv")

dados_consolidados <- dados_brutos %>%
  rowwise() %>%
  mutate(
    municipio = Municipio_mun,
    
    # Soma dos Óbitos Totais (2019 a 2023)
    obitos_totais = sum(c_across(starts_with("obitos_")), na.rm = TRUE),
    
    # Média da População Feminina (2019 a 2023)
    pop_feminina_media = mean(c_across(c(
      POP_Feminina_2019_mun, POP_Feminina_2020_mun, 
      POP_Feminina_2021_mun, POP_Feminina_2022_mun, POP_Feminina_2023_mun
    )), na.rm = TRUE),
    
    # Médias Anuais dos Indicadores (2019 a 2024)
    cobertura_media      = mean(c_across(starts_with("cobertura_cito_")), na.rm = TRUE),
    insatisfatorio_medio = mean(c_across(starts_with("prop_tnsatisfatoria_")), na.rm = TRUE),
    positividade_media   = mean(c_across(starts_with("indic_positiv_")), na.rm = TRUE)
  ) %>%
  ungroup()

# 3. CÁLCULO ROBUSTO DO BAYES EMPÍRICO GLOBAL
# ------------------------------------------------------------------------------
pop_5anos <- dados_consolidados$pop_feminina_media * 5
taxa_bruta_i <- dados_consolidados$obitos_totais / pop_5anos
taxa_global  <- sum(dados_consolidados$obitos_totais, na.rm = TRUE) / sum(pop_5anos, na.rm = TRUE)

variancia_bruta <- var(taxa_bruta_i, na.rm = TRUE)
variancia_esperada <- taxa_global / mean(pop_5anos, na.rm = TRUE)
variancia_area <- max(0, variancia_bruta - variancia_esperada)
w_i <- variancia_area / (variancia_area + (taxa_global / pop_5anos))

dados_analise <- dados_consolidados %>%
  mutate(
    taxa_bayes_bruta = (w_i * taxa_bruta_i) + ((1 - w_i) * taxa_global),
    taxa_bayes_anual = (taxa_bayes_bruta / 5) * 100000
  )

# 4. ESCORE-Z, RANKINGS E REGRA DE COR DINÂMICA
# ------------------------------------------------------------------------------
dados_escore <- dados_analise %>%
  mutate(
    z_mortalidade    = as.vector(scale(taxa_bayes_anual)),
    z_cobertura      = as.vector(scale(cobertura_media)) * -1, # Menor cobertura = Maior risco
    z_insatisfatorio = as.vector(scale(insatisfatorio_medio)),
    z_positividade   = as.vector(scale(positividade_media)),
    
    # Escore Global de Prioridade Sanitária
    escore_prioridade = z_mortalidade + z_cobertura + z_insatisfatorio + z_positividade,
    
    # Rankings Individuais de Risco (1º = Pior Situação)
    rk_mort = min_rank(desc(taxa_bayes_anual)),
    rk_cob  = min_rank(cobertura_media), 
    rk_ins  = min_rank(desc(insatisfatorio_medio)),
    rk_pos  = min_rank(desc(positividade_media)),
    
    # Ranking Global
    rk_global = min_rank(desc(escore_prioridade)),
    municipio_rotulo = paste0(rk_global, "º - ", municipio)
  )

# 5. ESTRUTURAÇÃO DO BANCO EM FORMATO LONGO
# ------------------------------------------------------------------------------
df_z <- dados_escore %>%
  dplyr::select(municipio_rotulo, escore_prioridade, z_mortalidade, z_cobertura, z_insatisfatorio, z_positividade) %>%
  pivot_longer(cols = starts_with("z_"), names_to = "indicador_cod", values_to = "escore_z")

df_detalhes <- dados_escore %>%
  dplyr::select(
    municipio_rotulo, taxa_bayes_anual, cobertura_media, insatisfatorio_medio, positividade_media,
    rk_mort, rk_cob, rk_ins, rk_pos
  ) %>%
  pivot_longer(
    cols = c(taxa_bayes_anual, cobertura_media, insatisfatorio_medio, positividade_media),
    names_to = "indicador_bruto", values_to = "valor_bruto"
  ) %>%
  mutate(
    ranking_indiv = case_when(
      indicador_bruto == "taxa_bayes_anual"     ~ rk_mort,
      indicador_bruto == "cobertura_media"      ~ rk_cob,
      indicador_bruto == "insatisfatorio_medio" ~ rk_ins,
      indicador_bruto == "positividade_media"   ~ rk_pos
    )
  )

dados_heatmap_limpo <- df_z %>%
  bind_cols(df_detalhes %>% dplyr::select(valor_bruto, ranking_indiv)) %>%
  mutate(
    indicador = case_when(
      indicador_cod == "z_mortalidade"    ~ "1. Mortalidade (Bayes)",
      indicador_cod == "z_cobertura"      ~ "2. Cobertura Citopatológica",
      indicador_cod == "z_insatisfatorio" ~ "3. Exames Insatisfatórios",
      indicador_cod == "z_positividade"   ~ "4. Índice de Positividade"
    ),
    # Formatando rótulo limpo lado a lado: "Valor | Rankingº"
    texto_rotulo = paste0(round(valor_bruto, 1), " | ", ranking_indiv, "º"),
    
    # COR DINÂMICA: Se a célula for clara (escore_z alto), texto PRETO; senão, BRANCO
    cor_texto = ifelse(escore_z > 1.2, "black", "white")
  )

# 6. GERAR HEATMAP COM CONTRASTE E PROPORÇÕES CORRIGIDAS
# ------------------------------------------------------------------------------
grafico_heatmap_final <- ggplot(
  dados_heatmap_limpo, 
  aes(x = indicador, y = reorder(municipio_rotulo, escore_prioridade), fill = escore_z)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = texto_rotulo, color = cor_texto), 
    size = 2.1, 
    fontface = "bold"
  ) +
  scale_color_identity() + # Aplica diretamente as cores calculadas ('black' / 'white')
  scale_fill_viridis_c(option = "magma", name = "Nível de Risco\n(Escore-Z)") +
  labs(
    title    = "Perfil Epidemiológico e Assistencial Integrado com Rankings de Risco",
    subtitle = "Eixo Y: Ranking Global | Células: Valor Médio | (Ranking de Risco do Indicador)",
    x        = "Indicadores do Estudo",
    y        = "Municípios (Ordenados do Maior para o Menor Risco Geral)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold", size = 9),
    axis.text.y = element_text(size = 7),
    panel.grid  = element_blank()
  )

# Exibe o gráfico
print(grafico_heatmap_final)

# 7. SALVAR A IMAGEM EM ALTA RESOLUÇÃO E PROPORÇÃO CORRETA
# ------------------------------------------------------------------------------
# Salva uma imagem perfeita sem compressão vertical das 38 linhas
ggsave(
  filename = "heatmap_perfil_risco_corrigido.png", 
  plot     = grafico_heatmap_final, 
  width    = 12, 
  height   = 14, 
  dpi      = 300
)