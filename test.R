install.packages("renv")
library(renv)
renv::init()
install.packages('tidyverse')
install.packages('datatable')
install.packages('readxl')
install.packages('rvest')
install.packages('tinytex')
install.packages("ggplot2")
install.packages('scales')
install.packages('dplyr')
tinytex::install_tinytex(force = TRUE)
tinytex::install_tinytex()


library(tinytex)
library(tidyverse)
library(rvest)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(dplyr)
renv::snapshot()
renv::restore()

update.packages()


# Evolução da frota de veículos por unidade da federação e tipo de veículo
frota <- readr::read_csv("br_denatran_frota_uf_tipo.csv")
frota
kable(head(frota))

# Frota do estado de São Paulo
frota_sp <- frota %>%
  filter(mes == 12, sigla_uf == "SP") %>%
  select(-mes, -sigla_uf)




frota_sp

frota_sp_ajustada <- frota_sp %>%
  mutate(
    automoveis = ifelse(tipo_veiculo == "automovel", quantidade, 0),
    motocicletas = ifelse(tipo_veiculo %in% c("motocicleta", "motoneta"), quantidade, 0),
    caminhoes = ifelse(tipo_veiculo %in% c("caminhao", "caminhonete", "caminhoneta"), quantidade, 0)
  ) %>%
  select(ano, automoveis, motocicletas, caminhoes) %>%
  group_by(ano) %>%
  summarise(
    automoveis = sum(automoveis),
    motocicletas = sum(motocicletas),
    caminhoes = sum(caminhoes)
  )

print(frota_sp_ajustada)




ggplot(frota_sp_ajustada, aes(x = ano, y = automoveis)) +
  geom_line() +
  geom_point() +
  labs(title = "Frota de Automóveis por Ano",
       x = "Ano",
       y = "Frota de Automóveis")




frota_sp_long <- frota_sp_ajustada %>%
  pivot_longer(cols = c(automoveis, motocicletas, caminhoes), 
               names_to = "tipo_veiculo", 
               values_to = "quantidade")


ggplot(frota_sp_long, aes(x = ano, y = quantidade, color = tipo_veiculo)) +
  geom_line() +
  geom_point() +
  labs(title = "Frota de Veículos por tipo",
       x = "Ano",
       y = "Frota de Veículos",
       color = "Tipo de Veículo") +
  scale_y_continuous(labels = number_format(big.mark = ".")) +
  theme_minimal()


frota_sp_ajustada %>% dplyr::select(automoveis) %>% summarytools::descr(., style = 'rmarkdown') %>% kable()


# Carregar o pacote dplyr
library(dplyr)

# Calcular a média, desvio padrão e quantis (25% e 75%)
estatisticas <- frota_sp_ajustada %>%
  summarise(
    media = mean(motocicletas, na.rm = TRUE),
    desvio_padrao = sd(motocicletas, na.rm = TRUE),
    quantil_25 = quantile(motocicletas, probs = 0.25, na.rm = TRUE),
    quantil_75 = quantile(motocicletas, probs = 0.75, na.rm = TRUE)
  )

# Visualizar as estatísticas
print(estatisticas) %>% kable()

kable(estatisticas)



frota_motocicletas_br <- frota %>%
  filter(mes == 12, tipo_veiculo == 'motocicleta') %>%
  select(-mes, -tipo_veiculo)
frota_motocicletas_br
frota_motocicletas_br %>% dplyr::select(quantidade) %>% ggplot(aes(x = quantidade)) + geom_histogram(aes(y = after_stat(density)), bins = 7)


frota_motocicletas_br <- frota_motocicletas_br %>%
  rename(motocicletas = quantidade)
frota_motocicletas_br


ggplot(frota_motocicletas_br, aes(x = ano, y = motocicletas)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Frota de Motocicletas por Ano",
    x = "Ano",
    y = "Frota de Motocicletas",
    color = "Legenda"
  ) +
  theme_minimal()
