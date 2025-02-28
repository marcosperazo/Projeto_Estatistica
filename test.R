install.packages("renv")
library(renv)
renv::init()
install.packages('tidyverse')
install.packages('datatable')
install.packages('readxl')
install.packages('rvest')
install.packages('tinytex')
install.packages("ggplot2")
tinytex::install_tinytex(force = TRUE)
tinytex::install_tinytex()


library(tinytex)
library(tidyverse)
library(rvest)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)

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




