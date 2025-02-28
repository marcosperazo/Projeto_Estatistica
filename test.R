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


# Evolução da frota de veículos por unidade da federação e tipo de veículo
frota <- readr::read_csv("br_denatran_frota_uf_tipo.csv")
frota
kable(head(frota))

# Frota do estado de São Paulo
frota_sp <- frota %>%
  filter(mes == 12, sigla_uf == "SP") %>%
  select(-mes, -sigla_uf)


frota_sp

# Filtrar os dados para incluir apenas 'automovel'
nova_frota_automovel <- frota_sp %>%
  filter(tipo_veiculo == "automovel")

# Criar o gráfico
ggplot(data = nova_frota_automovel, aes(x = ano, y = quantidade)) +
  geom_line() +
  labs(title = "Quantidade Total de Automóveis por Ano",
       x = "Ano",
       y = "Quantidade Total") +
  theme_minimal()






# Filtrando os automoveis
automoveis <- frota %>%
  filter(tipo_veiculo == "automovel", ano == 2023, mes == 12)

# Classificando pela quantidade
automoveis <- automoveis %>%
  arrange(desc(quantidade))

# Criar uma nova coluna 'margem_percentual'
automoveis <- automoveis %>%
  mutate(margem_percentual = (quantidade / sum(quantidade)) * 100)


# Exibindo a nova tabela automoveis
print(automoveis)





