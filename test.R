install.packages("renv")
library(renv)
renv::init()
install.packages('tidyverse')
install.packages('datatable')
install.packages('readxl')
install.packages('rvest')
library(tidyverse)
library(rvest)
library(data.table)
library(readxl)
renv::snapshot()

# Evolução da frota de veículos por unidade da federação e tipo de veículo
frota <- readr::read_csv("br_denatran_frota_uf_tipo.csv")

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




aguas <- readr::read_csv("br_ana_reservatorios_sin.csv")
petroleo <- readr::read_csv2("anuario-2024-dados_abertos-tabela1-2.csv")
typeof(
  petroleo
)
petroleo2 <- tibble::as_tibble(petroleo)

