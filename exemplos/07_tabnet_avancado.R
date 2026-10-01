# =============================================================================
# Exemplo 7 — TabNets regionais direto (uso avançado)
# =============================================================================
# As funções simples (obitos_bairro etc.) usam por baixo:
#   fontes_tabnet()  cadastro dos TabNets com bairro
#   tabnet_opcoes()  o que dá para cruzar e filtrar em cada um
#   tabnet_bairro()  a consulta, com qualquer filtro do site

library(localdatasus)

# 1) O cadastro de fontes
fontes_tabnet()[, c("fonte", "descricao", "unidade")]

# 2) O que o TabNet de óbitos do Rio oferece
tabnet_opcoes("RIO-SIM", "anos")
tabnet_opcoes("RIO-SIM", "colunas")
tabnet_opcoes("RIO-SIM", "filtros")
tabnet_opcoes("RIO-SIM", "filtros", filtro = "Raça/Cor")

# 3) Uma consulta com filtros do próprio site:
#    óbitos de mulheres pretas e pardas, por bairro e faixa etária
x <- tabnet_bairro("RIO-SIM", 2023, coluna = "Faixa Etária",
                   filtros = list("Sexo" = "Feminino", "Raça/Cor" = c("Preta", "Parda")))
head(x)
attr(x, "ligacao")               # como os nomes foram ligados ao IBGE

# 4) Um TabNet que ainda não está no cadastro: copie uma linha e ajuste.
#    (aqui, só para mostrar, a mesma fonte com outro nome)
minha_fonte <- fontes_tabnet()[fontes_tabnet()$fonte == "RIO-SINASC", ]
minha_fonte$fonte <- "MEU-TABNET"
y <- tabnet_bairro(minha_fonte, 2023)
head(y)


# --- Conferência -------------------------------------------------------------
stopifnot(
  "2023" %in% tabnet_opcoes("RIO-SIM", "anos"),
  "Sexo" %in% tabnet_opcoes("RIO-SIM", "colunas"),
  nrow(x) > 0, "categoria" %in% names(x),
  all(y$fonte == "MEU-TABNET")
)
