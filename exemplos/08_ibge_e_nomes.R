# =============================================================================
# Exemplo 8 — Tabelas do IBGE e ligação de nomes de bairro
# =============================================================================
#   populacao_bairro()  população por bairro oficial (Censo 2022)
#   distritos_censo()   população e coordenadas dos distritos de um município
#   cep_bairro()        cada CEP, o bairro e as coordenadas (CNEFE 2022)
#   ligar_bairros()     liga nomes escritos de qualquer jeito ao bairro do IBGE

library(localdatasus)

# 1) População por bairro do RJ
pop <- populacao_bairro("RJ")
head(pop[pop$municipio == "Rio de Janeiro", ])

# 2) Distritos da cidade de São Paulo (baixa o CNEFE do município, 177 MB, uma vez)
dist_sp <- distritos_censo("355030")
head(dist_sp)

# 3) CEPs do RJ
ceps <- cep_bairro("RJ")
head(ceps$ceps)
head(ceps$bairros)

# 4) Nomes de bairro "sujos", como aparecem em planilhas e sistemas
nomes <- c("Jd. América", "COMPLEXO DA MARE", "freguesia-ilha", "Tijuka", "Ignorado")
ligados <- ligar_bairros(nomes, codmun = "330455", uf = "RJ")
ligados[, c("nome", "ligacao", "nome_ibge", "lat", "lon")]


# --- Conferência -------------------------------------------------------------
lig <- ligar_bairros(c("Jd. América", "Ignorado"), "330455", "RJ")
stopifnot(
  sum(pop$populacao[pop$municipio == "Rio de Janeiro"]) > 6e6,
  nrow(dist_sp) == 96,
  all(!is.na(dist_sp$lat_distrito)),
  nrow(ceps$ceps) > 50000,
  lig$nome_ibge[1] == "Jardim América",
  lig$ligacao[2] == "ignorado"
)
