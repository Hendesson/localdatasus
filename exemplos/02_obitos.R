# =============================================================================
# Exemplo 2 — Óbitos por bairro
# =============================================================================
# obitos_bairro(municipio, anos, cid, por)
#   municipio: nome ou código IBGE ("Rio de Janeiro", "Niterói", "355030")
#   anos:      um ano ou vários (2023, 2019:2023)
#   cid:       letra = capítulo inteiro ("I" circulatório, "J" respiratório),
#              código de 3 caracteres ("I21") ou intervalo ("I20-I25")
#   por:       variável para abrir a contagem ("sexo", "faixa etaria", "raca")
#
# Na primeira vez o pacote baixa tabelas do IBGE (CNEFE do RJ, ~340 MB).
# Depois fica tudo em cache e as consultas são rápidas.

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# 1) Doenças do aparelho circulatório no Rio, 2023
circ <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
head(circ)

# Os 10 bairros com maior taxa por 10 mil habitantes
top <- circ[order(-circ$taxa_por_10mil), c("bairro", "obitos", "populacao", "taxa_por_10mil")]
head(top, 10)

# 2) Circulatório + respiratório, por sexo
por_sexo <- obitos_bairro("Rio de Janeiro", 2023, cid = c("I", "J"), por = "sexo")
head(por_sexo)

# 3) Série histórica do respiratório (2019 a 2023)
resp <- obitos_bairro("Rio de Janeiro", 2019:2023, cid = "J")
aggregate(obitos ~ ano, data = resp, FUN = sum)

# 4) Doenças isquêmicas do coração (I20 a I25) em Niterói
#    (fonte: TabNet da SES-RJ, que cobre todos os municípios do estado)
isq <- obitos_bairro("Niterói", 2022, cid = "I20-I25")
head(isq)

# 5) Cidade de São Paulo, por distrito e faixa etária
sp <- obitos_bairro("São Paulo", 2023, por = "faixa etaria")
head(sp)

# 6) Mapa rápido (sem pacotes extras): cada bairro é um ponto,
#    do tamanho da taxa
com_coord <- subset(circ, !is.na(lat) & !is.na(taxa_por_10mil))
plot(com_coord$lon, com_coord$lat, pch = 19, col = "firebrick",
     cex = com_coord$taxa_por_10mil / 20, asp = 1,
     xlab = "longitude", ylab = "latitude",
     main = "Óbitos por doenças circulatórias por 10 mil hab. — Rio, 2023")

# 7) Salvar (abre no QGIS como "texto delimitado", com X = lon e Y = lat)
write.csv(circ, "resultados/obitos_circulatorio_rio_2023.csv", row.names = FALSE)


# --- Conferência -------------------------------------------------------------
total <- sum(obitos_bairro("Rio de Janeiro", 2023, cid = c("I", "J"))$obitos)
stopifnot(
  nrow(circ) > 150,                                       # o Rio tem ~160 bairros
  all(c("bairro", "obitos", "populacao", "taxa_por_10mil", "lat", "lon") %in% names(circ)),
  # a soma por sexo é igual ao total
  sum(por_sexo$obitos) == total,
  # a taxa é óbitos / população x 10 mil
  isTRUE(all.equal(circ$taxa_por_10mil,
                   round(10000 * circ$obitos / circ$populacao, 2))),
  setequal(unique(resp$ano), 2019:2023),
  all(isq$municipio == "Niterói"),
  "distrito" %in% names(sp), "faixa_etaria" %in% names(sp)
)
