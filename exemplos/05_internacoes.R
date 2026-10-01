# =============================================================================
# Exemplo 5 — Internações por bairro (SIH, pelo CEP do paciente)
# =============================================================================
# internacoes_bairro(municipio, anos, cid, uf)
#   Baixa as AIHs do DATASUS (via microdatasus), localiza cada paciente pelo
#   CEP e conta pela DATA DE INTERNAÇÃO.
#
# ATENÇÃO: é o exemplo mais pesado. Baixa ~18 meses de arquivos do DATASUS
# (o ano pedido + meses seguintes, porque internações de dezembro são
# processadas até meses depois). Pode levar de 10 a 30 minutos.

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# 1) Doenças circulatórias e respiratórias, residentes do Rio, 2023
int <- internacoes_bairro("Rio de Janeiro", 2023, cid = c("I", "J"), uf = "RJ")
head(int)

# Quantas internações ganharam bairro (e por que as outras não)
attr(int, "cobertura")

# Bairros com maior taxa de internação por 10 mil
head(int[order(-int$taxa_10mil), c("bairro", "grupo", "internacoes", "taxa_10mil")], 10)

write.csv(int, "resultados/internacoes_IJ_rio_2023.csv", row.names = FALSE)


# --- Conferência -------------------------------------------------------------
stopifnot(
  sum(int$internacoes) > 10000,
  all(c("I", "J") %in% int$grupo),
  all(int$codmun == "330455")
)
