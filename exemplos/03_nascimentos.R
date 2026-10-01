# =============================================================================
# Exemplo 3 — Nascimentos por bairro
# =============================================================================
# nascimentos_bairro(municipio, anos, por)
#   Rio (capital) e São Paulo: TabNet das prefeituras.
#   Demais municípios do RJ: microdados da SES-RJ, localizados pelo CEP da
#   mãe e, quando o CEP não resolve, pelo nome do bairro na declaração.

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# 1) Rio de Janeiro, 2023, por tipo de parto
nasc_rio <- nascimentos_bairro("Rio de Janeiro", 2023, por = "tipo de parto")
head(nasc_rio)

# Proporção de cesáreas por bairro
tab <- xtabs(nascimentos ~ bairro + tipo_de_parto, data = nasc_rio)
cesarea <- round(100 * tab[, "Cesário"] / rowSums(tab), 1)
head(sort(cesarea, decreasing = TRUE), 10)

# 2) Niterói, 2023 (microdados da SES-RJ)
nasc_nit <- nascimentos_bairro("Niterói", 2023)
head(nasc_nit)

write.csv(nasc_rio, "resultados/nascimentos_rio_2023.csv", row.names = FALSE)


# --- Conferência -------------------------------------------------------------
stopifnot(
  sum(nasc_rio$nascimentos) > 40000,              # o Rio tem ~60 mil nascimentos/ano
  "tipo_de_parto" %in% names(nasc_rio),
  all(nasc_nit$municipio == "Niterói"),
  # pelo menos 80% dos nascimentos de Niterói ganharam bairro
  sum(nasc_nit$nascimentos[nasc_nit$ligacao == "exata"]) / sum(nasc_nit$nascimentos) > 0.8
)
