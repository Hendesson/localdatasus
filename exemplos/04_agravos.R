# =============================================================================
# Exemplo 4 — Agravos de notificação (SINAN) por bairro
# =============================================================================
# agravos_bairro(agravo, municipio, anos, por)
#   agravo: em linguagem comum — "dengue", "tuberculose", "sifilis congenita",
#           "violencia", "zika"... (veja onde_tem_bairro())
#   Disponível para: Rio (capital), São Paulo (capital, por distrito) e
#   todos os municípios de Santa Catarina.

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# 1) Dengue no Rio: 2010 e 2024. O site do Rio tem duas bases
#    (2007-2011 e 2012 em diante); o pacote escolhe a certa para cada ano.
dengue_rio <- agravos_bairro("dengue", "Rio de Janeiro", c(2010, 2024))
aggregate(casos ~ ano, data = dengue_rio, FUN = sum)

# 2) Tuberculose em São Paulo, por distrito
tb_sp <- agravos_bairro("tuberculose", "São Paulo", 2022:2024)
head(tb_sp[order(-tb_sp$taxa_por_10mil), c("ano", "distrito", "casos", "taxa_por_10mil")])

# 3) Dengue em Joinville (SC), por sexo
dengue_jlle <- agravos_bairro("dengue", "Joinville", 2024, por = "sexo")
head(dengue_jlle)

# 4) Sífilis congênita no Rio
sifcong <- agravos_bairro("sifilis congenita", "Rio de Janeiro", 2023)
head(sifcong[order(-sifcong$casos), c("bairro", "casos", "taxa_por_10mil")])

write.csv(dengue_rio, "resultados/dengue_rio.csv", row.names = FALSE)

# 5) Quando o pedido é ambíguo, o pacote avisa o que fazer:
tryCatch(agravos_bairro("sifilis", "Rio de Janeiro", 2023),
         error = function(e) message("Mensagem do pacote: ", conditionMessage(e)))


# --- Conferência -------------------------------------------------------------
erro_ambiguo <- tryCatch({agravos_bairro("sifilis", "Rio de Janeiro", 2023); ""},
                         error = conditionMessage)
stopifnot(
  setequal(unique(dengue_rio$ano), c(2010, 2024)),
  "distrito" %in% names(tb_sp),
  all(dengue_jlle$municipio == "Joinville"), "sexo" %in% names(dengue_jlle),
  sum(sifcong$casos) > 0,
  grepl("amb", erro_ambiguo)
)
