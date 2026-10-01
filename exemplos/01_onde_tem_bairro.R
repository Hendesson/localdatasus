# =============================================================================
# Exemplo 1 — Que dados existem por bairro?
# =============================================================================
# Antes de tudo, veja o que existe, para que lugar e com qual função buscar.

library(localdatasus)

# Tabela "de gente": dado, local, unidade (bairro ou distrito) e a função.
catalogo <- onde_tem_bairro()
print(catalogo[, c("dado", "local", "unidade", "funcao")])

# Só o que existe para o Rio de Janeiro:
subset(catalogo, grepl("Rio de Janeiro", local))[, c("dado", "local", "funcao")]

# Só os agravos (SINAN) de Santa Catarina:
subset(catalogo, funcao == "agravos_bairro()" & grepl("Santa Catarina", local))$dado


# --- Conferência -------------------------------------------------------------
stopifnot(
  nrow(catalogo) > 30,
  all(c("obitos_bairro()", "nascimentos_bairro()", "agravos_bairro()",
        "internacoes_bairro()") %in% catalogo$funcao)
)
