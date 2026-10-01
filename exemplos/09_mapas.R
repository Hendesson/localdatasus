# =============================================================================
# Exemplo 9 — Mapas
# =============================================================================
# mapa_bairro(tabela)      mapa estático (ggplot2), pronto para artigo
# mapa_interativo(tabela)  mapa interativo (leaflet), para explorar
#
# Funciona com a tabela de qualquer função do pacote. Os contornos dos
# bairros são os oficiais do IBGE (Censo 2022), baixados uma vez.
# Precisa (uma vez): install.packages(c("sf", "ggplot2", "leaflet"))

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# 1) Óbitos por doenças circulatórias no Rio: o mapa pinta a taxa por 10 mil
circ <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
mapa_bairro(circ, titulo = "Óbitos por doenças circulatórias, Rio de Janeiro, 2023")

# 2) Pintar pela contagem, com outra paleta
mapa_bairro(circ, valor = "obitos", cores = "YlOrRd")

# 3) Um painel por sexo (vem de `por = "sexo"`)
por_sexo <- obitos_bairro("Rio de Janeiro", 2023, cid = "I", por = "sexo")
mapa_bairro(subset(por_sexo, sexo != "Ignorado"))

# 4) Distritos de São Paulo e bairros de Joinville
mapa_bairro(obitos_bairro("São Paulo", 2023), cores = "Blues")
mapa_bairro(agravos_bairro("dengue", "Joinville", 2024))

# 5) É um ggplot comum: dá para mudar e salvar
mapa <- mapa_bairro(circ) + ggplot2::labs(subtitle = "Fonte: SMS-Rio")
ggplot2::ggsave("resultados/mapa_circulatorio_rio.png", mapa, width = 8, height = 5, dpi = 300)

# 6) Mapa interativo: passe o mouse e clique nos bairros
interativo <- mapa_interativo(agravos_bairro("dengue", "Rio de Janeiro", 2024))
interativo
htmlwidgets::saveWidget(interativo, "resultados/dengue_rio_2024.html")


# --- Conferência -------------------------------------------------------------
stopifnot(
  inherits(mapa, "ggplot"),
  inherits(interativo, "leaflet"),
  file.exists("resultados/mapa_circulatorio_rio.png")
)
