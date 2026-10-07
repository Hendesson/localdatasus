# Galeria de mapas do README e da documentação (man/figures/mapa-*.png).
# Cada mapa é feito com mapa_bairro(), a partir de uma fonte diferente.
# Rode a partir da pasta do pacote: Rscript data-raw/galeria.R
suppressMessages({
  library(localdatasus)
  library(ggplot2)
})

tema <- theme(
  plot.title = element_text(size = 11, face = "bold"),
  plot.subtitle = element_text(size = 8.5, colour = "grey30"),
  plot.caption = element_blank(), plot.title.position = "plot",
  legend.title = element_text(size = 8), legend.text = element_text(size = 7),
  legend.key.height = unit(0.35, "cm"), legend.key.width = unit(0.3, "cm")
)
mapa <- function(dados, titulo, subtitulo, cores = "Reds") {
  mapa_bairro(dados, cores = cores) +
    labs(title = titulo, subtitle = subtitulo) + tema
}

rio <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
fortaleza <- nascimentos_bairro("Fortaleza", 2023)
recife <- agravos_bairro("dengue", "Recife", 2024)
curitiba <- atendimentos_bairro("Curitiba", 2026, meses = 6:8, cid = "J")
samu <- samu_bairro("Recife", 2024, tipo = "causas externas")
sp <- obitos_bairro("São Paulo", 2023, cid = "I")

# Um arquivo por mapa, todos do mesmo tamanho (a grade é montada no README).
mapas <- list(
  "mapa-rio"       = mapa(rio, "Rio de Janeiro", "\u00d3bitos por doen\u00e7as circulat\u00f3rias, 2023"),
  "mapa-sp"        = mapa(sp, "S\u00e3o Paulo", "\u00d3bitos por doen\u00e7as circulat\u00f3rias, 2023\n(por distrito)", "Oranges"),
  "mapa-fortaleza" = mapa(fortaleza, "Fortaleza", "Nascidos vivos, 2023", "Blues"),
  "mapa-recife"    = mapa(recife, "Recife", "Dengue, casos notificados, 2024", "Greens"),
  "mapa-curitiba"  = mapa(curitiba, "Curitiba", "Atendimentos por doen\u00e7as respirat\u00f3rias\n(e-Sa\u00fade), jun. a ago. 2026", "Purples"),
  "mapa-samu"      = mapa(samu, "Recife", "Chamados do SAMU por causas externas, 2024", "YlOrBr")
)
for (nome in names(mapas)) {
  ggsave(file.path("man/figures", paste0(nome, ".png")), mapas[[nome]],
         width = 5, height = 5, dpi = 96, bg = "white")
}
unlink("man/figures/galeria.png")
message("Gravados: ", paste(names(mapas), collapse = ", "))
