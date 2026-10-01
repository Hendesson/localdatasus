# =============================================================================
# Roda todos os exemplos e mostra um relatório: OK ou FALHOU, e quanto
# tempo cada um levou.
#
# Como usar (no RStudio):
#   1. Abra este arquivo e ajuste as opções abaixo, se quiser.
#   2. Session > Set Working Directory > To Source File Location
#   3. Clique em "Source".
#
# Na primeira vez, o pacote baixa as tabelas do IBGE (CNEFE do RJ ~340 MB,
# de SC ~110 MB e da cidade de SP ~180 MB). Depois fica tudo em cache.
# =============================================================================

# --- Opções -----------------------------------------------------------------
# O exemplo 5 (internações) baixa ~18 meses de arquivos do DATASUS e pode
# levar de 10 a 30 minutos. Mude para TRUE para incluí-lo.
RODAR_PESADOS <- FALSE

# (Opcional) Pasta onde o pacote guarda os downloads:
# options(localdatasus.cache = "~/localdatasus_cache")
# -----------------------------------------------------------------------------

library(localdatasus)

exemplos <- sort(list.files(pattern = "^[0-9]{2}_.*\\.R$"))
pesados  <- "05_internacoes.R"
if (!RODAR_PESADOS) exemplos <- setdiff(exemplos, pesados)

relatorio <- data.frame(exemplo = exemplos, situacao = NA_character_,
                        minutos = NA_real_, mensagem = "", stringsAsFactors = FALSE)

for (i in seq_along(exemplos)) {
  cat("\n=====================================================================\n")
  cat("Rodando", exemplos[i], "\n")
  cat("=====================================================================\n")
  inicio <- Sys.time()
  resultado <- tryCatch({
    # Cada exemplo roda num ambiente próprio, para não misturar objetos.
    source(exemplos[i], local = new.env(), echo = FALSE)
    "OK"
  }, error = function(e) {
    relatorio$mensagem[i] <<- conditionMessage(e)
    "FALHOU"
  })
  relatorio$situacao[i] <- resultado
  relatorio$minutos[i]  <- round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1)
  cat("->", resultado, "\n")
}

cat("\n\n=========================== RELATÓRIO ===============================\n")
print(relatorio[, c("exemplo", "situacao", "minutos")], row.names = FALSE)
if (any(relatorio$situacao == "FALHOU")) {
  cat("\nErros:\n")
  for (i in which(relatorio$situacao == "FALHOU")) {
    cat(" -", relatorio$exemplo[i], ":", relatorio$mensagem[i], "\n")
  }
}
if (!RODAR_PESADOS) cat("\n(", pesados, "não foi rodado: mude RODAR_PESADOS para TRUE.)\n")
cat("\nResultados (CSV) salvos na pasta 'resultados/'.\n")
