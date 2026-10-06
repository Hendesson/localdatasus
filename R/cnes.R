# Coordenadas dos estabelecimentos de saúde (CNES), pela API de Dados
# Abertos do Ministério da Saúde.

URL_API_CNES <- "https://apidadosabertos.saude.gov.br/cnes/estabelecimentos/"

#' Coordenadas de estabelecimentos de saúde (CNES)
#'
#' Consulta a API de Dados Abertos do Ministério da Saúde e devolve nome,
#' tipo, endereço e coordenadas de cada estabelecimento. Útil para ligar os
#' registros (coluna `CNES` no SIH, `AP_CODUNI` nas APACs) ao local de
#' atendimento — por exemplo, para medir a distância entre a residência
#' (`lat_cep`, `lon_cep`) e o hospital.
#'
#' Os resultados ficam em cache; só CNES novos são consultados.
#'
#' @param cnes Vetor de códigos CNES (com ou sem zeros à esquerda).
#' @return Um `data.frame` com `cnes` (7 dígitos), `nome`, `tipo_unidade`,
#'   `codmun`, `cep`, `endereco`, `bairro`, `lat` e `lon`. Coordenadas que
#'   caem fora do Brasil viram `NA`. Atenção: o cadastro tem erros (ex.:
#'   hospitais com coordenadas em outro município); confira `codmun`.
#' @export
#' @examplesIf interactive()
#' cnes_coordenadas(c("2269880", "2270234"))
cnes_coordenadas <- function(cnes) {
  cnes <- unique(sprintf("%07d", as.integer(gsub("\\D", "", cnes))))
  arq <- file.path(pasta_cache(), "cnes_coordenadas.rds")
  cache <- if (file.exists(arq)) readRDS(arq) else NULL
  faltam <- setdiff(cnes, cache$cnes)

  if (length(faltam) > 0) {
    detalhar("Consultando ", length(faltam), " estabelecimento(s) na API do CNES...")
    novos <- lapply(faltam, function(cod) {
      r <- tryCatch(jsonlite::fromJSON(paste0(URL_API_CNES, cod)), error = function(e) NULL)
      Sys.sleep(0.2)   # gentileza com a API
      campo <- function(x) if (is.null(r[[x]])) NA else r[[x]]
      data.frame(
        cnes         = cod,
        nome         = campo("nome_fantasia"),
        tipo_unidade = campo("codigo_tipo_unidade"),
        codmun       = as.character(campo("codigo_municipio")),
        cep          = as.character(campo("codigo_cep_estabelecimento")),
        endereco     = paste(campo("endereco_estabelecimento"), campo("numero_estabelecimento")),
        bairro       = campo("bairro_estabelecimento"),
        lat          = as.numeric(campo("latitude_estabelecimento_decimo_grau")),
        lon          = as.numeric(campo("longitude_estabelecimento_decimo_grau")),
        stringsAsFactors = FALSE
      )
    })
    cache <- rbind(cache, do.call(rbind, novos))
    saveRDS(cache, arq)
  }

  res <- cache[cache$cnes %in% cnes, ]
  # Coordenadas fora do território brasileiro = erro de cadastro.
  fora <- !is.na(res$lat) & (res$lat < -34 | res$lat > 6 | res$lon < -74 | res$lon > -34)
  res$lat[fora] <- NA
  res$lon[fora] <- NA
  rownames(res) <- NULL
  res
}
