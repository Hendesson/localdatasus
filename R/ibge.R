# Setores censitários e população do Censo 2022 (IBGE).

URL_SETORES_PASTA <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/",
  "Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/"
)
# Nome conhecido em out/2026. O IBGE inclui a data no nome do arquivo; se
# ele mudar, url_setores() procura o nome novo na listagem da pasta.
ARQ_SETORES_PADRAO <- "Agregados_por_setores_basico_BR_20260520.zip"

url_setores <- function() {
  listagem <- tryCatch(readLines(URL_SETORES_PASTA, warn = FALSE), error = function(e) "")
  # useBytes = TRUE: a página do IBGE tem acentos em latin1; comparamos bytes
  # para não gerar avisos de codificação.
  achado <- regmatches(listagem, regexpr("Agregados_por_setores_basico_BR[^\"]*\\.zip", listagem,
                                         useBytes = TRUE))
  arquivo <- if (length(achado) > 0) achado[1] else ARQ_SETORES_PADRAO
  paste0(URL_SETORES_PASTA, arquivo)
}

# Tabela nacional de setores (~470 mil linhas), com bairro e população.
# Baixada uma vez (~15 MB) e guardada no cache.
setores_censo <- function() {
  # "v2": inclui o distrito (CD_DIST, NM_DIST).
  arq_rds <- file.path(pasta_cache(), "setores_censo2022_v2.rds")
  if (file.exists(arq_rds)) return(readRDS(arq_rds))

  message("Baixando setores censit\u00e1rios do Censo 2022 (IBGE, ~15 MB)...")
  url <- url_setores()
  zip <- baixar(url, file.path(pasta_cache(), basename(url)))
  csv <- utils::unzip(zip, exdir = tempdir())
  on.exit(unlink(csv))

  s <- data.table::fread(
    csv, sep = ";", encoding = "Latin-1", colClasses = "character",
    select = c("CD_SETOR", "CD_UF", "CD_MUN", "NM_MUN", "CD_DIST", "NM_DIST",
               "CD_BAIRRO", "NM_BAIRRO", "CD_FCU", "v0001"),
    showProgress = FALSE
  )
  s[, `:=`(
    CD_BAIRRO = para_na(CD_BAIRRO),
    NM_BAIRRO = para_na(NM_BAIRRO),
    CD_FCU    = para_na(CD_FCU),
    codmun    = substr(CD_MUN, 1, 6),
    populacao = as.integer(v0001)
  )]
  s[, v0001 := NULL]
  saveRDS(s, arq_rds)
  s
}

#' População por bairro oficial (Censo 2022)
#'
#' Soma a população dos setores censitários do Censo 2022 por bairro
#' oficial do IBGE. Só municípios com bairros oficialmente delimitados
#' (895 no Brasil) aparecem.
#'
#' @param uf Sigla da UF, por exemplo `"RJ"`.
#' @return Um `data.frame` com `codmun` (6 dígitos), `municipio`,
#'   `cd_bairro_ibge`, `bairro`, `populacao` e `pop_em_favela` (pessoas em
#'   setores de favelas e comunidades urbanas).
#' @export
#' @examples
#' \dontrun{
#' pop <- populacao_bairro("RJ")
#' }
populacao_bairro <- function(uf) {
  cod <- codigo_uf(uf)
  s <- setores_censo()[CD_UF == cod & !is.na(CD_BAIRRO)]
  p <- s[, .(
    populacao     = sum(populacao),
    pop_em_favela = sum(populacao[!is.na(CD_FCU)])
  ), by = .(codmun, municipio = NM_MUN, cd_bairro_ibge = CD_BAIRRO, bairro = NM_BAIRRO)]
  as.data.frame(p)
}

#' População e coordenadas dos distritos de um município (Censo 2022)
#'
#' Distritos são a divisão oficial dos municípios usada, por exemplo, pela
#' Prefeitura de São Paulo nos seus TabNets ("Distrito Administrativo").
#' A população vem dos setores censitários; as coordenadas são a mediana
#' dos endereços do CNEFE 2022 de cada distrito (baixa só o CNEFE do
#' município, uma vez).
#'
#' @param codmun Código do município (6 ou 7 dígitos).
#' @return Um `data.frame` com `codmun`, `cd_distrito_ibge`, `distrito`,
#'   `populacao`, `lat_distrito` e `lon_distrito`.
#' @export
#' @examples
#' \dontrun{
#' distritos_censo("355030")   # cidade de São Paulo
#' }
distritos_censo <- function(codmun) {
  mun <- substr(as.character(codmun), 1, 6)
  if (length(mun) != 1) erro("LDS-02", "Passe UM munic\u00edpio.")
  # (`mun`, e não `codmun`: dentro do data.table, `codmun` seria a coluna.)
  s <- setores_censo()[substr(CD_MUN, 1, 6) == mun]
  if (nrow(s) == 0) erro("LDS-02", "Munic\u00edpio n\u00e3o encontrado: ", mun)
  pop <- s[, .(populacao = sum(populacao, na.rm = TRUE)),
           by = .(codmun, cd_distrito_ibge = CD_DIST, distrito = NM_DIST)]
  coord <- coordenadas_distritos(s$CD_MUN[1])
  as.data.frame(coord[pop, on = "cd_distrito_ibge"])
}

# Mediana das coordenadas dos endereços (CNEFE 2022) de cada distrito de
# um município. Baixa o CNEFE só desse município (ex.: 177 MB para a
# cidade de São Paulo, contra ~1 GB do estado).
coordenadas_distritos <- function(cd_mun7) {
  arq <- file.path(pasta_cache(), paste0("distritos_coord_", cd_mun7, ".rds"))
  if (file.exists(arq)) return(readRDS(arq))
  uf <- names(.ufs)[.ufs == substr(cd_mun7, 1, 2)]
  pasta <- sprintf(URL_CNEFE_MUN, substr(cd_mun7, 1, 2), uf)
  listagem <- tryCatch(readLines(pasta, warn = FALSE), error = function(e) "")
  nome <- regmatches(listagem, regexpr(paste0(cd_mun7, "_[^\"]*\\.zip"), listagem, useBytes = TRUE))
  if (length(nome) == 0) erro("LDS-19", "CNEFE do munic\u00edpio ", cd_mun7, " n\u00e3o encontrado no IBGE.")
  message("Baixando o CNEFE 2022 do munic\u00edpio ", cd_mun7, " (s\u00f3 na primeira vez)...")
  zip <- baixar(paste0(pasta, nome[1]), file.path(pasta_cache(), nome[1]))
  csv <- utils::unzip(zip, exdir = tempdir())
  on.exit(unlink(c(csv, zip)))
  end <- data.table::fread(csv, sep = ";", select = c("COD_SETOR", "LATITUDE", "LONGITUDE"),
                           colClasses = list(character = "COD_SETOR"), showProgress = FALSE)
  # Distrito = 9 primeiros dígitos do setor (UF + município + distrito).
  dist <- end[, .(lat_distrito = stats::median(LATITUDE, na.rm = TRUE),
                  lon_distrito = stats::median(LONGITUDE, na.rm = TRUE)),
              by = .(cd_distrito_ibge = substr(gsub("\\D", "", COD_SETOR), 1, 9))]
  saveRDS(dist, arq)
  dist
}
