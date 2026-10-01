# Funções internas de apoio: UFs, cache e download.

# Necessário para usar a sintaxe do data.table dentro de um pacote.
.datatable.aware <- TRUE

# Siglas das UFs e seus códigos IBGE (2 dígitos).
.ufs <- c(
  RO = "11", AC = "12", AM = "13", RR = "14", PA = "15", AP = "16", TO = "17",
  MA = "21", PI = "22", CE = "23", RN = "24", PB = "25", PE = "26", AL = "27",
  SE = "28", BA = "29", MG = "31", ES = "32", RJ = "33", SP = "35", PR = "41",
  SC = "42", RS = "43", MS = "50", MT = "51", GO = "52", DF = "53"
)

# Valida a sigla da UF e devolve o código IBGE de 2 dígitos.
codigo_uf <- function(uf) {
  if (!is.character(uf) || length(uf) != 1 || !toupper(uf) %in% names(.ufs)) {
    erro("LDS-01", "`uf` deve ser UMA sigla de UF, por exemplo \"RJ\".")
  }
  .ufs[[toupper(uf)]]
}

# Pasta de cache: onde ficam os arquivos baixados do IBGE e as tabelas
# já processadas. Pode ser trocada com options(localdatasus.cache = "...").
pasta_cache <- function() {
  pasta <- getOption("localdatasus.cache", tools::R_user_dir("localdatasus", "cache"))
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  pasta
}

# Baixa `url` para `destino`, se ainda não existir. Baixa primeiro para um
# arquivo ".parcial", para que um download interrompido não fique no cache
# parecendo completo.
baixar <- function(url, destino) {
  if (file.exists(destino)) return(destino)
  antigo <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(antigo))
  parcial <- paste0(destino, ".parcial")
  utils::download.file(url, parcial, mode = "wb", quiet = TRUE)
  file.rename(parcial, destino)
  destino
}

# Troca os marcadores de "sem valor" usados pelo IBGE ("." e "") por NA.
para_na <- function(x) {
  x[x %in% c(".", "")] <- NA
  x
}

# TRUE se um campo de configuração está vazio (NULL, NA ou "").
vazio <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[1]) || identical(x[1], "")
}

# Tipos de ligação de nome que contam como "achou o bairro no IBGE".
.ligacoes_ok <- c("exata", "sem numera\u00e7\u00e3o", "parcial", "aproximada")

# TRUE para as ligações que acharam o bairro (ou distrito) no IBGE.
foi_ligado <- function(ligacao) {
  ligacao %in% .ligacoes_ok
}

#' Apaga os arquivos baixados e as tabelas em cache
#'
#' O pacote guarda em cache os arquivos do IBGE e as tabelas CEP-bairro já
#' processadas, para não refazer o trabalho. Use esta função para liberar
#' espaço ou forçar um novo download.
#'
#' @return Invisivelmente, o caminho da pasta de cache.
#' @export
#' @examples
#' \dontrun{
#' limpar_cache()
#' }
limpar_cache <- function() {
  pasta <- pasta_cache()
  unlink(list.files(pasta, full.names = TRUE), recursive = TRUE)
  message("Cache apagado: ", pasta)
  invisible(pasta)
}
