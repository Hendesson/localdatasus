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
  on.exit(options(antigo), add = TRUE)
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
#' @examplesIf interactive()
#' limpar_cache()
limpar_cache <- function() {
  pasta <- pasta_cache()
  unlink(list.files(pasta, full.names = TRUE), recursive = TRUE)
  message("Cache apagado: ", pasta)
  invisible(pasta)
}

# Tira acentos do mesmo jeito em qualquer sistema. (iconv com
# "ASCII//TRANSLIT" depende do sistema: no macOS, "\u00e1" vira "'a".)
sem_acento <- function(x) {
  x <- chartr(
    "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1\u00fd\u00c1\u00c0\u00c2\u00c3\u00c4\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf\u00d3\u00d2\u00d4\u00d5\u00d6\u00da\u00d9\u00db\u00dc\u00c7\u00d1\u00dd",
    "aaaaaeeeeiiiiooooouuuucnyAAAAAEEEEIIIIOOOOOUUUUCNY", enc2utf8(as.character(x)))
  # Outros sinais (\u00ba, \u00aa, travess\u00e3o...) viram espa\u00e7o.
  gsub("[^ -~]", " ", x)
}
