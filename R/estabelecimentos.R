# Estabelecimentos de saúde por bairro: cadastro nacional do CNES publicado
# no Portal de Dados Abertos do SUS (um arquivo para o Brasil, atualizado
# diariamente), com CEP, nome do bairro e coordenadas de cada
# estabelecimento. Mede a OFERTA de serviços no bairro (onde está o
# estabelecimento), e não a residência dos pacientes.

URL_CNES_ABERTO <- "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/CNES/cnes_estabelecimentos_csv.zip"
URL_API_TIPOS   <- "https://apidadosabertos.saude.gov.br/cnes/tipounidades"

# Tipos de unidade (código -> descrição), pela API de Dados Abertos.
tipos_unidade_cnes <- function() {
  arq <- file.path(pasta_cache(), "cnes_tipos_unidade.rds")
  if (file.exists(arq)) return(readRDS(arq))
  r <- tryCatch(jsonlite::fromJSON(URL_API_TIPOS)$tipos_unidade, error = function(e) NULL)
  if (is.null(r)) return(NULL)
  t <- stats::setNames(as.character(r$descricao_tipo_unidade), as.character(r$codigo_tipo_unidade))
  saveRDS(t, arq)
  t
}

# Cadastro nacional (guardado no cache; baixado de novo após 30 dias).
cnes_aberto <- function() {
  zip <- file.path(pasta_cache(), "cnes_estabelecimentos_csv.zip")
  if (file.exists(zip) && difftime(Sys.time(), file.mtime(zip), units = "days") > 30) unlink(zip)
  if (!file.exists(zip)) message("Baixando o cadastro de estabelecimentos do CNES (56 MB)...")
  baixar(URL_CNES_ABERTO, zip)
  csv <- utils::unzip(zip, exdir = tempdir())
  on.exit(unlink(csv), add = TRUE)
  cols <- c("CO_CNES", "CO_IBGE", "NO_FANTASIA", "TP_UNIDADE", "CO_CEP", "NO_BAIRRO",
            "NU_LATITUDE", "NU_LONGITUDE", "CO_MOTIVO_DESAB", "CO_AMBULATORIAL_SUS",
            "ST_ATEND_HOSPITALAR")
  d <- data.table::fread(csv, sep = ";", select = cols, colClasses = "character",
                         encoding = "Latin-1", showProgress = FALSE)
  for (v in names(d)) data.table::set(d, j = v, value = trimws(d[[v]]))
  attr(d, "data_cadastro") <- as.Date(file.mtime(zip))
  d
}

#' Estabelecimentos de saúde por bairro
#'
#' Conta os estabelecimentos de saúde ativos por bairro, em qualquer
#' município do país, a partir do cadastro do CNES publicado no Portal de
#' Dados Abertos do SUS. O bairro vem do CEP do estabelecimento (pelo
#' CNEFE 2022) e, quando o CEP não resolve, do nome do bairro informado no
#' cadastro. Mede a oferta de serviços no bairro, não onde moram os
#' pacientes.
#'
#' @param municipio Nome ou código IBGE do município.
#' @param uf Sigla da UF, para pegar o estado inteiro ou desfazer nomes
#'   repetidos.
#' @param tipo Parte do nome do tipo de unidade, por exemplo `"hospital"`,
#'   `"unidade basica"`, `"pronto atendimento"`, `"psicossocial"` (CAPS).
#'   `NULL` = todos os tipos.
#' @param so_sus `TRUE` para contar só os estabelecimentos com atendimento
#'   ambulatorial pelo SUS.
#' @param por `"tipo"` para abrir a contagem por tipo de unidade.
#' @return Como em [obitos_bairro()], com `estabelecimentos` no lugar de
#'   `obitos` e `ano` igual ao ano do cadastro baixado. A taxa é por 10 mil
#'   habitantes. `attr(x, "registros")` traz cada estabelecimento, com
#'   nome, tipo, CEP, coordenadas e bairro.
#' @export
#' @examplesIf interactive()
#' estabelecimentos_bairro("Recife", tipo = "unidade basica")
#' estabelecimentos_bairro("Belo Horizonte", por = "tipo", so_sus = TRUE)
estabelecimentos_bairro <- function(municipio = NULL, uf = NULL, tipo = NULL, so_sus = FALSE,
                                    por = NULL) {
  lugar <- achar_lugar(municipio, uf)
  d <- cnes_aberto()
  data_cad <- attr(d, "data_cadastro")
  d <- d[is.na(CO_MOTIVO_DESAB) | CO_MOTIVO_DESAB == ""]           # só ativos
  if (!is.na(lugar$codmun)) d <- d[CO_IBGE == lugar$codmun] else d <- d[substr(CO_IBGE, 1, 2) == codigo_uf(lugar$uf)]
  if (so_sus) d <- d[CO_AMBULATORIAL_SUS == "SIM"]
  tipos <- tipos_unidade_cnes()
  d[, tipo := if (is.null(tipos)) TP_UNIDADE else
    data.table::fifelse(is.na(tipos[TP_UNIDADE]), TP_UNIDADE, unname(tipos[TP_UNIDADE]))]
  if (!is.null(tipo)) {
    pega <- grepl(normalizar_rotulo(tipo), normalizar_rotulo(d$tipo), fixed = TRUE)
    if (!any(pega)) {
      erro("LDS-10", "Nenhum tipo de unidade com \"", tipo, "\". Tipos: ",
           paste(sort(unique(tolower(d$tipo))), collapse = "; "), ".")
    }
    d <- d[pega]
  }
  if (nrow(d) == 0) erro("LDS-12", "Nenhum estabelecimento encontrado.")
  if (!is.null(por) && !startsWith(normalizar_rotulo(por), "tipo")) {
    erro("LDS-10", "Para estabelecimentos, use por = \"tipo\".")
  }

  uf_base <- names(.ufs)[.ufs == substr(d$CO_IBGE[1], 1, 2)]
  res <- adicionar_bairro(as.data.frame(d), uf_base, col_cep = "CO_CEP", col_mun = "CO_IBGE",
                          col_id = "CO_CNES")
  res$origem_bairro <- ifelse(is.na(res$id_bairro), NA_character_, "CEP")
  res <- completar_pelo_nome(res, "NO_BAIRRO", uf_base)
  res$ano_ref <- as.integer(format(data_cad, "%Y"))
  saida <- agregar_registros(res, por, "estabelecimentos", "CNES (dados abertos)")
  attr(saida, "registros") <- res[, c("CO_CNES", "NO_FANTASIA", "tipo", "CO_CEP", "NO_BAIRRO",
                                      "NU_LATITUDE", "NU_LONGITUDE", "bairro", "cd_bairro_ibge",
                                      "situacao_cep", "origem_bairro")]
  attr(saida, "data_cadastro") <- data_cad
  saida
}
