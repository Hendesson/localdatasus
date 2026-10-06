# Chamados do SAMU 192 do Recife (serviço metropolitano), publicados no
# portal de dados abertos da Prefeitura, com o município e o bairro do
# LOCAL DA OCORRÊNCIA. Usamos 2016 em diante: antes disso os arquivos não
# trazem o município, e nomes como "CENTRO" não dizem de que cidade são.

URL_RECIFE_BUSCA <- "https://dados.recife.pe.gov.br/api/3/action/package_search?q=samu&rows=100"

# Endereço do CSV de cada ano.
recursos_samu <- function() {
  r <- tryCatch(jsonlite::fromJSON(URL_RECIFE_BUSCA, simplifyVector = FALSE)$result$results,
                error = function(e) NULL)
  if (is.null(r)) erro("LDS-14", "O portal de dados abertos do Recife n\u00e3o respondeu.")
  linhas <- lapply(r, function(p) {
    ano <- suppressWarnings(as.integer(sub(".*([0-9]{4})$", "\\1", p$name)))
    csv <- Filter(function(x) toupper(x$format) == "CSV", p$resources)
    if (!grepl("samu", p$name) || is.na(ano) || length(csv) == 0) return(NULL)
    data.frame(ano = ano, url = sub(":443/", "/", csv[[1]]$url, fixed = TRUE), stringsAsFactors = FALSE)
  })
  do.call(rbind, linhas)
}

# Chamados dos anos pedidos (arquivos no cache; os dois anos mais recentes
# são baixados de novo, pois ainda mudam).
baixar_samu <- function(anos) {
  if (any(anos < 2016)) {
    avisar("LDS-11", "SAMU do Recife: s\u00f3 h\u00e1 munic\u00edpio e bairro a partir de 2016; anos anteriores ignorados.")
    anos <- anos[anos >= 2016]
  }
  rec <- recursos_samu()
  rec <- rec[rec$ano %in% anos, ]
  if (nrow(rec) == 0) erro("LDS-11", "N\u00e3o h\u00e1 arquivo do SAMU do Recife para esses anos.")
  pasta <- file.path(pasta_cache(), "recife_samu")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  partes <- lapply(seq_len(nrow(rec)), function(k) {
    detalhar("SAMU do Recife: ", rec$ano[k], "...")
    destino <- file.path(pasta, paste0(rec$ano[k], ".csv"))
    if (rec$ano[k] >= ano_atual - 1) unlink(destino)
    b <- data.table::fread(baixar(rec$url[k], destino), colClasses = "character",
                           encoding = "UTF-8", fill = TRUE, showProgress = FALSE)
    data.table::setnames(b, tolower(trimws(names(b))))
    b[, ano := rec$ano[k]]
    b
  })
  d <- data.table::rbindlist(partes, fill = TRUE)
  for (v in intersect(c("municipio", "bairro", "tipo", "subtipo", "sexo", "origem_chamado",
                        "motivo_finalizacao", "motivo_desfecho"), names(d))) {
    data.table::set(d, j = v, value = trimws(d[[v]]))
  }
  d
}

#' Chamados do SAMU por bairro (Recife e região metropolitana)
#'
#' Conta as solicitações ao SAMU 192 do Recife por bairro do LOCAL DA
#' OCORRÊNCIA (não da residência), com população, taxa por 10 mil
#' habitantes e coordenadas. O SAMU do Recife é metropolitano: também há
#' chamados de Jaboatão dos Guararapes, Olinda, Paulista e outros
#' municípios. Fonte: portal de dados abertos da Prefeitura do Recife,
#' 2016 em diante. As solicitações duplicadas são descartadas.
#'
#' @param municipio Nome do município da ocorrência (padrão: Recife).
#' @param anos Anos do chamado (2016 em diante).
#' @param tipo Parte do tipo de ocorrência, por exemplo `"causas externas"`,
#'   `"psiquiatrica"`, `"cardiologica"`, `"respiratoria"`. `NULL` = todos.
#' @param por Variável para abrir as contagens: `"tipo"`, `"subtipo"`,
#'   `"sexo"`, `"origem do chamado"`, `"motivo de finalizacao"` ou
#'   `"desfecho"`.
#' @return Como em [obitos_bairro()], com `chamados` no lugar de `obitos`.
#' @export
#' @examples
#' \dontrun{
#' samu_bairro("Recife", 2024)
#' samu_bairro("Recife", 2024, tipo = "causas externas", por = "subtipo")
#' samu_bairro("Olinda", 2023:2024)
#' }
samu_bairro <- function(municipio = "Recife", anos, tipo = NULL, por = NULL) {
  lugar <- achar_lugar(municipio, "PE")
  d <- baixar_samu(anos)
  d[, chave_mun := chave_bairro(municipio)]
  d <- d[chave_mun == chave_bairro(lugar$nome)]
  if ("motivo_finalizacao" %in% names(d)) {
    d <- d[!grepl("DUPLICAD", toupper(motivo_finalizacao))]
  }
  if (!is.null(tipo)) {
    alvo <- normalizar_rotulo(tipo)       # fora do [ ]: "tipo" também é coluna
    d <- d[grepl(alvo, normalizar_rotulo(d$tipo), fixed = TRUE)]
  }
  if (nrow(d) == 0) erro("LDS-12", "Nenhum chamado do SAMU encontrado para ", lugar$nome, ".")
  col_por <- NULL
  if (!is.null(por)) {
    op <- c("tipo", "subtipo", "sexo", "origem_chamado", "motivo_finalizacao", "motivo_desfecho")
    rotulos <- c("tipo", "subtipo", "sexo", "origem do chamado", "motivo de finalizacao", "desfecho")
    r <- normalizar_rotulo(rotulos)
    a <- normalizar_rotulo(por)
    pos <- which(r == a)
    if (length(pos) == 0) pos <- which(startsWith(r, a) | grepl(a, r, fixed = TRUE))
    if (length(pos) == 0) {
      erro("LDS-10", "N\u00e3o achei \"", por, "\". Op\u00e7\u00f5es: ", paste(rotulos, collapse = ", "), ".")
    }
    col_por <- op[pos[1]]
  }
  d[, categoria := if (is.null(col_por)) NA_character_ else get(col_por)]
  agg <- d[, .(n = .N), by = .(ano, bairro_tabnet = bairro, categoria)]
  agg[bairro_tabnet == "", bairro_tabnet := "(sem bairro)"]
  lig <- data.table::as.data.table(ligar_bairros(agg$bairro_tabnet, lugar$codmun, "PE"))
  agg <- cbind(agg, lig[, .(ligacao, id_unidade, nome_ibge, cd_ibge, lat, lon, populacao)])
  agg[, `:=`(codmun = lugar$codmun, municipio = lugar$nome, fonte = "SAMU 192 Recife",
             unidade = "bairro")]
  arrumar_tabela(agg, if (is.null(por)) NULL else nome_coluna(por), "chamados")
}
