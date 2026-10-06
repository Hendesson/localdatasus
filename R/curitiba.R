# Atendimentos médicos da rede municipal de Curitiba (Sistema e-Saúde),
# publicados no portal de dados abertos da Prefeitura, com o bairro de
# residência do paciente e a CID do atendimento.
#
# Cada arquivo publicado (~480 MB, um por mês) traz os três meses
# anteriores à data de publicação: o de 06/09/2026 vai de junho a agosto.
# Para não baixar tudo de novo, cada mês é guardado no cache, só com as
# colunas usadas.

URL_CURITIBA <- "https://dadosabertos.c3sl.ufpr.br/curitiba/SESPAMedicoUnidadesMunicipaisDeSaude/"
COD_CURITIBA <- "410690"

# Arquivos publicados: nome, data de publicação e mês de publicação.
arquivos_curitiba <- function() {
  pagina <- tryCatch(paste(readLines(URL_CURITIBA, warn = FALSE), collapse = " "),
                     error = function(e) NULL)
  if (is.null(pagina)) erro("LDS-14", "O portal de dados abertos de Curitiba n\u00e3o respondeu.")
  arq <- unique(regmatches(pagina, gregexpr(
    "[0-9]{4}-[0-9]{2}-[0-9]{2}_Sistema_E-Saude_Medicos_-_Base_de_Dados\\.csv", pagina))[[1]])
  data <- as.Date(substr(arq, 1, 10))
  a <- data.frame(arquivo = arq, data = data, mes = format(data, "%Y-%m"), stringsAsFactors = FALSE)
  a <- a[order(a$data, decreasing = TRUE), ]
  a[!duplicated(a$mes), ]                      # o mais recente de cada mês
}

# Soma k meses a "aaaa-mm".
somar_mes <- function(mes, k) {
  d <- seq(as.Date(paste0(mes, "-01")), by = paste(k, "months"), length.out = 2)[2]
  format(d, "%Y-%m")
}

# Lê um arquivo publicado e guarda no cache cada mês completo que ele cobre.
processar_arquivo_curitiba <- function(arquivo, mes_publicacao, pasta) {
  meses <- vapply(-3:-1, function(k) somar_mes(mes_publicacao, k), character(1))
  detalhar("Curitiba: baixando ", arquivo, " (cerca de 480 MB)...")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  baixar(paste0(URL_CURITIBA, arquivo), tmp)
  cab <- names(data.table::fread(tmp, sep = ";", nrows = 0, encoding = "Latin-1"))
  pega <- function(padrao) cab[grepl(padrao, cab)][1]
  cols <- c(data = pega("^Data do Atendimento"), nasc = pega("^Data de Nascimento"),
            sexo = pega("^Sexo"), tipo_unidade = pega("^Tipo de Unidade"),
            unidade = pega("^Descri.+o da Unidade"), cid = pega("^C.digo do CID"),
            profissional = pega("^Descri.+o do CBO"), internamento = pega("^Desencadeou Internamento"),
            municipio_res = pega("^Munic"), bairro = pega("^Bairro"))
  d <- data.table::fread(tmp, sep = ";", select = unname(cols), encoding = "Latin-1",
                         colClasses = "character", showProgress = FALSE)
  data.table::setnames(d, unname(cols), names(cols))
  for (v in names(d)) data.table::set(d, j = v, value = trimws(d[[v]]))
  d[, data := as.Date(substr(data, 1, 10), "%d/%m/%Y")]
  d[, mes := format(data, "%Y-%m")]
  dn <- as.Date(substr(d$nasc, 1, 10), "%d/%m/%Y")
  d[, idade := as.integer(as.numeric(data - dn) %/% 365.25)]
  d[, nasc := NULL]
  for (m in meses) {
    x <- d[mes == m]
    if (nrow(x) > 0) saveRDS(x, file.path(pasta, paste0(m, ".rds")))
  }
  meses[meses %in% d$mes]
}

# Atendimentos dos meses pedidos ("aaaa-mm"), do cache ou dos arquivos publicados.
baixar_curitiba <- function(meses) {
  pasta <- file.path(pasta_cache(), "curitiba")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  faltam <- meses[!file.exists(file.path(pasta, paste0(meses, ".rds")))]
  if (length(faltam) > 0) {
    arq <- arquivos_curitiba()
    sem_arquivo <- character()
    while (length(faltam) > 0) {
      m <- min(faltam)
      # Prefere o arquivo publicado 3 meses depois (cobre m, m+1 e m+2).
      cand <- arq[arq$mes %in% vapply(3:1, function(k) somar_mes(m, k), character(1)), ]
      cand <- cand[order(cand$mes, decreasing = TRUE), ]
      if (nrow(cand) == 0) {
        sem_arquivo <- c(sem_arquivo, m)
        faltam <- setdiff(faltam, m)
        next
      }
      feitos <- processar_arquivo_curitiba(cand$arquivo[1], cand$mes[1], pasta)
      faltam <- setdiff(faltam, c(feitos, m))
    }
    if (length(sem_arquivo) > 0) {
      avisar("LDS-11", "Curitiba: sem arquivo publicado para ", paste(sem_arquivo, collapse = ", "), ".")
    }
  }
  partes <- lapply(meses, function(m) {
    f <- file.path(pasta, paste0(m, ".rds"))
    if (file.exists(f)) readRDS(f) else NULL
  })
  data.table::rbindlist(partes, fill = TRUE)
}

#' Atendimentos médicos por bairro (Curitiba)
#'
#' Conta os atendimentos médicos da rede municipal de saúde de Curitiba
#' (unidades de saúde, UPAs e outros serviços do Sistema e-Saúde) por
#' bairro de residência do paciente, com população, taxa por 10 mil
#' habitantes e coordenadas. Fonte: portal de dados abertos da Prefeitura
#' de Curitiba (2019 em diante).
#'
#' Cada arquivo publicado tem cerca de 480 MB e cobre três meses; a
#' primeira consulta de um ano baixa quatro arquivos. Os meses ficam no
#' cache, e as consultas seguintes são rápidas.
#'
#' @param municipio Só `"Curitiba"` (residentes de Curitiba).
#' @param anos Anos do atendimento.
#' @param meses Meses do atendimento (1 a 12).
#' @param cid CID do atendimento: letras para capítulos (`"J"`) ou códigos
#'   (`"J45"`, `"J00-J06"`).
#' @param por Variável para abrir as contagens: `"sexo"`,
#'   `"faixa etaria"`, `"tipo de unidade"` (UPA, unidade de saúde...),
#'   `"profissional"` (especialidade do médico), `"internamento"` ou
#'   `"unidade de saude"` (o estabelecimento do atendimento).
#' @return Como em [obitos_bairro()], com `atendimentos` no lugar de
#'   `obitos`.
#' @export
#' @examplesIf interactive()
#' atendimentos_bairro("Curitiba", 2025, meses = 1:3, cid = "J")
#' atendimentos_bairro("Curitiba", 2025, meses = 6, por = "tipo de unidade")
atendimentos_bairro <- function(municipio = "Curitiba", anos, meses = 1:12, cid = NULL, por = NULL) {
  lugar <- achar_lugar(municipio, NULL)
  if (!identical(lugar$codmun, COD_CURITIBA)) sem_fonte("atendimentos m\u00e9dicos", lugar)
  pedidos <- as.vector(outer(sprintf("%02d", meses), anos, function(m, a) paste0(a, "-", m)))
  pedidos <- sort(pedidos[pedidos < format(Sys.Date(), "%Y-%m")])
  d <- baixar_curitiba(pedidos)
  if (nrow(d) == 0) erro("LDS-12", "Nenhum atendimento em Curitiba nesse per\u00edodo.")
  d <- d[municipio_res == "CURITIBA"]
  if (!is.null(cid)) {
    codigos <- expandir_cid(toupper(gsub("[[:space:].]", "", cid)))
    d <- d[filtrar_cid(cid, codigos)]
  }
  d[, faixa_etaria := cut(idade, c(-Inf, 4, 9, 19, 39, 59, 79, Inf),
                          labels = c("0-4", "5-9", "10-19", "20-39", "40-59", "60-79", "80+"))]
  d[, faixa_etaria := as.character(faixa_etaria)]
  col_por <- NULL
  if (!is.null(por)) {
    op <- c("sexo", "faixa_etaria", "tipo_unidade", "profissional", "internamento", "unidade")
    rotulos <- c("sexo", "faixa etaria", "tipo de unidade", "profissional", "internamento",
                 "unidade de saude")
    r <- normalizar_rotulo(rotulos)
    a <- normalizar_rotulo(por)
    pos <- which(r == a)
    if (length(pos) == 0) pos <- which(startsWith(r, a) | grepl(a, r, fixed = TRUE))
    if (length(pos) == 0) {
      erro("LDS-10", "N\u00e3o achei \"", por, "\". Op\u00e7\u00f5es: ", paste(rotulos, collapse = ", "), ".")
    }
    col_por <- op[pos[1]]
  }
  d[, ano := as.integer(substr(mes, 1, 4))]
  d[, categoria := if (is.null(col_por)) NA_character_ else get(col_por)]
  agg <- d[, .(n = .N), by = .(ano, bairro_tabnet = bairro, categoria)]
  lig <- data.table::as.data.table(ligar_bairros(agg$bairro_tabnet, COD_CURITIBA, "PR"))
  agg <- cbind(agg, lig[, .(ligacao, id_unidade, nome_ibge, cd_ibge, lat, lon, populacao)])
  agg[, `:=`(codmun = COD_CURITIBA, municipio = "Curitiba", fonte = "e-Sa\u00fade Curitiba",
             unidade = "bairro")]
  arrumar_tabela(agg, if (is.null(por)) NULL else nome_coluna(por), "atendimentos")
}
