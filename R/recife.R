# Arboviroses da Prefeitura do Recife: registros individuais de dengue,
# chikungunya e zika (2013 em diante), publicados no portal de dados abertos
# da cidade, com o nome do bairro e o CEP de residência.
#
# Os arquivos têm dois formatos: até 2020, nomes longos
# ("no_bairro_residencia"); a partir de 2021, os nomes do SINAN
# ("NM_BAIRRO"). O separador também muda (";" ou ","), e alguns anos têm
# linhas inteiras entre aspas. Tudo é padronizado aqui para os nomes do SINAN.

URL_RECIFE_ARBO <- paste0("https://dados.recife.pe.gov.br/api/3/action/package_show?",
                          "id=casos-de-dengue-zika-e-chikungunya")
COD_RECIFE <- "261160"

# Nome antigo -> nome do SINAN.
.nomes_recife <- c(
  nu_notificacao = "NU_NOTIFIC", dt_notificacao = "DT_NOTIFIC",
  co_municipio_residencia = "ID_MN_RESI", co_bairro_residencia = "ID_BAIRRO",
  no_bairro_residencia = "NM_BAIRRO", nu_cep_residencia = "NU_CEP", nu_cep = "NU_CEP",
  nome_logradouro_residencia = "NM_LOGRADO", nm_logradouro_residencia = "NM_LOGRADO",
  tp_classificacao_final = "CLASSI_FIN", tp_sexo = "CS_SEXO", dt_nascimento = "DT_NASC"
)

# Classificação final da dengue, segundo o dicionário publicado pela
# Prefeitura do Recife (metadados dos casos de dengue).
.classi_dengue <- c(
  "1" = "Dengue cl\u00e1ssico", "2" = "Dengue com complica\u00e7\u00f5es",
  "3" = "Febre hemorr\u00e1gica do dengue", "4" = "S\u00edndrome do choque da dengue",
  "5" = "Descartado", "8" = "Inconclusivo", "10" = "Dengue",
  "11" = "Dengue com sinais de alarme", "12" = "Dengue grave"
)

# Lista os arquivos do portal: agravo, ano e endereço.
recursos_recife <- function() {
  r <- tryCatch(jsonlite::fromJSON(URL_RECIFE_ARBO)$result$resources, error = function(e) NULL)
  if (is.null(r)) erro("LDS-14", "O portal de dados abertos do Recife n\u00e3o respondeu.")
  r <- r[toupper(r$format) == "CSV", c("name", "url")]
  nome <- trimws(r$name)
  agravo <- ifelse(grepl("Dengue", nome), "dengue",
                   ifelse(grepl("Chikungunya", nome), "chikungunya",
                          ifelse(grepl("Zika", nome, ignore.case = TRUE), "zika", NA)))
  ano <- suppressWarnings(as.integer(sub(".*([0-9]{4})$", "\\1", nome)))
  ok <- !is.na(agravo) & !is.na(ano)
  data.frame(agravo = agravo[ok], ano = ano[ok], url = sub(":443/", "/", r$url[ok], fixed = TRUE),
             stringsAsFactors = FALSE)
}

# Lê um CSV do Recife, consertando as linhas inteiras entre aspas.
ler_csv_recife <- function(arq) {
  linhas <- readLines(arq, encoding = "UTF-8", warn = FALSE)
  linhas <- sub("\r$", "", linhas)
  sep <- if (lengths(regmatches(linhas[1], gregexpr(";", linhas[1]))) >
             lengths(regmatches(linhas[1], gregexpr(",", linhas[1])))) ";" else ","
  # Linha inteira entre aspas: "1911016,2,A90,...,""AV, X"",,..."
  inteira <- grepl('^".*"$', linhas) & !grepl(paste0('^"[^"]*"', sep), linhas)
  linhas[inteira] <- gsub('""', '"', substr(linhas[inteira], 2, nchar(linhas[inteira]) - 1))
  d <- data.table::fread(text = linhas, sep = sep, colClasses = "character", fill = TRUE,
                         showProgress = FALSE, encoding = "UTF-8")
  nomes <- tolower(trimws(names(d)))
  novo <- ifelse(nomes %in% names(.nomes_recife), .nomes_recife[nomes], toupper(nomes))
  data.table::setnames(d, make.unique(unname(novo)))
  d
}

# Baixa e padroniza os casos de um agravo, ano a ano (arquivos no cache;
# os dois anos mais recentes são baixados de novo, pois ainda mudam).
baixar_recife <- function(agravo, ano_inicio, ano_fim, vars = NULL) {
  rec <- recursos_recife()
  rec <- rec[rec$agravo == agravo & rec$ano >= ano_inicio & rec$ano <= ano_fim, ]
  if (nrow(rec) == 0) {
    erro("LDS-11", "N\u00e3o h\u00e1 arquivo de ", agravo, " do Recife para ", ano_inicio,
         if (ano_fim != ano_inicio) paste0("-", ano_fim), ".")
  }
  pasta <- file.path(pasta_cache(), "recife")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  partes <- list()
  for (k in seq_len(nrow(rec))) {
    detalhar("Recife, ", agravo, ": ", rec$ano[k], "...")
    destino <- file.path(pasta, sprintf("%s_%d.csv", agravo, rec$ano[k]))
    if (rec$ano[k] >= ano_atual - 1) unlink(destino)
    b <- ler_csv_recife(baixar(rec$url[k], destino))
    b[, ANO_ARQUIVO := as.character(rec$ano[k])]
    partes[[k]] <- b
  }
  d <- data.table::rbindlist(partes, fill = TRUE)
  # Códigos numéricos gravados como decimais ("261160.0") e CEPs vazios.
  for (v in intersect(c("ID_MN_RESI", "ID_BAIRRO", "CLASSI_FIN", "NU_CEP"), names(d))) {
    data.table::set(d, j = v, value = sub("\\.0+$", "", trimws(d[[v]])))
  }
  d[NU_CEP %in% c("", "0", "00000000"), NU_CEP := NA_character_]
  d[, NM_BAIRRO := trimws(NM_BAIRRO)]
  # Data de notificação: "dd/mm/aaaa" ou "aaaa-mm-dd".
  dt <- d$DT_NOTIFIC
  br <- grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", dt)
  dt[br] <- format(as.Date(dt[br], "%d/%m/%Y"), "%Y-%m-%d")
  d[, DT_NOTIFIC := substr(dt, 1, 10)]
  # Colunas legíveis (usadas por agravos_bairro(por = ...)).
  sexo <- c(M = "Masculino", F = "Feminino", I = "Ignorado")
  if ("CS_SEXO" %in% names(d)) d[, sexo := unname(sexo[CS_SEXO])]
  d[, classificacao := if (agravo == "dengue") unname(.classi_dengue[CLASSI_FIN]) else
    ifelse(is.na(CLASSI_FIN) | CLASSI_FIN == "", NA_character_, paste("c\u00f3digo", CLASSI_FIN))]
  if (!is.null(vars)) d <- d[, intersect(c(vars, "ANO_ARQUIVO", "sexo", "classificacao"), names(d)),
                             with = FALSE]
  d
}
