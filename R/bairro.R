# Localização por bairro de qualquer tabela com CEP, download das bases do
# DATASUS que têm CEP e agregação por bairro.

#' Adiciona bairro e coordenadas a uma tabela com CEP
#'
#' Funciona com QUALQUER `data.frame` que tenha uma coluna de CEP — dados do
#' DATASUS ou seus próprios dados. Cada linha recebe o bairro (pelo CNEFE
#' 2022, ver [cep_bairro()]), as coordenadas do CEP e do bairro, e um
#' diagnóstico da qualidade do CEP (`situacao_cep`).
#'
#' @section Colunas adicionadas:
#' \describe{
#'   \item{cep_paciente}{CEP normalizado (8 dígitos).}
#'   \item{codmun_paciente}{município de residência (6 dígitos), se `col_mun` for dado.}
#'   \item{situacao_cep}{se o CEP permitiu atribuir bairro e, se não, por quê.}
#'   \item{id_bairro, bairro, fonte_bairro, cd_bairro_ibge}{o bairro (só quando
#'     `situacao_cep == "bairro atribuído"`).}
#'   \item{lat_bairro, lon_bairro}{ponto do bairro (mediana dos endereços).}
#'   \item{lat_cep, lon_cep}{ponto do CEP (mediana dos endereços do CEP).
#'     Preenchido para todo CEP que existe no CNEFE — inclusive CEPs gerais,
#'     cujo ponto representa o centro da área que eles cobrem; use
#'     `situacao_cep` e `n_end_cep` para julgar.}
#'   \item{codmun_cep, n_end_cep, pct_bairro}{município do CEP, nº de
#'     endereços do CEP e fração deles no bairro atribuído.}
#' }
#'
#' @param dados Um `data.frame`.
#' @param uf Sigla da UF dos CEPs (a tabela CEP-bairro é montada por UF).
#' @param col_cep Nome da coluna de CEP.
#' @param col_mun Nome da coluna do município de residência (6 ou 7
#'   dígitos), opcional. Permite detectar CEPs de outro município.
#' @param col_id Nome de uma coluna que identifica a pessoa ou o episódio,
#'   opcional. Usada para que pessoas com muitos registros (ex.: diálise
#'   mensal) não sejam confundidas com CEPs genéricos.
#' @inheritParams sih_bairro
#' @return `dados` com as colunas acima acrescentadas.
#' @export
#' @examplesIf interactive()
#' minha_tabela <- data.frame(id = 1:3, cep = c("20031170", "22041001", "24020005"))
#' adicionar_bairro(minha_tabela, "RJ", col_cep = "cep")
adicionar_bairro <- function(dados, uf, col_cep = "CEP", col_mun = NULL, col_id = NULL,
                             pct_min = 0.6, limite_excesso = 20) {
  cod <- codigo_uf(uf)
  if (!col_cep %in% names(dados)) erro("LDS-17", "Coluna de CEP n\u00e3o encontrada: ", col_cep)
  tab <- cep_bairro(uf)

  cep <- gsub("\\D", "", as.character(dados[[col_cep]]))
  # CEPs salvos como número perdem o zero da frente (01001000 -> 1001000).
  cep[!is.na(cep) & nchar(cep) == 7] <- paste0("0", cep[!is.na(cep) & nchar(cep) == 7])
  codmun <- if (is.null(col_mun)) rep(NA_character_, length(cep)) else
    substr(as.character(dados[[col_mun]]), 1, 6)
  id <- if (is.null(col_id)) seq_along(cep) else dados[[col_id]]

  info <- data.table::data.table(.ordem = seq_along(cep), cep = cep, codmun_paciente = codmun)
  info <- data.table::as.data.table(tab$ceps)[info, on = "cep"]
  info[, situacao_cep := classificar_cep(
    cep, codmun_paciente, codmun_cep, id_bairro, pct_bairro, n_end_cep,
    total_enderecos = sum(tab$ceps$n_end_cep), id = id, cod_uf = cod,
    pct_min = pct_min, limite_excesso = limite_excesso
  )]
  info[situacao_cep != "bairro atribu\u00eddo", id_bairro := NA]

  b <- data.table::as.data.table(tab$bairros)[, .(id_bairro, bairro, fonte_bairro, cd_bairro_ibge,
                                                  lat_bairro, lon_bairro)]
  info <- b[info, on = "id_bairro"]
  data.table::setorder(info, .ordem)

  novas <- info[, .(cep_paciente = cep, codmun_paciente, situacao_cep, id_bairro, bairro,
                    fonte_bairro, cd_bairro_ibge, lat_bairro, lon_bairro, lat_cep, lon_cep,
                    codmun_cep, n_end_cep, pct_bairro)]
  saida <- cbind(as.data.frame(dados), as.data.frame(novas))
  attr(saida, "cobertura") <- as.data.frame(
    info[, .N, by = situacao_cep][, pct := round(100 * N / sum(N), 1)][order(-N)]
  )
  detalhar(sprintf("%.1f%% dos registros receberam bairro.", 100 * mean(!is.na(info$id_bairro))))
  saida
}


#' Baixa uma base com CEP já com bairro e coordenadas
#'
#' Baixa os microdados de uma base que tem o CEP do paciente (ver
#' [sistemas_bairro()]) e devolve TODOS os registros e colunas originais,
#' acrescidos de bairro, coordenadas e qualidade do CEP (ver
#' [adicionar_bairro()]).
#'
#' Os arquivos do DATASUS são mensais, por mês de COMPETÊNCIA (processamento),
#' e por UF do ESTABELECIMENTO: pacientes de outras UFs atendidos na UF
#' aparecem com `situacao_cep = "residente de outra UF"`. O `"SINASC-RJ"`
#' (SES-RJ) vem em arquivos anuais por data de nascimento.
#'
#' Quando a base traz também o NOME do bairro (SINASC-RJ), os registros que
#' não recebem bairro pelo CEP são ligados pelo nome, com [ligar_bairros()].
#' A coluna `origem_bairro` diz de onde veio o bairro de cada registro.
#'
#' Colunas extras: `data_ref` e `ano_ref` (data de internação no SIH;
#' competência de realização nas APACs; data de nascimento no SINASC-RJ),
#' `origem_bairro` e, nas APACs, `id_paciente` — o cartão SUS
#' pseudonimizado (`AP_CNSPCN`) convertido em texto legível, que permite
#' seguir o mesmo paciente entre meses.
#'
#' @param sistema Código da base, por exemplo `"SIH-RD"` ou `"SIA-AQ"`
#'   (veja [sistemas_bairro()]).
#' @param uf Sigla da UF.
#' @param ano_inicio,ano_fim Anos de competência (de nascimento, no SINASC-RJ).
#' @param mes_inicio,mes_fim Meses (do primeiro e do último ano).
#' @param vars Colunas a baixar (`NULL` = todas). As colunas necessárias para
#'   localizar o bairro são incluídas automaticamente.
#' @inheritParams sih_bairro
#' @return Um `data.frame`, um registro por linha; o atributo `"cobertura"`
#'   resume `situacao_cep`.
#' @export
#' @examplesIf interactive()
#' quimio <- baixar_bairro("SIA-AQ", "RJ", 2024, mes_inicio = 1, mes_fim = 3)
#' internacoes <- baixar_bairro("SIH-RD", "RJ", 2023)
#' nascimentos <- baixar_bairro("SINASC-RJ", "RJ", 2023)
baixar_bairro <- function(sistema, uf, ano_inicio, ano_fim = ano_inicio,
                          mes_inicio = 1, mes_fim = 12, vars = NULL,
                          pct_min = 0.6, limite_excesso = 20) {
  cfg <- config_sistema(sistema)
  uf  <- toupper(uf)
  codigo_uf(uf)
  extras <- c(cfg$col_cep, cfg$col_mun, cfg$col_id, cfg$col_data, cfg$col_cid, cfg$col_bairro)
  if (!is.null(vars)) vars <- unique(c(vars, extras[!is.na(extras)]))

  if (cfg$origem == "microdatasus") {
    d <- baixar_microdatasus(cfg, uf, ano_inicio, ano_fim, mes_inicio, mes_fim, vars)
  } else if (cfg$sistema == "SINASC-RJ") {
    if (uf != "RJ") erro("LDS-18", "SINASC-RJ s\u00f3 existe para uf = \"RJ\".")
    d <- baixar_sinasc_rj(ano_inicio, ano_fim, vars)
  } else if (cfg$origem == "Prefeitura do Recife (CSV)") {
    if (uf != "PE") erro("LDS-18", cfg$sistema, " s\u00f3 existe para uf = \"PE\".")
    d <- baixar_recife(tolower(sub("-RECIFE$", "", cfg$sistema)), ano_inicio, ano_fim, vars)
  }
  if (nrow(d) == 0) erro("LDS-12", "Nenhum registro encontrado para esse per\u00edodo.")

  # Data de referência e ano.
  datas <- as.character(d[[cfg$col_data]])
  if (cfg$formato_data == "%Y%m") datas <- paste0(datas, "01")
  formato <- if (cfg$formato_data == "%Y%m") "%Y%m%d" else cfg$formato_data
  d[, data_ref := as.Date(datas, format = formato)]
  d[, ano_ref := as.integer(format(data_ref, "%Y"))]
  # Arquivos anuais por ano epidemiológico: vale o ano do arquivo.
  if ("ANO_ARQUIVO" %in% names(d)) d[, ano_ref := as.integer(ANO_ARQUIVO)]
  # Bases anuais por data do evento: filtra os meses pedidos.
  if (cfg$origem != "microdatasus") {
    mes <- as.integer(format(d$data_ref, "%m"))
    fora <- (d$ano_ref == ano_inicio & mes < mes_inicio) | (d$ano_ref == ano_fim & mes > mes_fim)
    d <- d[is.na(fora) | !fora]
  }

  # APAC: pseudônimo do cartão SUS em texto (hexadecimal) — o original tem
  # bytes que não são texto válido e quebram exportações.
  col_id <- if (vazio(cfg$col_id)) NULL else cfg$col_id
  if (identical(col_id, "AP_CNSPCN")) {
    d[, id_paciente := vapply(as.character(AP_CNSPCN), function(s)
      paste(as.character(charToRaw(s)), collapse = ""), character(1), USE.NAMES = FALSE)]
    col_id <- "id_paciente"
  }

  res <- adicionar_bairro(as.data.frame(d), uf, col_cep = cfg$col_cep, col_mun = cfg$col_mun,
                          col_id = col_id, pct_min = pct_min, limite_excesso = limite_excesso)
  res$origem_bairro <- ifelse(is.na(res$id_bairro), NA_character_, "CEP")
  if (!vazio(cfg$col_bairro)) res <- completar_pelo_nome(res, cfg$col_bairro, uf)
  res
}

# Baixa uma base do DATASUS pelo microdatasus, ano a ano.
baixar_microdatasus <- function(cfg, uf, ano_inicio, ano_fim, mes_inicio, mes_fim, vars) {
  partes <- list()
  for (ano in ano_inicio:ano_fim) {
    m1 <- if (ano == ano_inicio) mes_inicio else 1
    m2 <- if (ano == ano_fim) mes_fim else 12
    detalhar(cfg$sistema, " ", uf, ": compet\u00eancia ", ano, " (meses ", m1, " a ", m2, ")...")
    b <- microdatasus::fetch_datasus(
      year_start = ano, month_start = m1, year_end = ano, month_end = m2,
      uf = uf, information_system = cfg$sistema, vars = vars, quiet = TRUE
    )
    if (!is.null(b) && nrow(b) > 0) partes[[length(partes) + 1]] <- data.table::as.data.table(b)
  }
  data.table::rbindlist(partes, fill = TRUE)
}

# Baixa os CSVs anuais do SINASC publicados pela SES-RJ (um arquivo por ano
# de nascimento, ~17 MB cada, guardados no cache).
baixar_sinasc_rj <- function(ano_inicio, ano_fim, vars) {
  pasta <- file.path(pasta_cache(), "ses_rj")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  partes <- list()
  for (ano in ano_inicio:ano_fim) {
    detalhar("SINASC-RJ: nascimentos de ", ano, "...")
    url <- sprintf(URL_SINASC_RJ, ano)
    destino <- file.path(pasta, basename(url))
    # O ano corrente é atualizado pela SES-RJ: baixa de novo a cada vez.
    if (ano >= as.integer(format(Sys.Date(), "%Y"))) unlink(destino)
    zip <- baixar(url, destino)
    csv <- utils::unzip(zip, exdir = tempdir())
    b <- data.table::fread(csv, sep = ";", quote = "", encoding = "Latin-1",
                           colClasses = "character", showProgress = FALSE)
    unlink(csv)
    data.table::setnames(b, trimws(names(b)))
    for (v in names(b)) data.table::set(b, j = v, value = trimws(b[[v]]))
    if (!is.null(vars)) b <- b[, intersect(vars, names(b)), with = FALSE]
    partes[[length(partes) + 1]] <- b
  }
  data.table::rbindlist(partes, fill = TRUE)
}

# Para registros sem bairro pelo CEP, tenta o NOME do bairro escrito na
# declaração (ex.: "Parturiente - Bairro de residência" no SINASC-RJ).
completar_pelo_nome <- function(res, col_bairro, uf) {
  faltam <- which(is.na(res$id_bairro) & !is.na(res$codmun_paciente) &
                    substr(res$codmun_paciente, 1, 2) == codigo_uf(uf))
  if (length(faltam) == 0) return(res)
  lig <- ligar_bairros(res[[col_bairro]][faltam], res$codmun_paciente[faltam], uf)
  ok <- foi_ligado(lig$ligacao)
  i <- faltam[ok]
  b <- cep_bairro(uf)$bairros
  m <- match(lig$id_unidade[ok], b$id_bairro)
  res$id_bairro[i]      <- b$id_bairro[m]
  res$bairro[i]         <- b$bairro[m]
  res$fonte_bairro[i]   <- b$fonte_bairro[m]
  res$cd_bairro_ibge[i] <- b$cd_bairro_ibge[m]
  res$lat_bairro[i]     <- b$lat_bairro[m]
  res$lon_bairro[i]     <- b$lon_bairro[m]
  res$origem_bairro[i]  <- paste0("nome do bairro (", lig$ligacao[ok], ")")
  detalhar(sprintf("+%.1f%% dos registros receberam bairro pelo nome; total com bairro: %.1f%%.",
                  100 * length(i) / nrow(res), 100 * mean(!is.na(res$id_bairro))))
  cob <- as.data.frame(table(origem_bairro = res$origem_bairro, useNA = "ifany"))
  names(cob)[2] <- "N"
  cob$pct <- round(100 * cob$N / sum(cob$N), 1)
  attr(res, "cobertura_nome") <- cob[order(-cob$N), ]
  res
}


#' Conta registros por bairro e calcula taxas
#'
#' Agrega o resultado de [baixar_bairro()] ou [adicionar_bairro()] por
#' bairro (e por outras colunas, se pedido) e junta a população do Censo
#' 2022 dos bairros oficiais para calcular a taxa por 10 mil habitantes.
#' Registros sem bairro ficam numa linha `"(sem bairro)"` por município.
#'
#' @param dados Saída de [baixar_bairro()] ou [adicionar_bairro()].
#' @param por Nomes de colunas extras para agrupar, por exemplo
#'   `c("ano_ref", "AP_CIDPRI")`.
#' @param contar_distintos Nome de uma coluna para contar valores
#'   DISTINTOS (ex.: `"id_paciente"`, para contar pacientes em vez de
#'   registros). `NULL` = conta registros.
#' @return Um `data.frame` com o bairro, as colunas de `por`, `n`,
#'   `populacao` e `taxa_10mil`.
#' @export
#' @examplesIf interactive()
#' quimio <- baixar_bairro("SIA-AQ", "RJ", 2024)
#' agregar_bairro(quimio, por = "ano_ref", contar_distintos = "id_paciente")
agregar_bairro <- function(dados, por = NULL, contar_distintos = NULL) {
  necessarias <- c("codmun_paciente", "id_bairro", "bairro", "fonte_bairro",
                   "cd_bairro_ibge", "lat_bairro", "lon_bairro")
  faltam <- setdiff(c(necessarias, por, contar_distintos), names(dados))
  if (length(faltam) > 0) {
    erro("LDS-17", "Colunas ausentes: ", paste(faltam, collapse = ", "),
         ". Use antes baixar_bairro() ou adicionar_bairro().")
  }
  d <- data.table::as.data.table(dados)
  grupos <- c(necessarias, por)
  agg <- if (is.null(contar_distintos)) d[, .(n = .N), by = grupos] else
    d[, .(n = data.table::uniqueN(get(contar_distintos))), by = grupos]
  agg[is.na(id_bairro), bairro := "(sem bairro)"]

  # População dos bairros oficiais, para cada UF presente nos dados.
  ufs <- names(.ufs)[.ufs %in% unique(substr(agg$cd_bairro_ibge[!is.na(agg$cd_bairro_ibge)], 1, 2))]
  if (length(ufs) > 0) {
    pop <- data.table::rbindlist(lapply(ufs, function(u)
      data.table::as.data.table(populacao_bairro(u))[, .(cd_bairro_ibge, populacao)]))
    agg <- pop[agg, on = "cd_bairro_ibge"]
  } else {
    agg[, populacao := NA_integer_]
  }
  agg[, taxa_10mil := round(10000 * n / populacao, 2)]
  data.table::setcolorder(agg, c(necessarias, por, "n", "populacao", "taxa_10mil"))
  as.data.frame(agg)
}
