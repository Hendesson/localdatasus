# Internações do SIH por bairro de residência.

#' Internações do SIH/SUS por bairro de residência
#'
#' Baixa as AIHs aprovadas (arquivo RD do SIH/SUS) de uma UF, conta uma
#' internação por AIH principal, localiza o bairro de residência de cada
#' paciente pelo CEP e devolve a contagem por bairro, ano e grupo de causa.
#'
#' @section Como o bairro é atribuído:
#' Cada CEP é ligado ao bairro onde está a maioria dos seus endereços no
#' CNEFE 2022 (ver [cep_bairro()]). Um CEP NÃO recebe bairro quando:
#' \itemize{
#'   \item é inválido ou ausente;
#'   \item é especial (final 900 a 999: grandes usuários, caixas postais);
#'   \item não existe no CNEFE 2022;
#'   \item o paciente mora em outra UF;
#'   \item tem internações demais para o número de endereços — típico do
#'     CEP geral do município usado como "padrão", ou do CEP do hospital
#'     (ver `limite_excesso`);
#'   \item pertence a outro município que não o de residência;
#'   \item cobre vários bairros sem maioria clara (ver `pct_min`).
#' }
#' Essas internações aparecem com `bairro = "(sem bairro)"`, para que os
#' totais por município continuem corretos. O motivo de cada caso fica no
#' atributo `"cobertura"` do resultado.
#'
#' @section Datas:
#' Os arquivos do SIH são organizados por mês de COMPETÊNCIA (faturamento),
#' mas a contagem é feita pela DATA DE INTERNAÇÃO. Por isso a função baixa
#' também `meses_extras` meses depois de `ano_fim`, para pegar as AIHs
#' faturadas com atraso.
#'
#' @param uf Sigla da UF, por exemplo `"RJ"`.
#' @param ano_inicio,ano_fim Anos de INTERNAÇÃO (inclusive).
#' @param cid Vetor com prefixos da CID-10 do diagnóstico principal, por
#'   exemplo `c("I", "J")` (capítulos IX e X) ou `c("I21", "I22")`. Cada
#'   prefixo vira um grupo no resultado. `NULL` (padrão) = todas as causas.
#' @param nivel `"bairro"` (padrão) devolve contagens por bairro;
#'   `"internacao"` devolve uma linha por internação, com CEP, bairro e
#'   coordenadas (ver [adicionar_bairro()]).
#' @param minimo Se maior que 0, contagens entre 1 e `minimo - 1` viram
#'   `NA` (supressão de células pequenas). Padrão: 0 (não suprime).
#' @param meses_extras Meses de competência baixados após `ano_fim`.
#' @param pct_min Fração mínima dos endereços de um CEP no mesmo bairro.
#' @param limite_excesso Razão máxima entre a fração de pessoas/registros
#'   e a fração de endereços de um CEP (valor esperado: ~1).
#' @return Com `nivel = "bairro"`, um `data.frame` com `codmun`,
#'   `id_bairro`, `bairro`, `fonte_bairro`, `cd_bairro_ibge`, `lat_bairro`, `lon_bairro`,
#'   `ano`, `grupo`, `internacoes`, `obitos_hosp`, `populacao` (só bairros
#'   oficiais), `taxa_10mil` e `suprimido`.
#' @references
#' Instituto Brasileiro de Geografia e Estatística. Coordenadas geográficas
#' dos endereços no Censo Demográfico 2022: nota metodológica n. 01. Rio de
#' Janeiro: IBGE; 2024.
#'
#' Empresa Brasileira de Correios e Telégrafos. Endereçamento de
#' correspondências: guia técnico. Versão 1.4. Brasília: Correios; 2021.
#' (Estrutura do CEP e sufixos especiais.)
#'
#' Abordagem relacionada, com serviços de geocodificação: Rocha TAH, Silva
#' NC, Amaral PVM et al. Geolocalização de internações cadastradas no Sistema
#' de Informações Hospitalares do Sistema Único de Saúde: uma solução baseada
#' no programa estatístico R. Epidemiologia e Serviços de Saúde 2018;
#' 27(4):e2017444. \doi{10.5123/s1679-49742018000400016}.
#' @export
#' @examplesIf interactive()
#' # Internações por doenças circulatórias e respiratórias, Acre, 2023
#' x <- sih_bairro("AC", 2023, cid = c("I", "J"))
#' attr(x, "cobertura")
sih_bairro <- function(uf, ano_inicio, ano_fim = ano_inicio, cid = NULL,
                       nivel = c("bairro", "internacao"), minimo = 0,
                       meses_extras = 6, pct_min = 0.6, limite_excesso = 20) {
  nivel <- match.arg(nivel)
  cod   <- codigo_uf(uf)
  uf    <- toupper(uf)

  # 1. AIHs principais de residentes da UF, pela data de internação
  int <- baixar_sih(uf, cod, ano_inicio, ano_fim, cid, meses_extras)
  if (nrow(int) == 0) erro("LDS-12", "Nenhuma interna\u00e7\u00e3o encontrada com esses filtros.")

  # 2. CEP -> bairro e coordenadas (mesma função usada para qualquer base)
  x <- adicionar_bairro(as.data.frame(int), uf, col_cep = "cep", col_mun = "codmun_res",
                        col_id = "n_aih", pct_min = pct_min, limite_excesso = limite_excesso)
  if (mean(!is.na(x$id_bairro)) < 0.3) {
    avisar("LDS-22", "Cobertura baixa: comum onde os munic\u00edpios t\u00eam CEP \u00fanico ou os hospitais ",
            "registram CEPs gen\u00e9ricos. Veja attr(x, \"cobertura\").")
  }
  if (nivel == "internacao") return(x)

  # 3. Agregar por bairro, ano e grupo
  d <- data.table::as.data.table(x)
  agg <- d[, .(internacoes = .N, obitos_hosp = sum(obito_hosp)),
           by = .(codmun = codmun_res, id_bairro, bairro, fonte_bairro,
                  cd_bairro_ibge, lat_bairro, lon_bairro, ano, grupo)]
  agg[is.na(id_bairro), bairro := "(sem bairro)"]

  # 4. População (bairros oficiais) e taxa por 10 mil habitantes
  pop <- data.table::as.data.table(populacao_bairro(uf))[, .(cd_bairro_ibge, populacao)]
  agg <- pop[agg, on = "cd_bairro_ibge"]
  agg[, taxa_10mil := round(10000 * internacoes / populacao, 2)]

  # 5. Supressão opcional de contagens pequenas (desligada com minimo = 0)
  agg <- suprimir(agg, minimo)

  data.table::setcolorder(agg, c("codmun", "id_bairro", "bairro", "fonte_bairro",
                                 "cd_bairro_ibge", "lat_bairro", "lon_bairro", "ano", "grupo",
                                 "internacoes", "obitos_hosp", "populacao",
                                 "taxa_10mil", "suprimido"))
  data.table::setorder(agg, codmun, ano, grupo, bairro)
  res <- as.data.frame(agg)
  attr(res, "cobertura") <- attr(x, "cobertura")
  res
}


# --- Funções internas ---------------------------------------------------------

# Baixa o RD do SIH por ano de competência e devolve as AIHs principais de
# residentes da UF, já filtradas por CID e pelo período de internação.
# Trabalha com os códigos BRUTOS do DATASUS (sem process_sih), que é mais
# rápido: IDENT 1 = AIH principal; MORTE 1 = óbito; SEXO 1 = masc., 3 = fem.
baixar_sih <- function(uf, cod, ano_inicio, ano_fim, cid, meses_extras) {
  vars <- c("N_AIH", "IDENT", "MUNIC_RES", "CEP", "DT_INTER", "DIAG_PRINC", "MORTE", "SEXO")
  partes <- list()
  for (ano in ano_inicio:(ano_fim + 1)) {
    mes_fim <- if (ano == ano_fim + 1) meses_extras else 12
    if (mes_fim < 1) next
    detalhar("SIH ", uf, ": compet\u00eancia ", ano, " (meses 1 a ", mes_fim, ")...")
    bruto <- microdatasus::fetch_datasus(
      year_start = ano, month_start = 1, year_end = ano, month_end = mes_fim,
      uf = uf, information_system = "SIH-RD", vars = vars, quiet = TRUE
    )
    if (is.null(bruto) || nrow(bruto) == 0) next
    d <- data.table::as.data.table(bruto)
    d <- d[IDENT == "1" & substr(MUNIC_RES, 1, 2) == cod]
    if (!is.null(cid)) d <- d[filtrar_cid(DIAG_PRINC, cid)]
    partes[[length(partes) + 1]] <- d
  }
  int <- data.table::rbindlist(partes, fill = TRUE)
  if (nrow(int) == 0) return(int)

  # A mesma AIH principal pode ser reapresentada em outra competência.
  int <- int[!duplicated(N_AIH)]
  int[, `:=`(
    n_aih      = N_AIH,
    codmun_res = MUNIC_RES,
    cep        = CEP,
    diag_princ = DIAG_PRINC,
    dt_inter   = as.Date(DT_INTER, format = "%Y%m%d"),
    obito_hosp = MORTE == "1",
    sexo       = data.table::fcase(SEXO == "1", "Masculino", SEXO == "3", "Feminino",
                                   default = "Ignorado"),
    grupo      = if (is.null(cid)) "todas" else grupo_cid(DIAG_PRINC, cid)
  )]
  int[, ano := as.integer(format(dt_inter, "%Y"))]
  int[ano >= ano_inicio & ano <= ano_fim,
      .(n_aih, codmun_res, cep, diag_princ, dt_inter, ano, grupo, obito_hosp, sexo)]
}

# TRUE para diagnósticos que começam com algum dos prefixos.
filtrar_cid <- function(diag, cid) {
  Reduce(`|`, lapply(cid, function(p) startsWith(diag, p)))
}

# O primeiro prefixo (na ordem dada) com que cada diagnóstico começa.
grupo_cid <- function(diag, cid) {
  g <- rep(NA_character_, length(diag))
  for (p in cid) g[is.na(g) & startsWith(diag, p)] <- p
  g
}

# Classifica cada registro quanto ao uso do CEP para atribuir bairro.
# Os argumentos são vetores alinhados (uma posição por registro);
# `total_enderecos` é o total de endereços da UF no CNEFE; `id` identifica a
# pessoa/episódio (o "excesso" é medido em pessoas DISTINTAS por CEP, para
# que quem tem muitos registros — ex.: diálise mensal — não pareça CEP
# genérico); `cod_uf` é o código da UF da tabela de CEPs.
classificar_cep <- function(cep, codmun_res, codmun_cep, id_bairro, pct_bairro, n_end_cep,
                            total_enderecos, id = seq_along(cep), cod_uf = NA,
                            pct_min = 0.6, limite_excesso = 20, min_excesso = 30) {
  valido   <- !is.na(cep) & grepl("^[0-9]{8}$", cep)
  especial <- valido & suppressWarnings(as.integer(substr(cep, 6, 8))) >= 900
  outra_uf <- !is.na(codmun_res) & !is.na(cod_uf) & substr(codmun_res, 1, 2) != cod_uf

  # Razão de excesso: (fração das pessoas no CEP) / (fração dos endereços).
  dt <- data.table::data.table(cep = cep, id = id)
  n_id_cep <- dt[, n := data.table::uniqueN(id), by = cep]$n
  razao    <- (n_id_cep / data.table::uniqueN(id)) / (n_end_cep / total_enderecos)
  excesso  <- !is.na(razao) & razao > limite_excesso & n_id_cep >= min_excesso

  data.table::fcase(
    !valido,                                    "CEP inv\u00e1lido/ausente",
    outra_uf,                                   "residente de outra UF",
    especial,                                   "CEP especial (grande usu\u00e1rio/caixa postal)",
    is.na(id_bairro),                           "CEP n\u00e3o existe no CNEFE 2022",
    excesso,                                    "CEP com excesso de registros (gen\u00e9rico/hospital)",
    !is.na(codmun_res) & codmun_cep != codmun_res, "CEP de outro munic\u00edpio",
    pct_bairro < pct_min,                       "CEP geral/amplo (v\u00e1rios bairros)",
    default =                                   "bairro atribu\u00eddo"
  )
}

# Suprime contagens entre 1 e minimo - 1 (e a taxa calculada a partir delas).
suprimir <- function(agg, minimo) {
  agg[, suprimido := minimo > 0 & internacoes > 0 & internacoes < minimo]
  agg[suprimido == TRUE, `:=`(internacoes = NA, taxa_10mil = NA)]
  agg[minimo > 0 & !is.na(obitos_hosp) & obitos_hosp > 0 & obitos_hosp < minimo,
      obitos_hosp := NA]
  agg
}
