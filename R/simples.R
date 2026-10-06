# Funções "de pesquisador": a pessoa diz O QUÊ (óbitos, nascimentos,
# agravos, internações), ONDE (município ou UF) e QUANDO (anos). O pacote
# escolhe sozinho a fonte certa e devolve uma tabela simples, uma linha por
# bairro (e por ano, e pela variável de `por`).

# ---------------------------------------------------------------------------
# Lugar

# Capitais com TabNet próprio (códigos IBGE de 6 dígitos).
COD_RIO       <- "330455"
COD_SAO_PAULO <- "355030"
COD_FORTALEZA <- "230440"

# Acha um município pelo nome (sem ligar para acentos e maiúsculas) ou pelo
# código; devolve código (6 dígitos), nome e UF.
achar_municipio <- function(municipio, uf = NULL) {
  m <- unique(setores_censo()[, .(codmun, nome = NM_MUN, cod_uf = CD_UF)])
  m[, sigla := names(.ufs)[match(cod_uf, .ufs)]]
  texto <- as.character(municipio)
  if (grepl("^[0-9]{6,7}$", texto)) {
    r <- m[codmun == substr(texto, 1, 6)]
  } else {
    r <- m[chave_bairro(nome) == chave_bairro(texto)]
    if (!is.null(uf)) r <- r[sigla == toupper(uf)]
  }
  if (nrow(r) == 0) erro("LDS-02", "Munic\u00edpio n\u00e3o encontrado: \"", texto, "\".")
  if (nrow(r) > 1) {
    erro("LDS-03", "H\u00e1 mais de um munic\u00edpio chamado \"", texto, "\" (", paste(r$sigla, collapse = ", "),
         "). Diga a UF, por exemplo: uf = \"", r$sigla[1], "\".")
  }
  list(codmun = r$codmun, nome = r$nome, uf = r$sigla)
}

# Interpreta municipio/uf e devolve list(codmun, nome, uf); codmun = NA
# quando o pedido é a UF inteira.
achar_lugar <- function(municipio, uf) {
  if (is.null(municipio) && is.null(uf)) {
    erro("LDS-04", "Diga o lugar: municipio = \"Rio de Janeiro\" ou uf = \"RJ\".")
  }
  if (!is.null(municipio)) return(achar_municipio(municipio, uf))
  codigo_uf(uf)
  list(codmun = NA_character_, nome = NA_character_, uf = toupper(uf))
}

# ---------------------------------------------------------------------------
# CID-10

# Capítulo(s) da CID-10 de cada letra.
.capitulos_cid <- list(
  A = 1, B = 1, C = 2, D = c(2, 3), E = 4, F = 5, G = 6, H = c(7, 8), I = 9, J = 10,
  K = 11, L = 12, M = 13, N = 14, O = 15, P = 16, Q = 17, R = 18, S = 19, T = 19,
  U = 22, V = 20, W = 20, X = 20, Y = 20, Z = 21
)

# Expande "I20-I25" em I20, I21, ..., I25.
expandir_cid <- function(cid) {
  unlist(lapply(cid, function(x) {
    if (!grepl("-", x, fixed = TRUE)) return(x)
    p <- strsplit(x, "-", fixed = TRUE)[[1]]
    if (substr(p[1], 1, 1) != substr(p[2], 1, 1)) {
      erro("LDS-08", "Intervalo de CID deve ficar dentro de uma letra, ex.: \"I20-I25\".")
    }
    sprintf("%s%02d", substr(p[1], 1, 1), as.integer(substr(p[1], 2, 3)):as.integer(substr(p[2], 2, 3)))
  }))
}

# Traduz `cid` em filtros do TabNet: letras -> capítulos; códigos de 3
# caracteres (ou intervalos) -> categorias.
filtros_cid <- function(cfg, form, cid) {
  if (is.null(cid)) return(list())
  cid <- toupper(gsub("[[:space:].]", "", cid))
  if (vazio(cfg$filtro_capitulo)) {
    erro("LDS-09", "A fonte ", cfg$fonte, " n\u00e3o permite filtrar por CID.")
  }
  if (all(nchar(cid) == 1)) {
    if (!all(cid %in% names(.capitulos_cid))) erro("LDS-08", "Letra de CID inv\u00e1lida: ", paste(cid, collapse = ", "))
    nums <- unique(unlist(.capitulos_cid[cid]))
    campo <- tn_campo_filtro(form, cfg$filtro_capitulo)
    rot <- setdiff(form$opcoes[[campo]]$rotulo, "Todas as categorias")
    romanos <- as.character(utils::as.roman(nums))
    # "IX.  Doenças do aparelho circulatório" ou "Capítulo  9 - Doenças ..."
    pega <- grepl(paste0("^(", paste(romanos, collapse = "|"), ")[^A-Z]"), rot) |
      grepl(paste0("^Cap.tulo\\s+(", paste(nums, collapse = "|"), ")\\b"), rot)
    if (!any(pega)) erro("LDS-09", "Cap\u00edtulo da CID n\u00e3o encontrado na fonte ", cfg$fonte, ".")
    return(stats::setNames(list(rot[pega]), cfg$filtro_capitulo))
  }
  if (vazio(cfg$filtro_cid3)) {
    erro("LDS-09", "A fonte ", cfg$fonte, " s\u00f3 filtra por cap\u00edtulo da CID: use letras (\"I\", \"J\").")
  }
  cid <- expandir_cid(cid)
  if (any(nchar(cid) != 3)) {
    erro("LDS-08", "Use letras (\"I\", \"J\") ou c\u00f3digos de 3 caracteres (\"I21\", \"I20-I25\"), sem misturar.")
  }
  campo <- tn_campo_filtro(form, cfg$filtro_cid3)
  rot <- setdiff(form$opcoes[[campo]]$rotulo, "Todas as categorias")
  pega <- substr(trimws(rot), 1, 3) %in% cid
  if (!any(pega)) erro("LDS-09", "Nenhum desses c\u00f3digos CID existe na fonte ", cfg$fonte, ".")
  stats::setNames(list(rot[pega]), cfg$filtro_cid3)
}

# ---------------------------------------------------------------------------
# Variável de abertura (`por`)

# Acha, entre as opções do site, a variável que o usuário pediu em
# linguagem comum ("sexo", "faixa etaria", "raca").
achar_coluna <- function(cfg, por) {
  op <- tabnet_opcoes(cfg$fonte, "colunas")
  op <- op[!grepl("^-", op)]
  a <- normalizar_rotulo(por)
  r <- normalizar_rotulo(op)
  pos <- which(r == a)
  if (length(pos) == 0) pos <- which(startsWith(r, a))
  if (length(pos) == 0) pos <- which(grepl(a, r, fixed = TRUE))
  if (length(pos) == 0) {
    erro("LDS-10", "N\u00e3o achei \"", por, "\" em ", cfg$fonte, ". Op\u00e7\u00f5es:\n  ",
         paste(op, collapse = "\n  "))
  }
  if (length(pos) > 1) {
    message("Usando \"", op[pos[1]], "\" (tamb\u00e9m existem: ",
            paste(op[pos[-1]], collapse = "; "), ").")
  }
  op[pos[1]]
}

# Nome de coluna "de gente" para a variável: "Faixa Etária" -> "faixa_etaria".
nome_coluna <- function(x) {
  gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", normalizar_rotulo(x)))
}

# ---------------------------------------------------------------------------
# Execução e tabela final

# Para cada ano, a primeira fonte candidata que tem aquele ano.
dividir_anos <- function(fontes, anos) {
  resto <- as.character(anos)
  plano <- list()
  for (f in fontes) {
    cfg <- config_fonte(f)
    disp <- anos_fonte(cfg, tn_formulario(cfg$url_def, cfg$dialeto))
    aqui <- intersect(resto, disp)
    if (length(aqui) > 0) plano[[f]] <- as.integer(aqui)
    resto <- setdiff(resto, aqui)
  }
  if (length(resto) > 0) {
    avisar("LDS-11", "Sem dados para: ", paste(resto, collapse = ", "), ".")
  }
  if (length(plano) == 0) erro("LDS-11", "Nenhum dos anos pedidos est\u00e1 dispon\u00edvel.")
  plano
}

# Consulta as fontes e devolve a tabela simples.
consultar_simples <- function(fontes, anos, cid, por, lugar, contagem) {
  plano <- dividir_anos(fontes, anos)
  partes <- list()
  nome_por <- NULL
  for (f in names(plano)) {
    cfg  <- config_fonte(f)
    form <- tn_formulario(cfg$url_def, cfg$dialeto)
    coluna <- if (is.null(por)) NULL else achar_coluna(cfg, por)
    if (!is.null(por) && is.null(nome_por)) nome_por <- nome_coluna(por)
    municipios <- if (!vazio(cfg$filtro_mun) && !is.na(lugar$codmun)) lugar$codmun else NULL
    r <- tabnet_bairro(f, plano[[f]], coluna = coluna, filtros = filtros_cid(cfg, form, cid),
                       municipios = municipios)
    # Fonte estadual e pedido de UM município: fica só com ele.
    if (!is.na(lugar$codmun)) r <- r[!is.na(r$codmun) & r$codmun == lugar$codmun, ]
    if (nrow(r) == 0) next
    r$unidade <- cfg$unidade
    partes[[length(partes) + 1]] <- data.table::as.data.table(r)
  }
  d <- data.table::rbindlist(partes, fill = TRUE)
  if (nrow(d) == 0) erro("LDS-12", "Nenhum registro encontrado.")
  arrumar_tabela(d, nome_por, contagem)
}

# Tabela final: uma linha por bairro do IBGE (ou nome do site, quando não
# ligado), ano e categoria, com nomes de colunas legíveis.
arrumar_tabela <- function(d, nome_por, contagem) {
  if (!"categoria" %in% names(d)) d[, categoria := NA_character_]
  ligado <- foi_ligado(d$ligacao)
  d[, chave := data.table::fifelse(ligado, id_unidade, paste0("site:", bairro_tabnet))]
  d[, nome_final := data.table::fifelse(ligado, nome_ibge, bairro_tabnet)]
  t <- d[, .(n = sum(n),
             bairro_no_site = paste(unique(bairro_tabnet), collapse = " / "),
             nome_final = nome_final[1], populacao = populacao[1], lat = lat[1], lon = lon[1],
             codigo_ibge = cd_ibge[1], ligacao = ligacao[1], fonte = fonte[1], unidade = unidade[1]),
         by = .(ano, codmun, municipio, chave, categoria)]
  t[, taxa_por_10mil := round(10000 * n / populacao, 2)]
  unidade <- t$unidade[1]
  saida <- t[, .(ano, codigo_municipio = codmun, municipio, nome_final, bairro_no_site,
                 categoria, n, populacao, taxa_por_10mil, lat, lon, codigo_ibge, ligacao, fonte)]
  data.table::setnames(saida, c("nome_final", "n"), c(unidade, contagem))
  if (is.null(nome_por)) saida[, categoria := NULL] else data.table::setnames(saida, "categoria", nome_por)
  saida <- as.data.frame(saida)
  # order() da base respeita acentos (\u00c1gua Rasa antes de Alto...).
  saida <- saida[order(saida$ano, saida$municipio, saida[[unidade]]), ]
  rownames(saida) <- NULL
  ok <- foi_ligado(saida$ligacao)
  pct <- 100 * sum(saida[[contagem]][ok]) / sum(saida[[contagem]])
  message(sprintf("Pronto: %s %s em %d %ss; %.1f%% ligados a um %s do IBGE.",
                  format(sum(saida[[contagem]]), big.mark = ".", decimal.mark = ","), contagem,
                  length(unique(saida[[unidade]][ok])), unidade, pct, unidade))
  saida
}

# ---------------------------------------------------------------------------
# Funções públicas

#' Óbitos por bairro
#'
#' Conta os óbitos de residentes por bairro (ou distrito), ano a ano, com
#' população, taxa por 10 mil habitantes e coordenadas de cada bairro. A
#' fonte é escolhida sozinha pelo lugar: TabNet da Prefeitura do Rio
#' (2006+), da Prefeitura de São Paulo (distritos, 2006+), da Prefeitura de
#' Fortaleza (1999+, só capítulos da CID) ou da SES-RJ (demais municípios
#' do RJ, bairro a partir de 2011). Veja [onde_tem_bairro()].
#'
#' @param municipio Nome ou código IBGE do município, por exemplo
#'   `"Rio de Janeiro"`, `"Niterói"` ou `"355030"`.
#' @param uf Sigla da UF, para pegar o estado inteiro (`uf = "RJ"`) ou para
#'   desfazer nomes repetidos de município.
#' @param anos Anos do óbito, por exemplo `2023` ou `2019:2023`.
#' @param cid Causa básica (CID-10): letras para capítulos (`"I"` =
#'   circulatório, `c("I", "J")`) ou códigos de 3 caracteres (`"I21"`,
#'   `"I20-I25"`). `NULL` = todas as causas.
#' @param por Variável para abrir as contagens, em linguagem comum:
#'   `"sexo"`, `"faixa etaria"`, `"raca"`, `"escolaridade"`...
#' @return Um `data.frame` com `ano`, `codigo_municipio`, `municipio`,
#'   `bairro` (ou `distrito`), `bairro_no_site` (como o site escreve),
#'   a variável de `por`, `obitos`, `populacao` (Censo 2022),
#'   `taxa_por_10mil`, `lat`, `lon`, `codigo_ibge`, `ligacao` (como o nome
#'   foi ligado ao IBGE) e `fonte`.
#' @export
#' @examples
#' \dontrun{
#' obitos_bairro("Rio de Janeiro", 2023, cid = "I")
#' obitos_bairro("Rio de Janeiro", 2019:2023, cid = c("I", "J"), por = "sexo")
#' obitos_bairro("Niterói", 2022, cid = "I20-I25")
#' obitos_bairro(uf = "RJ", anos = 2023)
#' obitos_bairro("São Paulo", 2023, por = "faixa etaria")
#' }
obitos_bairro <- function(municipio = NULL, anos, cid = NULL, por = NULL, uf = NULL) {
  lugar <- achar_lugar(municipio, uf)
  if (identical(lugar$codmun, COD_RIO)) {
    fonte <- "RIO-SIM"
  } else if (identical(lugar$codmun, COD_SAO_PAULO)) {
    fonte <- "SP-SIM"
  } else if (identical(lugar$codmun, COD_FORTALEZA)) {
    fonte <- "FOR-SIM"
  } else if (lugar$uf == "RJ") {
    fonte <- "SES-RJ-SIM"
  } else {
    sem_fonte("\u00f3bitos", lugar)
  }
  consultar_simples(fonte, anos, cid, por, lugar, "obitos")
}

#' Nascimentos por bairro
#'
#' Conta os nascidos vivos de mães residentes por bairro (ou distrito),
#' ano a ano, com população, taxa por 10 mil e coordenadas. Fontes: TabNet
#' das Prefeituras do Rio, de São Paulo e de Fortaleza; nos demais municípios do RJ, os
#' microdados da SES-RJ, localizados pelo CEP e pelo nome do bairro da mãe.
#'
#' @inheritParams obitos_bairro
#' @param anos Anos de nascimento.
#' @param por Variável para abrir as contagens: `"sexo"`, `"tipo de parto"`,
#'   `"idade da mae"`, `"consultas"`...
#' @return Como em [obitos_bairro()], com `nascimentos` no lugar de `obitos`.
#' @export
#' @examples
#' \dontrun{
#' nascimentos_bairro("Rio de Janeiro", 2023)
#' nascimentos_bairro("Niterói", 2022:2023, por = "tipo de parto")
#' }
nascimentos_bairro <- function(municipio = NULL, anos, por = NULL, uf = NULL) {
  lugar <- achar_lugar(municipio, uf)
  if (identical(lugar$codmun, COD_RIO)) {
    return(consultar_simples("RIO-SINASC", anos, NULL, por, lugar, "nascimentos"))
  }
  if (identical(lugar$codmun, COD_SAO_PAULO)) {
    return(consultar_simples("SP-SINASC", anos, NULL, por, lugar, "nascimentos"))
  }
  if (identical(lugar$codmun, COD_FORTALEZA)) {
    return(consultar_simples("FOR-SINASC", anos, NULL, por, lugar, "nascimentos"))
  }
  if (lugar$uf != "RJ") sem_fonte("nascimentos", lugar)
  # Demais municípios do RJ: microdados da SES-RJ, agregados aqui.
  d <- baixar_bairro("SINASC-RJ", "RJ", min(anos), max(anos))
  d <- d[d$ano_ref %in% anos, ]
  if (!is.na(lugar$codmun)) d <- d[!is.na(d$codmun_paciente) & d$codmun_paciente == lugar$codmun, ]
  agregar_registros(d, por, "nascimentos", "SINASC-RJ")
}

# Agrega registros (saída de baixar_bairro) na tabela simples.
agregar_registros <- function(d, por, contagem, fonte) {
  d <- data.table::as.data.table(d)
  col_por <- NULL
  if (!is.null(por)) {
    op <- names(d)[!grepl("c\u00f3digo|codigo", names(d), ignore.case = TRUE)]
    r <- normalizar_rotulo(op)
    # Nome exato primeiro ("sexo" e não "CS_SEXO"); senão, contém.
    pos <- which(r == normalizar_rotulo(por))
    if (length(pos) == 0) pos <- which(grepl(normalizar_rotulo(por), r, fixed = TRUE))
    if (length(pos) == 0) erro("LDS-10", "N\u00e3o achei \"", por, "\" nas colunas da base.")
    if (length(pos) > 1) message("Usando \"", op[pos[1]], "\".")
    col_por <- op[pos[1]]
    d[, categoria := get(col_por)]
  } else {
    d[, categoria := NA_character_]
  }
  muns <- unique(setores_censo()[, .(codmun_paciente = codmun, municipio = NM_MUN)])
  d <- muns[d, on = "codmun_paciente"]
  d[, `:=`(ano = ano_ref, codmun = codmun_paciente, n = 1L, fonte = fonte, unidade = "bairro",
           ligacao = data.table::fifelse(is.na(id_bairro), "sem bairro", "exata"),
           id_unidade = id_bairro, nome_ibge = bairro, cd_ibge = cd_bairro_ibge,
           lat = lat_bairro, lon = lon_bairro,
           bairro_tabnet = data.table::fifelse(is.na(bairro), "(sem bairro)", bairro))]
  ufs <- names(.ufs)[.ufs %in% unique(substr(stats::na.omit(d$codmun), 1, 2))]
  pop <- data.table::rbindlist(lapply(ufs, function(u) data.table::as.data.table(populacao_bairro(u))))
  pop <- pop[, .(cd_ibge = cd_bairro_ibge, populacao)]
  d <- pop[d, on = "cd_ibge"]
  arrumar_tabela(d[, .(ano, codmun, municipio, categoria, n, ligacao, id_unidade, nome_ibge,
                       bairro_tabnet, populacao, lat, lon, cd_ibge, fonte, unidade)],
                 if (is.null(por)) NULL else nome_coluna(por), contagem)
}

#' Casos de agravos de notificação por bairro
#'
#' Conta casos do SINAN (dengue, tuberculose, sífilis, violência...) por
#' bairro (ou distrito) de residência, com população, taxa por 10 mil e
#' coordenadas. Fontes: TabNets da Prefeitura do Rio, da Prefeitura de
#' São Paulo e da DIVE de Santa Catarina (todos os municípios) e, no
#' Recife, os registros de dengue, chikungunya e zika publicados pela
#' Prefeitura (a dengue traz os casos notificados; use
#' `por = "classificacao"` para separar confirmados e descartados). Veja
#' [onde_tem_bairro()].
#'
#' @inheritParams obitos_bairro
#' @param agravo Nome do agravo, em linguagem comum: `"dengue"`,
#'   `"tuberculose"`, `"sifilis congenita"`, `"violencia"`, `"zika"`...
#' @param anos Anos (de notificação ou de diagnóstico, conforme o site).
#' @param por Variável para abrir as contagens, por exemplo `"sexo"`.
#' @return Como em [obitos_bairro()], com `casos` no lugar de `obitos`.
#' @export
#' @examples
#' \dontrun{
#' agravos_bairro("dengue", "Rio de Janeiro", 2024)
#' agravos_bairro("tuberculose", "São Paulo", 2022:2024)
#' agravos_bairro("dengue", "Joinville", 2024, por = "sexo")
#' }
agravos_bairro <- function(agravo, municipio = NULL, anos, por = NULL, uf = NULL) {
  lugar <- achar_lugar(municipio, uf)
  if (identical(lugar$codmun, COD_RIO)) {
    prefixo <- "RIO-SINAN-"
  } else if (identical(lugar$codmun, COD_SAO_PAULO)) {
    prefixo <- "SP-SINAN-"
  } else if (lugar$uf == "SC") {
    prefixo <- "SC-SINAN-"
  } else if (identical(lugar$codmun, COD_RECIFE)) {
    return(agravos_recife(agravo, anos, por))
  } else {
    sem_fonte(paste("casos de", agravo), lugar)
  }
  f <- .fontes_tabnet[startsWith(.fontes_tabnet$fonte, prefixo), ]
  chave <- normalizar_rotulo(gsub("-", " ", sub(prefixo, "", f$fonte, fixed = TRUE)))
  desc  <- normalizar_rotulo(f$descricao)
  a <- normalizar_rotulo(agravo)
  pega <- startsWith(chave, a) | grepl(a, desc, fixed = TRUE)
  if (!any(pega)) {
    erro("LDS-06", "Agravo \"", agravo, "\" n\u00e3o dispon\u00edvel aqui. Op\u00e7\u00f5es: ",
         paste(tolower(sub(prefixo, "", f$fonte, fixed = TRUE)), collapse = ", "), ".")
  }
  # Vários resultados só são aceitos se forem o mesmo agravo em períodos
  # diferentes (ex.: dengue 2007-2011 e 2012+ no Rio).
  base <- unique(sub(" [0-9]{4}$", "", chave[pega]))
  if (length(base) > 1) {
    erro("LDS-07", "\"", agravo, "\" \u00e9 amb\u00edguo: ", paste(tolower(sub(prefixo, "", f$fonte[pega], fixed = TRUE)),
                                                     collapse = ", "), ".")
  }
  fontes <- f$fonte[pega][order(grepl("[0-9]{4}$", chave[pega]))]
  consultar_simples(fontes, anos, NULL, por, lugar, "casos")
}

#' Internações por bairro
#'
#' Conta as internações do SUS (AIHs aprovadas) de residentes por bairro,
#' pela data de internação, a partir do CEP do paciente. Atalho para
#' [sih_bairro()] com a mesma sintaxe das outras funções simples.
#'
#' @inheritParams obitos_bairro
#' @param uf Sigla da UF. Opcional quando `municipio` é dado (a UF sai do
#'   município); com `municipio = NULL`, pega a UF inteira.
#' @param anos Anos de internação.
#' @param cid Diagnóstico principal: letras (`"I"`) ou códigos (`"I21"`).
#' @return Um `data.frame` por bairro e ano com `internacoes`,
#'   `obitos_hosp`, `populacao`, `taxa_10mil` e coordenadas.
#' @export
#' @examples
#' \dontrun{
#' internacoes_bairro("Rio de Janeiro", 2023, cid = c("I", "J"), uf = "RJ")
#' }
internacoes_bairro <- function(municipio = NULL, anos, cid = NULL, uf = NULL) {
  lugar <- achar_lugar(municipio, uf)
  if (!is.null(cid)) cid <- expandir_cid(toupper(cid))
  x <- sih_bairro(lugar$uf, min(anos), max(anos), cid = cid)
  x <- x[x$ano %in% anos, ]
  if (!is.na(lugar$codmun)) x <- x[x$codmun == lugar$codmun, ]
  x
}

#' Onde há dados por bairro
#'
#' Lista, em linguagem simples, que dados por bairro existem, para que
#' lugar e para que anos.
#'
#' @return Um `data.frame` com `dado` (o nome a usar, por exemplo, em
#'   `agravos_bairro("dengue", ...)`), `descricao`, `local`, `unidade`,
#'   `funcao` (a função que busca) e `fonte`.
#' @export
#' @examples
#' onde_tem_bairro()
onde_tem_bairro <- function() {
  f <- .fontes_tabnet
  prefixo <- sub("-.*", "", f$fonte)          # "RIO", "SP", "SC", "FOR" ou "SES"
  local <- vapply(prefixo, function(p) switch(p,
    RIO = "Rio de Janeiro (capital)",
    SP  = "S\u00e3o Paulo (capital)",
    SC  = "Santa Catarina (todos os munic\u00edpios)",
    FOR = "Fortaleza (capital)",
    "Rio de Janeiro (todos os munic\u00edpios)"
  ), character(1), USE.NAMES = FALSE)
  funcao <- vapply(f$sistema, function(s) switch(s,
    SIM    = "obitos_bairro()",
    SINASC = "nascimentos_bairro()",
    "agravos_bairro()"
  ), character(1), USE.NAMES = FALSE)
  dado <- vapply(seq_along(funcao), function(i) switch(funcao[i],
    "obitos_bairro()"      = "\u00f3bitos",
    "nascimentos_bairro()" = "nascimentos",
    gsub("-", " ", tolower(sub("^[A-Z]+-SINAN-", "", f$fonte[i])))
  ), character(1))
  tab <- data.frame(dado, descricao = f$descricao, local, unidade = f$unidade, funcao,
                    fonte = f$fonte, stringsAsFactors = FALSE)
  extra <- data.frame(
    dado = c("nascimentos", "interna\u00e7\u00f5es", "dengue", "chikungunya", "zika",
             "atendimentos m\u00e9dicos", "estabelecimentos de sa\u00fade", "chamados do SAMU"),
    descricao = c("Nascidos vivos (CEP e bairro da m\u00e3e), 1996 em diante",
                  "AIHs aprovadas (CEP do paciente)",
                  .sistemas$descricao[match(c("DENGUE-RECIFE", "CHIKUNGUNYA-RECIFE", "ZIKA-RECIFE"),
                                            .sistemas$sistema)],
                  "Atendimentos m\u00e9dicos da rede municipal (e-Sa\u00fade), 2019 em diante",
                  "Estabelecimentos ativos do CNES (local do estabelecimento, n\u00e3o do paciente)",
                  "Solicita\u00e7\u00f5es ao SAMU 192 (local da ocorr\u00eancia), 2016 em diante"),
    local = c("Rio de Janeiro (todos os munic\u00edpios)", "todas as UFs", rep("Recife (capital)", 3),
              "Curitiba (capital)", "todas as UFs", "Recife e regi\u00e3o metropolitana"),
    unidade = "bairro",
    funcao = c("nascimentos_bairro()", "internacoes_bairro()", rep("agravos_bairro()", 3),
               "atendimentos_bairro()", "estabelecimentos_bairro()", "samu_bairro()"),
    fonte = c("SINASC-RJ", "SIH-RD", "DENGUE-RECIFE", "CHIKUNGUNYA-RECIFE", "ZIKA-RECIFE",
              "e-Sa\u00fade Curitiba", "CNES (dados abertos)", "SAMU 192 Recife"),
    stringsAsFactors = FALSE)
  tab <- rbind(tab, extra)
  # A dengue do Rio tem duas bases (2007-2011 e 2012+); para quem usa, é uma só.
  tab <- tab[!grepl(" [0-9]{4}$", tab$dado), ]
  tab <- tab[order(tab$funcao, tab$local, tab$dado), ]
  rownames(tab) <- NULL
  tab
}

sem_fonte <- function(o_que, lugar) {
  onde <- if (is.na(lugar$codmun)) lugar$uf else paste0(lugar$nome, " (", lugar$uf, ")")
  erro("LDS-05", "N\u00e3o h\u00e1 fonte p\u00fablica de ", o_que, " por bairro para ", onde,
       ". Veja onde_tem_bairro().")
}

# Recife: registros individuais da Prefeitura, agregados aqui.
agravos_recife <- function(agravo, anos, por) {
  opcoes <- c("dengue", "chikungunya", "zika")
  pega <- opcoes[startsWith(opcoes, normalizar_rotulo(agravo))]
  if (length(pega) != 1) {
    erro("LDS-06", "Agravo \"", agravo, "\" n\u00e3o dispon\u00edvel no Recife. Op\u00e7\u00f5es: ",
         paste(opcoes, collapse = ", "), ".")
  }
  sistema <- paste0(toupper(pega), "-RECIFE")
  d <- baixar_bairro(sistema, "PE", min(anos), max(anos))
  d <- d[d$ano_ref %in% anos & !is.na(d$codmun_paciente) & d$codmun_paciente == COD_RECIFE, ]
  if (nrow(d) == 0) erro("LDS-12", "Nenhum caso de ", pega, " no Recife em ", paste(anos, collapse = ", "), ".")
  agregar_registros(d, por, "casos", sistema)
}
