# Leitor genérico de TabNet: consulta tabulações de secretarias estaduais e
# municipais que publicam dados por bairro (ou distrito).
#
# Há dois "dialetos" de TabNet, com o MESMO formulário (campos Linha, Coluna,
# Incremento, período e filtros "S..."), mas respostas diferentes:
#   - "tabnet": TabNet clássico (Linux "dh"/"tabnet" e Win32 "deftohtm"/"tabcgi").
#     Pedimos formato=prn e lemos o texto entre <PRE>.
#   - "dhx": TabNet da SES-RJ (dhx.exe/webtabx.exe). A resposta traz um link
#     para um CSV.

# Formulários já lidos nesta sessão (evita baixar a página toda a cada consulta).
.formularios <- new.env(parent = emptyenv())

# Percent-encoding byte a byte em latin1: os TabNets não entendem UTF-8.
codificar_latin1 <- function(x) {
  vapply(x, function(texto) {
    bytes  <- as.integer(charToRaw(iconv(texto, from = "UTF-8", to = "latin1", sub = "?")))
    seguro <- bytes %in% c(48:57, 65:90, 97:122, 45, 46, 95, 126)
    paste(ifelse(seguro, intToUtf8(bytes, multiple = TRUE), sprintf("%%%02X", bytes)),
          collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# Troca entidades HTML (&aacute; etc.) pelos caracteres.
decodificar_html <- function(x) {
  ent <- c(aacute = "\u00e1", Aacute = "\u00c1", agrave = "\u00e0", Agrave = "\u00c0",
           acirc = "\u00e2", Acirc = "\u00c2", atilde = "\u00e3", Atilde = "\u00c3",
           eacute = "\u00e9", Eacute = "\u00c9", ecirc = "\u00ea", Ecirc = "\u00ca",
           iacute = "\u00ed", Iacute = "\u00cd", oacute = "\u00f3", Oacute = "\u00d3",
           ocirc = "\u00f4", Ocirc = "\u00d4", otilde = "\u00f5", Otilde = "\u00d5",
           uacute = "\u00fa", Uacute = "\u00da", uuml = "\u00fc", Uuml = "\u00dc",
           ccedil = "\u00e7", Ccedil = "\u00c7", nbsp = " ", quot = "\"", amp = "&")
  for (e in names(ent)) x <- gsub(paste0("&", e, ";"), ent[[e]], x, fixed = TRUE)
  x
}

# GET (corpo = NULL) ou POST; devolve o texto em UTF-8. Tenta 3 vezes, porque
# alguns servidores de TabNet falham de vez em quando (502, tempo esgotado).
tn_http <- function(url, corpo = NULL, tentativas = 3) {
  for (i in seq_len(tentativas)) {
    h <- curl::new_handle(timeout = 600, useragent = "localdatasus (pacote R)")
    if (!is.null(corpo)) {
      curl::handle_setopt(h, post = TRUE, postfields = corpo)
      curl::handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
    }
    r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (!inherits(r, "error") && r$status_code == 200) {
      return(iconv(rawToChar(r$content), from = "latin1", to = "UTF-8"))
    }
    motivo <- if (inherits(r, "error")) conditionMessage(r) else paste("HTTP", r$status_code)
    if (i < tentativas) Sys.sleep(5 * i)
  }
  erro("LDS-13", "Falha ao acessar ", url, " (", motivo, ")")
}

# Lê o formulário de uma página .def: para cada <SELECT>, o nome, o id e as
# opções (valor e rótulo); mais a URL de envio e os campos ocultos.
tn_formulario <- function(url_def, dialeto) {
  if (!is.null(.formularios[[url_def]])) return(.formularios[[url_def]])
  html <- tn_http(url_def)
  blocos <- regmatches(html, gregexpr("(?s)<SELECT[^>]*>.*?</SELECT>", html,
                                      perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(blocos) == 0) erro("LDS-14", "P\u00e1gina sem formul\u00e1rio de TabNet: ", url_def)
  cab   <- sub("(?s)>.*", "", blocos, perl = TRUE)
  nomes <- sub('(?i).*NAME="([^"]*)".*', "\\1", cab, perl = TRUE)
  ids   <- ifelse(grepl('(?i)ID="', cab, perl = TRUE), sub('(?i).*ID="([^"]*)".*', "\\1", cab, perl = TRUE), "")
  opcoes <- lapply(blocos, function(b) {
    m <- regmatches(b, gregexpr('(?i)<OPTION[^>]*VALUE="([^"]*)"[^>]*>([^\n<]*)', b, perl = TRUE))[[1]]
    data.frame(valor  = sub('(?i).*VALUE="([^"]*)".*', "\\1", m, perl = TRUE),
               rotulo = trimws(decodificar_html(sub(".*>", "", m))),
               stringsAsFactors = FALSE)
  })
  names(opcoes) <- nomes

  # Para onde enviar a consulta.
  acao <- regmatches(html, regexpr('(?i)<FORM[^>]*ACTION="[^"]*"', html, perl = TRUE))
  if (dialeto == "dhx") {
    envio <- sub("dhx.exe", "webtabx.exe", url_def, fixed = TRUE)
  } else if (length(acao) == 1) {
    caminho <- sub('(?i).*ACTION="([^"]*)"', "\\1", acao, perl = TRUE)
    raiz    <- sub("^(https?://[^/]+).*", "\\1", url_def)
    envio   <- if (startsWith(caminho, "http")) caminho else paste0(raiz, caminho)
  } else {
    erro("LDS-14", "N\u00e3o achei a URL de envio do formul\u00e1rio em ", url_def)
  }
  ocultos <- regmatches(html, gregexpr('(?i)<INPUT[^>]*TYPE="?hidden"?[^>]*>', html, perl = TRUE))[[1]]
  ocultos <- stats::setNames(sub('(?i).*VALUE="([^"]*)".*', "\\1", ocultos, perl = TRUE),
                             sub('(?i).*NAME="([^"]*)".*', "\\1", ocultos, perl = TRUE))

  f <- list(opcoes = opcoes, ids = stats::setNames(ids, nomes), envio = envio,
            ocultos = ocultos, dialeto = dialeto)
  assign(url_def, f, envir = .formularios)
  f
}

# Normaliza rótulos para comparação: sem acento, minúsculo, "_" vira espaço.
normalizar_rotulo <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("_", " ", x))
  trimws(gsub("\\s+", " ", x))
}

# Nome do <SELECT> de filtro que corresponde a um rótulo, ex.: "Sexo" -> "SSexo".
tn_campo_filtro <- function(form, rotulo) {
  nomes <- names(form$opcoes)
  cand  <- nomes[startsWith(nomes, "S") & nomes != "SLinha"]
  alvo  <- normalizar_rotulo(rotulo)
  base  <- normalizar_rotulo(substring(cand, 2))
  pos <- which(base == alvo)
  if (length(pos) == 0) pos <- which(startsWith(base, alvo))
  if (length(pos) != 1) {
    erro("LDS-15", "Filtro '", rotulo, "' ", if (length(pos) == 0) "n\u00e3o encontrado" else "amb\u00edguo",
         ". Veja tabnet_opcoes(fonte, \"filtros\").")
  }
  cand[pos]
}

# Valores das opções de um campo, escolhidas pelo rótulo: primeiro o rótulo
# idêntico, depois o rótulo que COMEÇA com o texto pedido.
tn_valores <- function(form, campo, rotulos) {
  op <- form$opcoes[[campo]]
  rot <- normalizar_rotulo(op$rotulo)
  vapply(rotulos, function(r) {
    alvo <- normalizar_rotulo(r)
    # 1) rótulo idêntico; 2) começa com o texto e logo depois vem um
    # separador ("X" acha "X. Doenças..." mas não "XI. ..."); 3) só começa.
    pos <- which(rot == alvo)
    if (length(pos) == 0) {
      seguinte <- substr(rot, nchar(alvo) + 1, nchar(alvo) + 1)
      pos <- which(startsWith(rot, alvo) & !grepl("[a-z0-9]", seguinte))
    }
    if (length(pos) == 0) pos <- which(startsWith(rot, alvo))
    if (length(pos) != 1) {
      exemplos <- paste(utils::head(op$rotulo[if (length(pos) > 1) pos else seq_along(rot)], 8),
                        collapse = "\n  ")
      erro("LDS-15", "Op\u00e7\u00e3o '", r, "' do campo '", sub("^S", "", gsub("_", " ", campo)), "' ",
           if (length(pos) == 0) "n\u00e3o encontrada" else "amb\u00edgua",
           ". Algumas op\u00e7\u00f5es:\n  ", exemplos)
    }
    op$valor[pos]
  }, character(1), USE.NAMES = FALSE)
}

# Nome do <SELECT> de período (ID="A": "Arquivos" no clássico, "PAno ..." no dhx).
tn_campo_periodo <- function(form) {
  nome <- names(form$ids)[form$ids == "A"]
  if (length(nome) != 1) erro("LDS-14", "Formul\u00e1rio sem campo de per\u00edodo.")
  nome[1]
}

# Faz UMA consulta e devolve um data.frame longo (linha, categoria, n).
tn_consultar <- function(form, linha, coluna, incremento, periodos, filtros) {
  campo_per <- tn_campo_periodo(form)
  campos <- c(
    Linha      = tn_valores(form, "Linha", linha),
    Coluna     = if (is.null(coluna)) "--N\u00e3o-Ativa--" else tn_valores(form, "Coluna", coluna),
    Incremento = tn_valores(form, "Incremento", incremento)
  )
  campos <- c(campos, stats::setNames(tn_valores(form, campo_per, periodos),
                                      rep(campo_per, length(periodos))))
  for (f in names(filtros)) {
    nome <- tn_campo_filtro(form, f)
    campos <- c(campos, stats::setNames(tn_valores(form, nome, filtros[[f]]),
                                        rep(nome, length(filtros[[f]]))))
  }
  if (form$dialeto == "dhx") {
    campos <- c(campos, form$ocultos, grafico = "")
  } else {
    campos <- c(campos, formato = "prn", mostre = "Mostra")
  }
  corpo <- paste(paste0(codificar_latin1(names(campos)), "=", codificar_latin1(campos)),
                 collapse = "&")
  html <- tn_http(form$envio, corpo)

  if (grepl("Nenhum registro selecionado", html, fixed = TRUE)) {
    return(data.frame(linha = character(), categoria = character(), n = numeric()))
  }
  if (form$dialeto == "dhx") {
    link <- regmatches(html, regexpr("csv/[^ >\"']*\\.csv", html))
    if (length(link) == 0) erro("LDS-14", "Resposta inesperada do TabNet (sem link do CSV).")
    texto <- tn_http(paste0(sub("[^/]*$", "", sub("\\?.*", "", form$envio)), link))
  } else {
    pre <- regmatches(html, regexpr("(?s)<PRE>.*?</PRE>", html, perl = TRUE, ignore.case = TRUE))
    if (length(pre) == 0) erro("LDS-14", "Resposta inesperada do TabNet (sem tabela).")
    # Tira só tags de verdade (<PRE>, <B>...): rótulos como "<1 Ano" ficam.
    texto  <- decodificar_html(gsub("<[/!A-Za-z][^<>]*>", "", pre))
    linhas <- strsplit(texto, "\n")[[1]]
    texto  <- paste(linhas[grepl('^"', linhas)], collapse = "\n")
  }
  tab <- utils::read.table(text = texto, sep = ";", quote = "\"", header = TRUE,
                           check.names = FALSE, colClasses = "character",
                           comment.char = "", encoding = "UTF-8")
  tab <- tab[trimws(tab[[1]]) != "Total", trimws(names(tab)) != "Total", drop = FALSE]
  valores <- tab[, -1, drop = FALSE]
  n <- trimws(unlist(valores, use.names = FALSE))
  n[n %in% c("-", "")] <- "0"
  # Vírgula decimal (ex.: taxas "12,5"): o ponto, nesse caso, é de milhar.
  if (any(grepl(",", n, fixed = TRUE))) n <- gsub(",", ".", gsub(".", "", n, fixed = TRUE), fixed = TRUE)
  n <- as.numeric(n)
  categoria <- if (is.null(coluna)) NA_character_ else trimws(rep(names(valores), each = nrow(valores)))
  data.frame(linha     = trimws(rep(tab[[1]], times = ncol(valores))),
             categoria = categoria,
             n         = n, stringsAsFactors = FALSE)
}


#' Opções de um TabNet regional
#'
#' Mostra o que dá para pedir a [tabnet_bairro()]: as variáveis que podem
#' ir na coluna, os filtros, as opções de um filtro, as medidas
#' ("incrementos") e os anos disponíveis.
#'
#' @param fonte Código da fonte (ver [fontes_tabnet()]) ou uma linha no
#'   mesmo formato.
#' @param tipo `"colunas"`, `"filtros"`, `"incrementos"` ou `"anos"`.
#' @param filtro Com `tipo = "filtros"`, o nome de um filtro: lista as
#'   opções dele.
#' @return Um vetor de rótulos.
#' @export
#' @examplesIf interactive()
#' tabnet_opcoes("RIO-SIM", "colunas")
#' tabnet_opcoes("RIO-SIM", "filtros", filtro = "Sexo")
tabnet_opcoes <- function(fonte, tipo = c("colunas", "filtros", "incrementos", "anos"),
                          filtro = NULL) {
  tipo <- match.arg(tipo)
  cfg  <- config_fonte(fonte)
  form <- tn_formulario(cfg$url_def, cfg$dialeto)
  switch(tipo,
    colunas     = setdiff(form$opcoes$Coluna$rotulo, "N\u00e3o ativa"),
    incrementos = form$opcoes$Incremento$rotulo,
    anos        = anos_fonte(cfg, form),
    filtros     = {
      if (is.null(filtro)) {
        nomes <- names(form$opcoes)
        gsub("_", " ", substring(nomes[startsWith(nomes, "S") & form$ids[nomes] != "A"], 2))
      } else {
        setdiff(form$opcoes[[tn_campo_filtro(form, filtro)]]$rotulo, "Todas as categorias")
      }
    })
}

# Anos disponíveis: rótulos dos arquivos ou, se cada arquivo junta vários
# anos, as opções do filtro de ano.
anos_fonte <- function(cfg, form) {
  if (vazio(cfg$filtro_ano)) {
    a <- form$opcoes[[tn_campo_periodo(form)]]$rotulo
  } else {
    a <- form$opcoes[[tn_campo_filtro(form, cfg$filtro_ano)]]$rotulo
  }
  sort(a[grepl("^[0-9]{4}$", a)])
}

# Interpreta "Campo=Opção;Campo2=Opção" (coluna filtros_fixos do cadastro).
ler_filtros_fixos <- function(x) {
  if (vazio(x)) return(list())
  pares <- strsplit(strsplit(x, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  stats::setNames(lapply(pares, `[`, 2), vapply(pares, `[`, character(1), 1))
}

# Uma consulta com cache em disco. Anos já encerrados ficam guardados; o ano
# corrente (dados provisórios) é sempre consultado de novo.
tn_consultar_cache <- function(cfg, form, ano, coluna, incremento, filtros) {
  chave <- paste(cfg$url_def, cfg$linha, ano, coluna, incremento,
                 paste(names(filtros), vapply(filtros, paste, character(1), collapse = ","),
                       collapse = "|"), sep = "#")
  tmp <- tempfile(); writeLines(chave, tmp, useBytes = TRUE)
  pasta <- file.path(pasta_cache(), "tabnet")
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  arq <- file.path(pasta, paste0(unname(tools::md5sum(tmp)), ".rds"))
  unlink(tmp)
  if (file.exists(arq)) return(readRDS(arq))
  if (vazio(cfg$filtro_ano)) {
    periodos <- ano
  } else {
    # Arquivos com vários anos: consulta todos e filtra o ano pedido.
    periodos <- form$opcoes[[tn_campo_periodo(form)]]$rotulo
    filtros  <- c(filtros, stats::setNames(list(ano), cfg$filtro_ano))
  }
  r <- tn_consultar(form, cfg$linha, coluna, incremento, periodos, filtros)
  Sys.sleep(1)   # gentileza com o servidor
  if (as.integer(ano) < as.integer(format(Sys.Date(), "%Y"))) saveRDS(r, arq)
  r
}

#' Consulta um TabNet regional por bairro
#'
#' Baixa de um TabNet de secretaria estadual ou municipal (ver
#' [fontes_tabnet()]) as contagens por bairro (ou distrito) de residência,
#' ano a ano, e liga cada bairro ao bairro do IBGE (código, coordenadas e
#' população) com [ligar_bairros()].
#'
#' São dados AGREGADOS: não há registro individual nem CEP. Cada consulta
#' cruza só duas variáveis (bairro x `coluna`); para mais dimensões, use
#' `filtros` e repita a chamada. As consultas ficam em cache.
#'
#' Nas fontes estaduais em que o rótulo do bairro não diz o município
#' (como a DIVE-SC), é feita uma consulta por município: primeiro descobre
#' quais municípios têm registros no ano, depois consulta cada um. Use
#' `municipios` para limitar.
#'
#' @param fonte Código da fonte (ver [fontes_tabnet()]) ou uma linha no
#'   mesmo formato, para usar um TabNet que ainda não está no cadastro.
#' @param anos Anos (de notificação, óbito ou nascimento, conforme a base).
#' @param coluna Rótulo de uma variável para abrir as contagens (ex.:
#'   `"Sexo"`, `"Faixa Etária"`); veja `tabnet_opcoes(fonte, "colunas")`.
#'   `NULL` = só o total por bairro.
#' @param filtros Lista nomeada: nome do filtro = rótulo(s) das opções
#'   aceitas, por exemplo `list("Causa (Cap CID10)" = c("IX", "X"))`. O
#'   rótulo pode ser só o começo do texto. Veja `tabnet_opcoes(fonte, "filtros")`.
#' @param incremento O que contar, quando a fonte oferece mais de uma
#'   medida; veja `tabnet_opcoes(fonte, "incrementos")`.
#' @param municipios Códigos de município (6 dígitos) a consultar, nas
#'   fontes estaduais. `NULL` = todos os que têm registros.
#' @param ligar Se `TRUE`, liga os nomes aos bairros/distritos do IBGE.
#' @return Um `data.frame` com `fonte`, `ano`, `codmun`, `municipio`,
#'   `bairro_tabnet` (como veio do site), `categoria` (valor de `coluna`),
#'   `n` e, com `ligar = TRUE`, as colunas de [ligar_bairros()]
#'   (`ligacao`, `id_unidade`, `nome_ibge`, `cd_ibge`, `lat`, `lon`,
#'   `populacao`) e `taxa_10mil` (por ano, sobre a população de 2022).
#' @export
#' @examplesIf interactive()
#' # Óbitos por doenças do aparelho circulatório, por bairro do Rio
#' tabnet_bairro("RIO-SIM", 2022:2023, coluna = "Faixa Etária",
#'               filtros = list("Causa (Cap CID10)" = "IX"))
#' # Dengue por bairro em Joinville (SC)
#' tabnet_bairro("SC-SINAN-DENGUE", 2024, municipios = "420910")
#' # Óbitos por distrito da cidade de São Paulo
#' tabnet_bairro("SP-SIM", 2023, coluna = "Sexo")
tabnet_bairro <- function(fonte, anos, coluna = NULL, filtros = list(), incremento = NULL,
                          municipios = NULL, ligar = TRUE) {
  cfg  <- config_fonte(fonte)
  form <- tn_formulario(cfg$url_def, cfg$dialeto)
  if (is.null(incremento)) {
    incremento <- if (vazio(cfg$incremento)) form$opcoes$Incremento$rotulo[1] else cfg$incremento
  }
  filtros <- c(ler_filtros_fixos(cfg$filtros_fixos), filtros)
  disponiveis <- anos_fonte(cfg, form)
  faltam <- setdiff(as.character(anos), disponiveis)
  if (length(faltam) > 0) {
    avisar("LDS-11", "Anos fora da base, ignorados: ", paste(faltam, collapse = ", "))
    anos <- intersect(as.character(anos), disponiveis)
  }
  por_municipio <- !vazio(cfg$filtro_mun)

  partes <- list()
  for (ano in as.character(anos)) {
    if (!por_municipio) {
      detalhar(cfg$fonte, " ", ano, "...")
      r <- tn_consultar_cache(cfg, form, ano, coluna, incremento, filtros)
      if (nrow(r) > 0) r$codmun <- if (vazio(cfg$codmun)) NA_character_ else cfg$codmun
    } else {
      # 1) Quais municípios têm registros neste ano?
      cfg_mun <- cfg; cfg_mun$linha <- cfg$filtro_mun
      muns <- tn_consultar_cache(cfg_mun, form, ano, NULL, incremento, filtros)
      muns <- muns[muns$n > 0 & grepl("^[0-9]{6}", muns$linha), ]
      cods <- substr(muns$linha, 1, 6)
      if (!is.null(municipios)) cods <- intersect(cods, substr(as.character(municipios), 1, 6))
      # 2) Uma consulta por município.
      r <- list()
      for (k in seq_along(cods)) {
        detalhar(sprintf("%s %s: munic\u00edpio %s (%d de %d)...", cfg$fonte, ano, cods[k], k, length(cods)))
        f <- c(filtros, stats::setNames(list(cods[k]), cfg$filtro_mun))
        rk <- tn_consultar_cache(cfg, form, ano, coluna, incremento, f)
        if (nrow(rk) > 0) rk$codmun <- cods[k]
        r[[k]] <- rk
      }
      r <- data.table::rbindlist(r, fill = TRUE)
    }
    if (nrow(r) == 0) next
    r$ano <- as.integer(ano)
    partes[[length(partes) + 1]] <- r
  }
  d <- data.table::rbindlist(partes, fill = TRUE)
  if (nrow(d) == 0) {
    avisar("LDS-12", cfg$fonte, ": nenhum registro em ", paste(anos, collapse = ", "), ".")
    return(data.frame(fonte = character(), ano = integer(), codmun = character(),
                      municipio = character(), bairro_tabnet = character(), n = numeric()))
  }

  # Alguns TabNets marcam a hierarquia com pontos ("..II CENTRO" = RA,
  # ".....CENTRO" = bairro): tiramos os pontos antes de aplicar as regras.
  d[, linha := sub("^\\.+", "", linha)]
  # Linhas que não são bairro (ex.: subtotais por AP e RA no TabNet do Rio).
  if (!vazio(cfg$descartar)) d <- d[!grepl(cfg$descartar, linha, perl = TRUE)]

  # Rótulo -> município e bairro.
  d[, bairro_tabnet := linha]
  if (cfg$rotulo == "codigo_nome") {
    d[, bairro_tabnet := sub("^[0-9]+\\s+", "", linha)]
  } else if (cfg$rotulo == "uf_municipio_bairro") {
    d[, `:=`(nome_mun = trimws(sub("^[A-Z]{2},\\s*", "", sub("\\s+-\\s.*$", "", linha))),
             uf_rot   = substr(linha, 1, 2),
             bairro_tabnet = trimws(sub("^.*?\\s+-\\s?", "", linha, perl = TRUE)))]
    muns <- unique(setores_censo()[CD_UF == codigo_uf(cfg$uf), .(codmun, NM_MUN)])
    muns[, chave := chave_bairro(NM_MUN)]
    d[, codmun := muns$codmun[match(chave_bairro(nome_mun), muns$chave)]]
    d[uf_rot != cfg$uf, codmun := NA_character_]
  }
  nomes_mun <- unique(setores_censo()[, .(codmun, municipio = NM_MUN)])
  d <- nomes_mun[d, on = "codmun"]

  d[, fonte := cfg$fonte]
  saida <- d[, .(fonte, ano, codmun, municipio, bairro_tabnet, categoria, n)]
  if (is.null(coluna)) saida[, categoria := NULL]

  if (ligar) {
    lig <- data.table::as.data.table(
      ligar_bairros(saida$bairro_tabnet, saida$codmun, cfg$uf, cfg$unidade))
    saida <- cbind(saida, lig[, .(ligacao, id_unidade, nome_ibge, fonte_unidade, cd_ibge,
                                  lat, lon, populacao)])
    saida[is.na(codmun) & ligacao != "ignorado", ligacao := "munic\u00edpio desconhecido"]
    saida[, taxa_10mil := round(10000 * n / populacao, 2)]
    resumo <- saida[, .(n = sum(n)), by = ligacao][, pct := round(100 * n / sum(n), 1)][order(-n)]
    attr(saida, "ligacao") <- as.data.frame(resumo)
    detalhar(sprintf("%.1f%% das contagens ligadas a um %s do IBGE.",
                    100 * sum(saida$n[foi_ligado(saida$ligacao)]) /
                      sum(saida$n), cfg$unidade))
  }
  as.data.frame(saida)
}
