# Mapas por bairro: estático (ggplot2 + sf) e interativo (leaflet).
# Os contornos vêm da malha de bairros e de distritos do Censo 2022 (IBGE).
# Bairros sem contorno (localidades do CNEFE) aparecem como pontos.

URL_MALHA <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/",
  "malha_com_atributos/%s/shp/UF/%s/%s_%s_CD2022.zip"
)

# Confere se os pacotes de mapa estão instalados.
precisa <- function(pacotes) {
  faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltam) > 0) {
    erro("LDS-20", "Para mapas, instale: install.packages(c(",
         paste0("\"", faltam, "\"", collapse = ", "), "))")
  }
}

# Malha (sf) de bairros ou distritos de uma UF, guardada em cache.
malha_ibge <- function(uf, unidade = c("bairro", "distrito")) {
  unidade <- match.arg(unidade)
  plural <- if (unidade == "bairro") "bairros" else "distritos"
  arq <- file.path(pasta_cache(), sprintf("malha_%s_%s.rds", plural, uf))
  if (file.exists(arq)) return(readRDS(arq))
  url <- sprintf(URL_MALHA, plural, uf, uf, plural)
  message("Baixando os contornos de ", plural, " de ", uf, " (IBGE, s\u00f3 na primeira vez)...")
  zip <- tryCatch(baixar(url, file.path(tempdir(), basename(url))), error = function(e) NULL)
  if (is.null(zip)) return(NULL)        # UF sem malha: o mapa usa pontos
  pasta <- file.path(tempdir(), paste0("malha_", plural, "_", uf))
  utils::unzip(zip, exdir = pasta)
  shp <- list.files(pasta, pattern = "\\.shp$", full.names = TRUE)[1]
  m <- sf::st_read(shp, quiet = TRUE)
  col <- if (unidade == "bairro") "CD_BAIRRO" else "CD_DIST"
  m <- m[, c("CD_MUN", col)]
  names(m)[1:2] <- c("codmun7", "codigo")
  m$codmun <- substr(m$codmun7, 1, 6)
  m <- sf::st_make_valid(m)
  # Alguns bairros vêm em mais de um pedaço: junta num polígono só.
  m$pedacos <- 1L
  m <- stats::aggregate(m[, "pedacos"], by = list(codigo = m$codigo, codmun = m$codmun), FUN = sum)
  saveRDS(m, arq)
  unlink(c(zip, pasta), recursive = TRUE)
  m
}

# Colunas conhecidas (das funções simples e das avançadas).
.colunas_fixas <- c("ano", "codigo_municipio", "municipio", "bairro", "distrito", "bairro_no_site",
                    "obitos", "nascimentos", "casos", "internacoes", "obitos_hosp", "n",
                    "populacao", "taxa_por_10mil", "taxa_10mil", "lat", "lon", "codigo_ibge",
                    "ligacao", "fonte", "codmun", "id_bairro", "fonte_bairro", "cd_bairro_ibge",
                    "lat_bairro", "lon_bairro", "suprimido", "codmun_paciente")
.rotulos_valor <- c(taxa_por_10mil = "por 10 mil hab.", taxa_10mil = "por 10 mil hab.",
                    obitos = "\u00f3bitos", nascimentos = "nascimentos", casos = "casos",
                    internacoes = "interna\u00e7\u00f5es", obitos_hosp = "\u00f3bitos hospitalares",
                    n = "registros", populacao = "popula\u00e7\u00e3o")

# Padroniza a tabela para o mapa: código, município, nome, coordenadas,
# valor a pintar e colunas para dividir em painéis (ano, categoria).
preparar_mapa <- function(dados, valor) {
  d <- as.data.frame(dados)
  # A primeira coluna que existir entre os nomes dados (as funções do pacote
  # usam nomes diferentes para a mesma coisa).
  pega <- function(...) {
    nome <- intersect(c(...), names(d))
    if (length(nome) == 0) return(NULL)
    d[[nome[1]]]
  }
  unidade <- if ("distrito" %in% names(d)) "distrito" else "bairro"
  codigo <- pega("codigo_ibge", "cd_bairro_ibge", "cd_ibge")
  lat <- pega("lat", "lat_bairro")
  if (is.null(codigo) && is.null(lat)) erro("LDS-21", "A tabela n\u00e3o tem c\u00f3digo IBGE nem coordenadas.")
  if (is.null(valor)) {
    candidatos <- c("taxa_por_10mil", "taxa_10mil", "obitos", "nascimentos", "casos", "internacoes", "n")
    valor <- candidatos[candidatos %in% names(d)][1]
  }
  if (is.na(valor) || !valor %in% names(d)) erro("LDS-17", "Coluna para pintar o mapa n\u00e3o encontrada: ", valor)
  # Painéis: o ano (se houver mais de um) e a variável de `por`.
  extras <- setdiff(names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))],
                    .colunas_fixas)
  paineis <- c(if ("ano" %in% names(d) && length(unique(d$ano)) > 1) "ano", extras)
  out <- data.frame(
    codigo = if (is.null(codigo)) NA_character_ else as.character(codigo),
    codmun = substr(as.character(pega("codigo_municipio", "codmun", "codmun_paciente")), 1, 6),
    nome   = as.character(pega(unidade, "bairro")),
    lat    = if (is.null(lat)) NA_real_ else lat,
    lon    = if (is.null(lat)) NA_real_ else pega("lon", "lon_bairro"),
    valor_mapa = d[[valor]],
    stringsAsFactors = FALSE
  )
  for (p in paineis) out[[p]] <- d[[p]]
  list(d = out, valor = valor, unidade = unidade, paineis = paineis,
       rotulo = if (valor %in% names(.rotulos_valor)) .rotulos_valor[[valor]] else valor,
       fonte = if ("fonte" %in% names(d)) paste(unique(d$fonte), collapse = ", ") else NULL,
       original = d)
}

# Contornos de todos os bairros/distritos dos municípios da tabela.
contornos <- function(p) {
  muns <- unique(stats::na.omit(p$d$codmun))
  ufs <- unique(names(.ufs)[match(substr(muns, 1, 2), .ufs)])
  m <- lapply(ufs, malha_ibge, unidade = p$unidade)
  m <- m[!vapply(m, is.null, logical(1))]
  if (length(m) == 0) return(NULL)
  m <- do.call(rbind, m)
  m[m$codmun %in% muns, ]
}

#' Mapa por bairro
#'
#' Desenha um mapa a partir da tabela devolvida por [obitos_bairro()],
#' [nascimentos_bairro()], [agravos_bairro()], [internacoes_bairro()] ou
#' [agregar_bairro()]. Cada bairro é pintado pelo valor escolhido (por
#' padrão, a taxa por 10 mil habitantes), com os contornos oficiais do
#' IBGE (Censo 2022). Bairros sem contorno oficial aparecem como pontos.
#'
#' Se a tabela tiver mais de um ano, ou uma variável de `por` (sexo, faixa
#' etária...), o mapa é dividido em painéis.
#'
#' O resultado é um gráfico `ggplot2` comum: dá para acrescentar títulos e
#' temas com `+`, e salvar com `ggplot2::ggsave()`.
#'
#' Precisa dos pacotes `sf` e `ggplot2`.
#'
#' @param dados Tabela de uma das funções do pacote.
#' @param valor Coluna que pinta o mapa, por exemplo `"obitos"`. Padrão:
#'   a taxa por 10 mil habitantes.
#' @param titulo Título do mapa (opcional).
#' @param cores Paleta do ColorBrewer, por exemplo `"Reds"` (padrão),
#'   `"Blues"`, `"YlOrRd"`.
#' @return Um gráfico `ggplot2`.
#' @export
#' @examples
#' \dontrun{
#' x <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
#' mapa_bairro(x)
#' mapa_bairro(x, valor = "obitos", titulo = "Óbitos por doenças circulatórias, 2023")
#' mapa_bairro(obitos_bairro("Rio de Janeiro", 2023, por = "sexo"))   # um painel por sexo
#' ggplot2::ggsave("mapa.png", width = 8, height = 6, dpi = 300)
#' }
mapa_bairro <- function(dados, valor = NULL, titulo = NULL, cores = "Reds") {
  precisa(c("sf", "ggplot2"))
  p <- preparar_mapa(dados, valor)
  base <- contornos(p)
  g <- ggplot2::ggplot()
  codigos_malha <- if (is.null(base)) character() else base$codigo
  tem_poligono <- !is.na(p$d$codigo) & p$d$codigo %in% codigos_malha
  com_poligono <- p$d[tem_poligono, ]
  pontos <- p$d[!tem_poligono & !is.na(p$d$lat) & !is.na(p$d$valor_mapa), ]

  if (!is.null(base)) {
    g <- g + ggplot2::geom_sf(data = base, fill = "grey93", color = "white", linewidth = 0.15)
    if (nrow(com_poligono) > 0) {
      pol <- merge(base[, "codigo"], com_poligono, by = "codigo")
      g <- g + ggplot2::geom_sf(data = pol, ggplot2::aes(fill = valor_mapa),
                                color = "white", linewidth = 0.15)
    }
  }
  if (nrow(pontos) > 0) {
    g <- g + ggplot2::geom_point(data = pontos, ggplot2::aes(x = lon, y = lat, size = valor_mapa,
                                                            fill = valor_mapa),
                                 shape = 21, color = "grey30", stroke = 0.2, alpha = 0.9) +
      ggplot2::scale_size_area(max_size = 6, guide = "none")
  }
  if (is.null(base)) g <- g + ggplot2::coord_sf(crs = 4674, default_crs = 4674)
  g <- g +
    ggplot2::scale_fill_distiller(palette = cores, direction = 1, name = p$rotulo,
                                  na.value = "grey85") +
    ggplot2::labs(title = titulo,
                  caption = paste0(if (!is.null(p$fonte)) paste0("Fonte: ", p$fonte, " | "),
                                   "Contornos e popula\u00e7\u00e3o: IBGE, Censo 2022 | localdatasus")) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 7, color = "grey40"),
                   plot.title = ggplot2::element_text(face = "bold"))
  if (length(p$paineis) > 0) {
    g <- g + ggplot2::facet_wrap(p$paineis)
  }
  g
}

#' Mapa interativo por bairro
#'
#' Abre um mapa interativo (leaflet), com um mapa de ruas ao fundo: passe o
#' mouse sobre um bairro para ver o nome e clique para ver contagem, taxa e
#' população. Útil para explorar os dados.
#'
#' O mapa mostra um valor por bairro. Se a tabela tiver vários anos ou
#' categorias, as contagens são somadas (filtre antes, se preferir).
#'
#' Precisa dos pacotes `sf` e `leaflet`.
#'
#' @inheritParams mapa_bairro
#' @return Um mapa `leaflet` (aparece no Viewer do RStudio). Para salvar
#'   como página web: `htmlwidgets::saveWidget(mapa, "mapa.html")`.
#' @export
#' @examples
#' \dontrun{
#' x <- agravos_bairro("dengue", "Rio de Janeiro", 2024)
#' mapa_interativo(x)
#' }
mapa_interativo <- function(dados, valor = NULL, cores = "Reds") {
  precisa(c("sf", "leaflet"))
  p <- preparar_mapa(dados, valor)
  d <- data.table::as.data.table(p$original)
  contagem <- intersect(c("obitos", "nascimentos", "casos", "internacoes", "n"), names(d))[1]
  if (length(p$paineis) > 0) {
    avisar("LDS-23", "A tabela tem v\u00e1rios ", paste(p$paineis, collapse = " e "),
           "; o mapa mostra a soma.")
  }
  # Um valor por bairro: soma as contagens e refaz a taxa.
  p$d$contagem <- if (is.na(contagem)) NA_real_ else d[[contagem]]
  p$d$populacao <- if ("populacao" %in% names(d)) d$populacao else NA_real_
  chave <- ifelse(is.na(p$d$codigo), p$d$nome, p$d$codigo)
  a <- data.table::as.data.table(p$d)[, list(
    codigo = codigo[1], codmun = codmun[1], nome = nome[1], lat = lat[1], lon = lon[1],
    contagem = sum(contagem), populacao = populacao[1], valor_mapa = sum(valor_mapa)
  ), by = list(chave = chave)]
  if (grepl("^taxa", p$valor)) a[, valor_mapa := round(10000 * contagem / populacao, 2)]
  a <- as.data.frame(a)

  base <- contornos(p)
  pal <- leaflet::colorNumeric(cores, domain = a$valor_mapa, na.color = "#d9d9d9")
  texto <- sprintf("<b>%s</b><br>%s: %s<br>Taxa por 10 mil: %s<br>Popula\u00e7\u00e3o: %s",
                   a$nome, if (is.na(contagem)) "valor" else contagem,
                   format(a$contagem, big.mark = ".", decimal.mark = ","),
                   format(round(10000 * a$contagem / a$populacao, 2), decimal.mark = ","),
                   format(a$populacao, big.mark = ".", decimal.mark = ","))
  m <- leaflet::addProviderTiles(leaflet::leaflet(), "CartoDB.Positron")
  codigos_malha <- if (is.null(base)) character() else base$codigo
  com_poligono <- !is.na(a$codigo) & a$codigo %in% codigos_malha
  if (any(com_poligono)) {
    pol <- merge(base[, "codigo"], data.frame(a[com_poligono, ], texto = texto[com_poligono]), by = "codigo")
    pol <- sf::st_transform(pol, 4326)
    m <- leaflet::addPolygons(m, data = pol, fillColor = pal(pol$valor_mapa), fillOpacity = 0.75,
                              color = "white", weight = 0.7, label = pol$nome, popup = pol$texto)
  }
  pts <- !com_poligono & !is.na(a$lat) & !is.na(a$valor_mapa)
  if (any(pts)) {
    m <- leaflet::addCircleMarkers(m, lng = a$lon[pts], lat = a$lat[pts], radius = 6,
                                   fillColor = pal(a$valor_mapa[pts]), fillOpacity = 0.85,
                                   color = "grey30", weight = 0.5,
                                   label = a$nome[pts], popup = texto[pts])
  }
  leaflet::addLegend(m, pal = pal, values = a$valor_mapa, title = p$rotulo, opacity = 0.8)
}
