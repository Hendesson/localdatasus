# Ligação de NOMES de bairro (como aparecem nos TabNets e nas declarações)
# aos bairros do IBGE/CNEFE e aos distritos do IBGE.

# Chave de comparação de nomes: maiúsculas, sem acento, sem pontuação,
# abreviações expandidas, sem preposições e com números finais em romano
# ("Jd. Sta Rosa 2" -> "JARDIM SANTA ROSA II").
chave_bairro <- function(x) {
  x <- chartr(
    "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1\u00fd\u00c1\u00c0\u00c2\u00c3\u00c4\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf\u00d3\u00d2\u00d4\u00d5\u00d6\u00da\u00d9\u00db\u00dc\u00c7\u00d1\u00dd",
    "aaaaaeeeeiiiiooooouuuucnyAAAAAEEEEIIIIOOOOOUUUUCNY", x)
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- paste0(" ", gsub("\\s+", " ", x), " ")
  abrev <- c(" JD " = " JARDIM ", " JDM " = " JARDIM ", " VL " = " VILA ", " PQ " = " PARQUE ",
             " PRQ " = " PARQUE ", " STA " = " SANTA ", " STO " = " SANTO ", " S " = " SAO ",
             " N S " = " NOSSA SENHORA ", " NSA " = " NOSSA ", " SRA " = " SENHORA ",
             " CJ " = " CONJUNTO ", " CONJ " = " CONJUNTO ", " RES " = " RESIDENCIAL ",
             " JPA " = " JACAREPAGUA ", " ENG " = " ENGENHO ", " DR " = " DOUTOR ",
             " LOT " = " LOTEAMENTO ", " PRES " = " PRESIDENTE ", " CEL " = " CORONEL ")
  for (a in names(abrev)) x <- gsub(a, abrev[[a]], x, fixed = TRUE)
  x <- gsub(" (DE|DA|DO|DAS|DOS|E) ", " ", x)
  x <- gsub(" (DE|DA|DO|DAS|DOS|E) ", " ", x)
  romanos <- c("1" = "I", "2" = "II", "3" = "III", "4" = "IV", "5" = "V")
  for (n in names(romanos)) x <- sub(paste0(" ", n, " $"), paste0(" ", romanos[[n]], " "), x)
  trimws(gsub("\\s+", " ", x))
}

# Tira a numeração final da chave ("SANTA ROSA IV" -> "SANTA ROSA").
sem_numero <- function(chave) sub(" (I|II|III|IV|V|[0-9]+)$", "", chave)

# Rótulos que significam "bairro desconhecido".
eh_ignorado <- function(nome) {
  ch <- chave_bairro(nome)
  is.na(nome) | trimws(nome) == "" | startsWith(trimws(nome), "~") | ch == "" |
    grepl("IGNORAD|EM BRANCO|NAO INFORMAD|NAO IDENTIFICAD|NAO ESPECIFICAD|NAO CONSTA|SEM INFORMAC|SEM BAIRRO", ch) |
    ch %in% c("IGN", "SB", "NI", "NAO SE APLICA", "OUTROS")
}

# Unidades do IBGE (bairros ou distritos) que podem receber os nomes.
unidades_ibge <- function(uf, unidade = c("bairro", "distrito"), codmuns = NULL) {
  unidade <- match.arg(unidade)
  if (unidade == "bairro") {
    b <- data.table::as.data.table(cep_bairro(uf)$bairros)
    pop <- data.table::as.data.table(populacao_bairro(uf))[, .(cd_bairro_ibge, populacao)]
    b <- pop[b, on = "cd_bairro_ibge"]
    b[, .(codmun, id_unidade = id_bairro, nome_ibge = bairro, fonte_unidade = fonte_bairro,
          cd_ibge = cd_bairro_ibge, lat = lat_bairro, lon = lon_bairro, populacao,
          oficial = !is.na(cd_bairro_ibge))]
  } else {
    # Distritos: só dos municípios pedidos (cada um baixa o seu CNEFE).
    codmuns <- unique(stats::na.omit(codmuns))
    d <- data.table::rbindlist(lapply(codmuns, function(m) data.table::as.data.table(distritos_censo(m))))
    d[, .(codmun, id_unidade = cd_distrito_ibge, nome_ibge = distrito,
          fonte_unidade = "IBGE 2022 (distrito)", cd_ibge = cd_distrito_ibge,
          lat = lat_distrito, lon = lon_distrito, populacao, oficial = TRUE)]
  }
}

#' Liga nomes de bairro aos bairros do IBGE, com coordenadas e população
#'
#' Recebe nomes de bairro escritos à mão ou vindos de outros sistemas (por
#' exemplo, de um TabNet ou do campo "bairro" de uma declaração) e procura,
#' DENTRO de cada município, o bairro correspondente: primeiro o nome
#' idêntico (ignorando acentos, abreviações e preposições), depois o nome
#' sem a numeração final ("Santa Rosa II" -> "Santa Rosa"), depois um nome
#' que contém o outro em palavras inteiras ("Complexo da Maré" -> "Maré")
#' e, por fim, o nome mais parecido (distância de edição de até 15% do
#' tamanho).
#'
#' Os bairros candidatos são os bairros oficiais do IBGE e, nos municípios
#' sem bairros oficiais, as localidades do CNEFE (ver [cep_bairro()]). Com
#' `unidade = "distrito"`, são os distritos do IBGE dos municípios em
#' `codmun` (ver [distritos_censo()]).
#'
#' @param nome Vetor de nomes de bairro.
#' @param codmun Vetor (do mesmo tamanho, ou de tamanho 1) com o código do
#'   município de cada nome (6 ou 7 dígitos).
#' @param uf Sigla da UF.
#' @param unidade `"bairro"` ou `"distrito"`.
#' @param distancia_max Distância de edição máxima, relativa ao tamanho do
#'   nome, para a ligação aproximada.
#' @return Um `data.frame` com uma linha por nome: `nome`, `codmun`,
#'   `ligacao` (`"exata"`, `"sem numeração"`, `"parcial"`, `"aproximada"`,
#'   `"não encontrado"` ou `"ignorado"`), `id_unidade`, `nome_ibge`,
#'   `fonte_unidade`, `cd_ibge`, `lat`, `lon` e `populacao` (só em unidades
#'   oficiais do IBGE).
#' @export
#' @examples
#' \dontrun{
#' ligar_bairros(c("Jd. América", "TIJUCA", "Ignorado"), "330455", "RJ")
#' }
ligar_bairros <- function(nome, codmun, uf, unidade = c("bairro", "distrito"),
                          distancia_max = 0.15) {
  unidade <- match.arg(unidade)
  codmun <- substr(as.character(codmun), 1, 6)
  d <- data.table::data.table(.ordem = seq_along(nome), nome = as.character(nome),
                              codmun = rep_len(codmun, length(nome)))
  u <- unique(d[, .(nome, codmun)])
  u[, `:=`(chave = chave_bairro(nome), ign = eh_ignorado(nome))]

  cand <- unidades_ibge(uf, unidade, unique(u$codmun))
  cand[, chave := chave_bairro(nome_ibge)]
  # Se dois candidatos do mesmo município têm a mesma chave, o oficial vence.
  data.table::setorder(cand, codmun, chave, -oficial)
  cand_exato <- cand[!duplicated(cand[, .(codmun, chave)])]
  cand[, chave_sn := sem_numero(chave)]
  cand_sn <- cand[!duplicated(cand[, .(codmun, chave_sn)])]

  u[, `:=`(id_unidade = NA_character_, ligacao = data.table::fifelse(ign, "ignorado", NA_character_))]
  # 1) nome idêntico
  i <- which(is.na(u$ligacao))
  m <- match(paste(u$codmun[i], u$chave[i]), paste(cand_exato$codmun, cand_exato$chave))
  ok <- !is.na(m)
  u$id_unidade[i[ok]] <- cand_exato$id_unidade[m[ok]]
  u$ligacao[i[ok]] <- "exata"
  # 2) sem a numeração final
  i <- which(is.na(u$ligacao))
  m <- match(paste(u$codmun[i], sem_numero(u$chave[i])), paste(cand_sn$codmun, cand_sn$chave_sn))
  ok <- !is.na(m)
  u$id_unidade[i[ok]] <- cand_sn$id_unidade[m[ok]]
  u$ligacao[i[ok]] <- "sem numera\u00e7\u00e3o"
  # 3) um nome contém o outro, em palavras inteiras, no início ou no fim
  #    ("COMPLEXO MARE" -> "MARE"; "FREGUESIA ILHA" -> "FREGUESIA ILHA GOVERNADOR")
  for (k in which(is.na(u$ligacao))) {
    cm <- cand_exato[codmun == u$codmun[k] & nchar(chave) >= 4]
    a <- u$chave[k]
    if (nrow(cm) == 0 || nchar(a) < 4) next
    contem <- startsWith(cm$chave, paste0(a, " ")) | endsWith(cm$chave, paste0(" ", a)) |
      startsWith(a, paste0(cm$chave, " ")) | endsWith(a, paste0(" ", cm$chave))
    # Mais de um candidato: fica com o bairro oficial, se for o único.
    if (sum(contem) > 1) contem <- contem & cm$oficial
    if (sum(contem) == 1) {
      u$id_unidade[k] <- cm$id_unidade[contem]
      u$ligacao[k] <- "parcial"
    }
  }
  # 4) nome mais parecido, dentro do município
  for (k in which(is.na(u$ligacao))) {
    cm <- cand_exato[codmun == u$codmun[k]]
    if (nrow(cm) == 0 || nchar(u$chave[k]) < 4) next
    # Sem espaços: "BOM SUCESSO" e "BONSUCESSO" diferem em 1 letra, não 2.
    a <- gsub(" ", "", u$chave[k], fixed = TRUE)
    dist <- utils::adist(a, gsub(" ", "", cm$chave, fixed = TRUE))[1, ] / nchar(a)
    melhor <- which(dist == min(dist))
    if (min(dist) <= distancia_max && length(melhor) == 1) {
      u$id_unidade[k] <- cm$id_unidade[melhor]
      u$ligacao[k] <- "aproximada"
    }
  }
  u[is.na(ligacao), ligacao := "n\u00e3o encontrado"]

  info <- unique(cand[, .(id_unidade, nome_ibge, fonte_unidade, cd_ibge, lat, lon, populacao)],
                 by = "id_unidade")
  u <- info[u, on = "id_unidade"]
  res <- u[d, on = .(nome, codmun)]
  data.table::setorder(res, .ordem)
  as.data.frame(res[, .(nome, codmun, ligacao, id_unidade, nome_ibge, fonte_unidade,
                        cd_ibge, lat, lon, populacao)])
}
