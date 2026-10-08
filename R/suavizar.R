# Suavização bayesiana empírica das taxas por bairro (Marshall, 1991).

# Colunas de contagem, na ordem de preferência.
.colunas_contagem <- c("obitos", "nascimentos", "casos", "internacoes", "atendimentos",
                       "chamados", "estabelecimentos", "n")

# Estimador bayesiano empírico global de Marshall (1991), para um grupo de
# áreas: puxa cada taxa em direção à taxa média do grupo, mais forte quanto
# menor a população da área.
eb_marshall <- function(casos, pop) {
  ok <- !is.na(casos) & !is.na(pop) & pop > 0
  res <- rep(NA_real_, length(casos))
  if (sum(ok) < 2) return(res)
  y <- casos[ok]; n <- pop[ok]
  r <- y / n
  b <- sum(y) / sum(n)                          # taxa média do grupo
  s2 <- sum(n * (r - b)^2) / sum(n) - b / mean(n)  # variância entre áreas
  s2 <- max(s2, 0)
  peso <- if (s2 == 0) rep(0, length(n)) else s2 / (s2 + b / n)
  res[ok] <- b + peso * (r - b)
  res
}

#' Suaviza as taxas por bairro (bayesiano empírico)
#'
#' Em bairros com poucos moradores, poucos casos já produzem taxas muito
#' altas ou muito baixas, que variam por acaso de um ano para outro (por
#' exemplo, 24 casos em um bairro de 447 habitantes dão 537 por 10 mil).
#' Esta função acrescenta a coluna `taxa_suavizada_por_10mil`, que puxa a
#' taxa de cada bairro em direção à taxa média do seu município: o ajuste é
#' forte nos bairros pouco populosos e quase nulo nos populosos.
#'
#' O cálculo usa o estimador bayesiano empírico global de Marshall (1991),
#' feito separadamente para cada município, ano e categoria (quando a
#' tabela tem uma abertura `por`). Bairros sem população (por exemplo,
#' localidades sem bairro oficial) ficam com `NA`.
#'
#' Use a taxa suavizada para mapas e comparações entre bairros; para
#' contagens e totais, use as colunas originais.
#'
#' @param dados Tabela de uma das funções do pacote, com a coluna
#'   `populacao` e uma coluna de contagem (`obitos`, `casos`...).
#' @return A mesma tabela, com a coluna `taxa_suavizada_por_10mil`.
#' @references
#' Marshall RJ. Mapping disease and mortality rates using empirical Bayes
#' estimators. Applied Statistics 1991; 40(2):283-94. \doi{10.2307/2347593}.
#' @seealso [mapa_bairro()], com `suavizar = TRUE`.
#' @export
#' @examples
#' x <- data.frame(
#'   ano = 2024, codigo_municipio = "261160",
#'   bairro = c("Grande", "Médio", "Pequeno"),
#'   casos = c(500, 120, 24), populacao = c(100000, 20000, 447)
#' )
#' suavizar_taxas(x)
suavizar_taxas <- function(dados) {
  d <- as.data.frame(dados)
  col <- intersect(.colunas_contagem, names(d))[1]
  if (is.na(col) || !"populacao" %in% names(d)) {
    erro("LDS-17", "A tabela precisa de uma coluna de contagem (obitos, casos...) e de populacao.")
  }
  fixas <- c(.colunas_fixas, "bairro", "distrito", "ligacao", "fonte", "bairro_no_site")
  grupos <- intersect(c("ano", "codigo_municipio", "codmun", "municipio"), names(d))
  categorias <- setdiff(names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))],
                        c(fixas, grupos))
  chave <- interaction(d[c(grupos, categorias)], drop = TRUE, lex.order = TRUE)
  if (length(c(grupos, categorias)) == 0) chave <- factor(rep(1, nrow(d)))
  taxa <- rep(NA_real_, nrow(d))
  for (g in split(seq_len(nrow(d)), chave)) {
    taxa[g] <- eb_marshall(d[[col]][g], d$populacao[g])
  }
  d$taxa_suavizada_por_10mil <- round(10000 * taxa, 2)
  d
}
