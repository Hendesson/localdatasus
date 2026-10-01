# Erros e avisos com código: cada problema tem um código (LDS-01, LDS-02...)
# e ajuda("LDS-01") explica o que aconteceu e como resolver.

.erros <- data.frame(
  codigo = sprintf("LDS-%02d", 1:23),
  titulo = c(
    "UF inv\u00e1lida",
    "Munic\u00edpio n\u00e3o encontrado",
    "Munic\u00edpio com nome repetido",
    "Lugar n\u00e3o informado",
    "N\u00e3o h\u00e1 dado por bairro para esse lugar",
    "Agravo n\u00e3o dispon\u00edvel",
    "Agravo amb\u00edguo",
    "CID escrita de forma inv\u00e1lida",
    "CID n\u00e3o dispon\u00edvel nesta fonte",
    "Vari\u00e1vel de 'por' n\u00e3o encontrada",
    "Ano fora da base",
    "Nenhum registro encontrado",
    "Falha de conex\u00e3o",
    "O site mudou ou respondeu algo inesperado",
    "Filtro ou op\u00e7\u00e3o do TabNet n\u00e3o encontrado",
    "Fonte ou sistema desconhecido",
    "Coluna n\u00e3o encontrada na tabela",
    "SINASC-RJ s\u00f3 existe para o RJ",
    "Arquivo do IBGE n\u00e3o encontrado",
    "Faltam pacotes para mapas",
    "Tabela sem dados de lugar para o mapa",
    "Poucos registros ganharam bairro",
    "Mais de um ano ou categoria no mapa interativo"
  ),
  causa = c(
    "A UF precisa ser UMA sigla v\u00e1lida, como \"RJ\" ou \"SP\".",
    "O nome (ou c\u00f3digo) do munic\u00edpio n\u00e3o bate com nenhum munic\u00edpio do Censo 2022.",
    "Existe mais de um munic\u00edpio com esse nome no Brasil (ex.: Bom Jardim, em MA, PE e RJ).",
    "A fun\u00e7\u00e3o precisa saber ONDE: um munic\u00edpio ou uma UF.",
    "S\u00f3 algumas secretarias publicam dados por bairro. Para esse lugar e esse tipo de dado, n\u00e3o h\u00e1 fonte p\u00fablica conhecida.",
    "Esse agravo n\u00e3o est\u00e1 no TabNet desse lugar.",
    "O nome dado combina com mais de um agravo (ex.: \"sifilis\" = gestante e cong\u00eanita).",
    "A CID deve ser letras (cap\u00edtulos: \"I\", \"J\") OU c\u00f3digos de 3 caracteres (\"I21\", \"I20-I25\"), sem misturar os dois tipos. Intervalos ficam dentro de uma letra.",
    "A fonte n\u00e3o permite filtrar por causa, ou nenhum dos c\u00f3digos pedidos aparece nela (pode n\u00e3o ter havido caso).",
    "O texto de 'por' n\u00e3o corresponde a nenhuma vari\u00e1vel que a fonte permite cruzar.",
    "A fonte n\u00e3o tem dados para algum dos anos pedidos; esses anos foram deixados de fora.",
    "A consulta funcionou, mas n\u00e3o h\u00e1 nenhum registro com esses filtros, lugar e anos.",
    "O site da secretaria (ou do IBGE/DATASUS) n\u00e3o respondeu. Pode estar fora do ar ou lento.",
    "A p\u00e1gina do TabNet n\u00e3o veio no formato esperado. Ou o site est\u00e1 inst\u00e1vel, ou mudou o formul\u00e1rio.",
    "O r\u00f3tulo pedido n\u00e3o existe no formul\u00e1rio do site, ou combina com mais de uma op\u00e7\u00e3o.",
    "O c\u00f3digo da fonte (TabNet) ou do sistema (microdados) n\u00e3o est\u00e1 no cadastro do pacote.",
    "A tabela passada n\u00e3o tem a coluna indicada.",
    "Os microdados de nascimentos com CEP e bairro s\u00e3o publicados s\u00f3 pela SES-RJ.",
    "O arquivo do IBGE para esse munic\u00edpio ou UF n\u00e3o foi achado no servidor.",
    "Os mapas usam os pacotes sf e ggplot2 (est\u00e1tico) ou sf e leaflet (interativo), que n\u00e3o est\u00e3o instalados.",
    "A tabela n\u00e3o tem c\u00f3digo IBGE do bairro nem coordenadas (lat/lon).",
    "Uma parte grande dos registros ficou sem bairro, geralmente por CEP gen\u00e9rico (um s\u00f3 CEP para a cidade toda) ou CEP do hospital no lugar do da resid\u00eancia.",
    "O mapa interativo mostra um valor por bairro. Com v\u00e1rios anos ou categorias, o pacote soma tudo."
  ),
  solucao = c(
    "Use a sigla com duas letras: uf = \"RJ\".",
    "Confira a grafia (acentos e mai\u00fasculas n\u00e3o importam) ou use o c\u00f3digo IBGE: municipio = \"330455\".",
    "Diga tamb\u00e9m a UF: obitos_bairro(\"Bom Jardim\", 2023, uf = \"RJ\").",
    "Exemplos: municipio = \"Rio de Janeiro\" ou uf = \"RJ\".",
    "Veja o que existe com onde_tem_bairro(). Para interna\u00e7\u00f5es, use internacoes_bairro(), que funciona em todas as UFs (pelo CEP).",
    "Veja os agravos de cada lugar: subset(onde_tem_bairro(), funcao == \"agravos_bairro()\").",
    "Seja mais espec\u00edfico: \"sifilis congenita\" ou \"sifilis gestante\".",
    "Exemplos v\u00e1lidos: cid = \"I\"; cid = c(\"I\", \"J\"); cid = \"I21\"; cid = \"I20-I25\".",
    "Tente um cap\u00edtulo inteiro (cid = \"I\") ou veja as op\u00e7\u00f5es: tabnet_opcoes(\"RIO-SIM\", \"filtros\", filtro = \"Causa (CID10 3C)\").",
    "Veja as vari\u00e1veis poss\u00edveis: tabnet_opcoes(\"RIO-SIM\", \"colunas\") (troque pela fonte certa; veja onde_tem_bairro()).",
    "Veja os anos dispon\u00edveis: tabnet_opcoes(\"RIO-SIM\", \"anos\").",
    "Amplie o per\u00edodo, tire filtros ou confira se o agravo teve casos nesses anos.",
    "Espere alguns minutos e tente de novo. O pacote j\u00e1 tenta 3 vezes. Consultas que j\u00e1 deram certo ficam em cache.",
    "Tente de novo mais tarde. Se continuar, avise os autores do pacote: o cadastro da fonte pode precisar de ajuste.",
    "Veja as op\u00e7\u00f5es exatas com tabnet_opcoes(fonte, \"filtros\", filtro = \"...\"). Pode usar s\u00f3 o come\u00e7o do texto.",
    "Veja as op\u00e7\u00f5es com fontes_tabnet() ou sistemas_bairro().",
    "Confira o nome da coluna com names(sua_tabela).",
    "Para outras UFs, use nascimentos_bairro() com a capital de SP, ou o TabNet da sua secretaria.",
    "Confira o c\u00f3digo do munic\u00edpio. Se o problema continuar, o IBGE pode ter mudado o nome do arquivo.",
    "Instale uma vez: install.packages(c(\"sf\", \"ggplot2\", \"leaflet\")).",
    "Use a tabela devolvida por obitos_bairro(), nascimentos_bairro(), agravos_bairro() ou agregar_bairro().",
    "N\u00e3o \u00e9 um erro. Compare bairros dentro do mesmo munic\u00edpio e confira attr(x, \"cobertura\").",
    "Para um ano s\u00f3, filtre antes: mapa_interativo(subset(x, ano == 2023))."
  ),
  stringsAsFactors = FALSE
)

# Interrompe com um erro que leva o código e a dica de ajuda.
erro <- function(codigo, ...) {
  msg <- paste0("[", codigo, "] ", paste0(..., collapse = ""),
                "\n  Para entender e resolver: ajuda(\"", codigo, "\")")
  stop(structure(class = c("localdatasus_erro", "error", "condition"),
                 list(message = msg, call = NULL, codigo = codigo)))
}

# Avisa sem interromper (curto, com o código).
avisar <- function(codigo, ...) {
  message("[", codigo, "] ", paste0(..., collapse = ""), "  (veja ajuda(\"", codigo, "\"))")
}

# Mensagens de progresso: só aparecem com options(localdatasus.detalhes = TRUE).
detalhar <- function(...) {
  if (isTRUE(getOption("localdatasus.detalhes", FALSE))) message(...)
}

# Texto da seção de códigos na página de ajuda (gerado a partir de .erros).
doc_erros <- function() {
  itens <- sprintf("  \\item{%s: %s}{%s \\strong{Como resolver:} %s}",
                   .erros$codigo, .erros$titulo, .erros$causa,
                   gsub("%", "\\\\%", .erros$solucao))
  c("@section C\u00f3digos de erro e aviso:", "\\describe{", itens, "}")
}

#' Ajuda sobre erros e avisos do pacote
#'
#' Todo erro ou aviso do localdatasus vem com um código, por exemplo
#' `[LDS-03]`. `ajuda("LDS-03")` explica o que aconteceu e como resolver.
#' Sem argumento, lista todos os códigos.
#'
#' O pacote trabalha em silêncio e mostra só um resumo no fim. Para ver
#' cada etapa (consultas, downloads), use
#' `options(localdatasus.detalhes = TRUE)`.
#'
#' @param codigo Código do erro, como `"LDS-03"`, `"03"` ou `3`.
#' @return Invisivelmente, a linha da tabela de erros (ou a tabela toda).
#' @eval doc_erros()
#' @export
#' @examples
#' ajuda()
#' ajuda("LDS-03")
#' ajuda(3)
ajuda <- function(codigo = NULL) {
  if (is.null(codigo)) {
    cat("C\u00f3digos de erro e aviso do localdatasus (use ajuda(\"LDS-xx\") para detalhes):\n\n")
    for (i in seq_len(nrow(.erros))) cat(" ", .erros$codigo[i], " ", .erros$titulo[i], "\n", sep = "")
    return(invisible(.erros))
  }
  num <- suppressWarnings(as.integer(gsub("\\D", "", as.character(codigo))))
  cod <- sprintf("LDS-%02d", num)
  i <- match(cod, .erros$codigo)
  if (is.na(i)) {
    cat("C\u00f3digo desconhecido: ", codigo, ". Veja a lista com ajuda().\n", sep = "")
    return(invisible(NULL))
  }
  largura <- min(getOption("width", 80), 80)
  quebra <- function(x) paste(strwrap(x, largura - 4, prefix = "    "), collapse = "\n")
  cat("\n", .erros$codigo[i], " \u2014 ", .erros$titulo[i], "\n\n",
      "  O que aconteceu:\n", quebra(.erros$causa[i]), "\n\n",
      "  Como resolver:\n", quebra(.erros$solucao[i]), "\n\n", sep = "")
  invisible(.erros[i, ])
}
