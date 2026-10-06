# Testes do leitor de TabNet e da ligação de nomes, SEM internet: as
# respostas dos sites são simuladas.

cache_bairros_falso <- function() {
  pasta <- tempfile("cache_localdatasus_")
  dir.create(pasta)
  bairros <- data.frame(
    id_bairro      = c("3304557001", "3304557116", "3304557158", "3304557046", "330455_ILHA"),
    codmun         = "330455",
    bairro         = c("Tijuca", "Freguesia (Ilha do Governador)", "Maré", "Jardim América", "ILHA"),
    fonte_bairro   = c(rep("IBGE 2022 (oficial)", 4), "CNEFE (localidade)"),
    cd_bairro_ibge = c("3304557001", "3304557116", "3304557158", "3304557046", NA),
    lat_bairro     = c(-22.92, -22.79, -22.86, -22.82, -22.80),
    lon_bairro     = c(-43.23, -43.17, -43.24, -43.32, -43.20),
    n_enderecos    = 100L
  )
  ceps <- data.frame(cep = character(), codmun_cep = character(), id_bairro = character(),
                     pct_bairro = numeric(), n_end_cep = integer(), lat_cep = numeric(),
                     lon_cep = numeric())
  saveRDS(list(ceps = ceps, bairros = bairros), file.path(pasta, "cep_bairro_v3_RJ.rds"))
  setores <- data.table::data.table(
    CD_SETOR = sprintf("33045570500000%d", 1:4), CD_UF = "33", CD_MUN = "3304557",
    NM_MUN = "Rio de Janeiro", CD_DIST = "330455705", NM_DIST = "Rio de Janeiro",
    CD_BAIRRO = c("3304557001", "3304557116", "3304557158", "3304557046"),
    NM_BAIRRO = c("Tijuca", "Freguesia (Ilha do Governador)", "Maré", "Jardim América"),
    CD_FCU = NA_character_, codmun = "330455", populacao = c(140000L, 20000L, 120000L, 25000L)
  )
  saveRDS(setores, file.path(pasta, "setores_censo2022_v2.rds"))
  pasta
}

test_that("chave_bairro normaliza acentos, abreviações e números", {
  expect_equal(localdatasus:::chave_bairro("Jd. Sta Rosa 2"), "JARDIM SANTA ROSA II")
  expect_equal(localdatasus:::chave_bairro("Freguesia (Ilha do Governador)"),
               "FREGUESIA ILHA GOVERNADOR")
  expect_equal(localdatasus:::chave_bairro("Maré"), "MARE")
})

test_that("eh_ignorado reconhece rótulos de bairro desconhecido", {
  x <- c("~Não informado/Ignorado", "EM BRANCO", "999 IGNORADO BAIRRO-MRJ", "NAO IDENTIFICADO",
         "", NA, "Tijuca")
  expect_equal(localdatasus:::eh_ignorado(x), c(rep(TRUE, 6), FALSE))
})

test_that("ligar_bairros liga nomes exatos, parciais e aproximados", {
  old <- options(localdatasus.cache = cache_bairros_falso())
  on.exit(options(old))
  nomes <- c("TIJUCA", "FREGUESIA-ILHA", "COMPLEXO DA MARE", "Jd. America", "JARDIM AMERIKA",
             "BAIRRO NOVO QUALQUER", "IGNORADO")
  x <- localdatasus::ligar_bairros(nomes, "330455", "RJ")
  expect_equal(x$ligacao, c("exata", "parcial", "parcial", "exata", "aproximada",
                            "não encontrado", "ignorado"))
  # "FREGUESIA ILHA" também "contém" a localidade "ILHA": o bairro oficial vence.
  expect_equal(x$nome_ibge[2], "Freguesia (Ilha do Governador)")
  expect_equal(x$populacao[1], 140000L)
  expect_equal(x$lat[3], -22.86)
})

test_that("codificar_latin1 codifica byte a byte em latin1", {
  expect_equal(localdatasus:::codificar_latin1("Óbitos já"), "%D3bitos%20j%E1")
  expect_equal(localdatasus:::codificar_latin1("2023|2023|4"), "2023%7C2023%7C4")
})

test_that("ler_filtros_fixos interpreta o cadastro", {
  expect_equal(localdatasus:::ler_filtros_fixos("Munic Resid=330455;Sexo=Fem"),
               list("Munic Resid" = "330455", "Sexo" = "Fem"))
  expect_equal(localdatasus:::ler_filtros_fixos(NA), list())
})

test_that("o cadastro de fontes é consistente", {
  f <- localdatasus::fontes_tabnet()
  expect_false(anyDuplicated(f$fonte) > 0)
  expect_true(all(f$dialeto %in% c("tabnet", "dhx")))
  expect_true(all(f$unidade %in% c("bairro", "distrito")))
  expect_true(all(f$rotulo %in% c("nome", "codigo_nome", "uf_municipio_bairro")))
  # Fonte estadual com rótulo só de nome precisa consultar município a município.
  estadual_nome <- is.na(f$codmun) & f$rotulo == "nome"
  expect_true(all(!is.na(f$filtro_mun[estadual_nome])))
})

# Página de formulário e resposta "prn" no formato do TabNet clássico.
FORM_FALSO <- paste0(
  '<FORM ACTION="/cgi-bin/tabnet?teste.def" METHOD=POST>',
  '<SELECT NAME="Linha" ID="L"><OPTION VALUE="Bairro_Resid">Bairro Resid',
  '<OPTION VALUE="Munic_Resid">Munic Resid</SELECT>',
  '<SELECT NAME="Coluna" ID="C"><OPTION VALUE="--N&atilde;o-Ativa--">N&atilde;o ativa',
  '<OPTION VALUE="Sexo">Sexo</SELECT>',
  '<SELECT NAME="Incremento" ID="I"><OPTION VALUE="Casos">Casos</SELECT>',
  '<SELECT NAME="Arquivos" ID="A"><OPTION VALUE="t24.dbf">2024<OPTION VALUE="t23.dbf">2023</SELECT>',
  '<SELECT NAME="SSexo" ID="S1"><OPTION VALUE="TODAS_AS_CATEGORIAS__">Todas as categorias',
  '<OPTION VALUE="1">Masculino<OPTION VALUE="2">Feminino</SELECT>',
  '<SELECT NAME="SMunic_Resid" ID="S2"><OPTION VALUE="1">330455 Rio de Janeiro</SELECT>',
  '</FORM>')
PRN_FALSO <- paste(
  "<PRE>",
  '"Bairro Resid";"<1 Ano";">50 Anos"',
  '"AP 1.0";5;10',
  '"I";4;6',
  '"..II TIJUCA";4;6',
  '"001 TIJUCA";4;6',
  '"002 COMPLEXO DA MARE";-;5',
  '"EM BRANCO";2;-',
  '"Total";6;11',
  "</PRE>", sep = "\n")

test_that("tabnet_bairro consulta, descarta subtotais e liga os bairros", {
  old <- options(localdatasus.cache = cache_bairros_falso())
  on.exit(options(old))
  pedidos <- list()
  testthat::local_mocked_bindings(
    tn_http = function(url, corpo = NULL, tentativas = 3) {
      if (is.null(corpo)) return(FORM_FALSO)
      pedidos[[length(pedidos) + 1]] <<- corpo
      PRN_FALSO
    },
    .package = "localdatasus"
  )
  fonte <- localdatasus::fontes_tabnet()[1, ]
  fonte$fonte <- "TESTE"; fonte$url_def <- "http://teste.invalid/cgi-bin/dh?teste.def"
  fonte$linha <- "Bairro Resid"; fonte$rotulo <- "codigo_nome"
  fonte$descartar <- "^(AP [0-9]|[IVXLC]+( |$))"; fonte$filtros_fixos <- "Munic Resid=330455"
  fonte$incremento <- NA
  x <- suppressMessages(localdatasus::tabnet_bairro(fonte, 2023, coluna = "Sexo", filtros = list(Sexo = "Fem")))

  x <- x[order(x$bairro_tabnet, x$categoria), ]
  expect_equal(unique(x$bairro_tabnet), c("COMPLEXO DA MARE", "EM BRANCO", "TIJUCA"))
  # Rótulos com "<" e ">" sobrevivem à limpeza do HTML; "-" vira zero.
  expect_equal(sort(unique(x$categoria)), c("<1 Ano", ">50 Anos"))
  expect_equal(x$n, c(0, 5, 2, 0, 4, 6))
  expect_equal(unique(x$ligacao), c("parcial", "ignorado", "exata"))
  expect_equal(x$taxa_10mil[x$bairro_tabnet == "TIJUCA" & x$categoria == ">50 Anos"],
               round(10000 * 6 / 140000, 2))
  # O pedido levou o ano, o filtro fixo e o filtro do usuário, em latin1.
  expect_match(pedidos[[1]], "Arquivos=t23.dbf", fixed = TRUE)
  expect_match(pedidos[[1]], "SMunic_Resid=1", fixed = TRUE)
  expect_match(pedidos[[1]], "SSexo=2", fixed = TRUE)
  expect_match(pedidos[[1]], "formato=prn", fixed = TRUE)
})

test_that("opção curta não confunde capítulos (X com XI, XII...)", {
  form <- list(opcoes = list(SCausa = data.frame(
    valor = c("1", "2", "3"),
    rotulo = c("IX.  Doenças do aparelho circulatório", "X.   Doenças do aparelho respiratório",
               "XI.  Doenças do aparelho digestivo"))))
  expect_equal(localdatasus:::tn_valores(form, "SCausa", "X"), "2")
  expect_equal(localdatasus:::tn_valores(form, "SCausa", "IX"), "1")
})

test_that("cid em letras vira capítulos; códigos e intervalos viram categorias", {
  form <- list(
    opcoes = list(
      "SCausa_(Cap_CID10)" = data.frame(valor = as.character(1:4), rotulo = c(
        "Todas as categorias", "IX.  Doenças do aparelho circulatório",
        "X.   Doenças do aparelho respiratório", "XI.  Doenças do aparelho digestivo")),
      "SCausa_(CID10_3C)" = data.frame(valor = as.character(1:4), rotulo = c(
        "I20   Angina pectoris", "I21   Infarto agudo do miocardio", "I25   Doenc isquemica",
        "J18   Pneumonia"))),
    ids = c("SCausa_(Cap_CID10)" = "S1", "SCausa_(CID10_3C)" = "S2"))
  cfg <- list(fonte = "T", filtro_capitulo = "Causa (Cap CID10)", filtro_cid3 = "Causa (CID10 3C)")
  f <- localdatasus:::filtros_cid(cfg, form, c("I", "J"))
  expect_equal(f[[1]], c("IX.  Doenças do aparelho circulatório", "X.   Doenças do aparelho respiratório"))
  f <- localdatasus:::filtros_cid(cfg, form, "I20-I22")
  expect_equal(f[[1]], c("I20   Angina pectoris", "I21   Infarto agudo do miocardio"))
  expect_error(localdatasus:::filtros_cid(cfg, form, c("I", "I21")), "sem misturar")
  # Formato da SES-RJ: "Capítulo  9 - ..."
  form$opcoes[["SCausa_(Cap_CID10)"]]$rotulo[2:4] <- c("Capítulo  9 - Circulatório",
    "Capítulo 10 - Respiratório", "Capítulo 19 - Lesões")
  expect_equal(localdatasus:::filtros_cid(cfg, form, "I")[[1]], "Capítulo  9 - Circulatório")
})

test_that("nome_coluna gera nomes legíveis", {
  expect_equal(localdatasus:::nome_coluna("Faixa Etária"), "faixa_etaria")
  expect_equal(localdatasus:::nome_coluna("Raça/Cor"), "raca_cor")
})

test_that("fonte só com capítulo recusa códigos de 3 caracteres", {
  cfg <- localdatasus:::config_fonte("FOR-SIM")
  expect_equal(cfg$codmun, "230440")
  expect_true(is.na(cfg$filtro_cid3))
  expect_error(localdatasus:::filtros_cid(cfg, list(opcoes = list()), "I21"), "LDS-09")
})
