# Testes de adicionar_bairro() e agregar_bairro() SEM internet: usamos um
# cache falso com uma tabela de CEPs e de setores inventada.

preparar_cache_falso <- function() {
  pasta <- tempfile("cache_localdatasus_")
  dir.create(pasta)
  ceps <- data.frame(
    cep        = c("20031170", "20031171", "24000000"),
    codmun_cep = c("330455",   "330455",   "330330"),
    id_bairro  = c("3304557001", "3304557001", "330330_CENTRO"),
    pct_bairro = c(1, 0.9, 0.3),
    n_end_cep  = c(100L, 50L, 5000L),
    lat_cep    = c(-22.90, -22.91, -22.89),
    lon_cep    = c(-43.17, -43.18, -43.12)
  )
  bairros <- data.frame(
    id_bairro      = c("3304557001", "330330_CENTRO"),
    codmun         = c("330455", "330330"),
    bairro         = c("Centro", "CENTRO"),
    fonte_bairro   = c("IBGE 2022 (oficial)", "CNEFE (localidade)"),
    cd_bairro_ibge = c("3304557001", NA),
    lat_bairro     = c(-22.905, -22.89),
    lon_bairro     = c(-43.175, -43.12),
    n_enderecos    = c(150L, 5000L)
  )
  saveRDS(list(ceps = ceps, bairros = bairros), file.path(pasta, "cep_bairro_v2_RJ.rds"))
  setores <- data.table::data.table(
    CD_SETOR = c("330455705000001", "330455705000002"), CD_UF = "33",
    CD_MUN = "3304557", NM_MUN = "Rio de Janeiro", CD_DIST = "330455705",
    NM_DIST = "Rio de Janeiro", CD_BAIRRO = "3304557001",
    NM_BAIRRO = "Centro", CD_FCU = NA_character_, codmun = "330455",
    populacao = c(30000L, 10000L)
  )
  saveRDS(setores, file.path(pasta, "setores_censo2022_v2.rds"))
  pasta
}

test_that("adicionar_bairro localiza CEPs e mantém as colunas originais", {
  old <- options(localdatasus.cache = preparar_cache_falso())
  on.exit(options(old))
  dados <- data.frame(id = 1:5,
                      cep = c("20031170", "20031-171", "24000000", "29999000", NA),
                      mun = c("330455", "330455", "330330", "330455", "330455"))
  x <- suppressMessages(localdatasus::adicionar_bairro(dados, "RJ", col_cep = "cep", col_mun = "mun"))
  expect_equal(nrow(x), 5)
  expect_equal(x$id, 1:5)                                  # ordem preservada
  expect_equal(x$cep_paciente[2], "20031171")              # hífen removido
  expect_equal(x$bairro, c("Centro", "Centro", NA, NA, NA))
  expect_equal(x$situacao_cep, c("bairro atribuído", "bairro atribuído",
                                 "CEP geral/amplo (vários bairros)",
                                 "CEP não existe no CNEFE 2022", "CEP inválido/ausente"))
  expect_equal(x$lat_cep[3], -22.89)                       # coordenada do CEP mesmo sem bairro
  expect_true(is.data.frame(attr(x, "cobertura")))
})

test_that("CEP numérico sem o zero da frente é corrigido", {
  old <- options(localdatasus.cache = preparar_cache_falso())
  on.exit(options(old))
  x <- suppressMessages(localdatasus::adicionar_bairro(data.frame(cep = 1001000), "RJ", col_cep = "cep"))
  expect_equal(x$cep_paciente, "01001000")
})

test_that("agregar_bairro conta e calcula taxa com a população", {
  old <- options(localdatasus.cache = preparar_cache_falso())
  on.exit(options(old))
  dados <- data.frame(pac = c("a", "a", "b", "c"),
                      cep = c("20031170", "20031170", "20031171", "24000000"),
                      mun = "330455")
  x <- suppressMessages(localdatasus::adicionar_bairro(dados, "RJ", col_cep = "cep", col_mun = "mun"))
  a <- localdatasus::agregar_bairro(x)
  centro <- a[a$bairro == "Centro", ]
  expect_equal(centro$n, 3)
  expect_equal(centro$populacao, 40000)
  expect_equal(centro$taxa_10mil, 0.75)
  expect_true("(sem bairro)" %in% a$bairro)
  # contando pacientes distintos
  a2 <- localdatasus::agregar_bairro(x, contar_distintos = "pac")
  expect_equal(a2$n[a2$bairro == "Centro"], 2)
})
