# Testes das regras internas (não precisam de internet).

test_that("codigo_uf aceita siglas válidas e recusa inválidas", {
  expect_equal(localdatasus:::codigo_uf("RJ"), "33")
  expect_equal(localdatasus:::codigo_uf("sp"), "35")
  expect_error(localdatasus:::codigo_uf("XX"))
  expect_error(localdatasus:::codigo_uf(c("RJ", "SP")))
})

test_that("para_na troca os marcadores do IBGE por NA", {
  expect_equal(localdatasus:::para_na(c("123", ".", "", "Centro")),
               c("123", NA, NA, "Centro"))
})

test_that("filtrar_cid e grupo_cid usam prefixos", {
  diag <- c("I219", "J189", "A09", "I500")
  expect_equal(localdatasus:::filtrar_cid(diag, c("I", "J")), c(TRUE, TRUE, FALSE, TRUE))
  # O primeiro prefixo que casa vence:
  expect_equal(localdatasus:::grupo_cid(diag, c("I21", "I", "J")), c("I21", "J", NA, "I"))
})

test_that("classificar_cep aplica as regras na ordem certa", {
  # 7 registros; total de 1000 endereços na UF (RJ = "33").
  cep        <- c(NA,       "20000999", "21000000", "22000000", "23000000", "24000000", "01001000")
  codmun_res <- c("330455", "330455",   "330455",   "330455",   "330455",   "330455",   "355030")
  codmun_cep <- c(NA,       "330455",   NA,         "330490",   "330455",   "330455",   NA)
  id_bairro  <- c(NA,       "b1",       NA,         "b2",       "b3",       "b4",       NA)
  pct_bairro <- c(NA,       1,          NA,         1,          0.4,        0.9,        NA)
  n_end_cep  <- c(NA,       10,         NA,         10,         10,         10,         NA)
  s <- localdatasus:::classificar_cep(cep, codmun_res, codmun_cep, id_bairro, pct_bairro,
                                     n_end_cep, total_enderecos = 1000, cod_uf = "33")
  expect_equal(s, c("CEP inválido/ausente",
                    "CEP especial (grande usuário/caixa postal)",
                    "CEP não existe no CNEFE 2022",
                    "CEP de outro município",
                    "CEP geral/amplo (vários bairros)",
                    "bairro atribuído",
                    "residente de outra UF"))
})

test_that("classificar_cep detecta CEP com excesso, contando pessoas distintas", {
  # 40 pessoas num CEP com só 2 endereços (de 10.000): excesso.
  # 40 pessoas espalhadas em CEPs normais: ok.
  cep <- c(rep("26180000", 40), sprintf("2618%04d", 1:40))
  n   <- c(rep(2, 40), rep(250, 40))
  s <- localdatasus:::classificar_cep(cep, rep("330045", 80), rep("330045", 80),
                                     rep("b", 80), rep(1, 80), n, total_enderecos = 10000,
                                     id = 1:80)
  expect_true(all(s[1:40] == "CEP com excesso de registros (genérico/hospital)"))
  expect_true(all(s[41:80] == "bairro atribuído"))

  # Os mesmos 40 registros, mas de 2 PESSOAS (ex.: diálise mensal): não é excesso.
  s2 <- localdatasus:::classificar_cep(cep, rep("330045", 80), rep("330045", 80),
                                      rep("b", 80), rep(1, 80), n, total_enderecos = 10000,
                                      id = c(rep(1:2, 20), 3:42))
  expect_true(all(s2[1:40] == "bairro atribuído"))
})

test_that("suprimir esconde contagens pequenas", {
  agg <- data.table::data.table(internacoes = c(0L, 3L, 5L, 20L),
                                obitos_hosp = c(0L, 1L, 2L, 7L),
                                taxa_10mil  = c(0, 1, 2, 3))
  r <- localdatasus:::suprimir(agg, minimo = 5)
  expect_equal(r$internacoes, c(0L, NA, 5L, 20L))
  expect_equal(r$obitos_hosp, c(0L, NA, NA, 7L))
  expect_equal(r$suprimido,   c(FALSE, TRUE, FALSE, FALSE))
  r0 <- localdatasus:::suprimir(data.table::copy(agg), minimo = 0)
  expect_equal(r0$internacoes, agg$internacoes)
})
