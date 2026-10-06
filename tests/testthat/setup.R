# Os testes nunca escrevem no cache do usuário: tudo vai para uma pasta
# dentro do tempdir() da sessão, apagada pelo R no fim (política do CRAN).
pasta_teste <- file.path(tempdir(), "cache_localdatasus_testes")
dir.create(pasta_teste, showWarnings = FALSE)
options(localdatasus.cache = pasta_teste)
