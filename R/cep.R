# Tabela CEP -> bairro, a partir do CNEFE 2022 (IBGE).

URL_CNEFE <- paste0(
  "https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/",
  "Censo_Demografico_2022/Arquivos_CNEFE/CSV/UF/%s_%s.zip"
)
# Pasta com um arquivo por município (ex.: .../Municipio/35_SP/).
URL_CNEFE_MUN <- paste0(
  "https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/",
  "Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/%s_%s/"
)

#' Tabela de CEPs e bairros de uma UF
#'
#' Monta, a partir do CNEFE 2022 (todos os endereços do Censo), a ligação
#' entre cada CEP e o bairro onde estão seus endereços.
#'
#' O bairro de cada endereço vem do bairro OFICIAL do IBGE (pelo setor
#' censitário), quando o município tem bairros oficiais; senão, da
#' localidade informada no próprio endereço do CNEFE. Cada CEP recebe o
#' bairro onde está a MAIORIA dos seus endereços.
#'
#' Na primeira vez baixa o CNEFE da UF (de ~10 MB no AC a ~1 GB em SP) e
#' guarda só a tabela resultante no cache; as próximas chamadas são
#' instantâneas.
#'
#' @param uf Sigla da UF, por exemplo `"RJ"`.
#' @param manter_cnefe Se `TRUE`, mantém o arquivo do CNEFE no cache depois
#'   de processado (ocupa espaço). Padrão: `FALSE`.
#' @return Uma lista com dois `data.frame`s:
#'   \describe{
#'     \item{ceps}{`cep`, `codmun_cep`, `id_bairro`, `pct_bairro` (fração
#'       dos endereços do CEP que está nesse bairro), `n_end_cep` (nº de
#'       endereços do CEP) e `lat_cep`, `lon_cep` (mediana das coordenadas
#'       dos endereços do CEP).}
#'     \item{bairros}{`id_bairro`, `codmun`, `bairro`, `fonte_bairro`,
#'       `cd_bairro_ibge`, `lat_bairro`, `lon_bairro` (mediana das
#'       coordenadas dos endereços do bairro) e `n_enderecos`.}
#'   }
#' @export
#' @examplesIf interactive()
#' tab <- cep_bairro("AC")
#' head(tab$ceps)
cep_bairro <- function(uf, manter_cnefe = FALSE) {
  cod <- codigo_uf(uf)
  uf  <- toupper(uf)
  # Versão do formato da tabela: v2 incluiu lat_cep/lon_cep; v3 passou a
  # dar o bairro oficial às localidades com o mesmo nome (ver abaixo).
  arq_rds <- file.path(pasta_cache(), paste0("cep_bairro_v3_", uf, ".rds"))
  if (file.exists(arq_rds)) return(readRDS(arq_rds))
  # Sem a barra de progresso do data.table ("Processados ... grupos").
  op <- options(datatable.showProgress = FALSE)
  on.exit(options(op), add = TRUE)

  setores <- setores_censo()[CD_UF == cod & !is.na(CD_BAIRRO), .(CD_SETOR, CD_BAIRRO, NM_BAIRRO)]

  message("Baixando o CNEFE 2022 de ", uf, " (s\u00f3 na primeira vez)...")
  url <- sprintf(URL_CNEFE, cod, uf)
  zip <- baixar(url, file.path(pasta_cache(), basename(url)))
  csv <- utils::unzip(zip, exdir = tempdir())
  on.exit(unlink(csv), add = TRUE)

  detalhar("Processando endere\u00e7os...")
  end <- data.table::fread(
    csv, sep = ";", encoding = "Latin-1",
    select = c("COD_MUNICIPIO", "COD_SETOR", "CEP", "DSC_LOCALIDADE", "LATITUDE", "LONGITUDE"),
    colClasses = list(character = c("COD_MUNICIPIO", "COD_SETOR", "CEP", "DSC_LOCALIDADE")),
    showProgress = FALSE
  )
  if (!manter_cnefe) unlink(zip)

  # O código do setor no CNEFE tem uma letra no fim; na tabela do Censo, não.
  end[, CD_SETOR := gsub("\\D", "", COD_SETOR)]
  end <- setores[end, on = "CD_SETOR"]           # left join: mantém todos os endereços
  end[, `:=`(
    codmun       = substr(COD_MUNICIPIO, 1, 6),
    oficial      = !is.na(CD_BAIRRO),
    localidade   = trimws(DSC_LOCALIDADE)
  )]
  # Endereços em setores sem bairro (ex.: 13,6% em Recife, 2,3% no Rio)
  # ficariam com a localidade escrita no endereço ("AFOGADOS"), duplicando
  # o bairro oficial ("Afogados"). Se o nome bate com um bairro oficial do
  # mesmo município, vale o oficial.
  ofic <- unique(setores_censo()[CD_UF == cod & !is.na(CD_BAIRRO), .(codmun, CD_BAIRRO, NM_BAIRRO)])
  ofic[, chave := chave_bairro(NM_BAIRRO)]
  ofic <- ofic[!duplicated(ofic[, .(codmun, chave)]) & !duplicated(ofic[, .(codmun, chave)], fromLast = TRUE)]
  locs <- unique(end[!oficial & codmun %in% ofic$codmun, .(codmun, localidade)])
  locs[, chave := chave_bairro(localidade)]
  locs <- ofic[locs, on = .(codmun, chave), nomatch = NULL]
  if (nrow(locs) > 0) {
    end[locs, on = .(codmun, localidade), `:=`(CD_BAIRRO = i.CD_BAIRRO, NM_BAIRRO = i.NM_BAIRRO)]
    end[, oficial := !is.na(CD_BAIRRO)]
  }
  end[, `:=`(
    bairro       = data.table::fifelse(oficial, NM_BAIRRO, localidade),
    fonte_bairro = data.table::fifelse(oficial, "IBGE 2022 (oficial)", "CNEFE (localidade)")
  )]
  # Coordenadas de cada CEP: mediana de TODOS os seus endereços.
  coord_cep <- end[, .(lat_cep = stats::median(LATITUDE, na.rm = TRUE),
                       lon_cep = stats::median(LONGITUDE, na.rm = TRUE)), by = .(cep = CEP)]

  end <- end[!is.na(bairro) & bairro != ""]
  end[, id_bairro := data.table::fifelse(oficial, CD_BAIRRO, paste0(codmun, "_", bairro))]

  bairros <- end[, .(
    codmun         = codmun[1],
    bairro         = bairro[1],
    fonte_bairro   = fonte_bairro[1],
    cd_bairro_ibge = CD_BAIRRO[1],
    lat_bairro     = stats::median(LATITUDE, na.rm = TRUE),
    lon_bairro     = stats::median(LONGITUDE, na.rm = TRUE),
    n_enderecos    = .N
  ), by = id_bairro]

  # Para cada CEP: bairro com mais endereços e a fração que ele representa.
  ceps <- end[, .N, by = .(cep = CEP, codmun_cep = codmun, id_bairro)]
  ceps[, `:=`(pct_bairro = N / sum(N), n_end_cep = sum(N)), by = cep]
  data.table::setorder(ceps, cep, -N)
  ceps <- ceps[!duplicated(cep), .(cep, codmun_cep, id_bairro, pct_bairro, n_end_cep)]
  ceps <- coord_cep[ceps, on = "cep"]

  res <- list(ceps = as.data.frame(ceps), bairros = as.data.frame(bairros))
  saveRDS(res, arq_rds)
  res
}
