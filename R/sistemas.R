# Bases cujo microdado público traz o CEP do paciente.
# (Verificado em out/2026 nos cabeçalhos dos arquivos do FTP do DATASUS.
#  Não têm CEP: SIM, SINASC, SINAN, SIA-PA, SIA-BI, SIA-PS, SIA-SAD,
#  SIH-SP e SIH-ER — nessas o menor nível é o município.)

.sistemas <- data.frame(
  sistema = c("SIH-RD", "SIH-RJ",
              "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-AN", "SIA-AM", "SIA-AD",
              "SIA-ACF", "SIA-AB", "SIA-ABO"),
  descricao = c(
    "Internacoes - AIHs aprovadas",
    "Internacoes - AIHs rejeitadas",
    "APAC Quimioterapia",
    "APAC Radioterapia",
    "APAC Tratamento dialitico",
    "APAC Nefrologia",
    "APAC Medicamentos (componente especializado)",
    "APAC Laudos diversos",
    "APAC Confeccao de fistula arteriovenosa",
    "APAC Cirurgia bariatrica",
    "APAC Acompanhamento pos-cirurgia bariatrica"
  ),
  col_cep   = c("CEP", "CEP", rep("AP_CEPPCN", 9)),
  col_mun   = c("MUNIC_RES", "MUNIC_RES", rep("AP_MUNPCN", 9)),
  # Identifica a PESSOA/episódio: usado para não confundir pacientes com
  # muitos registros (ex.: diálise mensal) com CEPs "genéricos".
  col_id    = c("N_AIH", "N_AIH", rep("AP_CNSPCN", 9)),
  col_data  = c("DT_INTER", "DT_INTER", rep("AP_CMP", 9)),
  formato_data = c("%Y%m%d", "%Y%m%d", rep("%Y%m", 9)),
  col_cid   = c("DIAG_PRINC", "DIAG_PRINC", rep("AP_CIDPRI", 9)),
  # De onde vêm os dados e, quando houver, o nome do bairro escrito na
  # própria declaração (usado quando o CEP não permite atribuir bairro).
  origem    = "microdatasus",
  col_bairro = NA_character_,
  stringsAsFactors = FALSE
)

# Bases publicadas pelas secretarias estaduais, registro a registro, com
# CEP de residência (levantamento de out/2026: só a SES-RJ publica assim).
.sistemas <- rbind(.sistemas, data.frame(
  sistema    = "SINASC-RJ",
  descricao  = "Nascidos vivos - SES-RJ (CSV anual, CEP e bairro da m\u00e3e)",
  col_cep    = "Parturiente - CEP de resid\u00eancia",
  col_mun    = "Parturiente - Munic\u00edpio de resid\u00eancia - c\u00f3digo",
  col_id     = NA_character_,
  col_data   = "Nascido Vivo - Data de nascimento",
  formato_data = "%d/%m/%Y",
  col_cid    = NA_character_,
  origem     = "SES-RJ (CSV)",
  col_bairro = "Parturiente - Bairro de resid\u00eancia",
  stringsAsFactors = FALSE
))

# Arboviroses publicadas pela Prefeitura do Recife, registro a registro, com
# CEP e nome do bairro (ver R/recife.R). O ano é o do arquivo (ano
# epidemiológico de notificação).
.sistemas <- rbind(.sistemas, data.frame(
  sistema    = c("DENGUE-RECIFE", "CHIKUNGUNYA-RECIFE", "ZIKA-RECIFE"),
  descricao  = c("Dengue - casos notificados (Prefeitura do Recife, 2013 em diante)",
                 "Chikungunya - casos (Prefeitura do Recife, 2015 em diante)",
                 "Zika - casos (Prefeitura do Recife, 2015 em diante)"),
  col_cep    = "NU_CEP",
  col_mun    = "ID_MN_RESI",
  col_id     = "NU_NOTIFIC",
  col_data   = "DT_NOTIFIC",
  formato_data = "%Y-%m-%d",
  col_cid    = NA_character_,
  origem     = "Prefeitura do Recife (CSV)",
  col_bairro = "NM_BAIRRO",
  stringsAsFactors = FALSE
))

URL_SINASC_RJ <- "https://sistemas.saude.rj.gov.br/tabnetbd/sinasc/dadoscsv/dnrj%d.zip"

#' Bases de dados disponíveis por bairro
#'
#' Lista as bases cujo microdado público traz o CEP do paciente e que, por
#' isso, podem ser localizadas por bairro, registro a registro, com
#' [baixar_bairro()]: as do DATASUS (via microdatasus) e as publicadas por
#' secretarias estaduais (SINASC do RJ). Para dados AGREGADOS por bairro
#' de outras bases (SIM, SINAN...), veja [fontes_tabnet()].
#'
#' @return Um `data.frame` com o código do sistema (como no microdatasus),
#'   a descrição, as colunas usadas para CEP, município de residência,
#'   identificador, data e CID, a origem dos dados e a coluna com o nome do
#'   bairro (quando existe).
#' @export
#' @examples
#' sistemas_bairro()
sistemas_bairro <- function() {
  .sistemas
}

config_sistema <- function(sistema) {
  cfg <- .sistemas[.sistemas$sistema == toupper(sistema), ]
  if (nrow(cfg) != 1) {
    erro("LDS-16", "Sistema n\u00e3o suportado: ", sistema, ". Veja sistemas_bairro().")
  }
  as.list(cfg)
}
