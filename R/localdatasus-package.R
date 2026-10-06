#' localdatasus: dados de saúde do SUS por bairro
#'
#' Dados de saúde do SUS por bairro.
#'
#' Funções simples (comece por aqui):
#' \itemize{
#'   \item [onde_tem_bairro()]: que dados existem por bairro, onde e para quais anos;
#'   \item [obitos_bairro()]: óbitos por bairro, por causa (CID) e ano;
#'   \item [nascimentos_bairro()]: nascidos vivos por bairro;
#'   \item [agravos_bairro()]: dengue, tuberculose, sífilis, violência... por bairro;
#'   \item [internacoes_bairro()]: internações do SUS por bairro (pelo CEP);
#'   \item [mapa_bairro()] e [mapa_interativo()]: mapas prontos dessas tabelas;
#'   \item [ajuda()]: explica os códigos de erro (ex.: `ajuda("LDS-03")`).
#' }
#'
#' Funções avançadas:
#' \itemize{
#'   \item [baixar_bairro()]: microdados com CEP, registro a registro, já com bairro;
#'   \item [adicionar_bairro()]: localiza por bairro qualquer tabela com CEP;
#'   \item [agregar_bairro()]: conta por bairro e calcula taxas;
#'   \item [tabnet_bairro()], [tabnet_opcoes()], [fontes_tabnet()]: TabNets regionais;
#'   \item [ligar_bairros()]: liga nomes de bairro aos bairros do IBGE;
#'   \item [cep_bairro()], [populacao_bairro()], [distritos_censo()]: tabelas do IBGE;
#'   \item [cnes_coordenadas()]: coordenadas dos estabelecimentos de saúde;
#'   \item [limpar_cache()]: apaga os arquivos baixados.
#' }
#'
#' @keywords internal
#' @importFrom data.table := .N
"_PACKAGE"

# Nomes de colunas usados dentro de data.table[...] (evita avisos do
# R CMD check sobre "variáveis globais sem definição").
utils::globalVariables(c(
  "chave_mun", "motivo_finalizacao",
  "CO_MOTIVO_DESAB", "CO_IBGE", "CO_AMBULATORIAL_SUS", "TP_UNIDADE", "tipo",
  "mes", "idade", "faixa_etaria", "municipio_res", "nasc", "data",
  "ANO_ARQUIVO", "CLASSI_FIN", "CS_SEXO", "DT_NOTIFIC", "NU_CEP", "classificacao",
  "i.CD_BAIRRO", "i.NM_BAIRRO",
  ".", "CD_SETOR", "CD_UF", "CD_MUN", "NM_MUN", "CD_BAIRRO", "NM_BAIRRO", "CD_FCU",
  "v0001", "codmun", "populacao", "COD_MUNICIPIO", "COD_SETOR", "CEP", "DSC_LOCALIDADE",
  "LATITUDE", "LONGITUDE", "oficial", "localidade", "bairro", "fonte_bairro", "id_bairro",
  "N", "cep", "codmun_cep", "pct_bairro", "n_end_cep", "situacao_cep", "codmun_res",
  "cd_bairro_ibge", "lat", "lon", "pct", "n_aih", "dt_inter", "ano", "grupo", "diag_princ",
  "sexo", "obito_hosp", "internacoes", "obitos_hosp", "taxa_10mil", "suprimido",
  "IDENT", "MUNIC_RES", "DIAG_PRINC", "N_AIH", "DT_INTER", "MORTE", "SEXO",
  "lat_bairro", "lon_bairro", "lat_cep", "lon_cep", ".ordem", "codmun_paciente",
  "data_ref", "ano_ref", "id_paciente", "AP_CNSPCN", "n",
  "CD_DIST", "NM_DIST", "id_unidade", "nome_ibge", "fonte_unidade", "cd_ibge", "chave",
  "chave_sn", "ign", "ligacao", "linha", "bairro_tabnet", "nome_mun", "uf_rot", "fonte",
  "categoria", "coluna", "cd_distrito_ibge", "distrito", "lat_distrito", "lon_distrito",
  "nome", "lat_bairro", "lon_bairro", "municipio",
  "sigla", "cod_uf", "nome_final", "taxa_por_10mil", "unidade", "codmun_paciente",
  "bairro_no_site", "codigo_municipio", "codigo_ibge",
  "valor_mapa", "contagem", "codigo"
))
