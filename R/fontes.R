# Cadastro de TabNets regionais que tabulam por bairro (ou distrito) de
# residência. Levantamento de out/2026: a maioria dos TabNets estaduais vai
# só até o município; os que descem ao bairro estão aqui.
#
# Colunas:
#   fonte       identificador usado em tabnet_bairro()
#   descricao   o que a base contém
#   sistema     SIM, SINASC ou SINAN
#   uf          UF da fonte
#   codmun      município (6 dígitos) quando a fonte é de UM município;
#               NA quando cobre a UF inteira
#   unidade     "bairro" ou "distrito"
#   dialeto     "tabnet" (clássico) ou "dhx" (SES-RJ)
#   url_def     página do formulário
#   linha       rótulo da opção de Linha que tabula por bairro
#   rotulo      como ler o rótulo de cada linha:
#                 "nome"                -> só o nome do bairro
#                 "codigo_nome"         -> "144 CAMPO GRANDE"
#                 "uf_municipio_bairro" -> "RJ, Niterói  - ICARAÍ"
#   descartar   expressão regular das linhas que NÃO são bairro (ex.: as
#               linhas de AP e RA intercaladas no TabNet do Rio)
#   filtro_mun  quando o rótulo não diz o município (fonte estadual com
#               rótulo "nome"): filtro de município de residência, usado
#               para consultar um município de cada vez
#   filtros_fixos  filtros sempre aplicados, "Campo=Opção" separados por ";"
#   incremento  o que é contado (NA = a primeira opção do formulário)
#   filtro_ano  quando cada arquivo do TabNet junta vários anos: filtro
#               usado para separar os anos (NA = um arquivo por ano)

URL_RIO  <- "https://tabnet.rio.rj.gov.br/cgi-bin/dh?"
URL_SC   <- "http://200.19.223.105/cgi-bin/dh?sinan/def/"
URL_SP   <- "https://tabnet.saude.prefeitura.sp.gov.br/cgi/deftohtm3.exe?secretarias/saude/TABNET/"
URL_SES_RJ <- "https://sistemas.saude.rj.gov.br/tabnetbd/dhx.exe?"
URL_FOR  <- "https://tabnet.sms.fortaleza.ce.gov.br/scripts/deftohtm.exe?"

fonte_tab <- function(fonte, descricao, sistema, uf, codmun, unidade, dialeto, url_def, linha,
                      rotulo, descartar = NA, filtro_mun = NA, filtros_fixos = NA,
                      incremento = NA, filtro_ano = NA) {
  data.frame(fonte, descricao, sistema, uf, codmun, unidade, dialeto, url_def, linha, rotulo,
             descartar, filtro_mun, filtros_fixos, incremento, filtro_ano,
             stringsAsFactors = FALSE)
}

# Agravos do SINAN no TabNet da SMS-Rio (todos com "AP RA e Bairro Resid").
.sinan_rio <- c(
  "AIDS"        = "aidsa2007.def|Aids (13 anos e mais)",
  "CHIKUNGUNYA" = "chikungunya.def|Chikungunya",
  "EXANTEMATICAS" = "exant2007.def|Sarampo e rub\u00e9ola",
  "GESTANTE-HIV" = "gest_hiv.def|Gestante HIV+",
  "SIFILIS-GESTANTE" = "gest_sifilis.def|S\u00edfilis em gestante",
  "HANSENIASE"  = "hanse2007.def|Hansen\u00edase",
  "HEPATITES"   = "hepatite.def|Hepatites virais",
  "LEPTOSPIROSE" = "leptospirose.def|Leptospirose",
  "MENINGITE"   = "meningite.def|Meningite",
  "SIFILIS-CONGENITA" = "sifcong2007.def|S\u00edfilis cong\u00eanita",
  "DENGUE-2007" = "sinandengue2007.def|Dengue (2007-2011)",
  "DENGUE"      = "sinandengue2012.def|Dengue (2012 em diante)",
  "TUBERCULOSE" = "tuberc2007.def|Tuberculose",
  "VIOLENCIA"   = "violencias.def|Viol\u00eancia interpessoal/autoprovocada",
  "ZIKA"        = "zika.def|Zika"
)

# Agravos do SINAN no TabNet da DIVE-SC (bairro de residência, estado todo).
.sinan_sc <- c(
  "CHIKUNGUNYA"  = "chikun.def|Chikungunya|Munic\u00edpio Resid SC",
  "DENGUE"       = "dengon.def|Dengue (2014 em diante)|Munic\u00edpio Resid SC",
  "LEISHMANIOSE" = "leish.def|Leishmaniose visceral|Munic\u00edpio Resid SC",
  # A página da leptospirose tem os acentos trocados no próprio site.
  "LEPTOSPIROSE" = "lepto.def|Leptospirose|Munic\u00ddpio Resid SC",
  "NOTIFICACAO"  = "notindiv.def|Notifica\u00e7\u00e3o individual (todos os agravos)|Mun Resid SC",
  "VIOLENCIA"    = "violencia.def|Viol\u00eancia dom\u00e9stica, sexual e outras|Munic\u00edpio Resid SC"
)

# SINAN e afins no TabNet da Prefeitura de São Paulo (por distrito).
.tab_sp <- c(
  "TUBERCULOSE" = "TBWEB/TBWEB.def|Tuberculose|Distrito Administ(Res)",
  "SRAG"        = "RSRAG/sragh.def|SRAG hospitalizado (2020 em diante)|Distrito Administ(Res)",
  "AIDS"        = "AIDS/AIDSAD.def|Aids adulto (at\u00e9 2016)|Dist Administ.(res)",
  "MENINGITE"   = "RMENIN/RmeningeN.def|Meningites|Distrito Administrativo(Res)",
  "VIOLENCIA"   = "SINAN/RVIOLE/RViolenciaNet.def|Viol\u00eancia interpessoal/autoprovocada|DistrAdminist(Res)"
)

dividir <- function(x, i) vapply(strsplit(x, "|", fixed = TRUE), `[`, character(1), i)

.fontes_tabnet <- rbind(
  fonte_tab("RIO-SIM", "\u00d3bitos de residentes (2006 em diante)", "SIM", "RJ", "330455",
            "bairro", "tabnet", paste0(URL_RIO, "sim/definicoes/sim_apos2005.def"),
            "Bairro Residencia", "codigo_nome", filtros_fixos = "Munic Resid=330455"),
  fonte_tab("RIO-SINASC", "Nascidos vivos de m\u00e3es residentes (2006 em diante)", "SINASC", "RJ",
            "330455", "bairro", "tabnet", paste0(URL_RIO, "sinasc/definicoes/sinasc_apos2005.def"),
            "Bairro Residencia", "codigo_nome", filtros_fixos = "Munic Residencia=330455"),
  fonte_tab(paste0("RIO-SINAN-", names(.sinan_rio)), dividir(.sinan_rio, 2), "SINAN", "RJ",
            "330455", "bairro", "tabnet",
            paste0(URL_RIO, "sinan/definicoes/", dividir(.sinan_rio, 1)),
            "AP RA e Bairro Resid", "nome",
            # Linhas intercaladas de Área de Planejamento ("AP 3.1") e de
            # Região Administrativa (números romanos: "XVIII", "VIII TIJUCA").
            descartar = "^(AP [0-9]|[IVXLC]+( |$))",
            filtros_fixos = "Munic. Resid\u00eancia=330455"),
  fonte_tab("SES-RJ-SIM", "\u00d3bitos de residentes no RJ, todos os munic\u00edpios (bairro a partir de 2011)",
            "SIM", "RJ", NA, "bairro", "dhx", paste0(URL_SES_RJ, "sim/tf_sim_do_geral.def"),
            "Bairro de resid\u00eancia", "uf_municipio_bairro",
            incremento = "\u00d3bitos n\u00e3o fetais de residentes RJ"),
  fonte_tab(paste0("SC-SINAN-", names(.sinan_sc)), dividir(.sinan_sc, 2), "SINAN", "SC", NA,
            "bairro", "tabnet", paste0(URL_SC, dividir(.sinan_sc, 1)), "Bairro Resid", "nome",
            filtro_mun = dividir(.sinan_sc, 3)),
  fonte_tab("SP-SIM", "\u00d3bitos de residentes (2006 em diante)", "SIM", "SP", "355030", "distrito",
            "tabnet", paste0(URL_SP, "SIM/obito.def"), "Distrito Admin resid\u00eancia", "nome",
            incremento = "\u00d3bitos Residentes MSP"),
  fonte_tab("SP-SINASC", "Nascidos vivos de m\u00e3es residentes (2006 em diante)", "SINASC", "SP",
            "355030", "distrito", "tabnet", paste0(URL_SP, "sinasc/nascido.def"),
            "Distrito Administ. resid\u00eancia", "nome", incremento = "NV parturientes residentes MSP"),
  fonte_tab(paste0("SP-SINAN-", names(.tab_sp)), dividir(.tab_sp, 2), "SINAN", "SP", "355030",
            "distrito", "tabnet", paste0(URL_SP, dividir(.tab_sp, 1)), dividir(.tab_sp, 3), "nome"),
  # TabNet da SMS de Fortaleza: a base traz também residentes de outros
  # municípios do CE, por isso o filtro fixo de município de residência.
  fonte_tab("FOR-SIM", "\u00d3bitos de residentes (1999 em diante)", "SIM", "CE", "230440", "bairro",
            "tabnet", paste0(URL_FOR, "Obitoscid.def"), "Bairro Resid.(Alfab.)", "nome",
            filtros_fixos = "Munic. Resid-CE=230440"),
  fonte_tab("FOR-SINASC", "Nascidos vivos de m\u00e3es residentes (1999 em diante)", "SINASC", "CE",
            "230440", "bairro", "tabnet", paste0(URL_FOR, "nascido.def"), "Bair.Res.M\u00e3e", "nome",
            filtros_fixos = "Munic Resid-CE M\u00e3e=230440")
)
.fontes_tabnet$sistema[.fontes_tabnet$fonte == "SP-SINAN-SRAG"] <- "SIVEP-Gripe"
# Filtros de causa (CID-10) das bases de óbito: por capítulo e por
# categoria de 3 caracteres. Usados por obitos_bairro(cid = ...).
.fontes_tabnet$filtro_capitulo <- NA_character_
.fontes_tabnet$filtro_cid3 <- NA_character_
cid_sim <- rbind(
  c("RIO-SIM",    "Causa (Cap CID10)",                   "Causa (CID10 3C)"),
  c("SP-SIM",     "Causa(Cap CID10)",                    "Causa(CID10 3C)"),
  c("SES-RJ-SIM", "Causa b\u00e1sica - cap\u00edtulo", "Causa b\u00e1sica - categoria"),
  # Fortaleza só filtra por capítulo (o campo "(CID10)" traz os capítulos).
  c("FOR-SIM",    "Causas B\u00e1sicas (CID10)",         NA)
)
i <- match(cid_sim[, 1], .fontes_tabnet$fonte)
.fontes_tabnet$filtro_capitulo[i] <- cid_sim[, 2]
.fontes_tabnet$filtro_cid3[i] <- cid_sim[, 3]
rm(cid_sim, i)

# Tuberculose em SP: cada arquivo junta vários anos.
.fontes_tabnet$filtro_ano[.fontes_tabnet$fonte == "SP-SINAN-TUBERCULOSE"] <- "Ano incid\u00eancia"

#' TabNets regionais com dados por bairro
#'
#' Lista as tabulações de secretarias estaduais e municipais que descem ao
#' bairro (ou ao distrito) de residência e que podem ser consultadas com
#' [tabnet_bairro()]. São dados AGREGADOS (contagens), não registros.
#'
#' Levantamento feito em out/2026 nos TabNets das 27 UFs e das capitais: a
#' maioria vai só até o município. Para acrescentar uma fonte nova, copie
#' uma linha desta tabela, ajuste-a e passe-a para [tabnet_bairro()].
#'
#' @return Um `data.frame`, uma linha por fonte (ver o código-fonte para o
#'   significado de cada coluna).
#' @export
#' @examples
#' fontes_tabnet()[, c("fonte", "descricao", "unidade")]
fontes_tabnet <- function() {
  .fontes_tabnet
}

config_fonte <- function(fonte) {
  if (is.data.frame(fonte)) {
    if (nrow(fonte) != 1) erro("LDS-16", "Passe UMA linha de fontes_tabnet().")
    return(as.list(fonte))
  }
  cfg <- .fontes_tabnet[.fontes_tabnet$fonte == toupper(fonte), ]
  if (nrow(cfg) != 1) erro("LDS-16", "Fonte desconhecida: ", fonte, ". Veja fontes_tabnet().")
  as.list(cfg)
}
