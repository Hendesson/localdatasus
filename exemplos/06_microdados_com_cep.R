# =============================================================================
# Exemplo 6 — Microdados com CEP, registro a registro (uso avançado)
# =============================================================================
# Para quem quer cada registro (não só a contagem por bairro):
#   baixar_bairro()    baixa uma base com CEP e acrescenta bairro e coordenadas
#   agregar_bairro()   conta por bairro e calcula a taxa
#   adicionar_bairro() faz o mesmo com QUALQUER tabela sua que tenha CEP
#   cnes_coordenadas() coordenadas dos hospitais e clínicas

library(localdatasus)
dir.create("resultados", showWarnings = FALSE)

# Bases disponíveis registro a registro
sistemas_bairro()[, c("sistema", "descricao")]

# 1) Quimioterapia (APAC) no RJ, janeiro de 2024
quimio <- baixar_bairro("SIA-AQ", "RJ", 2024, mes_inicio = 1, mes_fim = 1)
attr(quimio, "cobertura")          # quantos ganharam bairro e por que não
head(quimio[, c("AP_CIDPRI", "cep_paciente", "bairro", "lat_cep", "lon_cep")])

# Pacientes DISTINTOS por bairro (a mesma pessoa aparece em vários meses)
pacientes <- agregar_bairro(quimio, contar_distintos = "id_paciente")
head(pacientes[order(-pacientes$n), c("bairro", "n", "populacao", "taxa_10mil")])

# 2) Uma tabela sua, com CEP
minha <- data.frame(
  id  = 1:4,
  cep = c("20031-170", "22041001", "24020005", "00000000")
)
com_bairro <- adicionar_bairro(minha, "RJ", col_cep = "cep")
com_bairro[, c("id", "cep", "situacao_cep", "bairro", "lat_bairro", "lon_bairro")]

# 3) Onde ficam os estabelecimentos que atenderam esses pacientes
estab <- cnes_coordenadas(head(unique(quimio$AP_CODUNI), 5))
estab[, c("cnes", "nome", "bairro", "lat", "lon")]


# --- Conferência -------------------------------------------------------------
stopifnot(
  nrow(quimio) > 1000,
  is.data.frame(attr(quimio, "cobertura")),
  nrow(com_bairro) == 4,
  com_bairro$situacao_cep[4] == "CEP inválido/ausente" ||
    com_bairro$situacao_cep[4] != "bairro atribuído",
  nrow(estab) == 5
)
