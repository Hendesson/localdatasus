# Exemplos do localdatasus

Cada script mostra uma funcionalidade, com comentários. Todos terminam com uma **conferência** (`stopifnot`), que para com erro se o resultado não fizer sentido.

| Script | O que mostra |
|---|---|
| [`01_onde_tem_bairro.R`](01_onde_tem_bairro.R) | que dados existem por bairro, onde e com qual função |
| [`02_obitos.R`](02_obitos.R) | óbitos por bairro: CID, série histórica, `por = "sexo"`, Niterói, São Paulo, mapa rápido |
| [`03_nascimentos.R`](03_nascimentos.R) | nascidos vivos por bairro e proporção de cesáreas |
| [`04_agravos.R`](04_agravos.R) | dengue, tuberculose e sífilis congênita por bairro (Rio, São Paulo, Joinville) |
| [`05_internacoes.R`](05_internacoes.R) | internações do SUS por bairro, pelo CEP do paciente (**pesado**) |
| [`06_microdados_com_cep.R`](06_microdados_com_cep.R) | registros individuais com CEP, tabelas próprias com CEP e coordenadas de hospitais |
| [`07_tabnet_avancado.R`](07_tabnet_avancado.R) | consultas direto nos TabNets, com qualquer filtro do site |
| [`08_ibge_e_nomes.R`](08_ibge_e_nomes.R) | população por bairro e distrito, CEPs, e ligação de nomes de bairro "sujos" |
| [`09_mapas.R`](09_mapas.R) | mapas estáticos (ggplot2) e interativos (leaflet), painéis por sexo, salvar em PNG/HTML |
| [`testar_tudo.R`](testar_tudo.R) | roda todos os exemplos e mostra um relatório OK/FALHOU |

## Como rodar

```r
# 1. Instale o pacote (uma vez)
remotes::install_github("rfsaldanha/microdatasus")
remotes::install_github("Hendesson/localdatasus")

# 2. No RStudio, abra exemplos/testar_tudo.R,
#    Session > Set Working Directory > To Source File Location, e clique em "Source".
```

Na primeira vez são baixadas as tabelas do IBGE: CNEFE do RJ (~340 MB), de SC (~110 MB) e da cidade de São Paulo (~180 MB). Depois tudo fica em cache e as consultas levam segundos.

Os CSVs gerados vão para `resultados/`. Eles têm `lat` e `lon` de cada bairro e abrem no QGIS como "texto delimitado", com X = `lon` e Y = `lat`.
