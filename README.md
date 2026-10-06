# localdatasus <img src="man/figures/logo.png" align="right" height="139" alt="logo do localdatasus" />

Dados de saúde do SUS **por bairro**, em R, com população, taxas e coordenadas.

O [microdatasus](https://github.com/rfsaldanha/microdatasus) traz os dados do SUS por **município**. O `localdatasus` desce ao **bairro**. Ele junta três coisas:
- os microdados com CEP do paciente;
- os arquivos das secretarias estaduais;
- os TabNets das prefeituras e dos estados que tabulam por bairro.

Cada bairro é ligado ao bairro oficial do IBGE, com população do Censo 2022 e coordenadas.

## Instalação

```r
# install.packages("remotes")
remotes::install_github("Hendesson/localdatasus")
```

O pacote [microdatasus](https://CRAN.R-project.org/package=microdatasus), usado para os microdados do DATASUS, é instalado junto, a partir do CRAN.

## Uso rápido

Diga **o quê**, **onde** e **quando**. O pacote escolhe a fonte certa sozinho.

```r
library(localdatasus)

onde_tem_bairro()                      # o que existe, e onde

# Óbitos
obitos_bairro("Rio de Janeiro", 2023)                              # todas as causas
obitos_bairro("Rio de Janeiro", 2019:2023, cid = "I")              # circulatório (capítulo IX)
obitos_bairro("Rio de Janeiro", 2023, cid = c("I", "J"), por = "sexo")
obitos_bairro("Niterói", 2022, cid = "I20-I25")                    # doenças isquêmicas
obitos_bairro(uf = "RJ", anos = 2023)                              # todos os municípios do RJ
obitos_bairro("São Paulo", 2023, por = "faixa etaria")             # por distrito
obitos_bairro("Fortaleza", 2023, cid = "I")                        # TabNet da SMS de Fortaleza

# Nascimentos
nascimentos_bairro("Rio de Janeiro", 2023, por = "tipo de parto")
nascimentos_bairro("Niterói", 2023)
nascimentos_bairro("Fortaleza", 2023)

# Agravos de notificação (SINAN)
agravos_bairro("dengue", "Rio de Janeiro", 2024)
agravos_bairro("tuberculose", "São Paulo", 2022:2024)
agravos_bairro("dengue", "Joinville", 2024, por = "sexo")
agravos_bairro("dengue", "Recife", 2024, por = "classificacao")  # notificados: confirmados e descartados

# Internações (pelo CEP do paciente)
internacoes_bairro("Rio de Janeiro", 2023, cid = c("I", "J"), uf = "RJ")

# Chamados do SAMU 192 do Recife e região metropolitana (local da ocorrência)
samu_bairro("Recife", 2024, tipo = "causas externas", por = "subtipo")

# Estabelecimentos de saúde (CNES) por bairro, em qualquer município
estabelecimentos_bairro("Recife", tipo = "unidade basica")

# Atendimentos médicos da rede municipal de Curitiba (e-Saúde), com CID
atendimentos_bairro("Curitiba", 2025, meses = 1:3, cid = "J")
```

As regras de escrita são poucas:
- **Nomes sem rigor:** o município e o `por` aceitam maiúsculas ou minúsculas, com ou sem acento ("niteroi", "faixa etaria").
- **CID:** use uma letra para o capítulo inteiro (`"I"`, `"J"`), códigos de 3 caracteres (`"I21"`) ou intervalos (`"I20-I25"`).
- **Município repetido:** se o nome existir em mais de uma UF, informe também a UF: `obitos_bairro("Bom Jardim", 2023, uf = "RJ")`.

Todas as funções devolvem uma tabela no mesmo formato:

| coluna | o que é |
|---|---|
| `ano`, `codigo_municipio`, `municipio` | |
| `bairro` (ou `distrito`) | nome oficial do IBGE |
| `bairro_no_site` | como o bairro aparece na fonte |
| `sexo`, `faixa_etaria`… | a variável pedida em `por` |
| `obitos` / `nascimentos` / `casos` | a contagem |
| `populacao`, `taxa_por_10mil` | Censo 2022 |
| `lat`, `lon`, `codigo_ibge` | para mapas (QGIS, PostGIS) |
| `ligacao`, `fonte` | como o nome foi ligado ao IBGE e de onde veio o dado |

### Mapas

```r
x <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
mapa_bairro(x)                                   # mapa pronto (ggplot2), pintado pela taxa
mapa_bairro(x, valor = "obitos", cores = "Blues")
mapa_interativo(x)                               # mapa interativo (leaflet): passe o mouse
```

Os contornos são os bairros e distritos oficiais do IBGE (Censo 2022), baixados uma vez. Bairros sem contorno oficial aparecem como pontos. Se a tabela tiver vários anos, ou um `por`, o mapa é dividido em painéis. `mapa_bairro()` devolve um ggplot comum, que você pode ajustar com `+` e salvar com `ggplot2::ggsave()`. Para usar os mapas, instale uma vez: `install.packages(c("sf", "ggplot2", "leaflet"))`.

### Erros com código

Cada erro ou aviso traz um código. `ajuda()` explica o que aconteceu e como resolver:

```r
obitos_bairro("Bom Jardim", 2023)
#> Erro: [LDS-03] Há mais de um município chamado "Bom Jardim" (MA, PE, RJ). ...
#>   Para entender e resolver: ajuda("LDS-03")

ajuda("LDS-03")    # o que aconteceu e como resolver
ajuda()            # lista todos os códigos
```

O pacote trabalha em silêncio e mostra só um resumo no fim ("Pronto: ..."). Para acompanhar cada consulta e cada download, use `options(localdatasus.detalhes = TRUE)`.

Há exemplos comentados de cada funcionalidade na pasta [`exemplos/`](https://github.com/Hendesson/localdatasus/tree/main/exemplos). O script `exemplos/testar_tudo.R` roda todos de uma vez e mostra um relatório.

Para salvar e abrir no QGIS:

```r
x <- obitos_bairro("Rio de Janeiro", 2023, cid = "I")
write.csv(x, "obitos_circulatorio_rio_2023.csv", row.names = FALSE)
```

---

# Uso avançado

## Bases com CEP (registro a registro)

```r
library(localdatasus)
sistemas_bairro()
```

| Sistema | Base |
|---|---|
| `SIH-RD` | Internações (AIHs aprovadas) |
| `SIH-RJ` | Internações (AIHs rejeitadas) |
| `SIA-AQ` | APAC Quimioterapia |
| `SIA-AR` | APAC Radioterapia |
| `SIA-ATD` | APAC Tratamento dialítico |
| `SIA-AN` | APAC Nefrologia |
| `SIA-AM` | APAC Medicamentos (componente especializado) |
| `SIA-AD` | APAC Laudos diversos |
| `SIA-ACF` | APAC Confecção de fístula arteriovenosa |
| `SIA-AB` | APAC Cirurgia bariátrica |
| `SIA-ABO` | APAC Acompanhamento pós-bariátrica |
| `SINASC-RJ` | Nascidos vivos da SES-RJ (CSV anual, 1996 em diante), com CEP **e nome do bairro** da mãe |
| `DENGUE-RECIFE`, `CHIKUNGUNYA-RECIFE`, `ZIKA-RECIFE` | Arboviroses da Prefeitura do Recife (dados abertos, 2013 em diante), registro a registro, com CEP **e nome do bairro** |

`samu_bairro()` conta os **chamados do SAMU 192 do Recife** (serviço metropolitano: Recife, Jaboatão dos Guararapes, Olinda, Paulista e outros) por bairro do **local da ocorrência**, de 2016 em diante, pelo portal de dados abertos da Prefeitura do Recife.

`estabelecimentos_bairro()` conta os **estabelecimentos de saúde ativos** do CNES (cadastro nacional publicado no Portal de Dados Abertos do SUS) por bairro, em qualquer município, pelo CEP do estabelecimento e, quando ele não resolve, pelo nome do bairro informado no cadastro. Mede a oferta de serviços no bairro, não a residência dos pacientes.

Fora do DATASUS, `atendimentos_bairro()` traz os **atendimentos médicos da rede municipal de Curitiba** (Sistema e-Saúde, dados abertos da Prefeitura, 2019 em diante), com o bairro do paciente e a CID. Cada arquivo publicado tem cerca de 480 MB e cobre três meses; os meses baixados ficam no cache.

Os microdados do DATASUS destas bases **não têm CEP**, só o município: SIM (óbitos), SINASC (nascimentos), SINAN (agravos), SIA-PA e BPA-I (ambulatorial), SIA-PS (psicossocial), SIA-SAD, SIH-SP e SIH-ER. Para SIM, SINASC e SINAN existem, em alguns lugares, **contagens por bairro** nos TabNets regionais (abaixo).

## Bases agregadas por bairro (TabNets regionais)

```r
fontes_tabnet()
```

| Fonte | Abrangência | Unidade | Bases |
|---|---|---|---|
| `RIO-SIM`, `RIO-SINASC` | Rio de Janeiro (capital), 2006+ | bairro | óbitos e nascidos vivos |
| `RIO-SINAN-*` | Rio de Janeiro (capital), 2007+ | bairro | 15 agravos: dengue, tuberculose, violência, sífilis (gestante e congênita), aids, gestante HIV, hepatites, hanseníase, meningite, leptospirose, chikungunya, zika, sarampo/rubéola |
| `SES-RJ-SIM` | estado do RJ, todos os municípios (bairro a partir de 2011) | bairro | óbitos |
| `SC-SINAN-*` | estado de SC, todos os municípios | bairro | dengue, chikungunya, leishmaniose, leptospirose, violência, notificação individual |
| `SP-SIM`, `SP-SINASC`, `SP-SINAN-*` | São Paulo (capital) | distrito (96) | óbitos, nascidos vivos, tuberculose, SRAG, aids, meningite, violência |
| `FOR-SIM`, `FOR-SINASC` | Fortaleza (capital), 1999+ | bairro | óbitos (filtro só por capítulo da CID) e nascidos vivos |

Levantamento de out/2026 nos TabNets das 27 UFs e das capitais. Os TabNets estaduais de MG, PE, CE, ES, PB, TO e BA vão só até o município. Nos demais não achamos TabNet público com bairro. O TabNet de Campinas tabula por distrito de saúde e área de abrangência dos centros de saúde, e não por bairro; os de RR e MS estavam bloqueados.

Os TabNets de internações e procedimentos da Prefeitura de SP **não entram**: lá o distrito é o do estabelecimento, não o da residência.

```r
# Óbitos por doenças do aparelho circulatório, por bairro do Rio, por faixa etária
tabnet_opcoes("RIO-SIM", "colunas")                      # o que dá para cruzar
tabnet_opcoes("RIO-SIM", "filtros", filtro = "Causa (Cap CID10)")
tabnet_bairro("RIO-SIM", 2022:2023, coluna = "Faixa Etária",
              filtros = list("Causa (Cap CID10)" = "IX"))

# Dengue por bairro em Joinville; óbitos por distrito em São Paulo
tabnet_bairro("SC-SINAN-DENGUE", 2024, municipios = "420910")
tabnet_bairro("SP-SIM", 2023, coluna = "Sexo")

# Nomes de bairro quaisquer -> bairro do IBGE, coordenadas e população
ligar_bairros(c("Jd. América", "COMPLEXO DA MARE"), "330455", "RJ")
```

O TabNet traz o **nome** do bairro. `tabnet_bairro()` liga cada nome, dentro do município, ao bairro oficial do IBGE (ou à localidade do CNEFE, onde não há bairros oficiais) ou ao distrito do IBGE. A coluna `ligacao` diz como a ligação foi feita. A ordem de tentativa é:
1. nome idêntico (`exata`), ignorando acentos e abreviações;
2. sem a numeração final;
3. um nome que contém o outro (`parcial`);
4. o nome mais parecido (`aproximada`).

Se nada casar, a linha fica como `não encontrado`, e rótulos como "Ignorado" ficam como `ignorado`. Cada linha ganha `lat`, `lon`, `populacao` (Censo 2022) e `taxa_10mil`.

Uma fonte nova entra copiando uma linha de `fontes_tabnet()`, ajustando a URL e o nome do campo de bairro, e passando essa linha a `tabnet_bairro()`.

## Funções avançadas

```r
# 1. Registro a registro: todas as colunas originais + CEP, bairro e coordenadas
quimio <- baixar_bairro("SIA-AQ", "RJ", 2024, mes_inicio = 1, mes_fim = 6)
attr(quimio, "cobertura")          # quantos registros ganharam bairro, e por que não

# 2. Contar por bairro (aqui, pacientes distintos) e calcular a taxa por 10 mil
agregar_bairro(quimio, por = "ano_ref", contar_distintos = "id_paciente")

# 3. Atalho para internações, contadas pela DATA DE INTERNAÇÃO
sih_bairro("RJ", 2023, cid = c("I", "J"))                     # por bairro
sih_bairro("RJ", 2023, cid = "I21", nivel = "internacao")     # uma linha por internação

# 4. Nascidos vivos do RJ: o CEP dá o bairro; quando não dá, o nome do bairro da DN completa
nasc <- baixar_bairro("SINASC-RJ", "RJ", 2023)
table(nasc$origem_bairro, useNA = "ifany")

# 5. Qualquer tabela sua que tenha CEP
adicionar_bairro(minha_tabela, "RJ", col_cep = "cep")

# 6. Coordenadas dos estabelecimentos (hospitais, clínicas)
cnes_coordenadas(unique(quimio$AP_CODUNI))

# Tabelas de apoio
cep_bairro("RJ")         # CEP -> bairro, com coordenadas do CEP
populacao_bairro("RJ")   # população por bairro oficial (Censo 2022)
distritos_censo("355030")  # população e coordenadas dos distritos de um município
```

### Colunas acrescentadas a cada registro

| coluna | descrição |
|---|---|
| `cep_paciente` | CEP normalizado (8 dígitos) |
| `codmun_paciente` | município de residência (6 dígitos) |
| `situacao_cep` | se o CEP permitiu atribuir bairro e, se não, por quê |
| `id_bairro`, `bairro`, `fonte_bairro`, `cd_bairro_ibge` | o bairro: oficial do IBGE ou localidade do CNEFE |
| `lat_cep`, `lon_cep` | ponto do CEP (mediana das coordenadas dos seus endereços no CNEFE) |
| `lat_bairro`, `lon_bairro` | ponto do bairro (mediana dos endereços do bairro) |
| `n_end_cep`, `pct_bairro`, `codmun_cep` | nº de endereços do CEP, fração deles no bairro e município do CEP |
| `data_ref`, `ano_ref` | data de internação (SIH) ou competência de realização (APAC) |
| `id_paciente` | (APAC) cartão SUS pseudonimizado, em texto: segue o mesmo paciente entre meses |
| `origem_bairro` | `CEP` ou `nome do bairro (...)`: de onde veio o bairro do registro |

## Como o bairro é atribuído

1. **CNEFE 2022:** cada endereço tem CEP, coordenadas e setor censitário.
2. **Bairro de cada endereço:** o bairro **oficial do IBGE** (pelo setor) nos 895 municípios que têm bairros oficiais; nos demais, a **localidade** do endereço.
3. **Bairro de cada CEP:** o bairro onde está a maioria dos seus endereços.
4. **CEPs que não recebem bairro** (o motivo fica em `situacao_cep`):
   - CEP inválido;
   - paciente de outra UF;
   - CEP especial (final 900–999);
   - CEP inexistente no CNEFE;
   - CEP "com excesso", isto é, com pessoas distintas demais para o número de endereços (CEP geral do município usado como padrão, ou CEP do hospital);
   - CEP de outro município;
   - CEP que cobre vários bairros.

## Resultados de referência (RJ)

| Base | Período | Registros com bairro |
|---|---|---|
| SIH-RD (I e J) | 2023 | 57,0% |
| SIA-AQ | jan–fev/2024 | 75,5% |
| SIA-ATD | jan–mar/2024 | 75,6% |
| SINASC-RJ | 2023 | 86,1% (44,2% pelo CEP + 41,9% pelo nome do bairro) |

Nos TabNets, a parte das contagens ligadas a um bairro ou distrito do IBGE foi:

| Fonte | Ano | Ligado |
|---|---|---|
| RIO-SIM, RIO-SINASC | 2023 | 99–100% |
| RIO-SINAN-* (dengue, tuberculose, violência…) | 2023 | 97–100% |
| SES-RJ-SIM | 2023 | 86% (os loteamentos da Baixada não são bairros do IBGE) |
| SC-SINAN-DENGUE (Joinville e Florianópolis) | 2024 | 99,3% |
| SP-SIM, SP-SINASC | 2023 | 98–100% (o resto é "Ignorado") |
| FOR-SIM, FOR-SINASC | 2023 | 95% e 90% (o resto é "Ignorados" na fonte ou nomes sem bairro no IBGE) |

No Acre (SIH 2023), só 9% receberam bairro: os hospitais de Rio Branco registram CEPs dos Correios, e as cidades do interior têm CEP único. **Confira sempre `attr(x, "cobertura")`.**

## Limitações

- **Perda não aleatória:** é maior onde os municípios têm CEP único e onde os endereços são informais. Compare bairros dentro do mesmo município.
- **CNEFE de 2022:** CEPs criados depois ficam sem bairro.
- **Só SUS:** internações e procedimentos privados não entram.
- **APACs são mensais:** a mesma pessoa aparece em vários meses de tratamento. Use `contar_distintos = "id_paciente"` para contar pessoas.
- **Taxas:** são brutas (sem padronização por idade), usam a população de 2022 e subestimam a taxa real, porque registros sem bairro ficam de fora do numerador.
- **Coordenadas do CNES:** o cadastro tem erros. Por exemplo, um hospital do Rio aparece em Volta Redonda.
- **TabNets:** os dados são **contagens**, sem registro individual nem CEP; cada consulta cruza só bairro × uma variável (o resto vai em `filtros`). Os sites mudam e às vezes saem do ar; as consultas de anos encerrados ficam em cache. Nas fontes de SC, é feita uma consulta por município (cerca de 1 s cada).
- **Distritos de SP:** as coordenadas vêm do CNEFE do município (177 MB, baixado uma vez).

## Cache

Os arquivos ficam em `tools::R_user_dir("localdatasus", "cache")`, e há duas formas de controlar onde e quanto ocupam:
- **Pasta:** troque-a com `options(localdatasus.cache = "...")`.
- **Tamanho:** o CNEFE de cada UF (de ~10 MB no AC a ~1 GB em SP) é baixado uma vez e apagado depois de processado; só a tabela CEP → bairro fica guardada. `limpar_cache()` apaga tudo.

## Como citar

> Alves, Hendesson (2026). *localdatasus: Dados de Saúde do SUS por Bairro (Escala Local)*. Pacote R. <https://github.com/Hendesson/localdatasus>

No R, `citation("localdatasus")` mostra a citação com a versão instalada e a entrada BibTeX.

## Logo

O logo homenageia o mapa da cólera de **John Snow** (Londres, 1854), que ligou as mortes à bomba d'água da Broad Street e se tornou o marco zero da epidemiologia espacial. Ele usa os dados reais do mapa: as ruas do Soho (linhas pretas) e as 578 mortes (pontos vermelhos). Os dados vêm do pacote [HistData](https://cran.r-project.org/package=HistData) (`Snow.streets`, `Snow.deaths`), digitalizados em 1992 por Rusty Dodson (NCGIA) e distribuídos por Waldo Tobler (1994). O script que gera o logo está em [`data-raw/logo.py`](https://github.com/Hendesson/localdatasus/blob/main/data-raw/logo.py); veja os créditos completos em [`data-raw/README.md`](https://github.com/Hendesson/localdatasus/blob/main/data-raw/README.md).
