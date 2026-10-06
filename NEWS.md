# localdatasus 0.2.0

* Correção: a padronização de nomes e rótulos tirava os acentos de forma diferente no macOS ("Etária" virava "Et'aria"), o que podia impedir a escolha de opções nos TabNets. Agora o resultado é o mesmo em Linux, macOS e Windows.
* Preparação para o CRAN: o 'microdatasus' passa a vir do CRAN (sai o campo `Remotes`); ORCID do autor; exemplos que baixam dados usam `@examplesIf interactive()`; `ajuda()` devolve um objeto com método `print()`; os testes não usam a internet nem o cache do usuário; nova vinheta "Primeiros passos"; guia de contribuição e checagem automática no GitHub Actions (Linux, macOS e Windows).
* Nova fonte: TabNet da Secretaria Municipal de Saúde de Fortaleza (`FOR-SIM` e `FOR-SINASC`), com óbitos e nascidos vivos por bairro de residência desde 1999. `obitos_bairro("Fortaleza", ...)` e `nascimentos_bairro("Fortaleza", ...)` passam a usá-la.
* Nova função `samu_bairro()`: chamados do SAMU 192 do Recife e região metropolitana por bairro do local da ocorrência (dados abertos da Prefeitura do Recife, 2016 em diante), com filtro por tipo de ocorrência e aberturas por subtipo, sexo, origem do chamado e desfecho.
* Nova função `estabelecimentos_bairro()`: estabelecimentos de saúde ativos do CNES (Portal de Dados Abertos do SUS) por bairro, em qualquer município, com filtro por tipo de unidade (descrições da API de Dados Abertos do Ministério da Saúde) e opção de contar só os que atendem pelo SUS.
* Nova função `atendimentos_bairro()`: atendimentos médicos da rede municipal de Curitiba (Sistema e-Saúde, dados abertos, 2019 em diante) por bairro de residência do paciente, com filtro por CID e aberturas por sexo, faixa etária, tipo de unidade, profissional e internamento. Os meses baixados ficam no cache.
* Nova fonte: arboviroses da Prefeitura do Recife (`DENGUE-RECIFE`, `CHIKUNGUNYA-RECIFE`, `ZIKA-RECIFE`), registros individuais de 2013 em diante com CEP e nome do bairro. `agravos_bairro("dengue", "Recife", ...)` passa a usá-las; `por = "classificacao"` separa confirmados e descartados.
* Tabela CEP → bairro: endereços do CNEFE em setores sem bairro oficial passam a receber o bairro oficial quando o nome da localidade é o mesmo (em Recife, os endereços sem bairro oficial caíram de 13,6% para 0,4%; no Rio, de 2,3% para 0,3%). O cache é refeito automaticamente (`cep_bairro_v3`).
* `agregar_bairro()` e as funções simples usam a população dos bairros da UF certa (antes, só do RJ).
* A ligação aproximada de nomes ignora os espaços ("Bom Sucesso" liga a "Bonsucesso").
* Fontes que só filtram por capítulo da CID dão um erro claro (`LDS-09`) quando recebem códigos de 3 caracteres.
* Correção: consultas a fontes estaduais sem registros no município pedido davam erro interno; agora dão `LDS-12`.

# localdatasus 0.1.0

Primeira versão pública.

* Funções simples: `onde_tem_bairro()`, `obitos_bairro()`, `nascimentos_bairro()`, `agravos_bairro()` e `internacoes_bairro()`.
* Mapas: `mapa_bairro()` (ggplot2) e `mapa_interativo()` (leaflet), com contornos de bairros e distritos do Censo 2022.
* Erros e avisos com código (`LDS-01` a `LDS-23`) e `ajuda()`.
* Microdados com CEP (SIH, APACs, SINASC-RJ) localizados por bairro pelo CNEFE 2022.
* TabNets regionais com bairro: Prefeitura do Rio, SES-RJ, Prefeitura de São Paulo (distritos) e DIVE-SC.
* Exemplos comentados em `exemplos/`.
