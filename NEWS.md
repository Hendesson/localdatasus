# localdatasus (em desenvolvimento)

* Nova fonte: TabNet da Secretaria Municipal de Saúde de Fortaleza (`FOR-SIM` e `FOR-SINASC`), com óbitos e nascidos vivos por bairro de residência desde 1999. `obitos_bairro("Fortaleza", ...)` e `nascimentos_bairro("Fortaleza", ...)` passam a usá-la.
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
