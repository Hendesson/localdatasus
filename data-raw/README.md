# data-raw

Arquivos usados só para gerar o logo do pacote. Esta pasta não entra no pacote instalado (está no `.Rbuildignore`).

## Logo (`logo.py`)

O logo redesenha o mapa da cólera de John Snow (Londres, 1854): as ruas do Soho e os óbitos. O script `logo.py` é original deste pacote.

## Dados do mapa de John Snow (`snow/`)

Os três arquivos CSV (`snow_deaths.csv`, `snow_pumps.csv`, `snow_streets.csv`) foram exportados dos conjuntos `Snow.deaths`, `Snow.pumps` e `Snow.streets` do pacote R **HistData** (Friendly M et al. HistData: Data Sets from the History of Statistics and Data Visualization. Licença GPL. <https://CRAN.R-project.org/package=HistData>).

Segundo a documentação do HistData, os dados foram digitalizados em 1992 por Rusty Dodson, do National Center for Geographic Information and Analysis (NCGIA), Santa Barbara, a partir do mapa publicado em *Snow on Cholera* (Oxford University Press, 1936), e distribuídos por Tobler W. (1994), *Snow's Cholera Map*.

Por virem de um pacote sob licença GPL, estes arquivos ficam fora do pacote distribuído e mantêm a licença e os créditos de origem; não estão cobertos pela licença MIT do localdatasus.
