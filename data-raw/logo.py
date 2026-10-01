# Gera o logo hexagonal do localdatasus: homenagem ao mapa da cólera de
# John Snow (Londres, 1854), o marco zero da epidemiologia espacial.
# Dados reais do mapa (ruas, 578 mortes e 13 bombas d'água) do pacote R
# HistData (Snow.streets, Snow.deaths, Snow.pumps), salvos em data-raw/snow/.
#
# Para gerar: python3 data-raw/logo.py  (cria man/figures/logo.svg)
# PNG: Rscript -e 'rsvg::rsvg_png("man/figures/logo.svg", "man/figures/logo.png", width = 518, height = 600)'
import csv
from collections import defaultdict

W, H = 518, 600
PAPEL, TINTA, AZUL, VERMELHO = "#ffffff", "#000000", "#1d3557", "#d00000"

def ler(arq):
    with open(f"data-raw/snow/{arq}") as f:
        return list(csv.DictReader(f))

# Enquadramento: a bomba da Broad Street fica no centro do mapa.
BOMBA = (12.571360, 11.727170)
K = 36                        # pixels por unidade do mapa original
CX, CY = 259, 215             # onde a bomba aparece no logo
def xy(x, y):
    return CX + (float(x) - BOMBA[0]) * K, CY - (float(y) - BOMBA[1]) * K

# Hexágono um pouco recuado, para a moldura não ser cortada nas pontas.
f = 0.965
hexpts = [(259 + (x-259)*f, 300 + (y-300)*f) for x, y in
          [(259,0),(518,150),(518,450),(259,600),(0,450),(0,150)]]
hexstr = " ".join(f"{x:.1f},{y:.1f}" for x, y in hexpts)

out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
       f'<defs><clipPath id="hex"><polygon points="{hexstr}"/></clipPath></defs>',
       '<g clip-path="url(#hex)">',
       f'<rect width="{W}" height="{H}" fill="{PAPEL}"/>']

# Ruas: cada rua é uma linha com borda escura e miolo cor de papel,
# imitando o traço duplo do mapa original.
ruas = defaultdict(list)
for r in ler("snow_streets.csv"):
    ruas[r["street"]].append(xy(r["x"], r["y"]))
caminhos = [" ".join(("M" if i == 0 else "L") + f" {x:.1f} {y:.1f}" for i, (x, y) in enumerate(p))
            for p in ruas.values()]
for d in caminhos:
    out.append(f'<path d="{d}" stroke="{TINTA}" stroke-width="10" stroke-linecap="round" fill="none"/>')
for d in caminhos:
    out.append(f'<path d="{d}" stroke="{PAPEL}" stroke-width="6.5" stroke-linecap="round" fill="none"/>')

# Mortes: um ponto vermelho para cada morte por cólera (fundo branco,
# ruas pretas, sem bombas nem legenda: só o mapa).
for r in ler("snow_deaths.csv"):
    x, y = xy(r["x"], r["y"])
    out.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="3.3" fill="{VERMELHO}" stroke="{PAPEL}" stroke-width="0.8"/>')

out.append('</g>')

# Faixa com o nome
out.append('<g clip-path="url(#hex)">')
out.append(f'<rect x="0" y="392" width="{W}" height="220" fill="{AZUL}"/>')
out.append(f'<text x="259" y="470" text-anchor="middle" font-family="Lato, DejaVu Sans, sans-serif" '
           f'font-weight="bold" font-size="66" fill="white">localdatasus</text>')
out.append('</g>')

out.append(f'<polygon points="{hexstr}" fill="none" stroke="{AZUL}" stroke-width="18" stroke-linejoin="round"/>')
out.append('</svg>')
open("man/figures/logo.svg", "w").write("\n".join(out))
