# Como contribuir com o localdatasus

Obrigado pelo interesse! Toda ajuda é bem-vinda: relatar um erro, sugerir uma fonte de dados nova, melhorar a documentação ou enviar código.

## Relatar um problema

Abra uma [issue](https://github.com/Hendesson/localdatasus/issues) com:

- o código que você rodou (o menor exemplo possível);
- a mensagem de erro completa, incluindo o código `LDS-xx`, se houver;
- a saída de `sessionInfo()`.

Antes, veja se `ajuda("LDS-xx")` já explica o problema.

## Sugerir uma fonte de dados

Conhece um TabNet, um portal de dados abertos ou um arquivo de secretaria de saúde com dados por **bairro**? Abra uma issue com o endereço, o sistema (SIM, SINASC, SINAN...), o período e um exemplo de como o bairro aparece.

## Enviar código

1. Faça um *fork* do repositório e crie um *branch* para a sua mudança.
2. Siga o estilo do código existente: nomes de funções e argumentos em português, mensagens de erro com `erro("LDS-xx", ...)` e um código novo em `R/erros.R`, se for o caso.
3. Textos com acento no código R devem usar escapes `\uxxxx` (exigência do CRAN); nos comentários, acentos são permitidos.
4. Documente com roxygen2 e rode `devtools::document()`.
5. Escreva testes em `tests/testthat/` que **não acessem a internet** (use `testthat::local_mocked_bindings()` para simular downloads).
6. Rode `devtools::check()` e confira que não há erros, avisos ou notas novas.
7. Acrescente uma linha em `NEWS.md` e abra o *pull request*.

## Código de conduta

Seja respeitoso e acolhedor. Comentários ofensivos ou discriminatórios não são aceitos.
