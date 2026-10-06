## Submission

This is the first submission of localdatasus to CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the CRAN team

* The Title and Description are in English. Function names, messages and
  documentation are in Brazilian Portuguese (`Language: pt-BR`), because the
  package is meant for health researchers and practitioners in Brazil.
* The main functions download public data (DATASUS, IBGE and Brazilian
  health departments). Examples that need the internet are wrapped in
  `@examplesIf interactive()`, and the tests do not use the internet.
* Downloaded files are cached in `tools::R_user_dir("localdatasus", "cache")`;
  users can delete them with `limpar_cache()`.
