# ENUT-I Pipeline Changes

## Shared activity structure with ENUT-II

`enut-i` and `enut-ii` now produce the same aggregated activity variables, so
models can be estimated on both surveys with the same code.

* `t_agregados`: `t_paid_work`, `t_job_search`, `t_domestic_work`,
  `t_care_work`, `t_unpaid_voluntary`, `t_education`, `t_leisure`, `t_rest`,
  `t_personal_care`, `t_meals`, `t_sleep`, `t_commute` (was `t_commute1`).
* `t_agregados_new`: `Tw`, `Tf_social`, `Tf_events`, `Tf_hobbies`,
  `Tf_sports`, `Tf_read`, `Tf_listen`, `Tf_watch`, `Tf_computer`, `Tf_rest`,
  `Tc_meals`, `Tc_sleep`, `Tc_other`.

Leisure items are split as in the INE ENUT II aggregates:

| Variable | ENUT I items | ENUT II items |
| --- | --- | --- |
| `t_vsyo_csar` / `Tf_social` | s11 conversation, s22 civic or religious celebrations | vs1, vs3 |
| `t_vsyo_ev` / `Tf_events` | s21 cinema, theatre, concerts; s23 sports events | vs2 |
| `t_vsyo_aa` / `Tf_hobbies` | s31 music, dance, writing; s32 board or video games | vs4, vs5 |
| `t_vsyo_dep` / `Tf_sports` | s41 sports or exercise | vs6 |
| `t_descanso` / `Tf_rest` | not asked (always 0) | vs11 |

Before, `t_vsyo_csar` was s11 + s21 + s22 + s23 and `t_vsyo_aa` was
s31 + s32 + s41, so `Tf_hobbies` included sports and `Tf_social` included
events. The totals are unchanged.

Differences that remain because of the questionnaires or the methodology:
ENUT I asks three commutes (work, health, education) and ENUT II eight; ENUT I
has no rest item; ENUT II anchors commutes in the 168 hour rescaling while
ENUT I only anchors paid work and sleep; the sample filters differ.

## Fixes

1. `menor_edad` used `<= 18` while `mayor_edad` used `>= 18`, so
   `n_personas = n_menores_18 + n_mayores` counted 18 year olds twice (571
   people in the raw file). `n_menores_18` now counts members under 18, as in
   ENUT II.
2. `hay_tercera_edad` operator precedence: now 1 when at least one member aged
   60+ other than the respondent lives in the household.
3. `agregar_actividades()` gives each classification its own sleep residual
   and stops if any classification misses 168 hours.
4. The twin matrix script computes the Mahalanobis term with numpy instead of
   building a dense (n - 1) x (n - 1) matrix per individual (same result),
   returns rows from the workers instead of copying the full matrix to each,
   reads the worker count from `TWIN_WORKERS` and streams the output.
5. `data_processing.R --pre` stops after writing the pre weekend file.
6. Docs: money is nominal 2015 CLP; new variables documented.

The pre weekend rows (10,597) are unchanged by these fixes. Correction
(2026-10-14): the twin matrix in `data/raw/matriz_gemelos.csv.gzip` is from
2025-04-06 and was built with the original covariates (`edad_años`,
`horas_trabajo`, `n_menores`, `n_personas`, see commit 9259ceb). The current
script uses `edad_anios`, `horas_trabajo_contratadas`, `n_menores_0_4`,
`n_menores_5_14`, `n_personas_15_65` and `n_tercera_edad` (since f079685 and
cbe11ae), so the matrix does not match the current specification and should be
rebuilt: `sbatch --export=ALL,RUN_TWINS=1 enut-i/run_pipeline.bash`.

## Expenditure imputation: `_jdi` (same donors) and `_fmnl` columns

`imputacion_gastos()` now also writes two sets of broad categories (Ef1 =
food + recreation + restaurants + communications, Ef2 = clothing, Ec = bills +
household + health + transport + education), allocated to each person by
`prop_ing_hogar` like the other amounts:

- `Ef1_jdi`, `Ef2_jdi`, `Ec_jdi`, `savings_jdi`: cell hot-deck where savings
  and expenditure come from the same EPF VIII donors (`imputar_jdi()`), as in
  enut-ii and enut-bigcities. Each ENUT household draws one EPF household per
  member aged 15 or more (`n_personas - n_menores_0_4 - n_menores_5_14`) and
  gets the mean of their (10 divisions, savings) / household income vectors
  times its own income, so Ef1 + Ef2 + Ec + savings = income. The EPF VIII
  only identifies the zone (Gran Santiago or the rest of the regional
  capitals), with no comuna, stratum or NSE, so the cells are those of Hawkins
  and Habib (2022): zone x quintile x household size (1 to 5+), then
  zone x quintile, then quintile, using the first with at least 10 EPF
  households. Seed 2023.
- `Ef1_fmnl`, `Ef2_fmnl`, `Ec_fmnl`, `savings_fmnl`: the current method, FMNL
  shares times (income - savings) with savings from the regression.

Why: the savings regression has no residual, so it removes the dispersion of
savings and, being linear in pesos, predicts savings rates below -1 for low
incomes, outside the EPF support. The unsuffixed columns (the 10 divisions,
`savings`, `total_expenses`) are unchanged.

## Comuna code (`cut`)

The datasets now keep the comuna as `cut` (English: `comuna_code`), as in
enut-ii. ENUT 2015 has no comuna variable, but its sampling stratum `VarStrat`
is the CUT times 100 (for example 1310100 is Santiago, 13101): every value ends
in 00, the region of the CUT always matches `region`, and each household has a
single comuna (118 comunas). The codes are those of 2015, before the Nuble
region (Chillan is 8401).

`cut` cannot refine the `_jdi` donor cells: the public EPF VIII has no comuna,
region or labelled stratum (`VARSTRAT` and `VARUNIT` are anonymised variance
pseudo-strata), only the zone.

## Household income split (`repartir_ingreso()`)

ENUT 2015 has no per-person rent or child support item: `ing_g` comes from the household
questionnaire. It is still split in proportion to labor and pension income, but if nobody in the
household has own income it is now split equally among members aged 15+. Before, `0/0` was replaced
by 0 and the whole household income disappeared (254 households). `ingreso_hogar` and the quintiles do
not change. Same function as `enut-ii`. Tests: `Rscript tests/test_ingresos.R`.
