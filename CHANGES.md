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

## Joint distribution inference for the `_jdi` draw (`metodo = "jdi"`)

`imputacion_gastos()` now takes `metodo = c("jdi", "hotdeck", "fmnl")` (default
`"jdi"`) and `pesos`. The `_jdi` columns come from joint distribution inference
instead of the cell hot-deck; the cell hot-deck above stays available as
`imputar_hawkins()` (`metodo = "hotdeck"`). Output columns and units are
unchanged (`Ef1/Ef2/Ec/savings_jdi` still add up to the household income), and
the unsuffixed divisions are still the FMNL ones (unlike enut-ii, where
`metodo` also picks the unsuffixed divisions), so downstream code keeps working.

Method (Ye et al. 2017, JASSS, sections 3.1-3.13; Hawkins and Habib 2022,
Transportation, "Data fusion by joint distribution inference"). ENUT (time use)
and EPF (expenditure) share only household covariates D, so there is no table
that crosses time use with expenditure and f(D, time, exp) = f_ENUT(D, time) *
g_EPF(exp | D). Imputing means drawing EPF households from g(exp | D):

1. Dimensions D: income quintile, zone (`zona`: Gran Santiago against the rest
   of the regional capitals in the EPF VIII, `macrozona == "metropolitana"`
   against the other three macrozones in the ENUT), household members aged 15+
   (`n_15_mas`, binned 1, 2, 3, 4+) and workers (`n_trabajadores`, binned 0, 1,
   2, 3+). The EPF VIII has no region or comuna variable (only `ZONA`), so zone
   is the finest geography present in both surveys. Dwelling type is not in the
   ENUT and is dropped.
2. Independence tests (`pruebas_independencia()`): weighted chi-square (weights
   normalised to the EPF sample size) of each dimension against quintiles of
   savings/income, of the Ec share and of the Ef1 share, alpha = 0.05. A
   dimension independent of the three would leave D (Ye eq. 9); the quintile
   never leaves. The strength is the largest Cramer's V. Result on the current
   data (13,475 EPF households): all four dimensions are associated with the
   three classes (p < 0.002); V is 0.14-0.15 for the quintile, 0.04-0.07 for
   zone, 0.04-0.11 for members 15+ and 0.08-0.09 for workers, so zone is the
   weakest (dropped first), then workers. Printed on every run and saved to
   `output/jdi_chi2.csv`.
3. IPF (`ipf_multivariado()`): EPF expansion factors (`fe`) are raked so that
   the one-way and two-way margins of the retained dimensions match the ENUT
   household distribution (weighted by `wgtH`, the household expansion factor of
   the ENUT; `data_processing.R` passes it as `pesos`). Tolerance 1e-8, at most
   1000 sweeps, error if it does not converge. ENUT households with a profile
   that has no EPF household in some margin (for example two workers and one
   member 15+) are set aside before the targets are computed, because margins
   renormalised one by one are inconsistent with each other and the IPF cannot
   converge (error 1e-4 after 1000 sweeps without this). Current data: 15 sweeps,
   error 8.0e-09, 0.01% of the ENUT mass set aside.
4. Draws by member (`sorteo_jdi()`): each ENUT household draws
   m = max(1, `n_15_mas`) EPF households with replacement, with probability
   proportional to the raked weight among those with its exact profile. With
   fewer than 10 donors the weakest dimension is dropped, then the next, never
   the quintile. The level used per household is returned as `nivel` (and saved
   to `output/jdi_nivel.csv`).
5. As before, the mean of the donors' (10 divisions, savings) / household income
   is multiplied by the ENUT household income. Seed 2023.

On the pre-weekend ENUT households (7,077): 96.4% use the full profile
(quintile + zone + members 15+ + workers), 2.9% drop zone and 0.7% drop zone
and workers. The cell hot-deck used zone x quintile x household size for all of
them. Against the hot-deck, at person level the `_jdi` means are unchanged
(Ef1 28.3, Ef2 3.0, Ec 38.8, savings 7.7 against 7.6) and the standard
deviations are 40.9, 6.8, 71.0 and 47.4 (hot-deck 40.0, 5.3, 63.3 and 59.8); the
correlation between both draws is 0.84, 0.50, 0.76 and 0.30.

`expenditures.R` now keeps `fe` (EPF VIII expansion factor, `FE`) and
`n_15_mas` in `data/gastos.csv`, and `--solo-datos` rebuilds only that file
(`Rscript data_processing/expenditures.R --solo-datos`). Bug fixed: `n_trabajadores`
was `sum((INGDTD_HD + INGDTD_HD) > 0)`, so it only counted dependent labour
income; it is now dependent plus independent (`INGDTD_HD + INGDTI_HD`), which
raises the mean from 1.08 to 1.46 workers per household (4,674 of 15,010
households change). The FMNL (`output/FMNL-epf-viii`) and the savings regression
use `n_trabajadores_cut`, so the FMNL should be re-estimated
(`Rscript data_processing/expenditures.R`) to be consistent with the corrected
donor table; the stored model was estimated with the old count. On the ENUT side
`n_15_mas` is now a household column built in `new_variables_prefilter()` from the
ages of all household members (not only those with a diary), as in enut-ii.
Tests: `Rscript tests/test_jdi.R`.

Differences to enut-ii: geography is `zona` (macrozona) instead of `region`;
the ENUT expansion factor is `wgtH`; `ipf_multivariado()` sets aside target
households without EPF support before computing the margins (enut-ii had no
unsupported mass on its data, so its result is the same there); the unsuffixed
divisions stay FMNL.

References: Ye, P., Hu, X., Yuan, Y., Wang, F.-Y. (2017).
Population synthesis based on joint distribution inference without disaggregate
samples. JASSS. Hawkins, J., Habib, K. N. (2022). A multisource data fusion
framework for joint population, expenditure, and time use synthesis. Transportation, doi:10.1007/s11116-022-10279-8.

## EPF VIII worker count fix and re-estimation

`n_trabajadores` of the EPF households (data_processing/expenditures.R) used
`INGDTD_HD > 0` only, so workers with income as independent workers were not counted. It
now counts a person as a worker when `INGDTD_HD + INGDTI_HD > 0`, matching the ENUT
definition used by the JDI and the FMNL covariates. The FMNL (Apollo) and the savings
regression were re-estimated with `Rscript data_processing/expenditures.R`, and the final
datasets were regenerated with `Rscript data_processing/data_processing.R`.

- FMNL LL(final): -30142.03 (old) to -30121.63 (new), same 15010 outcomes.
- `bntrabajadores_*` changed (for example educacion -0.323 to -0.418, salud -0.081 to
  -0.169, restaurantes 0.000 to 0.091, transporte 0.023 to 0.100).
- The previous estimates were kept by Apollo as `output/FMNL-epf-viii_OLD1_*`.
- The aggregated datasets now have 9,497 rows (the old ones had 9,412). The difference is
  not caused by this fix: the row filter `diagnostico_trabajo` runs before any expenditure
  code and gives 9,497 with the pre-change `processing_functions.R` too, from the current
  raw inputs and twin matrix.

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
