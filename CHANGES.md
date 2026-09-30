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

The pre weekend rows (10,597) and the twin covariates are unchanged by these
fixes, so the existing twin matrix remains valid.
