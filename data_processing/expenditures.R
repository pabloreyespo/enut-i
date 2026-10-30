rm(list = ls())  # .rs.restartR()
pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "lubridate", "haven", "reshape2","apollo")
invisible(lapply(pkgs, library, character.only=TRUE))

codigos <- c(g.alimentos = "01", g.vestimenta = "03",g.cuentas = "04", g.hogar = "05", g.salud = "06",
             g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09", g.educacion = "10", g.restaurantes = "11")

vector_gastos <- c("g.alimentos","g.vestimenta","g.cuentas","g.hogar",
                   "g.salud","g.transporte","g.comunicaciones","g.recreacion","g.educacion",
                   "g.restaurantes","total_expenses")

data_ingresos <- haven::read_dta("data/raw/base-personas-viii-epf-(stata).dta")
data_gastos <- haven::read_dta("data/raw/base-gastos-viii-epf-(stata).dta")

data_ingresos <- data_ingresos %>%
  dplyr::select(FOLIO,
                ID_MISSING, #-----
                FE,
                ZONA, # solo zona
                NPERSONAS,
                JHOGAR,
                PARENTESCO,
                SEXO,
                EDAD,
                ECIVIL,
                SPRINCIPAL,
                ECOMPRAS,
                EDUE,
                EDUACTUAL,
                EDUCURSO,
                EDUNIVEL,
                EDUTERMINA,
                EDUDEPENDENCIA,
                CAE,
                CAEG,
                ING_TOTAL_HOG_HD,
                SALUD,
                CONTRIBUCIONES,
                PREVISION,
                IMPUESTO,
                OT05,
                OT06,
                GASTOT_HD,
                GASTOT_HD_AI,
                INGDTD_HD,
                INGDTI_HD,
                INGDA_HD,
                INGDH_HD,
                INGEMP_HD,
                INGDTCP_HD,
                INGDNP_HD,
                INGDPI_HD,
                INGDOTA,
                INGDJ_HD,
                INGDOTI,
                INGP,
                INGF,
                TRANSFERENCIAS_EMITIDAS,
                TRANSFERENCIAS_RECIBIDAS,
                ING_DISP_HOG_HD) %>%
  mutate(INGDTI_HD = case_when(INGDTI_HD < 0 ~0, T~INGDTI_HD)) %>%
  mutate(OT05 = case_when(OT05 < 0 ~ 0, T ~ OT05), OT06 = case_when(OT06 < 0 ~ 0, T ~ OT06),
         EDAD = case_when(EDAD < 0 ~ 0, T ~ EDAD)) %>%
  mutate(ingreso_disponible = INGDTD_HD + INGDTI_HD  + INGDJ_HD + INGDOTA+ INGDOTI + INGP + INGF - TRANSFERENCIAS_EMITIDAS + TRANSFERENCIAS_RECIBIDAS) %>%
  mutate(PARENTESCO = case_when(PARENTESCO == 1 ~ 1, T ~ 0)) %>%
  filter(ID_MISSING == 0 & FOLIO!="") %>%
  filter_all(all_vars(. != -99)) %>%
  filter_all(all_vars(. != -88)) %>%
  drop_na(FOLIO) %>%
  arrange(ING_DISP_HOG_HD) %>%
  mutate(percentil = cumsum(FE) / sum(FE)) %>%
  mutate(quintil = case_when(percentil<= 0.20000001 ~1,
                               percentil<= 0.40000001 ~2,
                               percentil<= 0.60000001 ~3,
                               percentil<= 0.80000001 ~4,
                               T ~ 5 )) %>% dplyr::select(-percentil) %>%
  group_by(FOLIO) %>%
  summarise(fe                     = first(FE), # factor de expansion del hogar (imputacion JDI)
            n_15_mas               = sum( EDAD >= 15 ),
            n_menores_0_4          = sum( EDAD <= 4 ),
            n_personas             = max(NPERSONAS),
            macrozona              = max(ZONA),
            n_menores_5_14         = sum( EDAD >= 5  & EDAD <= 14),
            n_personas_15_65       = sum( EDAD >= 15 & EDAD <= 65),
            n_trabajadores         = sum( (INGDTD_HD + INGDTI_HD) > 0),
            income_person_week     = mean(ING_DISP_HOG_HD) / n(),
            edad_promedio          = mean(EDAD),
            n_profesionales        = sum(EDUNIVEL >= 14 & !(EDUNIVEL == 14 & EDUTERMINA == 2)),
            ingreso_hogar          = sum(ingreso_disponible),
            quintil                = max(quintil),
            ING_DISP_HOG_HD        = max(ING_DISP_HOG_HD)) %>%
 mutate(macrozona = case_when(
    macrozona == 1 ~ "metropolitana",
    macrozona == 2 ~ "regiones")) %>%
  ungroup() %>% distinct(FOLIO,.keep_all = TRUE)

data_gastos <- data_gastos %>%
  filter(CCIF!="04.2.1.01.01" & CCIF!="04.2.2.01.01" & CCIF!="04.2.2.01.02" & FOLIO!="") %>%
  drop_na(FOLIO) %>%
  mutate(codigo = D ) %>% #paste(D,G, sep =".")) %>%
  mutate(codigo = case_when(
    D=="09" & G=="1" ~ "08",
    D=="02"          ~ "01", # alcoho
    D=="12" & G=="1" ~ "06",
    D=="12" & G=="2" ~ "09",
    D=="12" & G=="3" ~ "03",
    D=="12" & G=="4" ~ "04",
    D=="12" & G=="5" ~ "04",
    D=="12" & G=="6" ~ "04",
    D=="12" & G=="7" ~ "04",
    D=="12" & G=="8" ~ "04",
    T ~ codigo )) %>%
  group_by(FOLIO, codigo) %>%
  summarise(monto_total = sum(GASTO)) %>%
  mutate(codigo = names(codigos)[match(codigo, codigos)]) %>%
  ungroup() %>%
  dcast(FOLIO ~ codigo, value.var="monto_total", fun.aggregate = sum) %>% dplyr::select(-c("NA")) %>%
  mutate_at(names(codigos), ~replace_na(.,0))

data_ambos <- merge(data_ingresos, data_gastos, "FOLIO") %>%
  arrange(FOLIO) %>%
  mutate(total_expenses = dplyr::select(.,names(codigos)) %>% rowSums()) %>% # legal.discounts
  mutate(savings = ingreso_hogar - total_expenses) %>%
filter(total_expenses > 0 & ingreso_hogar > 0 ) # & EDAD > 0 & EDUE > 0 & EDUNIVEL>0

#data_ambos <- data_ambos %>% filter(JHOGAR == 1)
#data_ambos %>% group_by(quintil) %>% summarise(minim = min(ING_DISP_HOG_HD))
#data_ambos %>% group_by(quintil) %>% summarise(minim = min(ingreso_hogar))

gastos <- data_ambos %>% mutate(id_hogar = 1:n()) %>%
dplyr::select(FOLIO, id_hogar, fe, n_15_mas, all_of(vector_gastos), savings, n_personas, macrozona,
                 quintil, n_menores_0_4, n_menores_5_14, n_trabajadores, edad_promedio,
                n_profesionales, income_person_week, ingreso_hogar)

gastos[,c(vector_gastos,"ingreso_hogar", "savings","income_person_week" )] <-
  gastos[,c(vector_gastos,"ingreso_hogar", "savings","income_person_week" )] / 4000 / 1.025 # (4 semanas, miles de pesos, 1.025 la inflación)
gastos <- gastos %>%
  mutate(n_personas_cut      = case_when(n_personas      >= 7 ~ 7, T ~ n_personas),
         n_menores_0_4_cut   = case_when(n_menores_0_4   >= 3 ~ 3, T ~ n_menores_0_4),
         n_menores_5_14_cut  = case_when(n_menores_5_14  >= 2 ~ 3, T ~ n_menores_5_14),
         n_trabajadores_cut  = case_when(n_trabajadores  >= 3 ~ 3, T ~ n_trabajadores),
         n_profesionales_cut = case_when(n_profesionales >= 2 ~ 3, T ~ n_profesionales))
write.csv(gastos, "data/gastos.csv")
# Rscript data_processing/expenditures.R --solo-datos se detiene aqui: la imputacion JDI
# (imputacion_gastos(metodo = "jdi")) solo necesita data/gastos.csv, no el modelo FMNL.
if ("--solo-datos" %in% commandArgs(trailingOnly = TRUE)) quit(save = "no")

########################################################################################################################
################                                 MODELOS                                                ################
########################################################################################################################

alts <- c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
                      "comunicaciones", "recreacion", "educacion", "restaurantes")

temp = sweep(gastos[,paste0("g.", alts)],1,rowSums(gastos[,paste0("g.", alts)]),"/")
colMeans(temp)
apollo_initialise()
apollo_control <- list(
  modelName       = "FMNL-epf-viii"   ,
  modelDescr      = "Fractional MNL model on time use data",
  indivID         = "id_hogar",
  outputDirectory = "output",
  nCores = 22)

database <- gastos
database[,names(codigos)] <- database[, names(codigos)] / rowSums(database[, names(codigos)])

apollo_beta <- c(asc_alimentos          = 0, bnpersonas_alimentos          = 0, bnmenores_0_4_alimentos = 0          , bnmenores_5_14_alimentos = 0          , bntrabajadores_alimentos = 0         , bnprofesionales_alimentos = 0          , bedadpromedio_alimentos = 0          , bq2_alimentos = 0          , bq3_alimentos = 0          , bq4_alimentos = 0          , bq5_alimentos = 0          , bmetro_alimentos = 0          ,
                 asc_vestimenta         = 0, bnpersonas_vestimenta         = 0, bnmenores_0_4_vestimenta = 0         , bnmenores_5_14_vestimenta = 0         , bntrabajadores_vestimenta = 0        , bnprofesionales_vestimenta = 0         , bedadpromedio_vestimenta = 0         , bq2_vestimenta = 0         , bq3_vestimenta = 0         , bq4_vestimenta = 0         , bq5_vestimenta = 0         , bmetro_vestimenta = 0         ,
                 asc_cuentas            = 0, bnpersonas_cuentas     = 0, bnmenores_0_4_cuentas = 0     , bnmenores_5_14_cuentas = 0     , bntrabajadores_cuentas = 0    , bnprofesionales_cuentas = 0     , bedadpromedio_cuentas = 0     , bq2_cuentas = 0     , bq3_cuentas = 0     , bq4_cuentas = 0     , bq5_cuentas = 0     , bmetro_cuentas = 0     ,
                 asc_hogar              = 0, bnpersonas_hogar = 0, bnmenores_0_4_hogar = 0 , bnmenores_5_14_hogar = 0 , bntrabajadores_hogar = 0, bnprofesionales_hogar = 0 , bedadpromedio_hogar = 0 , bq2_hogar = 0 , bq3_hogar = 0 , bq4_hogar = 0 , bq5_hogar = 0 , bmetro_hogar = 0 ,
                 asc_salud              = 0, bnpersonas_salud              = 0, bnmenores_0_4_salud = 0              , bnmenores_5_14_salud = 0              , bntrabajadores_salud = 0             , bnprofesionales_salud = 0              , bedadpromedio_salud = 0              , bq2_salud = 0              , bq3_salud = 0              , bq4_salud = 0              , bq5_salud = 0              , bmetro_salud = 0              ,
                 asc_transporte         = 0, bnpersonas_transporte         = 0, bnmenores_0_4_transporte = 0         , bnmenores_5_14_transporte = 0         , bntrabajadores_transporte = 0        , bnprofesionales_transporte = 0         , bedadpromedio_transporte = 0         , bq2_transporte = 0         , bq3_transporte = 0         , bq4_transporte = 0         , bq5_transporte = 0         , bmetro_transporte = 0         ,
                 asc_comunicaciones     = 0, bnpersonas_comunicaciones     = 0, bnmenores_0_4_comunicaciones = 0     , bnmenores_5_14_comunicaciones = 0     , bntrabajadores_comunicaciones = 0    , bnprofesionales_comunicaciones = 0     , bedadpromedio_comunicaciones = 0     , bq2_comunicaciones = 0     , bq3_comunicaciones = 0     , bq4_comunicaciones = 0     , bq5_comunicaciones = 0     , bmetro_comunicaciones = 0     ,
                 asc_recreacion         = 0, bnpersonas_recreacion         = 0, bnmenores_0_4_recreacion = 0         , bnmenores_5_14_recreacion = 0         , bntrabajadores_recreacion = 0        , bnprofesionales_recreacion = 0         , bedadpromedio_recreacion = 0         , bq2_recreacion = 0         , bq3_recreacion = 0         , bq4_recreacion = 0         , bq5_recreacion = 0         , bmetro_recreacion = 0         ,
                 asc_educacion          = 0, bnpersonas_educacion          = 0, bnmenores_0_4_educacion = 0          , bnmenores_5_14_educacion = 0          , bntrabajadores_educacion = 0         , bnprofesionales_educacion = 0          , bedadpromedio_educacion = 0          , bq2_educacion = 0          , bq3_educacion = 0          , bq4_educacion = 0          , bq5_educacion = 0          , bmetro_educacion = 0          ,
                 asc_restaurantes       = 0, bnpersonas_restaurantes       = 0, bnmenores_0_4_restaurantes = 0       , bnmenores_5_14_restaurantes = 0       , bntrabajadores_restaurantes = 0      , bnprofesionales_restaurantes = 0       , bedadpromedio_restaurantes = 0       , bq2_restaurantes = 0       , bq3_restaurantes = 0       , bq4_restaurantes = 0       , bq5_restaurantes = 0       , bmetro_restaurantes = 0       )

apollo_fixed <- c("asc_vestimenta", "bnpersonas_vestimenta", "bnmenores_0_4_vestimenta", "bnmenores_5_14_vestimenta", "bntrabajadores_vestimenta",
                  "bnprofesionales_vestimenta", "bedadpromedio_vestimenta", "bq2_vestimenta", "bq3_vestimenta", "bq4_vestimenta", "bq5_vestimenta", "bmetro_vestimenta")
apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  V <- list()
  for (alt in c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte","comunicaciones", "recreacion", "educacion", "restaurantes")) {
    V[[alt]] <- get(paste0("asc_", alt)) +
      get(paste0("bnpersonas_", alt))      * n_personas_cut +#(n_personas >= 4 )                   +
      get(paste0("bnmenores_0_4_", alt))      * n_menores_0_4_cut +
      get(paste0("bnmenores_5_14_", alt))     * n_menores_5_14_cut +
      get(paste0("bntrabajadores_", alt))  * n_trabajadores_cut +#(n_trabajadores >= 2 )               +
      get(paste0("bnprofesionales_", alt)) * n_profesionales_cut +#(n_trabajadores >= 2 )               +
      get(paste0("bedadpromedio_", alt))   * edad_promedio +#(n_trabajadores >= 2 )               +
      get(paste0("bq2_", alt)) * (quintil == 2) +
      get(paste0("bq3_", alt)) * (quintil == 3) +
      get(paste0("bq4_", alt)) * (quintil == 4) +
      get(paste0("bq5_", alt)) * (quintil == 5) +
      get(paste0("bmetro_", alt)) * (macrozona == "metropolitana")
  }

  ### Define settings for MNL model component
  fmnl_settings <- list(
    alternatives  = c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte","comunicaciones", "recreacion", "educacion", "restaurantes"),
    choiceShares  = list(alimentos = g.alimentos, vestimenta = g.vestimenta, cuentas = g.cuentas,
                         hogar = g.hogar, salud = g.salud, transporte = g.transporte, comunicaciones = g.comunicaciones,
                         recreacion = g.recreacion, educacion = g.educacion, restaurantes = g.restaurantes),
    utilities     = V)

  P[["model"]] <- apollo_fmnl(fmnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(model)
apollo_saveOutput(model)