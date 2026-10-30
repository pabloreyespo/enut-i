pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr",
          "forcats", "lubridate", "haven", "reshape2","apollo")
invisible(lapply(pkgs, library, character.only=TRUE))

acts_corregidas <- c(
    # confiables:
    't_to',
    't_to_js', #job search
    "t_tcnr_ce", # cuidados escenciales
    "t_tcnr_re", # cuidados relativos a la eseñanza
    "t_tcnr_oac", # otros cuidados
    #"t_tcnr_0_4",
    #"t_tcnr_5_14",
    #"t_tcnr_nna",  #(niños y adolescentes)
    #"t_tcnr_15_65",
    #"t_tcnr_66",
    #"t_tncr_psdf"
    "t_tdnr_psc",
    "t_tdnr_lv",
    "t_tdnr_lrc",
    "t_tdnr_mrm",
    "t_tdnr_admnhog",
    "t_tdnr_comphog",
    "t_tdnr_cmp",
    "t_tvaoh_tv", # voluntario a la comunidad
    "t_tvaoh_oh", #voluntario a otros hogares
    "t_cpaf_cp", # cuidados personales
    "t_cpag_comer",
    "t_cpag_dormir",
    "t_ed",
    "t_vsyo_csar", # convivencia social (s11) y celebraciones civicas o religiosas (s22)
    "t_vsyo_ev", # cine, teatro, conciertos (s21) y eventos deportivos (s23)
    "t_vsyo_aa", # arte y aficiones (s31, s32)
    "t_vsyo_dep", # deporte o ejercicio fisico (s41)
    "t_descanso", # descanso: no se pregunta en ENUT I, siempre 0 (ENUT II vs11)
    't_mcm_leer',
    't_mcm_video',
    't_mcm_audio',
    't_mcm_computador',
    "t_tt1" # traslados enut 2015
  )

# Same aggregated structure as enut-ii.
t_agregados <- c(
  "t_paid_work",
  "t_job_search",
  "t_domestic_work",
  "t_care_work",
  "t_unpaid_voluntary",
  "t_education",
  "t_leisure",
  "t_rest",
  "t_personal_care",
  "t_meals",
  "t_sleep",
  "t_commute")

t_agregados_new <- c(
  "Tw",
  "Tf_social",
  "Tf_events",
  "Tf_hobbies",
  "Tf_sports",
  "Tf_read",
  "Tf_listen",
  "Tf_watch",
  "Tf_computer",
  "Tf_rest",
  "Tc_meals",
  "Tc_sleep",
  "Tc_other"
)


identificadores <- c("id_persona", "id_hogar")
composicion_hogar <- c("parentesco",
                       "n_menores_0_4",
                       "n_menores_5_14",
                       "n_nna",
                       "n_menores_18",
                       "n_personas_15_65",
                       "n_mayores",
                       "n_tiempo",
                       "n_trabajadores",
                       "n_profesionales",
                       "n_tercera_edad",
                       "hay_tercera_edad",
                       "n_personas",
                       "n_15_mas",
                       "edad_promedio",
                       "tiene_hijos",
                       "en_pareja",
                       "vive_pareja")

sociodemograficas <- c("sexo",
                       "edad_anios",
                       "tramo_edad",
                       "nivel_escolaridad",
                       "estudia",
                       "trabaja",
                       "horas_trabajo_contratadas",
                       "horas_trabajo_habituales",
                       "dias_trabajo_semana",
                       "quintil",
                       "macrozona",
                       "region",
                       "cut",
                       "prop_ing_hogar")
laborales <- c("cae", "cise", "ciuo_agrupada")
proveedores_externos <- c("servicio_domestico", "ayuda_cercanos", "fuentes_externas")
ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc", "ing_gpp", "ing_personal", "ingreso_hogar" ,"income_person_week", "I")
tipo_muestra <- c("es_trabajador", "es_familia")

new_variables_prefilter<- function(data) { # OJO, DEBEN ENTRAR TODOS INDEPENDIENTE DE SI DECLARAN TIEMPO O NO
  act_s_domestico <- c("f12_1_1","f15_1_1","f15_1_2","f15_1_3","f16_1_1","f16_1_2","f16_1_3","f22_1_1","f27_1_1",
                       "f27_1_2","f27_1_3","f28_1_1","f28_1_2","f28_1_3", 'f11_1_1', 'f21_1_1')
  recibe_servicio_domestico <- c("f11_1_1","f21_1_1")

  # INFORMACIÓN DEL INDIVIDUO
  data <- data %>%
  mutate(
    # under 18; with <= 18 the 18 year olds were also counted in n_mayores,
    # so n_personas = n_menores_18 + n_mayores counted them twice
    menor_edad        = case_when(c14_1_1 < 18 ~ 1, TRUE ~ 0),
    menor_0_4         = case_when(c14_1_1 < 5 ~ 1, TRUE ~ 0),
    menor_0_14        = case_when(c14_1_1 < 15 ~ 1, TRUE ~ 0),
    menor_5_14        = case_when(c14_1_1 >= 5 & c14_1_1 <= 14 ~ 1, TRUE ~ 0),
    persona_15_65     = case_when(c14_1_1 >= 15 & c14_1_1 <= 65 ~ 1, TRUE ~ 0),
    mayor_edad        = case_when(c14_1_1 >= 18 ~ 1, TRUE ~ 0),
    tercera_edad      = case_when(c14_1_1 >= 60 ~ 1, TRUE ~ 0),
    nivel_escolaridad = case_when(
      educ <= 1 ~ 1, # nada
      educ <= 4 ~ 2, # Básica
      educ %in% c(5,6,7,9) ~ 3, # Media
      educ <= 8 ~ 4, # Técnica
      T  ~ 5), # Profesional),
    tiene_hijos = case_when(t11_1_3 < 6 ~ 1, TRUE ~ 0),
    trabaja     = case_when(k11_1_1 == 1 ~ 1, TRUE ~ 0),
    en_pareja   = case_when(c15_1_1 <= 2 ~ 1, TRUE ~0),
    vive_pareja = case_when(c16_1_1 == 1 ~ 1, TRUE ~ 0),
    dia_semana = dia_semana,
    dia_fin_semana = dia_fin_semana + 5
  )

  # COMPOSICIÓN DEL HOGAR
  data <- data %>%
    group_by(id_hogar) %>%
    mutate(n_menores_18 = sum(menor_edad,na.rm=TRUE),
           n_menores_0_4  = sum(menor_0_4,  na.rm=TRUE),
           n_menores_5_14 = sum(menor_5_14, na.rm=TRUE),
           n_nna = sum(menor_0_14, na.rm=TRUE),
           n_personas_15_65 = sum(persona_15_65, na.rm=TRUE),
           n_mayores = sum(mayor_edad,na.rm=TRUE),
           n_tercera_edad = sum(tercera_edad,na.rm=TRUE),
           n_tiempo = sum(tiempo),
           n_trabajadores = sum(trabaja),
           n_profesionales = sum(nivel_escolaridad >=4 ),
           edad_promedio = mean(c14_1_1),
           n_15_mas = sum(c14_1_1 >= 15, na.rm = TRUE)) %>%
    # at least one member aged 60+ other than the respondent
    mutate(hay_tercera_edad = case_when(n_tercera_edad - tercera_edad >= 1 ~ 1, T ~ 0),
           n_personas = n_menores_18 + n_mayores) %>%
    ungroup()

  # AYUDAS QUE RECIBE EL HOGAR
  data <- data %>%
    mutate_at(act_s_domestico, ~ replace_na(.,0)) %>%
    mutate_at(act_s_domestico, ~ifelse(. == 96, 0, .)) %>%
    group_by(id_hogar) %>%
    mutate(
      servicio_domestico = abs(max(f11_1_1) - 2),
      ayuda_cercanos = abs(max(f21_1_1) - 2)) %>%
    mutate(fuentes_externas = case_when(
      servicio_domestico  == 1 | ayuda_cercanos == 1 ~ 1, T ~ 0)) %>%
    ungroup()

  # PREPROCESAMIENTO DE INGRESOS
  data <- data %>%
    # filter(tiempo == 1) %>% # será relevante desaserme de las personas que no declaran tiempo antes?
    mutate_at(c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc"),
              ~replace_na(.,0)) %>%
    # ing_g comes from the household questionnaire: ENUT 2015 has no rent or child support item per
    # person, so it is split by own income, or among members aged 15+ if nobody has own income
    repartir_ingreso(hogar = "id_hogar", edad = "c14_1_1") %>%
    mutate_at(c("prop_ing_hogar", "ingreso_hogar", "ing_gpp", "income_person_week", "ingreso_personal"),
              ~replace_na(.,0)) %>%
    mutate(I = ing_jub_aps + ing_g)
  return(data)
}

# Splits household income among members. Own income = labor + pension + ing_otros. The household
# residual ing_g (ing_t_hogar minus own income, or the one the survey provides) goes in proportion
# to own income; if nobody in the household has own income, in equal parts among members aged 15+
# (everyone if there are none). ing_gpp = ing_otros + the residual share, so
# ingreso_personal = ing_trab + ing_jub_aps + ing_gpp adds up to the household income.
repartir_ingreso <- function(data, hogar, edad) {
  if (!"ing_otros" %in% names(data)) data$ing_otros <- 0
  data <- data %>%
    mutate(
      ingresos_propios = ing_trab + ing_jub_aps + ing_otros,
      .adulto = as.numeric(coalesce(.data[[edad]] >= 15, FALSE))
    ) %>%
    group_by(across(all_of(hogar)))
  if (!"ing_g" %in% names(data)) {
    data <- data %>% mutate(ing_g = (sum(ing_t_hogar) - sum(ingresos_propios)) * (ing_t_hogar > 0))
  }
  data %>%
    mutate(
      .s_prop = sum(ingresos_propios),
      .s_g = sum(ing_g),
      .n_adultos = sum(.adulto),
      .peso = case_when(
        .s_prop > 0 ~ ingresos_propios / .s_prop,
        .n_adultos > 0 ~ .adulto / .n_adultos,
        TRUE ~ 1 / n()
      ),
      ingreso_hogar = .s_prop + .s_g,
      ing_gpp = ing_otros + .s_g * .peso,
      income_person_week = (sum(ing_trab) + sum(ing_jub_aps) + ing_gpp) / n(),
      ingreso_personal = ing_trab + ing_jub_aps + ing_gpp,
      prop_ing_hogar = if (sum(ingreso_personal) > 0) ingreso_personal / sum(ingreso_personal) else 0
    ) %>%
    ungroup() %>%
    dplyr::select(-c(.adulto, .s_prop, .s_g, .n_adultos, .peso))
}


na_completion <- function(data) {
  act_traslados_ds <- c("m12_1_1", "m12_1_2", "q18_1_1", "q18_1_2", "r12_1_1", "r12_1_2")
  act_traslados_fds <- c("m12_2_1", "m12_2_2", "q18_2_1", "q18_2_2", "r12_2_1", "r12_2_2")
  vars_todos <- c("t11_1_1", "t11_1_6", "t11_1_7", "t11_1_8","t12_1_3","t12_1_4")

  acts <- c('m11', 'q11','m21', 'o11', 'o13', 'o14', 'o21', 'o32', 'o33', 'o34', 'o42', 'o43', 'o44', 'o51', 'o61', 'n11', 'n12', 'n13', 'n14', 'n15', 'n16', 'n17', 'n18', 'n19', 'n110', 'n111', 'n112', 'n113', 'n21', 'n22', 'n23', 'n24', 'n25', 'n26', 'n27', 'n28', 'n29', 'n210', 'n211', 'n212', 'n31', 'n32', 'n33', 'n34', 'n35', 'n36', 'n37', 'n38', 'n39', 'n310', 'n311', 'n41', 'n42', 'n43', 'n44', 'n45', 'n46', 'n51', 'n52', 'n53', 'n54', 'p11', 'p12', 'p13', 'p14', 'p15', 'p21', 'p22', 'p23', 'r11', 'r21', 'r22', 's31', 's41', 'q17', 'o12', 'o22', 'o23', 'o31', 'o41', 'o52', 'o62', 'o63', 'o71', 'o72', 's11', 's21', 's22', 's23', 's32', 's51', 's52', 's53', 's54', 'q12', 'q13', 'q14', 'q15', 'q16')

  data<- data  %>%
    filter(tiempo == 1) %>%
    mutate_at(c("c14_1_1","m14_1_1","m14_2_1","k42_1_1","k15_1_1",
                "l15_1_1","l16_1_1","l17_1_1","l117_1_1", "l118_1_1","l119_1_1"),
              ~ifelse(is.na(.), 85, .)) %>%
    filter(if_all(c('q11_1_2', 'q11_2_2', 'm11_1_2', 'm11_2_2'), ~ !.x  %in% 96)) %>%
    #filter(if_all(n_linea_p:te_ayuda_cercanos, ~ !.x  %in% 96)) %>%
    #filter(l110_1_1 != 1 & l122_1_1 != 1) %>% # en cama enfermos
    #filter(l111_1_1 != 1 & l123_1_1 != 1) %>%# pasaron cosas fuera de lo común
    #filter(m14_1_1 != 2 & m14_2_1 != 2) %>% # huelga
    #filter(k42_1_1 != 6) %>%# no buscó trabajo por estar embarazada
    #filter(!(l15_1_1 == 2 & l16_1_1 == 2 & l17_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>% # los de vacaciones
    #filter(!(l117_1_1 == 2 & l118_1_1 == 2 & l119_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>%
    #filter(k15_1_1 != 1) %>%
    mutate_at(c(paste0(acts, "_2_2"), paste0(acts, "_1_2")), ~replace_na(.,0)) %>%
    mutate_at(c(paste0(acts, "_2_1"), paste0(acts, "_1_1")), ~replace_na(.,2)) %>%
    mutate_at(c(paste0(acts, "_2_1"), paste0(acts, "_1_1")), ~abs(. -2)) %>%
    mutate_at(c(paste0(acts, "_1_2"), paste0(acts, "_2_2")), ~ifelse(. >= 25, 0, .)) %>%
    mutate_at(c(act_traslados_ds, act_traslados_fds), ~replace_na(.,0)) %>%
    mutate_at(c(act_traslados_ds, act_traslados_fds), ~ifelse(. >= 25, 0, .)) %>%
    mutate_at(c(110: ncol(data)), ~replace_na(.,0)) %>%
    filter(if_all(all_of(vars_todos), ~ !.x  %in% 85)) %>%
    mutate(
      t_total_ds = dplyr::select(
        ., all_of(c(paste0(acts, "_1_2"), act_traslados_ds))) %>% rowSums(na.rm = TRUE),
      t_total_fds = dplyr::select(
        ., all_of(c(paste0(acts, "_2_2"), act_traslados_fds))) %>% rowSums(na.rm = TRUE)) %>%
    mutate(sexo = abs(c13_1_1 - 1))
  return(data)
}

outlier_detection_Vallejo <- function(data)  {

  data["dias_normal"] <- TRUE
  data <- data %>%
    # group_by(años_escolaridad, k11_1_1, rango_edad,sexo) %>%
    group_by(quintil, trabaja, tramo_edad, sexo) %>%
    mutate(xi_ds = mean(t_total_ds) , sd_ds = sd(t_total_ds),
           xi_fds = mean(t_total_fds), sd_fds = sd(t_total_fds),) %>%
    mutate(gamma_ds = sd_ds/xi_ds, gamma_fds = sd_fds/xi_fds) %>%
    mutate(lower_limit_ds = xi_ds - sd_ds + gamma_ds/2,
           upper_limit_ds = xi_ds + sd_ds,
           lower_limit_fds = xi_fds - sd_fds + gamma_fds/2,
           upper_limit_fds = xi_fds + sd_fds) %>%
    ungroup() %>%
    mutate(dias_normal = case_when(
      dias_normal == FALSE ~ FALSE,
      t_total_ds  < lower_limit_ds  | t_total_ds  > upper_limit_ds  ~ FALSE,
      t_total_fds < lower_limit_fds | t_total_fds > upper_limit_fds ~ FALSE,
      TRUE ~ TRUE)) %>%
    filter(dias_normal == TRUE)

  return(data)
}

new_variables_postfilter <- function(data) {
  ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc",
                    "ing_gpp", "ing_personal", "ingreso_hogar", "income_person_week")

  data <- data %>%
    mutate(horas_trabajo_contratadas = case_when(k31_1_3 > 0 ~ k31_1_3, T ~ 0),
           horas_trabajo_habituales  = case_when(k31_1_1 > 0 ~ k31_1_1, T ~ 0),
           dias_trabajo_semana       = case_when(k31_1_2 > 0 ~ k31_1_2, T ~ 0),
           # comuna (CUT 2015, before Nuble: Chillan is 8401): VarStrat is the CUT times 100
           cut = as.integer(VarStrat %/% 100),
           macrozona = case_when(region <= 5 | region == 15 ~ "norte",
                                 region ==13 ~ "metropolitana",
                                 region <= 8 & region > 5 ~ "centro",
                                 TRUE ~ "sur"),
           edad_anios = c14_1_1,
           tramo_edad = case_when(edad_anios <=24 ~ "12-24",
                                  edad_anios <=44 ~ "25-44",
                                  edad_anios <=65 ~ "45-65",
                                  T            ~ "66+"),
           estudia = abs(abs(d14_1_1-1)-1),
           n_menores_18  = case_when(n_menores_18  <= 3 ~ n_menores_18 , T ~ 4),
           n_mayores = case_when(n_mayores <= 5 ~ n_mayores, T ~ 6),
           nivel_escolaridad = case_when( # todos los niveles son completos, excepto el inicial
             nivel_escolaridad == 1 ~ "ninguna",
             nivel_escolaridad == 2 ~ "primaria",
             nivel_escolaridad == 3 ~ "secundaria",
             nivel_escolaridad == 4 ~ "técnica",
             T                      ~ "universitaria"),
           parentesco = c12_1_1,
           ing_personal = ing_trab + ing_jub_aps + ing_gpp) %>%
    mutate_at(ingresos, ~. / 1000 / 4) %>% #/654
    mutate(ing_personal = round(ing_personal, 2))

  data <- data %>% mutate(
        cae = cae,
        cise = cise_5,
        ciuo_agrupada = case_when(
          ciuo88_1d %in% c(1,2)   ~ 1,
          ciuo88_1d == 3          ~ 2,
          ciuo88_1d %in% c(4,5,9) ~ 3,
          ciuo88_1d == 6          ~ 4,
          ciuo88_1d == 7          ~ 5,
          ciuo88_1d == 8          ~ 6,
          ciuo88_1d %in% c(0,99)  ~ 7,
        )
    )

  cae_id <- c("Menor de 15 años"=1, "Ocupada(o)"=2, "Desocupada(o)"=3, "Inactiva(o)"=4, "Sin clasificar"=96)
  data$cae <- names(cae_id)[match(data$cae, cae_id)]

  return(data)
}

get_25activities <- function(data) {
  # trabajo (libre)
  act_t_to          <- c("m11")
  act_t_to_js       <- c("m21")

  # trabajo de cuidados
  act_t_tcnr_ce <- c("n13", "n23",
                     "n14", "n24", "n32",
                     "n15", "n25", "n33",
                     "n11", "n21", "n31",
                     "n111", "n112", "n211", "n212", "n310", "n311",
                     "n12", "n22")

  act_t_tcnr_re <- c(
    "n19", "n29", "n37", "n44",
    "n110", "n210", "n38", "n45",
    "n39"
  )

  act_t_tcnr_oac <- c(
    "n16", "n17", "n18", "n113",
    "n26", "n27", "n28",
    "n34", "n35", "n36",
    "n41", "n42", "n43", "n46",
    "n51", "n52", "n53", "n54"
  )

  # trabajo doméstico (podría dividir en las relacionadas al hogar y relacionadas a los miembros del hogar)
  act_t_tdnr_psc <- c("o11", "o12", "o13", "o14")
  act_t_tdnr_lv  <- c("o21", "o22", "o23")
  act_t_tdnr_lrc <- c("o31", "o32", "o33", "o34")
  act_t_tdnr_mrm <- c("o41", "o42", "o43", "o44")
  act_t_tdnr_admnhog <- c("o51", "o52") # puse aqui buscar trabajo
  act_t_tdnr_comphog <- c("o61", "o62", "o63")
  act_t_tdnr_cmp <- c("o71", "o72")

  # trabajo no remunerado
  act_t_tvaoh_tv <- c("p21", "p22", "p23")
  act_t_tvaoh_oh <- c("p11", "p12", "p13", "p14", "p15")
  # aprendizaje
  act_t_ed <- c("r11", "r21", "r22")

  # ocio
  # Same split as the INE ENUT II aggregates: t_vsyo_csar = vs1 + vs3,
  # events vs2, t_vsyo_aa = vs4 + vs5, sports vs6.
  act_t_vsyo_csar <- c("s11", "s22") # conversar con amigos o familiares, celebraciones civicas o religiosas
  act_t_vsyo_ev <- c("s21", "s23") # cine, teatro, concierto; evento deportivo o estadio
  act_t_vsyo_aa <- c("s31", "s32") # instrumento, baile, escritura; juegos de mesa o videojuegos
  act_t_vsyo_dep <- c("s41") # deporte o ejercicio fisico

  act_t_mcm_leer       <- c("s51")
  act_t_mcm_video      <- c("s52")
  act_t_mcm_audio      <- c("s53")
  act_t_mcm_computador <- c("s54")

  # cuidados personales
  act_t_cpaf_cp <- c("q12", "q17")
  act_t_cpag_comer <- c("q13", "q14", "q15", "q16")
  act_t_cpag_dormir <- c("q11")

  # traslados (committed)

  act_t_tt_ds <- c("m12_1_1", "m12_1_2", "q18_1_1", "q18_1_2", "r12_1_1", "r12_1_2")
  act_t_tt_fds <- c("m12_2_1", "m12_2_2", "q18_2_1", "q18_2_2", "r12_2_1", "r12_2_2")

  data <- data %>%
    mutate(
      t_to_ds = dplyr::select(., paste0(act_t_to, "_1_2")) %>% rowSums(na.rm = T),
      t_to_js_ds = dplyr::select(., paste0(act_t_to_js, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_ce_ds = dplyr::select(., paste0(act_t_tcnr_ce, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_re_ds = dplyr::select(., paste0(act_t_tcnr_re, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_oac_ds = dplyr::select(., paste0(act_t_tcnr_oac, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_psc_ds = dplyr::select(., paste0(act_t_tdnr_psc, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_lv_ds = dplyr::select(., paste0(act_t_tdnr_lv, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_lrc_ds = dplyr::select(., paste0(act_t_tdnr_lrc, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_mrm_ds = dplyr::select(., paste0(act_t_tdnr_mrm, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_admnhog_ds = dplyr::select(., paste0(act_t_tdnr_admnhog, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_comphog_ds = dplyr::select(., paste0(act_t_tdnr_comphog, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_cmp_ds = dplyr::select(., paste0(act_t_tdnr_cmp, "_1_2")) %>% rowSums(na.rm = T),
      t_tvaoh_tv_ds = dplyr::select(., paste0(act_t_tvaoh_tv, "_1_2")) %>% rowSums(na.rm = T),
      t_tvaoh_oh_ds = dplyr::select(., paste0(act_t_tvaoh_oh, "_1_2")) %>% rowSums(na.rm = T),
      t_ed_ds = dplyr::select(., paste0(act_t_ed, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_csar_ds = dplyr::select(., paste0(act_t_vsyo_csar, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_ev_ds = dplyr::select(., paste0(act_t_vsyo_ev, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_aa_ds = dplyr::select(., paste0(act_t_vsyo_aa, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_dep_ds = dplyr::select(., paste0(act_t_vsyo_dep, "_1_2")) %>% rowSums(na.rm = T),
      t_descanso_ds = 0,
      t_mcm_leer_ds = dplyr::select(., paste0(act_t_mcm_leer, "_1_2")) %>% rowSums(na.rm = T),
      t_mcm_video_ds = dplyr::select(., paste0(act_t_mcm_video, "_1_2")) %>% rowSums(na.rm = T),
      t_mcm_audio_ds = dplyr::select(., paste0(act_t_mcm_audio, "_1_2")) %>% rowSums(na.rm = T),
      t_mcm_computador_ds = dplyr::select(., paste0(act_t_mcm_computador, "_1_2")) %>% rowSums(na.rm = T),
      t_cpaf_cp_ds = dplyr::select(., paste0(act_t_cpaf_cp, "_1_2")) %>% rowSums(na.rm = T),
      t_cpag_comer_ds = dplyr::select(., paste0(act_t_cpag_comer, "_1_2")) %>% rowSums(na.rm = T),
      t_cpag_dormir_ds = dplyr::select(., paste0(act_t_cpag_dormir, "_1_2")) %>% rowSums(na.rm = T),
      t_tt1_ds = dplyr::select(., all_of(act_t_tt_ds)) %>% rowSums(na.rm = TRUE)) %>%
    mutate(
      t_to_fds = dplyr::select(., paste0(act_t_to, "_2_2")) %>% rowSums(na.rm = T),
      t_to_js_fds = dplyr::select(., paste0(act_t_to_js, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_ce_fds = dplyr::select(., paste0(act_t_tcnr_ce, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_re_fds = dplyr::select(., paste0(act_t_tcnr_re, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_oac_fds = dplyr::select(., paste0(act_t_tcnr_oac, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_psc_fds = dplyr::select(., paste0(act_t_tdnr_psc, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_lv_fds = dplyr::select(., paste0(act_t_tdnr_lv, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_lrc_fds = dplyr::select(., paste0(act_t_tdnr_lrc, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_mrm_fds = dplyr::select(., paste0(act_t_tdnr_mrm, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_admnhog_fds = dplyr::select(., paste0(act_t_tdnr_admnhog, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_comphog_fds = dplyr::select(., paste0(act_t_tdnr_comphog, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_cmp_fds = dplyr::select(., paste0(act_t_tdnr_cmp, "_2_2")) %>% rowSums(na.rm = T),
      t_tvaoh_tv_fds = dplyr::select(., paste0(act_t_tvaoh_tv, "_2_2")) %>% rowSums(na.rm = T),
      t_tvaoh_oh_fds = dplyr::select(., paste0(act_t_tvaoh_oh, "_2_2")) %>% rowSums(na.rm = T),
      t_ed_fds = dplyr::select(., paste0(act_t_ed, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_csar_fds = dplyr::select(., paste0(act_t_vsyo_csar, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_ev_fds = dplyr::select(., paste0(act_t_vsyo_ev, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_aa_fds = dplyr::select(., paste0(act_t_vsyo_aa, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_dep_fds = dplyr::select(., paste0(act_t_vsyo_dep, "_2_2")) %>% rowSums(na.rm = T),
      t_descanso_fds = 0,
      t_mcm_leer_fds = dplyr::select(., paste0(act_t_mcm_leer, "_2_2")) %>% rowSums(na.rm = T),
      t_mcm_video_fds = dplyr::select(., paste0(act_t_mcm_video, "_2_2")) %>% rowSums(na.rm = T),
      t_mcm_audio_fds = dplyr::select(., paste0(act_t_mcm_audio, "_2_2")) %>% rowSums(na.rm = T),
      t_mcm_computador_fds = dplyr::select(., paste0(act_t_mcm_computador, "_2_2")) %>% rowSums(na.rm = T),
      t_cpaf_cp_fds = dplyr::select(., paste0(act_t_cpaf_cp, "_2_2")) %>% rowSums(na.rm = T),
      t_cpag_comer_fds = dplyr::select(., paste0(act_t_cpag_comer, "_2_2")) %>% rowSums(na.rm = T),
      t_cpag_dormir_fds = dplyr::select(., paste0(act_t_cpag_dormir, "_2_2")) %>% rowSums(na.rm = T),
      t_tt1_fds = dplyr::select(., all_of(act_t_tt_fds)) %>% rowSums(na.rm = TRUE)) %>%
  mutate(
    t_total_ds = dplyr::select(., paste0(acts_corregidas, "_ds")) %>% rowSums(na.rm = TRUE),
    t_total_fds = dplyr::select(., paste0(acts_corregidas, "_fds")) %>% rowSums(na.rm = TRUE))
}

data_to168hours <- function(data) {
  act_confiable    <- c("t_to", "t_cpag_dormir")
  act_no_confiable <- acts_corregidas[!acts_corregidas %in% act_confiable]

  data <- data %>%
    #mutate_at(c(act_confiable, act_no_confiable), ~ifelse(. <= 1/4,0 ,.)) %>%
    mutate(t_confiable =  dplyr::select(.,all_of(act_confiable)) %>% rowSums(na.rm = TRUE),
           t_no_confiable =  dplyr::select(.,all_of(act_no_confiable)) %>% rowSums(na.rm = TRUE)) %>%
    mutate(t_total = t_confiable + t_no_confiable)  %>%
    filter(t_confiable <= 168) %>%
    mutate(mult_no_confiable = (168- t_confiable)/(t_total - t_confiable)) %>%
    mutate_at(act_no_confiable, ~ .*mult_no_confiable) %>%
    mutate(
      tt_otal = dplyr::select(., all_of(c(act_confiable, act_no_confiable))) %>%
        rowSums(na.rm = TRUE))

  return(data)
}


impute_weekend <- function(data, twin_matrix) {
  twin_matrix <- as.matrix(twin_matrix)
  # the twin matrix is indexed by the rows of the pre weekend file; a stale
  # matrix from another run would silently misalign the weekend imputation
  if (nrow(twin_matrix) != nrow(data) || ncol(twin_matrix) != nrow(data)) {
    stop("twin_matrix dimensions (", nrow(twin_matrix), " x ", ncol(twin_matrix),
         ") do not match data rows (", nrow(data), "); rebuild it with gemelos_matriz.py")
  }

  acts <- list(
    "6" = paste0(acts_corregidas, "_sab"),
    "7" = paste0(acts_corregidas, "_dom"))

  finsemana <- paste0(acts_corregidas, "_fds")
  dias_semana <- paste0(acts_corregidas, "_ds")

  semana_completa <- acts_corregidas
  for (i in 6:7) {
    mask <- data$dia_fin_semana == i
    finde <- unlist(acts[as.character(i)])

    data[mask, finde]  <- data[mask, finsemana]
    suma <- rowSums(twin_matrix[!mask, mask])
    data[!mask, finde] <- as.matrix(twin_matrix[!mask, mask] ) %*% as.matrix(data[mask, finsemana])
    data[!mask, finde] <-  sweep(data[!mask, finde],1,suma,"/")
  }

  sabados <- unlist(acts["6"])
  domingos <- unlist(acts["7"])
  data_post <- data %>%
    #mutate_at(sabados , ~ifelse(. <= 1/12,0 ,.)) %>% # todo tiempo al que se le dedique menos de 5 minutos
    #mutate_at(domingos , ~ifelse(. <= 1/12,0 ,.)) %>%  # todo tiempo al que se le dedique menos de 5 minutos
    mutate(sum_sabados  = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
           sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE))%>%
    mutate_at(sabados , ~ . * 24 / sum_sabados)  %>%
    mutate_at(domingos, ~ . * 24 / sum_domingos) %>%
    mutate(sum_sabados = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
           sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE)) %>%
    mutate_at(dias_semana, ~case_when((dias_trabajo_semana < 5) & (dias_trabajo_semana > 0) ~ . * dias_trabajo_semana, T ~ . * 5))

  temp <- data_post[,"t_to_ds"]
  data_post[,"t_to_ds"] <- 0
  data_post[,dias_semana] <- sweep(data_post[,dias_semana] , 1, rowSums(data_post[,dias_semana]), "/")
  data_post[,dias_semana] <- sweep(data_post[,dias_semana] , 1, as.numeric(unlist(24*5 - temp)), "*")
  data_post[,"t_to_ds"] <- temp

  data_post[,semana_completa] <- data_post[,dias_semana] + data_post[,sabados] + data_post[,domingos]
  data_post[,"t_total"] <-rowSums(data_post[,semana_completa])

  return(data_post)
}

diagnostico_trabajo <- function(data, plots, intercuartil) {
  diagnostico <- data %>%
    mutate(diferencia = t_to - horas_trabajo_contratadas)

  cat("Número inicial de filas",nrow(diagnostico), end = "\n")

  if (plots) {
    ggplot(diagnostico, aes(x = horas_trabajo_habituales)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = dias_trabajo_semana)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = horas_trabajo_contratadas)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = t_to)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = diferencia)) + geom_histogram(binwidth = 1)
  }

  if (intercuartil) {
    cuantiles <- quantile(diagnostico[(diagnostico$t_to > 0) | (diagnostico$horas_trabajo_contratadas > 0),]$diferencia, c(0.25, 0.75))
    intercuantil <- cuantiles[2] - cuantiles[1]
    diagnostico <- diagnostico %>% filter(diferencia < cuantiles[2] + intercuantil, diferencia > cuantiles[1] - intercuantil)
    cat("Número final de filas",nrow(diagnostico))
    return(diagnostico)
  } else {
    diagnostico <- diagnostico %>% filter(diferencia < 10, diferencia > -10)
    cat("Número final de filas",nrow(diagnostico))
    return(diagnostico)
  }
}

filtrar_salarios_extremos <- function(data, k = 3, ingreso = "ing_trab", horas = "t_to") {
  # Among workers with a positive wage rate w = ingreso / horas, drops the rows whose
  # log w lies outside [Q1 - k IQR, Q3 + k IQR]. Rows without a positive w are kept.
  cat("Numero inicial de filas", nrow(data), "
")
  w <- data[[ingreso]] / data[[horas]]
  con_salario <- is.finite(w) & w > 0
  mantener <- !con_salario
  if (any(con_salario)) {
    log_w <- log(w[con_salario])
    q <- quantile(log_w, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    mantener[con_salario] <- log_w >= q[1] - k * iqr & log_w <= q[2] + k * iqr
  }
  data <- data[mantener, ]
  cat("Numero final de filas", nrow(data), "
")
  return(data)
}

adjust_working_hours <- function(data, normalize = TRUE) {
  act_ajustar <- acts_corregidas[!acts_corregidas %in% c("t_to")]
  data <- data %>% mutate(t_to = horas_trabajo_contratadas)

  # if (normalize) {
  #   data <- data %>%
  #     mutate(t_ajustable = dplyr::select(.,act_ajustar) %>% rowSums(na.rm = TRUE)) %>%
  #     mutate(mult_ajuste= (168- tt)/(t_ajustable)) %>%
  #     mutate_at(act_ajustar, ~ .*mult_ajuste) %>%
  #     mutate(ttotal_ajuste = dplyr::select(., c("tt", act_ajustar))  %>% rowSums(na.rm = TRUE))
  # }

  return(data)
}

agregar_actividades <- function(data_post) {

  data_post <- data_post %>%
    mutate(
      Tw = t_to,
      Tf_social = t_vsyo_csar,
      Tf_events = t_vsyo_ev,
      Tf_hobbies = t_vsyo_aa,
      Tf_sports = t_vsyo_dep,
      Tf_read = t_mcm_leer,
      Tf_listen = t_mcm_audio,
      Tf_watch = t_mcm_video,
      Tf_computer = t_mcm_computador,
      Tf_rest = t_descanso,
      Tc_meals = t_cpag_comer,
      Tc_sleep = t_cpag_dormir,
      Tc_other = dplyr::select(., c(
        "t_to_js",
        "t_tdnr_psc", "t_tdnr_lv", "t_tdnr_lrc", "t_tdnr_mrm",
        "t_tdnr_admnhog", "t_tdnr_comphog", "t_tdnr_cmp",
        "t_tcnr_ce", "t_tcnr_re", "t_tcnr_oac",
        "t_tvaoh_tv", "t_tvaoh_oh",
        "t_cpaf_cp", "t_ed", "t_tt1"
      )) %>% rowSums(na.rm = TRUE),
      t_paid_work = t_to, # free
      t_job_search = t_to_js, # free
      t_domestic_work = dplyr::select(., c("t_tdnr_psc", "t_tdnr_lv", "t_tdnr_lrc", "t_tdnr_mrm",
                                           "t_tdnr_admnhog", "t_tdnr_comphog", "t_tdnr_cmp",)) %>%
        rowSums(na.rm = TRUE), # commited/free
      t_care_work = dplyr::select(., c("t_tcnr_ce", "t_tcnr_re", "t_tcnr_oac",)) %>%
        rowSums(na.rm = TRUE),  # commited/free
      t_unpaid_voluntary = t_tvaoh_tv + t_tvaoh_oh, # free
      t_education = t_ed, # committed / free
      t_leisure = t_vsyo_csar + t_vsyo_ev + t_vsyo_aa + t_vsyo_dep + t_mcm_leer + t_mcm_video + t_mcm_audio + t_mcm_computador, # free
      t_rest = t_descanso, # not collected in ENUT I
      t_personal_care = t_cpaf_cp, #committed/free
      t_meals         = t_cpag_comer, #committed/free
      t_sleep = t_cpag_dormir, #committed/free
      t_commute = t_tt1)  # committed

  data_post <- data_post %>%
    mutate(
      es_trabajador = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 ~ 1, T ~ 0),
      es_familia = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 & vive_pareja == 1 & tiene_hijos ==1  ~ 1, T ~0))


  data25 <- data_post %>% dplyr::select(
    all_of(identificadores),
    all_of(tipo_muestra),
    dia_semana, dia_fin_semana,
    all_of(composicion_hogar),
    all_of(proveedores_externos),
    all_of(sociodemograficas),
    all_of(laborales),
    t11_1_1:t15_1_1,
    all_of(ingresos),
    all_of(acts_corregidas)) %>%
    mutate(t_total = dplyr::select(., all_of(acts_corregidas)) %>% rowSums(na.rm = TRUE))

  data25[,acts_corregidas]  = round(data25[,acts_corregidas],2)
  data25[,"temp"] = rowSums(data25[,acts_corregidas])
  data25[,"t_cpag_dormir"]       = data25[,"t_cpag_dormir"] - (data25[,"temp"] - 168)
  data25[,"temp"] = rowSums(data25[,acts_corregidas])
  data25 <- data25 %>% dplyr::select(-c("temp"))

  data11 <- data_post %>%
    dplyr::select(
      all_of(identificadores),
      all_of(tipo_muestra),
      dia_semana, dia_fin_semana,
      all_of(composicion_hogar),
      all_of(proveedores_externos),
      all_of(sociodemograficas),
      all_of(laborales),
      t11_1_1:t15_1_1,
      all_of(ingresos),
      all_of(t_agregados),
      all_of(t_agregados_new)) %>%
  mutate(t_total = dplyr::select(., all_of(t_agregados)) %>% rowSums(na.rm = TRUE))

  data11[, c(t_agregados, t_agregados_new)] <- round(data11[, c(t_agregados, t_agregados_new)], 2)
  # Sleep absorbs the rounding residual of each classification separately.
  data11[, "t_sleep"]  <- data11[, "t_sleep"]  - (rowSums(data11[, t_agregados]) - 168)
  data11[, "Tc_sleep"] <- data11[, "Tc_sleep"] - (rowSums(data11[, t_agregados_new]) - 168)
  data11[, "t_total"] <- rowSums(data11[, t_agregados])

  check_168 <- function(df, cols, label) {
    gap <- max(abs(rowSums(df[, cols]) - 168))
    if (gap > 1e-6) stop(label, " does not add up to 168 hours (max gap ", gap, ")")
  }
  check_168(data25, acts_corregidas, "enut-i-raw activities")
  check_168(data11, t_agregados, "enut-i aggregated classification")
  check_168(data11, t_agregados_new, "enut-i Tw/Tf/Tc classification")

  return(list(data25 = data25, data11 = data11))
}
###### MANTENER POR AHI ########
# grupos <- data %>%
#   group_by(dia_semana, años_escolaridad, trabaja, rango_edad,sexo) %>%
#   summarise(cuantos = n())
#######

# Hot-deck por celdas (Hawkins y Habib 2022, seccion 8 de CHANGES.md; `metodo = "hotdeck"`): cada hogar ENUT
# recibe el promedio de (10 divisiones, savings) / ingreso del hogar de m hogares EPF sorteados de su celda,
# con m = integrantes de 15 anios o mas. Esas razones suman 1, asi que ahorro y gasto salen de los mismos
# donantes. La EPF VIII solo identifica la zona (Gran Santiago o resto de capitales regionales), sin comuna,
# estrato ni NSE, por lo que las celdas son las de Hawkins, de la mas fina a la mas gruesa (la primera con al
# menos `min_donantes` hogares EPF):
#   1. zona x quintil x tamanio del hogar (1 a 5+);  2. zona x quintil;  3. quintil.
imputar_hawkins <- function(data_hogar, gastos, min_donantes = 10, semilla = 2023) {
  set.seed(semilla)
  razones <- as.matrix(gastos[, c(paste0("g.", alts), "savings")]) / gastos$ingreso_hogar
  colnames(razones) <- c(alts, "savings")
  tam_epf <- pmin(gastos$n_personas, 5)
  zona <- ifelse(data_hogar$macrozona == "metropolitana", "metropolitana", "regiones")
  tam <- pmin(data_hogar$n_personas, 5)
  n_15_mas <- data_hogar$n_personas - data_hogar$n_menores_0_4 - data_hogar$n_menores_5_14
  clave <- paste(zona, data_hogar$quintil, tam)

  R <- matrix(NA_real_, nrow(data_hogar), ncol(razones), dimnames = list(NULL, colnames(razones)))
  nivel <- rep(NA_character_, nrow(data_hogar))
  for (k in unique(clave)) {
    filas <- which(clave == k)
    i1 <- filas[1]
    q <- gastos$quintil == data_hogar$quintil[i1]
    celdas <- list(
      "zona x quintil x tamanio" = q & gastos$macrozona == zona[i1] & tam_epf == tam[i1],
      "zona x quintil" = q & gastos$macrozona == zona[i1],
      "quintil" = q
    )
    for (n in names(celdas)) {
      idx <- which(celdas[[n]] %in% TRUE)
      if (length(idx) >= min_donantes) break
    }
    if (length(idx) == 0) next # sin quintil: queda NA, como en el FMNL
    for (i in filas) {
      m <- max(1, n_15_mas[i])
      R[i, ] <- colMeans(razones[idx[sample.int(length(idx), m, replace = TRUE)], , drop = FALSE])
    }
    nivel[filas] <- n
  }
  print(table(nivel, useNA = "ifany"))
  list(razones = R, nivel = nivel)
}


# ---------------------------------------------------------------------------------------------------------
# Inferencia de distribucion conjunta (JDI), Ye et al. (2017, JASSS) y Hawkins y Habib (2022,
# Transportation). La ENUT y la EPF solo comparten covariables de hogar D (quintil, zona, integrantes
# de 15+, trabajadores), asi que f(D, tiempo, gasto) = f_ENUT(D, tiempo) * g_EPF(gasto | D): imputar es
# sortear hogares EPF de g(gasto | D). El IPF deja la composicion de D de los donantes igual a la de la
# ENUT, y las celdas ralas se colapsan por la dimension de asociacion mas debil (el quintil nunca).
# Detalle en CHANGES.md, seccion 10.
# ---------------------------------------------------------------------------------------------------------

# Dimensiones comunes D de una tabla de hogares (ENUT o EPF), ya agrupadas.
dimensiones_jdi <- function(d) {
  data.frame(
    quintil = d$quintil,
    zona = ifelse(d$macrozona == "metropolitana", "metropolitana", "regiones"),
    n15 = pmin(pmax(d$n_15_mas, 1), 4),
    ntrab = pmin(d$n_trabajadores, 3)
  )
}

# Chi-cuadrado de independencia sobre la tabla ponderada. Los pesos se normalizan a suma n (muestra
# efectiva = observaciones), si no el estadistico crece con el factor de expansion. Devuelve el
# estadistico, los grados de libertad, el valor p y la V de Cramer.
chi2_independencia <- function(x, y, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  n <- length(x)
  w <- w * n / sum(w)
  tab <- tapply(w, list(x, y), sum)
  tab[is.na(tab)] <- 0
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(list(estadistico = 0, gl = 0, p = 1, V = 0))
  esperado <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  est <- sum((tab - esperado)^2 / esperado)
  gl <- (nrow(tab) - 1) * (ncol(tab) - 1)
  list(
    estadistico = est, gl = gl, p = pchisq(est, gl, lower.tail = FALSE),
    V = sqrt(est / (n * min(nrow(tab) - 1, ncol(tab) - 1)))
  )
}

# Pruebas de cada dimension contra cada clase de gasto (columnas de `epf`).
pruebas_independencia <- function(epf, dims, clases, w, alpha = 0.05) {
  filas <- lapply(dims, function(dim) {
    do.call(rbind, lapply(clases, function(cl) {
      t <- chi2_independencia(epf[[dim]], epf[[cl]], w)
      data.frame(
        dimension = dim, clase = cl, estadistico = t$estadistico, gl = t$gl, p = t$p, V = t$V,
        asociada = t$p < alpha
      )
    }))
  })
  do.call(rbind, filas)
}

# Una dimension independiente de las tres clases sale de D (Ye eq. 9). Las de `fijas` (el quintil) se
# quedan siempre. V = mayor V de Cramer entre las clases, por dimension retenida.
dimensiones_retenidas <- function(tabla, dims, fijas = "quintil") {
  retenidas <- dims[vapply(dims, function(d) any(tabla$asociada[tabla$dimension == d]) || d %in% fijas, logical(1))]
  V <- vapply(retenidas, function(d) max(tabla$V[tabla$dimension == d]), numeric(1))
  list(retenidas = retenidas, V = V)
}

# Quintiles ponderados de x (los empates comparten clase).
cuantiles_ponderados <- function(x, w, k = 5) {
  ux <- sort(unique(x))
  g <- match(x, ux)
  acum <- cumsum(as.vector(rowsum(w, g))) / sum(w)
  pmin(ceiling(acum[g] * k - 1e-12), k)
}

# IPF multivariado (Ye 3.9-3.10): ajusta los pesos de los hogares EPF (`perfiles`, una fila por hogar,
# dimensiones en columnas) hasta que las marginales de una y de dos dimensiones coincidan con las del
# objetivo (ENUT, ponderado por `w_objetivo`), ambas como participaciones. Las celdas vacias en la EPF
# siguen vacias. Los hogares del objetivo con un perfil sin hogares EPF en alguna marginal se sacan antes
# de calcular las metas (masa_sin_soporte: su participacion en el objetivo); si no, las marginales
# renormalizadas por separado serian inconsistentes entre si y el IPF no podria converger. Una celda EPF
# sin objetivo queda en cero.
ipf_multivariado <- function(perfiles, w, objetivo, w_objetivo = NULL, tol = 1e-8, max_iter = 1000,
                             error_si_falla = TRUE) {
  dims <- names(perfiles)
  if (is.null(w_objetivo)) w_objetivo <- rep(1, nrow(objetivo))
  clave <- function(d, v) do.call(paste, c(lapply(d[v], as.character), sep = "|"))
  margenes <- c(as.list(dims), if (length(dims) >= 2) combn(dims, 2, simplify = FALSE))
  w <- w / sum(w)
  con_soporte <- Reduce(`&`, lapply(margenes, function(m) clave(objetivo, m) %in% unique(clave(perfiles, m)[w > 0])))
  masa_sin_soporte <- sum(w_objetivo[!con_soporte]) / sum(w_objetivo)
  objetivo <- objetivo[con_soporte, , drop = FALSE]
  w_objetivo <- w_objetivo[con_soporte]
  M <- lapply(margenes, function(m) {
    k_epf <- clave(perfiles, m)
    meta <- tapply(w_objetivo, clave(objetivo, m), sum)
    meta <- meta / sum(meta)
    list(k = match(k_epf, names(meta)), meta = as.vector(meta))
  })
  suma_celdas <- function(w, m) {
    ok <- !is.na(m$k)
    cur <- numeric(length(m$meta))
    s <- rowsum(w[ok], m$k[ok])
    cur[as.integer(rownames(s))] <- s[, 1]
    cur
  }
  error_max <- function(w) max(vapply(M, function(m) max(abs(suma_celdas(w, m) - m$meta)), numeric(1)))
  iter <- 0
  err <- error_max(w)
  while (err >= tol && iter < max_iter) {
    iter <- iter + 1
    for (m in M) {
      f <- m$meta / suma_celdas(w, m)
      f[!is.finite(f)] <- 0
      ok <- !is.na(m$k)
      w[ok] <- w[ok] * f[m$k[ok]]
      w[!ok] <- 0
    }
    w <- w / sum(w)
    err <- error_max(w)
  }
  convergio <- err < tol
  if (!convergio && error_si_falla) {
    stop(sprintf("IPF no convergio en %d iteraciones (error maximo %.3e)", max_iter, err))
  }
  list(w = w, iter = iter, error = err, convergio = convergio, masa_sin_soporte = masa_sin_soporte)
}

# Sorteo por integrante con colapso. Para cada hogar ENUT (fila de `enut`, con las columnas de
# `perfiles_epf`) sortea m hogares EPF con reemplazo, con probabilidad proporcional a `w` entre los
# hogares con exactamente su perfil. Con menos de `min_donantes` hogares saca la dimension de menor V
# (V: vector con nombre sobre las dimensiones colapsables; el quintil no esta y nunca se saca). Devuelve,
# por cada matriz de `matrices` (filas = hogares EPF), el promedio de los m donantes, y el nivel usado.
sorteo_jdi <- function(enut, perfiles_epf, w, V, m, matrices, min_donantes = 10, semilla = 2023) {
  if (!is.null(semilla)) set.seed(semilla)
  colapsables <- names(V)
  orden <- colapsables[order(V)]
  fijas <- setdiff(names(perfiles_epf), colapsables)
  niveles <- lapply(0:length(orden), function(j) setdiff(c(fijas, colapsables), orden[seq_len(j)]))
  etiqueta <- vapply(niveles, paste, character(1), collapse = "+")
  clave <- function(d, v) do.call(paste, c(lapply(d[v], as.character), sep = "|"))
  k_epf <- lapply(niveles, function(v) clave(perfiles_epf, v))
  k_enut <- lapply(niveles, function(v) clave(enut, v))
  positivo <- w > 0
  n <- nrow(enut)
  m <- rep_len(m, n)
  salida <- lapply(matrices, function(M) matrix(NA_real_, n, ncol(M), dimnames = list(NULL, colnames(M))))
  nivel <- character(n)
  cache <- new.env()
  for (i in seq_len(n)) {
    k0 <- k_enut[[1]][i]
    d <- cache[[k0]]
    if (is.null(d)) {
      for (j in seq_along(niveles)) {
        idx <- which(k_epf[[j]] == k_enut[[j]][i] & positivo)
        if (length(idx) >= min_donantes || j == length(niveles)) break
      }
      d <- list(idx = idx, nivel = etiqueta[j])
      cache[[k0]] <- d
    }
    if (length(d$idx) == 0) stop("sorteo_jdi: no hay donantes con el quintil del hogar ", i)
    s <- d$idx[sample.int(length(d$idx), m[i], replace = TRUE, prob = w[d$idx])]
    for (nm in names(matrices)) salida[[nm]][i, ] <- colMeans(matrices[[nm]][s, , drop = FALSE])
    nivel[i] <- d$nivel
  }
  c(salida, list(nivel = nivel))
}

# Imputacion de gasto por JDI. Devuelve shares (participaciones de las 10 divisiones) y razones (10
# divisiones y ahorro sobre el ingreso del hogar) mas `nivel` por hogar, la
# tabla chi-cuadrado y el resumen del IPF. `data_hogar` necesita quintil, macrozona, n_15_mas,
# n_trabajadores y, si existe, fe_hogar (factor de expansion ENUT; sin el, ponderacion uniforme).
imputar_jdi <- function(data_hogar, gastos, min_donantes = 10, semilla = 2023, alpha = 0.05) {
  stopifnot(all(c("fe", "n_15_mas", "n_trabajadores", "macrozona", "quintil") %in% names(gastos)))
  w_enut <- if ("fe_hogar" %in% names(data_hogar)) data_hogar$fe_hogar else rep(1, nrow(data_hogar))
  stopifnot(!anyNA(w_enut))
  div <- paste0("g.", alts)
  shares <- as.matrix(gastos[, div]) / gastos$total_expenses
  colnames(shares) <- alts
  razones <- as.matrix(gastos[, c(div, "savings")]) / gastos$ingreso_hogar
  colnames(razones) <- c(alts, "savings")

  perf_epf <- dimensiones_jdi(gastos)
  perf_enut <- dimensiones_jdi(data_hogar)
  stopifnot(!anyNA(perf_enut), !anyNA(perf_epf))
  dims <- names(perf_epf)

  # clases de gasto: quintiles de ahorro/ingreso y de las participaciones Ec y Ef1
  ef1 <- rowSums(gastos[, paste0("g.", c("recreacion", "restaurantes", "comunicaciones"))])
  ec <- rowSums(gastos[, paste0("g.", c("cuentas", "hogar", "salud", "transporte", "educacion"))])
  epf <- cbind(perf_epf, data.frame(
    c_ahorro = cuantiles_ponderados(gastos$savings / gastos$ingreso_hogar, gastos$fe),
    c_ec = cuantiles_ponderados(ec / gastos$total_expenses, gastos$fe),
    c_ef1 = cuantiles_ponderados(ef1 / gastos$total_expenses, gastos$fe)
  ))
  pruebas <- pruebas_independencia(epf, dims, c("c_ahorro", "c_ec", "c_ef1"), gastos$fe, alpha)
  sel <- dimensiones_retenidas(pruebas, dims, fijas = "quintil")
  cat("\nJDI: chi-cuadrado dimension x clase de gasto (EPF ponderada, alpha =", alpha, ")\n")
  print(transform(pruebas, estadistico = round(estadistico, 1), p = signif(p, 3), V = round(V, 3)), row.names = FALSE)
  cat("Dimensiones retenidas:", paste(sel$retenidas, collapse = ", "), "\n")
  retenidas <- sel$retenidas

  ipf <- ipf_multivariado(perf_epf[, retenidas, drop = FALSE], gastos$fe, perf_enut[, retenidas, drop = FALSE],
    w_objetivo = w_enut
  )
  cat(sprintf(
    "JDI IPF: convergio en %d iteraciones, error maximo de marginales %.2e, masa ENUT sin soporte EPF %.4f\n",
    ipf$iter, ipf$error, ipf$masa_sin_soporte
  ))

  V <- sel$V[setdiff(names(sel$V), "quintil")]
  s <- sorteo_jdi(perf_enut[, retenidas, drop = FALSE], perf_epf[, retenidas, drop = FALSE], ipf$w, V,
    pmax(1, data_hogar$n_15_mas), list(shares = shares, razones = razones), min_donantes, semilla
  )
  cat("JDI: hogares por nivel de colapso\n")
  print(table(s$nivel))
  list(
    shares = s$shares, razones = s$razones, nivel = s$nivel, pruebas = pruebas,
    ipf = ipf[c("iter", "error", "masa_sin_soporte")]
  )
}

# Ef0 (alimentos), Ef1, Ef2 y Ec (las grandes categorias de utils.R) desde montos de las 10 divisiones.
# Ef1 incluye alcohol y tabaco porque ya estan plegados en recreacion (plegar_alcohol).
agregar_categorias <- function(M, sufijo) {
  out <- data.frame(
    M[, "alimentos"],
    rowSums(M[, c("recreacion", "restaurantes", "comunicaciones"), drop = FALSE]),
    M[, "vestimenta"],
    rowSums(M[, c("cuentas", "hogar", "salud", "transporte", "educacion"), drop = FALSE])
  )
  names(out) <- paste0(c("Ef0", "Ef1", "Ef2", "Ec"), sufijo)
  out
}

# metodo: "jdi" (inferencia de distribucion conjunta, por defecto), "hotdeck" (celdas Hawkins, seccion 8 de
# CHANGES.md) o "fmnl". Las columnas _fmnl y _jdi se guardan siempre y las 10 divisiones sin sufijo son
# siempre las del FMNL con el ahorro de la regresion (ver CHANGES.md); `metodo` elige el donante de las
# columnas _jdi. `pesos`: data.frame(id_hogar, fe_hogar) con el factor de expansion ENUT del hogar (wgtH);
# sin el, el JDI pondera uniforme los hogares ENUT.
imputacion_gastos <- function(data, metodo = "jdi", pesos = NULL) {
  metodo <- match.arg(metodo, c("jdi", "hotdeck", "fmnl"))
  library("minpack.lm")
  data_hogar <- data %>%
    dplyr::select(id_hogar, n_personas, n_menores_0_4, n_menores_5_14, n_menores_18, n_trabajadores, n_personas,
                  edad_promedio, quintil, macrozona, ingreso_hogar, income_person_week, n_profesionales, n_15_mas) %>%
    distinct(id_hogar, .keep_all = T) %>%
    mutate(n_personas_cut      = case_when(n_personas      >= 7 ~ 7, T ~ n_personas),
           n_menores_0_4_cut   = case_when(n_menores_0_4   >= 3 ~ 3, T ~ n_menores_0_4),
           n_menores_5_14_cut  = case_when(n_menores_5_14  >= 2 ~ 3, T ~ n_menores_5_14),
           n_trabajadores_cut  = case_when(n_trabajadores  >= 3 ~ 3, T ~ n_trabajadores),
           n_profesionales_cut = case_when(n_profesionales >= 2 ~ 3, T ~ n_profesionales))
  #hist(gastos$savings/gastos$ingresos, breaks =100)
  gastos  <<- read.csv("data/gastos.csv") %>% filter(ingreso_hogar > 0, savings/ingreso_hogar > -1)

  model   <<- apollo_loadModel("output/FMNL-epf-viii")
  lin_reg <<- lm(savings ~
                     ingreso_hogar +
                     (quintil==2) + (quintil==3) + (quintil==4) + (quintil==5) +
                     n_menores_0_4_cut + n_menores_5_14_cut + n_personas_cut +
                     n_trabajadores_cut  + n_profesionales_cut +
                     edad_promedio +
                     (macrozona == "metropolitana") ,
                 data = gastos)
  summary(lin_reg)
  codigos <<- c(g.alimentos = "01", g.vestimenta = "03",g.cuentas = "04", g.hogar = "05", g.salud = "06",
             g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09", g.educacion = "10", g.restaurantes = "11")
  alts <<- c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
             "comunicaciones", "recreacion", "educacion", "restaurantes")

  mask <- data_hogar[, "ingreso_hogar"] >= 0
  data_hogar[mask,"savings"] <- predict(lin_reg, data_hogar[mask,])

  sum(mask)
  sum((data_hogar[mask,"savings"] < 0), na.rm = T )
  sum(( data_hogar[mask,"savings"] > data_hogar[mask,"ingreso_hogar"]), na.rm = T)
  data_hogar %>%
    distinct(id_hogar, .keep_all = T) %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar))
  gastos %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar))

  data_hogar <-  data_hogar %>%
    mutate(savings = case_when(
      #savings < 0 ~ 0,
      savings > ingreso_hogar ~ ingreso_hogar, T ~ savings ))
  data_hogar$id_hogar <- as.numeric(data_hogar$id_hogar)
  if (!is.null(pesos)) {
    data_hogar$fe_hogar <- pesos$fe_hogar[match(data_hogar$id_hogar, as.numeric(pesos$id_hogar))]
    stopifnot(!anyNA(data_hogar$fe_hogar))
  }

  apollo_initialise()
  apollo_control <<- list(
    modelName       = "FMNL"   ,
    modelDescr      = "Fractional MNL model on time use data",
    indivID         = "id_hogar",
    outputDirectory = "output")
  apollo_probabilities <<- model$apollo_probabilities
  database <<- data_hogar
  database[,names(codigos)] <<- 0
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_inputs <<- apollo_validateInputs()
  predictions_base <<- apollo_prediction(model,apollo_probabilities,apollo_inputs)
  # las 10 divisiones sin sufijo siguen siendo las del FMNL con el ahorro de la regresion; las columnas
  # _fmnl son esas mismas en Ef1/Ef2/Ec/savings y las _jdi salen de los donantes (imputar_jdi)
  fmnl <- as.matrix(predictions_base[, alts]) * (data_hogar$ingreso_hogar - data_hogar$savings)
  data_hogar[, alts] <- fmnl
  hd <- if (metodo == "hotdeck") imputar_hawkins(data_hogar, gastos) else imputar_jdi(data_hogar, gastos)
  if (metodo != "hotdeck") {
    write.csv(hd$pruebas, "output/jdi_chi2.csv", row.names = FALSE)
    write.csv(data.frame(id_hogar = data_hogar$id_hogar, nivel = hd$nivel), "output/jdi_nivel.csv", row.names = FALSE)
  }
  jdi <- hd$razones * data_hogar$ingreso_hogar
  ok <- !is.na(jdi[, 1])
  stopifnot(isTRUE(all.equal(rowSums(jdi[ok, ]), data_hogar$ingreso_hogar[ok], check.attributes = FALSE)))
  sufijos <- c(
    paste0(c("Ef0", "Ef1", "Ef2", "Ec", "savings"), "_jdi"),
    paste0(c("Ef0", "Ef1", "Ef2", "Ec", "savings"), "_fmnl")
  )
  data_hogar <- cbind(
    data_hogar,
    agregar_categorias(jdi, "_jdi"), savings_jdi = jdi[, "savings"],
    agregar_categorias(fmnl, "_fmnl"), savings_fmnl = data_hogar$savings
  )

  data <- merge(data, data_hogar[,c("id_hogar", "savings", alts, sufijos)], by = "id_hogar", all.x = TRUE)
  data[,c("savings", alts, sufijos)] = sweep(data[,c("savings", alts, sufijos)], 1, unlist(data[,"prop_ing_hogar"]), "*")
  data[,"total_expenses"] = data[,"ing_personal"] - data["savings"]

  data[,c(alts, sufijos)] = round(data[,c(alts, sufijos)], 2)

  #ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc", "ing_gpp", "ing_personal", "income_person_week")
  # data[, ingresos] = data[, ingresos] * 1000
  # data[, alts] = data[, alts] * 1000
  # data[, "savings"] = data[, "savings"] * 1000
  # data[, "w"] = data[, "w"] * 1000

  return(data)
}

.rename_common_eng <- c(
  # identifiers
  id_person            = "id_persona",
  id_household         = "id_hogar",
  # sample type
  is_worker            = "es_trabajador",
  is_family            = "es_familia",
  # diary
  weekday              = "dia_semana",
  weekend_day          = "dia_fin_semana",
  # household composition
  relationship_to_head = "parentesco",
  n_children_0_4       = "n_menores_0_4",
  n_children_5_14      = "n_menores_5_14",
  n_children_0_14      = "n_nna",
  n_underage           = "n_menores_18",
  n_persons_15_65      = "n_personas_15_65",
  n_adults             = "n_mayores",
  n_time_reporters     = "n_tiempo",
  n_workers            = "n_trabajadores",
  n_professionals      = "n_profesionales",
  n_elderly            = "n_tercera_edad",
  has_elderly          = "hay_tercera_edad",
  household_size       = "n_personas",
  mean_age             = "edad_promedio",
  has_children         = "tiene_hijos",
  in_couple            = "en_pareja",
  lives_with_partner   = "vive_pareja",
  # external support
  domestic_service     = "servicio_domestico",
  help_from_relatives  = "ayuda_cercanos",
  external_support     = "fuentes_externas",
  # sociodemographics
  female               = "sexo",
  age                  = "edad_anios",
  age_bracket          = "tramo_edad",
  education_level      = "nivel_escolaridad",
  in_education         = "estudia",
  employed             = "trabaja",
  contracted_hours     = "horas_trabajo_contratadas",
  usual_hours          = "horas_trabajo_habituales",
  work_days_per_week   = "dias_trabajo_semana",
  quintile             = "quintil",
  macrozone            = "macrozona",
  region               = "region",
  comuna_code          = "cut",
  income_share         = "prop_ing_hogar",
  # labor
  employment_status    = "cae",
  occupation_group     = "ciuo_agrupada",
  # income
  inc_main_job         = "ing_ocuppal",
  inc_labor            = "ing_trab",
  inc_pension          = "ing_jub_aps",
  inc_group            = "ing_g",
  inc_group_personal   = "ing_gpp",
  inc_personal         = "ing_personal",
  household_income     = "ingreso_hogar",
  inc_person_week      = "income_person_week",
  # derived
  wage                 = "w",
  # expenditures
  food                 = "alimentos",
  clothing             = "vestimenta",
  utilities            = "cuentas",
  household_goods      = "hogar",
  health               = "salud",
  transportation       = "transporte",
  communications       = "comunicaciones",
  recreation           = "recreacion",
  education_exp        = "educacion",
  restaurants          = "restaurantes"
)

rename_to_english_raw <- function(data) {
  time_mapping <- c(
    t_paid_work              = "t_to",
    t_job_search             = "t_to_js",
    t_care_essential         = "t_tcnr_ce",
    t_care_education_related = "t_tcnr_re",
    t_care_other             = "t_tcnr_oac",
    t_domestic_meals         = "t_tdnr_psc",
    t_domestic_cleaning      = "t_tdnr_lv",
    t_domestic_laundry       = "t_tdnr_lrc",
    t_domestic_maintenance   = "t_tdnr_mrm",
    t_domestic_admin         = "t_tdnr_admnhog",
    t_domestic_shopping      = "t_tdnr_comphog",
    t_domestic_pets          = "t_tdnr_cmp",
    t_voluntary_community    = "t_tvaoh_tv",
    t_voluntary_households   = "t_tvaoh_oh",
    t_personal_care          = "t_cpaf_cp",
    t_meals                  = "t_cpag_comer",
    t_sleep                  = "t_cpag_dormir",
    t_education              = "t_ed",
    t_leisure_social         = "t_vsyo_csar",
    t_leisure_hobbies        = "t_vsyo_aa",
    t_leisure_events         = "t_vsyo_ev",
    t_leisure_sports         = "t_vsyo_dep",
    t_rest                   = "t_descanso",
    t_media_reading          = "t_mcm_leer",
    t_media_audio            = "t_mcm_audio",
    t_media_video            = "t_mcm_video",
    t_media_computer         = "t_mcm_computador",
    t_commute1               = "t_tt1"
  )
  data %>% dplyr::rename(any_of(c(.rename_common_eng, time_mapping)))
}

rename_to_english_enut <- function(data) {
  data %>% dplyr::rename(any_of(.rename_common_eng))
}
