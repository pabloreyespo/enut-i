# Parche de las columnas _jdi sobre los datasets finales PUBLICADOS de la ENUT I.
#
# Por que: una corrida completa de data_processing.R ya no reproduce lo publicado (el filtro de
# diagnostico_trabajo da 9,497 filas en vez de 9,412 con los insumos actuales), asi que el nuevo JDI
# (CHANGES.md) se aplica directamente a los datos publicados sin rehacer el pipeline. Solo cambian
# Ef1_jdi, Ef2_jdi, Ec_jdi y savings_jdi; el resto de las columnas y todas las filas quedan iguales
# (se agrega n_15_mas si el archivo publicado no lo trae). Las columnas sin sufijo y _fmnl no se tocan.
#
# Uso (desde enut-i): Rscript data_processing/parche_gastos_jdi.R
# Lee data/enut-i.dta y data/enut-i-raw.dta, reconstruye la tabla de hogares que necesita el JDI,
# llama a imputar_jdi() (la misma ruta de imputacion_gastos(), semilla 2023) y reescribe las cuatro
# variantes (raw, agregada, -ENG) en csv y dta.
rm(list = ls())
pkgs <- c("dplyr", "tidyr", "readr", "haven")
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
options(dplyr.summarise.inform = FALSE)

source("data_processing/processing_functions.R")

sufijos_jdi <- paste0(c("Ef1", "Ef2", "Ec", "savings"), "_jdi")
# imputar_jdi() usa `alts` como global (lo define imputacion_gastos)
alts <- c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
          "comunicaciones", "recreacion", "educacion", "restaurantes")

# ---- 1. datos publicados ----------------------------------------------------------------------------
pub_enut <- haven::read_dta("data/enut-i.dta")
pub_raw  <- haven::read_dta("data/enut-i-raw.dta")
stopifnot(identical(pub_enut$id_persona, pub_raw$id_persona))
cat("Filas publicadas:", nrow(pub_enut), " hogares:", n_distinct(pub_enut$id_hogar), "\n")

# ---- 2. n_15_mas y factor de expansion del hogar desde la ENUT cruda -----------------------------------
base <- haven::read_dta("data/raw/BASE_USUARIO_corregida.dta", col_select = c("id_hogar", "c14_1_1", "wgtH"))
hogares_raw <- base %>%
  group_by(id_hogar) %>%
  summarise(
    n_15_mas = sum(as.numeric(c14_1_1) >= 15, na.rm = TRUE),
    fe_hogar = suppressWarnings(max(as.numeric(wgtH), na.rm = TRUE))
  ) %>%
  filter(is.finite(fe_hogar))

agregar_n_15_mas <- function(d) {
  if ("n_15_mas" %in% names(d)) return(d)
  d$n_15_mas <- hogares_raw$n_15_mas[match(d$id_hogar, hogares_raw$id_hogar)]
  stopifnot(!anyNA(d$n_15_mas))
  # misma posicion que en la salida del pipeline (despues de la 19a columna)
  dplyr::relocate(d, n_15_mas, .after = 19)
}
enut <- agregar_n_15_mas(pub_enut)
raw  <- agregar_n_15_mas(pub_raw)

# ---- 3. tabla de hogares (orden de aparicion por id_persona, como en el pipeline) --------------------
data_hogar <- enut %>%
  arrange(id_persona) %>%
  distinct(id_hogar, .keep_all = TRUE) %>%
  dplyr::select(id_hogar, quintil, macrozona, n_trabajadores, ingreso_hogar, n_15_mas)
data_hogar$fe_hogar <- hogares_raw$fe_hogar[match(data_hogar$id_hogar, hogares_raw$id_hogar)]
stopifnot(!anyNA(data_hogar$fe_hogar))
cat("Hogares:", nrow(data_hogar), "\n")

gastos <- read.csv("data/gastos.csv") %>% filter(ingreso_hogar > 0, savings / ingreso_hogar > -1)

# ---- 4. JDI (misma ruta que imputacion_gastos con metodo = "jdi") -------------------------------------
hd <- imputar_jdi(data_hogar, gastos)
write.csv(hd$pruebas, "output/jdi_chi2.csv", row.names = FALSE)
write.csv(data.frame(id_hogar = data_hogar$id_hogar, nivel = hd$nivel), "output/jdi_nivel.csv", row.names = FALSE)
jdi <- hd$razones * data_hogar$ingreso_hogar
ok <- !is.na(jdi[, 1])
stopifnot(isTRUE(all.equal(rowSums(jdi[ok, ]), data_hogar$ingreso_hogar[ok], check.attributes = FALSE)))
hogar_jdi <- cbind(
  data_hogar["id_hogar"],
  agregar_categorias(jdi, "_jdi"), savings_jdi = jdi[, "savings"]
)

# ---- 5. columnas _jdi por persona: razones x ingreso_hogar x prop_ing_hogar, redondeo a 2 decimales ----
aplicar_jdi <- function(d) {
  idx <- match(d$id_hogar, hogar_jdi$id_hogar)
  stopifnot(!anyNA(idx))
  M <- as.matrix(hogar_jdi[idx, sufijos_jdi]) * d$prop_ing_hogar
  d[, sufijos_jdi] <- round(as.data.frame(M), 2)
  d
}
nuevo_enut <- aplicar_jdi(enut)
nuevo_raw  <- aplicar_jdi(raw)

# ---- 6. verificacion ------------------------------------------------------------------------------------
comparar <- function(viejo, nuevo, nombre) {
  stopifnot(nrow(viejo) == nrow(nuevo), identical(viejo$id_persona, nuevo$id_persona),
            identical(viejo$id_hogar, nuevo$id_hogar))
  cols <- setdiff(names(viejo), sufijos_jdi)
  dif <- cols[!vapply(cols, function(c) identical(as.vector(viejo[[c]]), as.vector(nuevo[[c]])), logical(1))]
  cat(sprintf("[%s] filas %d, ids iguales; columnas no _jdi distintas: %s\n", nombre, nrow(nuevo),
              if (length(dif)) paste(dif, collapse = ", ") else "ninguna"))
  stopifnot(length(dif) == 0, identical(names(nuevo)[names(nuevo) %in% names(viejo)], names(viejo)))
}
comparar(pub_enut, dplyr::select(nuevo_enut, -any_of(if (!"n_15_mas" %in% names(pub_enut)) "n_15_mas")), "agregada")
comparar(pub_raw, dplyr::select(nuevo_raw, -any_of(if (!"n_15_mas" %in% names(pub_raw)) "n_15_mas")), "raw")

# las variantes -ENG publicadas deben coincidir con el renombre del dataset parchado (salvo _jdi)
for (v in list(list("data/enut-i-ENG.dta", rename_to_english_enut(nuevo_enut)),
               list("data/enut-i-raw-ENG.dta", rename_to_english_raw(nuevo_raw)))) {
  viejo <- haven::read_dta(v[[1]]); nuevo <- v[[2]]
  cols <- setdiff(names(viejo), sufijos_jdi)
  dif <- cols[!vapply(cols, function(c) identical(as.vector(viejo[[c]]), as.vector(nuevo[[c]])), logical(1))]
  cat(sprintf("[%s] filas %d vs %d; columnas no _jdi distintas: %s\n", v[[1]], nrow(viejo), nrow(nuevo),
              if (length(dif)) paste(dif, collapse = ", ") else "ninguna"))
  stopifnot(length(dif) == 0, nrow(viejo) == nrow(nuevo))
}

cat("\nSuma Ef1+Ef2+Ec+savings (_jdi) menos ing_personal, maximo absoluto (hogares con JDI):\n")
for (n in c("agregada", "raw")) {
  d <- if (n == "agregada") nuevo_enut else nuevo_raw
  s <- rowSums(d[, sufijos_jdi]) - d$ing_personal
  cat(sprintf("  %s: %.4f (NA en _jdi: %d)\n", n, max(abs(s), na.rm = TRUE), sum(is.na(d$Ef1_jdi))))
}

cat("\n_jdi viejo vs nuevo (nivel persona):\n")
res <- do.call(rbind, lapply(sufijos_jdi, function(c) {
  a <- pub_enut[[c]]; b <- nuevo_enut[[c]]
  data.frame(columna = c, media_vieja = mean(a, na.rm = TRUE), media_nueva = mean(b, na.rm = TRUE),
             sd_vieja = sd(a, na.rm = TRUE), sd_nueva = sd(b, na.rm = TRUE),
             correlacion = cor(a, b, use = "complete.obs"))
}))
print(transform(res, media_vieja = round(media_vieja, 2), media_nueva = round(media_nueva, 2),
                sd_vieja = round(sd_vieja, 2), sd_nueva = round(sd_nueva, 2), correlacion = round(correlacion, 3)),
      row.names = FALSE)

# ---- 7. escritura de las variantes -------------------------------------------------------------------------
# El dta no guarda Inf (w = ing_trab / Tw da Inf en 3 personas y el dta lo lee como NA), pero el csv
# publicado si. Al escribir el csv se restauran los valores no finitos del csv publicado.
restaurar_inf <- function(d, archivo) {
  pub <- readr::read_csv(archivo, show_col_types = FALSE, guess_max = 1e5, progress = FALSE)
  stopifnot(nrow(pub) == nrow(d))
  for (c in intersect(names(pub), names(d))) {
    if (is.numeric(pub[[c]]) && is.numeric(d[[c]])) {
      idx <- which(is.infinite(pub[[c]]))
      if (length(idx)) d[[c]][idx] <- pub[[c]][idx]
    }
  }
  d
}
variantes <- list(
  list("data/enut-i-raw",     nuevo_raw),
  list("data/enut-i",         nuevo_enut),
  list("data/enut-i-raw-ENG", rename_to_english_raw(nuevo_raw)),
  list("data/enut-i-ENG",     rename_to_english_enut(nuevo_enut))
)
for (v in variantes) {
  csv_nuevo <- restaurar_inf(v[[2]], paste0(v[[1]], ".csv"))
  haven::write_dta(v[[2]], paste0(v[[1]], ".dta"))
  write_csv(csv_nuevo, paste0(v[[1]], ".csv"))
}
cat("\nEscritas las 8 variantes (csv y dta) en data/.\n")
