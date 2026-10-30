# Categories of the EPF IX expenditure base (CCIF IX divisions).
#
# codigos: category name -> CCIF division code it collects directly.
# grupo_ef: category name -> expenditure group of the model.
#   Ef0 food (free by default, can be tested as committed).
#   Ef1 leisure and free goods (alcohol and tobacco count here).
#   Ef2 clothing.
#   Ec committed expenditure.
#
# Divisions 12 (insurance and financial services) and 13 (personal care and miscellaneous goods)
# are committed in the MAED classification, so both go to Ec through the bills category.

codigos <- c(
  g.alimentos = "01", g.alcohol_tabaco = "02", g.vestimenta = "03", g.cuentas = "04", g.hogar = "05",
  g.salud = "06", g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09",
  g.educacion = "10", g.restaurantes = "11"
)

grupo_ef <- c(
  g.alimentos = "Ef0", g.alcohol_tabaco = "Ef1", g.vestimenta = "Ef2", g.cuentas = "Ec", g.hogar = "Ec",
  g.salud = "Ec", g.transporte = "Ec", g.comunicaciones = "Ef1", g.recreacion = "Ef1",
  g.educacion = "Ec", g.restaurantes = "Ef1"
)

# Division code of the category each CCIF IX division feeds.
#   09.1 (cameras and photographic equipment) goes to communications.
#   12 and 13 go to bills (cuentas, Ec).
# The ten categories of the imputation and of the FMNL. Alcohol and tobacco are folded into recreation
# (Ef1) before these shares are computed, so they are not an extra alternative.
codigos_composicion <- setdiff(names(codigos), "g.alcohol_tabaco")

plegar_alcohol <- function(gastos) {
  gastos$g.recreacion <- gastos$g.recreacion + gastos$g.alcohol_tabaco
  gastos
}

codigo_categoria <- function(d, g) {
  out <- d
  out[d == "09" & g == "1"] <- codigos[["g.comunicaciones"]]
  out[d %in% c("12", "13")] <- codigos[["g.cuentas"]]
  out
}
