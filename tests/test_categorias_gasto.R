source("data_processing/categorias_gasto.R")

# Every CCIF IX division maps to one of the category codes, or to a category that is grouped.
stopifnot(setequal(names(grupo_ef), names(codigos)))
stopifnot(all(grupo_ef %in% c("Ef0", "Ef1", "Ef2", "Ec")))

# Food is its own free group (Ef0) so it can be tested as committed.
stopifnot(grupo_ef[["g.alimentos"]] == "Ef0")

# Alcohol and tobacco are visible but count as free leisure spending (Ef1).
stopifnot(codigo_categoria("02", "1") == codigos[["g.alcohol_tabaco"]])
stopifnot(grupo_ef[["g.alcohol_tabaco"]] == "Ef1")

# Divisions 12 and 13 go to the committed bucket that holds bills (cuentas), regardless of group.
for (g in c("1", "2", "3", "5", "6", "8", "9")) {
  stopifnot(codigo_categoria("12", g) == codigos[["g.cuentas"]])
  stopifnot(codigo_categoria("13", g) == codigos[["g.cuentas"]])
}
stopifnot(grupo_ef[["g.cuentas"]] == "Ec")

# Cameras (09.1) stay in communications, the rest of division 09 stays in recreation.
stopifnot(codigo_categoria("09", "1") == codigos[["g.comunicaciones"]])
stopifnot(codigo_categoria("09", "2") == codigos[["g.recreacion"]])

# Food and the other unchanged divisions keep their code.
stopifnot(codigo_categoria("01", "1") == codigos[["g.alimentos"]])
stopifnot(codigo_categoria("07", "1") == codigos[["g.transporte"]])

# The imputation works on ten alternatives; alcohol and tobacco are folded into recreation.
stopifnot(length(codigos_composicion) == 10)
stopifnot(!("g.alcohol_tabaco" %in% codigos_composicion))
pliegue <- plegar_alcohol(data.frame(g.recreacion = 5, g.alcohol_tabaco = 2))
stopifnot(pliegue$g.recreacion == 7)
stopifnot(setequal(codigos_composicion, names(codigos)[names(codigos) != "g.alcohol_tabaco"]))

# Vectorised over a whole column, with no NA created.
d <- c("01", "02", "09", "12", "13", "04")
g <- c("1", "1", "1", "3", "9", "1")
out <- codigo_categoria(d, g)
stopifnot(!anyNA(out))
stopifnot(all(out %in% codigos))

cat("test_categorias_gasto.R: all checks passed\n")
