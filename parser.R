data = read.csv("/home/alex/Documents/encuesta.csv", header = TRUE, stringsAsFactors = FALSE)
postal = read.csv("/home/alex/Documents/postal.cat.csv", header = TRUE, colClasses = rep("character", 8))
codes <- c("08400", postal$codigopostalid[which(postal$provincia %in% c("Barcelona", "Tarragona", "Lleida", "Girona"))])

eliminated_rows <- c()

# data$Timestamp

#data$Año.de.nacimiento

format <- "%d-%m-%Y"
hyphen_year_format <- which(!is.na(as.Date(data$Año.de.nacimiento, format = format)))
data$Año.de.nacimiento[hyphen_year_format] <- format(as.Date(data$Año.de.nacimiento[hyphen_year_format], format = format), "%Y")
  
format <- "%d/%m/%Y"
bar_year_format <- which(!is.na(as.Date(data$Año.de.nacimiento, format = format)))
data$Año.de.nacimiento[bar_year_format] <- format(as.Date(data$Año.de.nacimiento[bar_year_format], format = format), "%Y")

bad_format <- which(is.na(as.integer(data$Año.de.nacimiento)))
data$Año.de.nacimiento[bad_format] <- c(1958, 1979, 1971, 1978, 1978, 0, 1990, 1977, 1975, 1993, 1990, 1974, 1967, 1963, 1978, 1979, 0, 1969, 2000, 1978, 1989, 2000, 1964, 1984, 1971, 1959, 1961, 1975, 1977)

wrong_years_too_young <- which(as.integer(data$Año.de.nacimiento) > 2000 & as.integer(data$Año.de.nacimiento) < 2019)

wrong_years_two_numbers <- which(as.integer(data$Año.de.nacimiento) < 100)
data$Año.de.nacimiento[wrong_years_two_numbers] <- c(1983, 0, 1963, 1968, 1966, 1982, 1982, 0, 1999, 1986, 1975, 1978, 1967, 1979, 1990, 1951, 1967, 1997, 1998, 1995, 1958, 1967, 1972, 1968, 0, 1983, 1982, 1998, 1959, 1978, 1968, 1953, 1961, 1962, 1973, 1990)

wrong_years_typo <- which(as.integer(data$Año.de.nacimiento) < 1900 | as.integer(data$Año.de.nacimiento) > 2019)
data$Año.de.nacimiento[wrong_years_typo] <- c(1983, 1969, 1972, 0, 1970, 1971, 1978, 1953, 1999, 1964, 0, 1959, 1976, 0, 1978, 0, 1975, 0, 1971, 1986, 1971, 1976, 0, 1976, 1986, 1973, 1973, 1956, 1962, 1994, 1980, 0, 1972, 0, 1976, 0, 1970, 1962, 1962)

eliminated_rows <- c(eliminated_rows, bad_format[[6]], bad_format[[17]])
eliminated_rows <- union(eliminated_rows, wrong_years_too_young)
eliminated_rows <- union(eliminated_rows, c(wrong_years_two_numbers[[2]], wrong_years_two_numbers[[8]], wrong_years_two_numbers[[25]]))
eliminated_rows <- union(eliminated_rows, c(wrong_years_typo[[14]], wrong_years_typo[[16]], wrong_years_typo[[18]], wrong_years_typo[[32]], wrong_years_typo[[34]], wrong_years_typo[[36]]))

# data$Género

# data$X.Cuántos.hermanos.tienes.
bad_format <- which(is.na(as.integer(data$X.Cuántos.hermanos.tienes.)))
data$X.Cuántos.hermanos.tienes.[bad_format] <- sapply(data$X.Cuántos.hermanos.tienes.[bad_format], function(input) {
  if (input %in% c("Uno", "Una", "Un", "1/", "uno", "Uno (1)", "Uno ")) {
    1
  } else if (input %in% c("Dos", "dos")) {
    2
  } else if (input %in% c("cero", "Cero", "o", "ninguno", "Ninguno")) {
    0
  } else {
    input
  }
})

bad_format <- which(is.na(as.integer(data$X.Cuántos.hermanos.tienes.)))
data$X.Cuántos.hermanos.tienes.[bad_format] <- c(1, 3, 1, -1, 2, 4, 4, 3, 1, 8, 5, 4, 3, 1, 1, 4, 2, 1, 3, 1, 1, 4, 2, 5, 0, 3, 0, 3, 2, 3, 1, 0, 2, 3, 2, 1, 1, 1, -1, 3, 3, 3, 1, 3)
data$X.Cuántos.hermanos.tienes.[c(119, 266, 490, 1735, 2969, 4427, 4614, 4937)] <- c(1, 1, 1, 3, 2, 3, 4, 2)

# data$Orden.de.nacimiento

data$Orden.de.nacimiento[c(2004, 2988, 4363, 5539, 6396)] <- rep(1, 5)
data$Orden.de.nacimiento[c(123, 310, 2823, 4739, 5987)] <- rep(2, 5)
data$Orden.de.nacimiento[c(358, 2044, 2316, 4585, 4590, 5153, 5348, 5349, 5550, 6031, 6139)] <- rep(2, 11)
data$Orden.de.nacimiento[c(5806)] <- 3

bad_format_mayor <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("Mayor", orden) || grepl("mayor", orden)))
data$Orden.de.nacimiento[bad_format_mayor] <- rep(1, length(bad_format_mayor))

bad_format_primero <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("Primero", orden) || grepl("primero", orden) || grepl("Primera", orden) || grepl("primera", orden) || grepl("Primer", orden) || grepl("primer", orden)))
data$Orden.de.nacimiento[bad_format_primero] <- rep(1, length(bad_format_primero))

bad_format_segundo <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("Segundo", orden) || grepl("segundo", orden) || grepl("Segunda", orden) || grepl("segunda", orden) || grepl("Segon", orden) || grepl("segon", orden) || grepl("Segona", orden) || grepl("segona", orden)))
data$Orden.de.nacimiento[bad_format_segundo] <- rep(2, length(bad_format_segundo))

bad_format_gran <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("Gran", orden) || grepl("gran", orden) || grepl("Unic", orden) || grepl("unic", orden)))
data$Orden.de.nacimiento[bad_format_gran] <- rep(1, length(bad_format_gran))

bad_format_petit <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("Petit", orden) || grepl("petit", orden) || grepl("Peque", orden) || grepl("peque", orden)))
data$Orden.de.nacimiento[bad_format_petit] <- as.integer(data$X.Cuántos.hermanos.tienes.[bad_format_petit]) + 1

bad_format_symbol <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("1er", orden) || grepl("1era", orden) || grepl("1ª", orden) || grepl("1º", orden)))
data$Orden.de.nacimiento[bad_format_symbol] <- rep(1, length(bad_format_symbol))

bad_format_yo <- which(sapply(data$Orden.de.nacimiento, function(orden) grepl("^Yo", orden) || grepl("^yo", orden) || grepl("^Jo", orden) || grepl("^jo", orden)))
data$Orden.de.nacimiento[bad_format_yo] <- rep(1, length(bad_format_yo))

bad_format <- which(is.na(as.integer(data$Orden.de.nacimiento)))
# data.frame(data$Orden.de.nacimiento[bad_format], data$Año.de.nacimiento[bad_format], data$X.Cuántos.hermanos.tienes.[bad_format], data$Género[bad_format])
options(max.print=1500)

data$Orden.de.nacimiento[bad_format] <-
  c(2, 3, 1, 1, 3, 6, 2, -1, 2, 1,
    2, 4, 3, 2, 2, 1, 3, 2, -1, 1,
    2, -1, -1, 4, 1, 1, 1, 4, 1, 2,
    1, -1, 1, 2, 2, 1, -1, 3, 3, 1,
    2, 1, 1, 3, 1, 1, 1, 3, 1, 1,
    2, 2, -1, 5, 1, -1, 2, 3, 3, 5,
    4, 1, 2, 1, 6, 1, 1, 1, 1, 1,
    5, 2, 5, 2, -1, 2, 1, 3, 4, -1,
    1, 2, 3, 1, 4, -1, 1, 1, 1, 1,
    -1, 1, 1, 4, 1, 1, 1, 1, 3, 1,
    -1, 2, -1, 2, 3, 3, -1, 2, 3, 2, 
    5, 4, 1, 9, 1, 2, 1, 1, 2, 3, 
    3, 3, 3, 1, 2, 2, 1, 3, 1, 1,
    1, 1, 5, 1, 4, 1, 1, 1, 1, 4,
    1, 1, 2, 1, 1, 2, 1, -1, 3, 7,
    1, -1, 3, 2, 3, 4, 2, 1, 1, 3, 
    3, 2, 3, 2, 2, 1, 1, 2, 4, 2,
    2, 1, 3, 3, -1, 2, 2, 7, 1, 1,
    3, 2, -1, 1, 3, 4, 3, 3, 3, 3,
    1, -1, 1, 1, -1, 2, 5, 3, 1, 4,
    -1, 3, 2, 2, 3, 2, 1, 1, 1, 1, 
    3, 1, 1, 1, 1, -1, 1, 1, 2, 2,
    -1, 2, 1, 2, 2, -1, -1, 2, 1, 3,
    1, 3, 1, 2, 2, 3, 2, 1, 3, 1, 
    1, 1, 4, 1, 1, 2, 1, 3, 2, 3,
    2, 1, 2, 2, 2, 9, 2, -1, 1, 1, 
    1, 3, 2, 1, 2, 3, 1, 1, -1, 2,
    3, 2, 4, 3, 1, 4, 2, 1, 1, 3, 
    5, 1, 1, 1, 2, 7, 2, 1, 2, 1, 
    -1, 1, 2, 1, -1, 3, 1, 2, 2, -1,
    2, 2, 3, 1, -1, 2, 3, 2, 2, -1, 
    1, 2, 1, 2, 2, 1, 3, -1, 1, 1, 
    1, -1, 1, 2, 2, 1, 1, 3, 1, 1, 
    1, 1, 4, 1, -1, 1, 1, 1, 1, 3,
    1, 3, 1, 2, -1, 2, 1, 1)

eliminated_rows <- union(eliminated_rows, bad_format[c(8, 19, 22, 23, 32, 37, 53, 56, 75, 80, 86, 91, 101, 103, 107, 148, 152, 175, 183, 192, 195, 201, 216, 221, 226, 227, 258, 269, 291, 295, 300, 305, 310, 318, 322, 335, 345)])

# data$Durante.la.mayor.parte.de.tu.infancia...cuál.era.la.situación.de.tu.familia.

# data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.

bad_format_anos <- which(sapply(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a., function(orden) grepl("ños", orden)))
data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format_anos] <- sapply(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format_anos], function(input) {
  x <- as.integer(gsub("[A-Z,a-z,ñ, ]*", "", input))
  if (x > 100) {
    x %/% 10
  } else {
    x
  }
})

bad_format_alos <- which(sapply(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a., function(orden) grepl("l[o,a]*s", orden)))
data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format_alos] <- sapply(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format_alos], function(input) {
  x <- as.integer(gsub("[A-Z,a-z,ñ,ó,\\,, ]*", "", input))
  if (x > 100) {
    x %/% 1000
  } else {
    x
  }
})

bad_format_nose <- which(sapply(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a., function(orden) grepl("o[ ]*[lo]*s[e,é]", orden)))
data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format_nose] <- rep("", length(bad_format_nose))

bad_format <- which(is.na(as.integer(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.)))
data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[bad_format] <- c(25, "", 30, "", 25, "", 22, 22, 32, "", -1, 25, 25, 23, "", "", 21, 35, "", 30, 24, "", "", 24)

eliminated_rows <- union(eliminated_rows, c(491, 666, 1898, 3415))
eliminated_rows <- union(eliminated_rows, which(as.integer(data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.) > 60))

# data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.

data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[5706] <- data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[5706]
data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[1545] <- data$X.A.qué.edad.tuvo.tu.MADRE.su.primer.hijo.a.[1545]

bad_format_anos <- which(sapply(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a., function(orden) grepl("ños", orden)))
data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format_anos] <- sapply(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format_anos], function(input) {
  x <- as.integer(gsub("[A-Z,a-z,ñ, ]*", "", input))
  while (x > 100) {
    x <- x %/% 10
  }
  x
})

bad_format_alos <- which(sapply(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a., function(orden) grepl("l[o,a]*s", orden)))
data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format_alos] <- sapply(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format_alos], function(input) {
  x <- as.integer(gsub("[A-Z,a-z,ñ,é,\\,, ]*", "", input))
  while (x > 100) {
    x <- x %/% 10
  }
  x
})

bad_format_nose <- which(sapply(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a., function(orden) grepl("o[ ]*[lo]*s[e,é]", orden)))
data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format_nose] <- rep("", length(bad_format_nose))

data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[which(as.integer(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.) == 0)] <- c("", "", "")

bad_format <- which(is.na(as.integer(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.)))
data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.[bad_format] <- 
  c("", "", 25, "", "", "", 41, "", "", "", "", "", "", 30, "", "", "", 27, 23, 
    "", "", "", "", "", "", "", -1, "", "", 35, "", "", 25, "", "", "", "", "", "", 24, "", "")

eliminated_rows <- union(eliminated_rows, c(3415))
eliminated_rows <- union(eliminated_rows, which(as.integer(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.) > 85))
eliminated_rows <- union(eliminated_rows, which(as.integer(data$X.A.qué.edad.tuvo.tu.PADRE.su.primer.hijo.a.) < 15))

# data$X.Cuál.es.el.nivel.de.estudios.máximo.obtenido.por.tu.madre.

# data$X.Cuál.es.el.nivel.de.estudios.máximo.obtenido.por.tu.padre.

# data$X.Qué.tipo.de.educación.recibió.tu.madre.

# data$X.Qué.tipo.de.educación.recibió.tu.padre.

# data$X.Cuál.era.la.profesion.principal.de.tu.madre.

# TODO

# data$X.Cuál.era.la.profesion.principal.de.tu.padre.

# TODO

# data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.

data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[c(274, 2288, 2541, 2601, 4268, 5092, 5628, 6236)] <- c(2, 1, "", "", "", "", 2, 3)
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[c(1178, 2275, 2303, 4164, 4865, 5595)] <- c("", 2, 2, 2, 2, "")

bad_format_todos <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[T,t][o,O][D,d,T,t]", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_todos] <- rep("", length((bad_format_todos)))

bad_format_none <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[N,n]ing", orden) || grepl("[N,n]ad", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_none] <- rep(0, length(bad_format_none))

bad_format_una <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) is.na(as.integer(orden)) && (grepl("1", orden) || grepl("[U,u]n", orden))))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_una] <- rep(1, length(bad_format_una))

bad_format_dos <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) is.na(as.integer(orden)) && (grepl("2", orden) || grepl("[D,d]os", orden) || grepl("[A,a]mbo", orden))))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_dos] <- rep(2, length(bad_format_dos))

bad_format_padres <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[P,p]adres", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_padres] <- rep(2, length(bad_format_padres))

bad_format_padre <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[P,p]adre", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_padre] <- rep(1, length(bad_format_padre))

bad_format_madre <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[M,m]adre", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_madre] <- rep(1, length(bad_format_madre))

bad_format_yo <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo., function(orden) grepl("[Y,y]o", orden) || grepl("arido", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format_yo] <- rep("", length(bad_format_yo))

too_many <- which(as.integer(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.) > 5)
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[too_many] <- c(6, 6, "", "", "", 6, "", 7, 11, "", 7, "", "", "", "", "", "", "", 10, "", "", 6, "", "", "", "", "", "")

bad_format <- setdiff(which(is.na(as.integer(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.))), which(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo. == ""))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.completo.[bad_format] <- c(3, 1, 1, 2, "", 0, 1, 2, 3, 1, 1, 0, 3, "", "", 3, "", 3, 0, 2, 0, 2, 0, 0, 3, 2, 3, "", 3)

# data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.

data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[c(274, 2288, 4266, 4435, 4460)] <- c(1, 1, "", "", 0)
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[c(310, 1852, 3183, 5595, 5894, 6031, 6460)] <- c(0, "", 2, "", 2, "", "")

bad_format_todos <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[T,t][o,O][D,d,T,t]", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_todos] <- rep("", length((bad_format_todos)))

bad_format_none <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[N,n]ing", orden) || grepl("[N,n]ad", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_none] <- rep(0, length(bad_format_none))

bad_format_una <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) is.na(as.integer(orden)) && (grepl("1", orden) || grepl("[U,u]n", orden))))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_una] <- rep(1, length(bad_format_una))

bad_format_dos <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) is.na(as.integer(orden)) && (grepl("2", orden) || grepl("[D,d]os", orden) || grepl("[A,a]mbo", orden))))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_dos] <- rep(2, length(bad_format_dos))

bad_format_padres <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[P,p]adres", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_padres] <- rep(2, length(bad_format_padres))

bad_format_padre <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[P,p]adre", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_padre] <- rep(0, length(bad_format_padre))

bad_format_madre <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[M,m]adre", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_madre] <- rep(1, length(bad_format_madre))

bad_format_yo <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("[Y,y]o", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_yo] <- rep(1, length(bad_format_yo))

bad_format_cero <- which(sapply(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial., function(orden) grepl("^[o,O]", orden) || grepl("[C,Z]ero", orden) || grepl("[C,c]ap", orden)))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format_cero] <- rep(0, length(bad_format_cero))

too_many <- which(as.integer(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.) > 5)
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[too_many] <- c("", "", 7, "", 9, "")

bad_format <- setdiff(which(is.na(as.integer(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.))), which(data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial. == ""))
data$X.Cuántas.personas.trabajaban.en.tu.hogar.a.tiempo.parcial.[bad_format] <- c(0, "", "", "", "", 1, "", 1, 1, 0, 0, 3, "", "", 1, 0, 0, 1, "", 0, "")

# data$X.En.qué.categoría.clasificarías.la.situación.económica.de.tu.familia.durante.tu.infancia.

# data$X.Cuál.era.la.situación.de.tu.familia.en.cuanto.a.vivienda.

# data$Indica.el.código.postal..o.códigos.postales..de.tu.casa.durante.tu.infancia.

data$Indica.el.código.postal..o.códigos.postales..de.tu.casa.durante.tu.infancia. <- sapply(data$Indica.el.código.postal..o.códigos.postales..de.tu.casa.durante.tu.infancia., function(codigo) {
  codigo <- gsub("O", "0", codigo)
  codigo <- gsub(" ", "", codigo)
})

bad_codes <- which(!(data$Indica.el.código.postal..o.códigos.postales..de.tu.casa.durante.tu.infancia. %in% codes))
data$Indica.el.código.postal..o.códigos.postales..de.tu.casa.durante.tu.infancia.[bad_codes]
#  c("0", "0", "0", "0", "0", "0", "08021", "0", "08202", "43790", 
      "0" "0", "0", "0", "0", "08031", "08028", "0", "08001", "0",
      "0", "08041", "0", "08191", "08034", "0", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "0", "0", "08201", "0", "08191",
      "08017", "08120", "081920", "0", "0", "08019" "08013", "0", "0", "08027",
      "08035", "0", "0","08027", "0", "08032", "0", "08016", "0", "0",
      "0", "08027", "0", "0", "08206", "0", "0", "0", "0", "08005",
      "0", "0", "0", "08032", "0", "08196", "0", "0", "0", "08018",
      "08005", "08291", "0", "0", "0", "0", "0", "0", "0", "0",
      "0", "08921", "0", "08221", "0", "0", "0", "08902", "0", "08002",
      "08031", "0", "08026", "08033", "08042", "0", "0", "08205", "08025", "08224",
      "08921", "0", "0", "0", "08420", "08830", "0", "08006", "0", "08030", 
      "0", "08018", "08031", "17494", "0", "0", "0", "08013", "08226", "0", 
      "0", "0", "0", "0", "0", "0", "08922", "08140", "08110", 
      "0", "08914", "0", "0", "0", "08016", "0", "0", "0", "0", 
      "08018", "08917", "0", "08014", "0", "08922", "0", "0", "0", "0", 
      ) 
eliminated_rows <- union(eliminated_rows, c(bad_codes[[2]], bad_codes[[6]], bad_codes[[11]], bad_codes[[13]], bad_codes[[14]], bad_codes[[20]], bad_codes[[23]], bad_codes[[26]], bad_codes[[27]], 
    bad_codes[[28]], bad_codes[[29]], bad_codes[[31]], bad_codes[[32]], bad_codes[[35]], bad_codes[[36]], bad_codes[[37]], bad_codes[[45]], bad_codes[[48]], bad_codes[[52]], bad_codes[[53]],
    bad_codes[[55]], bad_codes[[60]], bad_codes[[66]], bad_codes[[67]], bad_codes[[68]], bad_codes[[71]], bad_codes[[77]], bad_codes[[78]], bad_codes[[84]], bad_codes[[85]], bad_codes[[87]],
    bad_codes[[88]], bad_codes[[89]], bad_codes[[90]], bad_codes[[91]], bad_codes[[93]], bad_codes[[95]], bad_codes[[99]], bad_codes[[102]], bad_codes[[106]], bad_codes[[112]], bad_codes[[113]],
    bad_codes[[114]], bad_codes[[117]], bad_codes[[121]], bad_codes[[125]], bad_codes[[126]], bad[[127]], bad_codes[[130]], bad_codes[[132]], bad_codes[[133]], bad_codes[[135]], bad_codes[[143]],
    bad_codes[[144]], bad_codes[[147]], bad_codes[[148]], bad_codes[[149]], bad_codes[[150]], bad_codes[[153]], bad_codes[[155]], bad_codes[[157]], bad_codes[[158]], bad_codes[[160]], ))

data$CP2 <- c("")
data$CP3 <- c("")
data$CP4 <- c("")

# data$CP2[bad_codes[c(7, 9, 16, 17, 19, 22, 24, 25, 40, 43, 46, 47, 50, 51, 54, 56, 58, 62, 65, 74, 76, 80, 82, 94, 101, 103, 104, 105, 108, 109, 110, 111, 115, 116, 118, 120, 122, 123, 128, 
#           129, 138, "139", 142, 146, 151, 152, 705)]] <- 
#    c("08028", "08130", "08022", "08029", "08290", "08012", "08231", "08190", "08003", "08130", "08130", "08025", "08030", "08016", "08030", "08030", "08030", "08020", "08211", "08017", 
        "08923", "08005", "08233", "08233", "08032", "08018", "08290", "08033", "08181", "08013", "08223", "08233", "08170", "08940", "08022", "08181", "08003", "08042", "08940", "08225", 
        "08184", "08181", "08192", "08130", "08026", "08923" "08635")



# data$CP3[bad_codes[c(17, 62, 74, 128)]] <- c("08014", "08026", "08196", "08970")
# data$CP4[bad_codes[c()]] <- c()

# data$X.A.qué.edad.empezaste.a.trabajar.

stop("done")
data$X.A.qué.edad.empezaste.a.trabajar.[c(1003)] <- c(20)

bad_format_no <- which(sapply(data$X.A.qué.edad.empezaste.a.trabajar., function(orden) grepl("^[N,n][O,o]", orden)))
data$X.A.qué.edad.empezaste.a.trabajar.[bad_format_no] <- rep("", length(bad_format_no))

bad_format <- setdiff(which(is.na(as.integer(data$X.A.qué.edad.empezaste.a.trabajar.))), which(data$X.A.qué.edad.empezaste.a.trabajar. == ""))
data$X.A.qué.edad.empezaste.a.trabajar.[bad_format] <- c()

# data$X.Has.terminado.de.estudiar.

# data$Si.has.terminado.de.estudiar...cuál.es.tu.grado.máximo.de.estudios.

# data$Si.no.has.terminado.de.estudiar...qué.tipo.de.estudios.estás.cursando.ahora.mismo.

# data$X.Has.cursado.otras.modalidades.de.bachillerato.

# data$Entre.1º.de.primaria.y.2º.de.bachillerato.o.grado.medio...cuántos.cursos.estudiaste.en.un.colegio.público.

# data$Entre...

# data$Durante.tu.educación.primaria.y.secundaria...recibiste.alguna.de.las.siguientes.

# data$X.Practicabas.algún.deporte.de.manera.extraescolar.

# data$X.Has.hecho.selectividad.

# data$Si.hiciste.la.selectividad.en.2009.o.antes...cuál.fue.tu.nota.

# data$Si.hiciste.la.selectividade.en.2010.o.en.adelante...cuál.fue.tu.nota.

# data$Si.accediste.a.la.universidad...a.qué.titulación.

# TODO

# data$X.Accediste.a.la.universidad.el.primer.año.que.solicitaste.

# data$Si.no.accediste.a.la.primera...cuántas.veces.solicitaste.la.entrada.

# TODO

# data$X.Fue.tu.primera.opción.

# data$Si.no.fue.tu.primera.opción...cuál.fue.

