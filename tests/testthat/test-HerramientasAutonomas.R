test_that("left works", {
  expect_equal(left("IZQUIERDACENTRODERECHA",9), "IZQUIERDA")
})

test_that("mid works", {
  expect_equal(mid("IZQUIERDACENTRODERECHA",10,6), "CENTRO")
})

test_that("right works", {
  expect_equal(right("IZQUIERDACENTRODERECHA", 7), "DERECHA")
})

test_that("insertarTitulo works", {
  expect_output(insertarTitulo("Título de nivel 3", 3), "### Título de nivel 3") # Lo saca por cat, así que no produce salida.
})

test_that("sustituirNAs works", {
  expect_equal(sustituirNAs(c(25, NA, 3)), c("25", "·", "3"))
})

test_that("not in works", {
  expect_equal(c(1, 2, 3, 4, 5) %!in% c(1, 2, 3, 5, 7), c(FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("formatSegundos works", {
  expect_equal(formatSegundos(605), "0:10:05")
})

test_that("extraerFecha works", {
  expect_equal(extraerFecha("2019-10-02 23:33:14") , "2019-10-02")
})

test_that("extraerHora works", {
  expect_equal(extraerHora("2019-10-02 23:33:14"),  "23:33:14")
})

test_that("getTipo works", {
  expect_equal(getTipo("00176a223d658759746323cc1281e93d.jpg"), "jpg")
})

test_that("haSalida works", {
  expect_output(haSalida("Incremento: ", 47, "Seg"), "<p>Incremento: 47Seg</p>")
})

test_that("aSalida works", {
  expect_equal(aSalida("Incremento: ", 47, "Seg"), "Incremento: 47Seg")
})

# test_that("aDebug works", {
#   DEBUG <- TRUE
#   expect_equal(aDebug("Incremento: ", 47, "Seg"), "Incremento: 47Seg")
# })

test_that("cuidado works", {
  expect_warning(cuidado("Peligro: ", 47, "Seg"), "Peligro: 47Seg")
})

test_that("agnadirNivel works", {
  a <- agnadirNivel(factor(c("a", "b", "c")), c("d", "a"))
  expect_equal(levels(a), c("a", "b", "c", "d"))
})

test_that("recuento works", {
  expect_equal(recuento(c(3, 3)), 2)
})

test_that("hay works", {
  expect_true(hay(c(3, 3)))
})

test_that("sumar_fechas works", {
  expect_equal(sumar_fechas("2020-12-04 14:46:47", -4, 4), "2020-11-30 18:46:47")
})


# test_that("existe works", {
#   temporal3x3mpl0 <- "un objeto"
#   expect_true(existe(temporal3x3mpl0))
# })

# test_that("equal works", { # Plantilla
#   expect_equal(11111, 11111)
# })
