test_that("color_transparente works", {
  expect_equal(color_transparente("#11aaff", 25), "#11AAFFBF")
})

test_that("convertirRutaDeRaFormatoWindows works", {
  expect_equal(convertirRutaDeRaFormatoWindows("G:/Unidades compartidas/EMPRESA/BBDD/JSON", FALSE), "G:\\Unidades compartidas\\EMPRESA\\BBDD\\JSON")
})
