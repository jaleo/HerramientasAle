test_that("bwt works", {
  expect_equal(bwt('SIX.MIXED.PIXIES.SIFT'), "TXDSEXIIXSSMP..E.!FIII")
})

test_that("ibwt works", {
  expect_equal(ibwt("TXDSEXIIXSSMP..E.!FIII"), "SIX.MIXED.PIXIES.SIFT")
})

test_that("trataDeComprimir works", { # Cuidado: el resultado que funciona en test no el el mismo que funciona en consola ¿Por qué?
  expect_equal(trataDeComprimir("En un lugar de", forzar = TRUE), "1·2·1·1·1·1·1·1·1·1·1·1·1·1çrne!g du uEal ")
})

test_that("trataDeDescomprimir works", { # Cuidado: el resultado que funciona en test no el el mismo que funciona en consola ¿Por qué?
  expect_equal(trataDeDescomprimir("1·2·1·1·1·1·1·1·1·1·1·1·1·1çrne!g du uEal "), "En un lugar de")
})
