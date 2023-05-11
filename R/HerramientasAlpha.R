# Contiene funciones en estado alpha: no funcionan muy bien.

#' Realiza la transformación de Burrows-Wheeler
#'
#' @param x El texto a transformar
#' @param eof Carácter de control para marcar el número de línea que puede reconstruir el texto transformado. No puede estar contenido en el texto. Por defecto, !
#' @source https://gist.github.com/aaronwolen/cbdd74180a714267bf0c
#' @seealso ibwt La operación contraria
#' @examples bwt('SIX.MIXED.PIXIES.SIFT') #-> "TXDSEXIIXSSMP..E.!FIII"
#' @export
bwt <- function(x, eof = "!") {

  if (grepl(eof, "[[:cntrl:]]")) stop("eof can't be a RegEx control character")
  if (grepl(eof, x)) stop("x can't contain eof character")

  x <- paste0(x, eof)
  n <- nchar(x)

  # Take first character and add to the end
  rotate <- function(x) paste0(substring(x, 2), substring(x, 1, 1))

  # Create table of all possible rotations
  tbl <- c(x, vector("character", n - 1))
  for(i in 2:n) tbl[i] <- rotate(tbl[i - 1])

  # Sort rows alphabetically
  tbl <- sort(tbl)

  # Return last column of the table
  out <- sapply(tbl, substring, first = n, USE.NAMES = FALSE)
  paste(out, collapse = "")
}

#' Realiza la operación inversa transformación de Burrows-Wheeler. Recupera el texto original
#'
#' @param x El texto a descifrar
#' @param eof Caracter de contol para marcar el número de línea que puede reconstruir el texto transformado. Debe estar contenido en el texto. Por defecto, !
#' @source https://gist.github.com/aaronwolen/cbdd74180a714267bf0c
#' @seealso bwt La operación contraria
#' @examples ibwt("TXDSEXIIXSSMP..E.!FIII") #-> "SIX.MIXED.PIXIES.SIFT"
#' @export
ibwt <- function(x, eof = "!") {
  if (!grepl(eof, x)) stop("x doesn't contain eof character")

  tbl <- x <- strsplit(x, "")[[1]]

  while (nchar(tbl[1]) < length(x)) {
    sorted <- sort(tbl)
    tbl <- apply(cbind(x, sorted), 1, paste, collapse = "")
  }

  sub(eof, "", tbl[grepl(paste0(eof, "$"), tbl)])
}

#' Trata de comprimir un texto en una cadena lineal que mantiene toda la información y puede ser descomprimido.
#'
#' Es útil para textos repetitivos. No debería usarse con cadenas excesivamente grandes
#' El texto original no puede contener ni el carácter aglutinador, ni el separador ni el eof.
#' No se garantiza que se haga la transformación salvo que forzar sea = TRUE.
#' En caso contrario, sólo se devuelve el texto transformado si es menor que el origina.
#' @param texto_original El texto que se quiere comprimir.
#' @param aglutinador Caracter de relleno que separa los números. No debería ser un número ni estar contenido en el texto. Por defecto ·.
#' @param separador Caracter que separa las 2 partes de la cadena se salida. No debería estar contenido en el texto. Por defecto, ç.
#' @param eof Marca de posición de fila correcta para el algoritmo de transformación de Burrows-Wheeler. No debería estar contenido en el texto. Por defecto, !.
#' @param forzar Indica si se debe forzar la conversión del texto. Si es TRUE, se devolverá el resultado del algoritmo incluso aunque el texto resultante sea más largo que el original. Puede servir para encriptarlo a humanos. Por defecto, FALSE.
#' @seealso bwt
#' @seealso trataDeDescomprimir
#' @author Jose Alejandro Morán Pérez <jaleomp@gmail.com>
#' @examples
#' # example code
#' trataDeComprimir("En un lugar de la mancha", forzar = TRUE)
#' #-> "1·1·1·1·1·1·1·1·1·1·1·1·1·1·1·1·3·1·1·2·1·1çrenanalhmgn d!uc uEal "
#' @export
#'
trataDeComprimir <- function(texto_original, aglutinador = "\u00b7", separador = "\u00e7", eof = "\u0021", forzar = FALSE){
  tmp5 <- bwt(texto_original, eof)
  y <- rle(strsplit(tmp5, "")[[1]])

  y2 <- paste0(y$lengths, collapse = aglutinador)
  y3 <-paste0(y$values, collapse = "")
  tmp6 <- paste0(y2, separador, y3)

  if(!forzar){
    comparacion <- "igual"
    if(nchar(texto_original) > nchar(tmp6)){
      comparacion <- "mayor"
    }else if(nchar(texto_original) < nchar(tmp6)){
      comparacion <- "menor"
    }
    if(comparacion != "mayor") return(texto_original)
  }
  return(tmp6)
}

#' Trata de descomprimir la cadena producida por trataDeComprimir.
#'
#' Es útil para textos repetitivos. No debería usarse con cadenas excesivamente grandes. Es muy lento.
#' Los parámetros de descompresión deben ser exactamente iguales a los parámetros con que fue producida la cadena. Especialmente el carácter aglutinador, el separador y el eof.
#' Si no ha sido comprimido con trataDeComprimir, se devuelve el mismo texto.
#' @param texto_comprimido El texto que se quiere descomprimir.
#' @param aglutinador Caracter de relleno que separa los números. Por defecto ·.
#' @param separador Caracter que separa las 2 partes de la cadena se salida.  Por defecto, ç.
#' @param eof Marca de posición de fila correcta para el algoritmo de transformación de Burrows-Wheeler. Por defecto, !.
#' @seealso ibwt
#' @seealso trataDeComprimir
#' @author Jose Alejandro Morán Pérez <jaleomp@gmail.com>
#' @examples
#' # example code
#'  trataDeDescomprimir("1·1·1·1·1·1·1·1·1·1·1·1·1·1·1·1·3·1·1·2·1·1çrenanalhmgn d!uc uEal ")
#'  # -> "En un lugar de la mancha"
#' @export
#'
trataDeDescomprimir <- function(texto_comprimido, aglutinador = "\u00b7", separador = "\u00e7", eof = "\u0021"){
  if(!grepl(pattern = separador, x =  texto_comprimido )) return(texto_comprimido)
  tmp7 <- strsplit(texto_comprimido, separador)
  tmpy2 <- tmp7[[1]][1]
  tmpy3 <- tmp7[[1]][2]
  tmplengths <- as.integer(strsplit(tmpy2,aglutinador)[[1]])
  tmpvalues <- strsplit(tmpy3, "")[[1]]
  y_reconstruido <- base::rle(rep(tmpvalues, tmplengths))
  tmp5_regenerado <- paste0(inverse.rle(y_reconstruido),collapse = "")
  texto_regenerado <- ibwt(tmp5_regenerado, eof)
  return(texto_regenerado)
}

# Eliminado. Esto no ha funcionado nunca nada bien.
# #' getOutputFormat
# #'
# #' Detecta el formato de salida de script.
# #'
# #' Está pensado para elegir diferentes funciones de reperesentación de tablas en función del tipo de documento que se esté generando.
# #' @author Jose Alejandro Morán Pérez
# #' @references \url{https://stackoverflow.com/questions/35144130/in-knitr-how-can-i-test-for-if-the-output-will-be-pdf-or-word}
# #' @return Una cadena de texto indicando el formato de salida de RMarkdown o en blanco si no se está utilizando.
# getOutputFormat <- function() {
#   result <- tryCatch(
#     {
#       output <- rmarkdown:::parse_yaml_front_matter(
#         readLines(knitr::current_input())
#       )$output
#       if (is.list(output)) {
#         return(names(output)[1])
#       } else {
#         return(output[1])
#       }
#     },
#     warning = function(w) {
#       # cat("Hubo warning")#//TODO Debug
#       # warning-handler-code
#       result <- ""
#     },
#     error = function(e) {
#       if (exists("formato_de_salida")) {
#         result <- formato_de_salida
#       } else {
#         result <- ""
#       }
#       # cat("\nHubo error")#//TODO Debug
#       return(result)
#     },
#     finally = {
#       # cat("\nFinal")#//TODO Debug
#       if (is.null(result)) {
#         return("")
#       }
#       if (result != "" || result != "word_document" || result != "pdf_document" || result != "html_document") {
#         # print(paste0("El formato de salida no está contemplado: '",substr(result,1,50),"'"))
#         # otroIntento <- tryCatch(
#         #   rmarkdown::all_output_formats(knitr::current_input()
#         # )
#         # cat("\nDentro del IF")#//TODO Debug
#         # cat(paste0("Es nulo: ",is.null(result), " dimensión: ", dim(result), " structura " , str(result), " resultado: '", result, "'"))
#         # str(result)
#         formatos <- (rmarkdown::all_output_formats(knitr::current_input()))
#         # cat("\nDentro del IF 2")#//TODO Debug
#         return(formatos[1])
#         # return("word_document")
#       }
#     }
#   )
#   # cat(result)
#   if (is.null(result)) {
#     return("")
#   }
#   return(result)
# }
