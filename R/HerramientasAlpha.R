# Contien funciones en estado alpha: no funcionan muy bien.

#' limpiarDatosDeHoja
#'
#' Elimina las filas vacías de los datos extraídos de una hoja de excel.
#'
#' Sustituye los puntos por espacios en el título.
#' Si los datos no los tienen, añade, al menos una fila y 2 columnas, para que se puedan dibujar como tablas
#' Está pensado para eliminar filas inútiles de las hojas de datos complejas.
#' @author Jose Alejandro Morán Pérez <jaleomp@gmail.com>
#' @param datos Los datos a limpiar.
#' @param titulos Los títulos a añadir al resultado. Si no se incluyen, se extraen de los datos
#' @return un dataframe con los datos límpios, el título sin puntos y con, al menos, una fila y 2 columnas.
#' @export
limpiarDatosDeHoja <- function(datos, titulos = NULL) {
  if (dim(as.matrix(datos))[2] == 1) { # Sólo tiene una columna
    datos <- as.data.frame(datos[!is.na(datos)])
    # modifico datos para poder imprimirlo bien.
    datos <- cbind(datos, datos)
    titulos <- cbind(titulos, "")
    if (dim(datos)[1] != 0) {
      datos[, 2] <- NA
    } else { # Cargo datos
      datos[1, ] <- c("-", "")
    }
    colnames(datos) <- c(titulos)
  } else { # Tiene más de una columna
    datos <- as.data.frame(datos[!is.na(datos[, 1]), ])
    if (dim(datos)[1] == 0) { # Cargo datos
      datos[1, ] <- ""
      datos[1, 1] <- "-"
    }
  }
  if (is.null(titulos)) {
    titulos <- colnames(datos)
  }
  titulos <- gsub("\\.", " ", titulos) # Limpieza
  names(datos) <- titulos
  return(datos)
}


#' getOutputFormat
#'
#' Detecta el formato de salida de script.
#'
#' Está pensado para elegir diferentes funciones de reperesentación de tablas en función del tipo de documento que se esté generando.
#' @author Jose Alejandro Morán Pérez
#' @references \url{https://stackoverflow.com/questions/35144130/in-knitr-how-can-i-test-for-if-the-output-will-be-pdf-or-word}
#' @return Una cadena de texto indicando el formato de salida de RMarkdown o en blanco si no se está utilizando.
getOutputFormat <- function() {
  result <- tryCatch(
    {
      output <- rmarkdown:::parse_yaml_front_matter(
        readLines(knitr::current_input())
      )$output
      if (is.list(output)) {
        return(names(output)[1])
      } else {
        return(output[1])
      }
    },
    warning = function(w) {
      # cat("Hubo warning")#//TODO Debug
      # warning-handler-code
    },
    error = function(e) {
      if (exists("formato_de_salida")) {
        result <- formato_de_salida
      } else {
        result <- ""
      }
      # cat("\nHubo error")#//TODO Debug
      return(result)
    },
    finally = {
      # cat("\nFinal")#//TODO Debug
      if (is.null(result)) {
        return("")
      }
      if (result != "" || result != "word_document" || result != "pdf_document" || result != "html_document") {
        # print(paste0("El formato de salida no está contemplado: '",substr(result,1,50),"'"))
        # otroIntento <- tryCatch(
        #   rmarkdown::all_output_formats(knitr::current_input()
        # )
        # cat("\nDentro del IF")#//TODO Debug
        # cat(paste0("Es nulo: ",is.null(result), " dimensión: ", dim(result), " structura " , str(result), " resultado: '", result, "'"))
        # str(result)
        formatos <- (rmarkdown::all_output_formats(knitr::current_input()))
        # cat("\nDentro del IF 2")#//TODO Debug
        return(formatos[1])
        # return("word_document")
      }
    }
  )
  # cat(result)
  if (is.null(result)) {
    return("")
  }
  return(result)
}
