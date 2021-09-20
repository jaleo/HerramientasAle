# Este archivo contiene herramientas que no dependen de otras bibliotecas.



#' @author Jose Alejandro Morán Pérez
#' @title left
#' @description Extrae la parte izquierda de una cadena de caracteres.
#' @source https://www.rforexcelusers.com/how-to-mid-right-left-r/
#' @param text La cadena de caracteres.
#' @param num_char Número de caracteres.
#' @export
left <- function(text, num_char) {
  text <- as.character(text)
  substr(text, 1, num_char)
}

#' mid
#'
#' Extrae la parte central de una cadena de caracteres
#' @author Jose Alejandro Morán Pérez
#' @source https://www.rforexcelusers.com/how-to-mid-right-left-r/
#' @param text La cadena de caracteres
#' @param start_num La posición inicial de la extracción.
#' @param num_char Número de caracteres
#' @export
mid <- function(text, start_num, num_char) {
  text <- as.character(text)
  substr(text, start_num, start_num + num_char - 1)
}

#' right
#'
#' Extrae la parte derecha de una cadena de caracteres
#' @author Jose Alejandro Morán Pérez
#' @source https://www.rforexcelusers.com/how-to-mid-right-left-r/
#' @param text La cadena de caracteres.
#' @param num_char Número de caracteres.
#' @export
right <- function(text, num_char) {
  text <- as.character(text)
  substr(text, nchar(text) - (num_char - 1), nchar(text))
}


#' insertarTitulo
#'
#' Inserta un título para R Markdown del nivel que se necesite
#' @author Jose Alejandro Morán Pérez
#' @param titulo El título a inserta.
#' @param nivel El nivel que debe tener dicho título.
#' @examples insertarTitulo("Título de nivel 3", 3) #-> ### Título de nivel 3
#' @export
insertarTitulo <- function(titulo, nivel) {
  # marca_de_nivel <- paste(replicate(nivel, "#"), collapse = "")
  marca_de_nivel <- strrep("#", nivel)
  cat(marca_de_nivel, titulo, "\n")
}


#' sustituirNAs
#'
#' Sustituye los NA por otro valor en un conjunto de datos
#' @author Jose Alejandro Morán Pérez
#' @param datos Un conjunto de datos que deben imprimir. Se esperan dataframes.
#' @param sustituto El valor por el que se reemplazan los NA. Por defecto: ·.
#' @references \url{https://stackoverflow.com/questions/19516302/do-not-print-na-when-printing-data-frame}
#' @return El conjunto de datos con los NAs sustituidos.
#' @examples
#' ejemplo <- iris[1:2, 1:2]
#' ejemplo
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9         3.0
#' ejemplo[2, 2] <- NA
#' ejemplo
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9          NA
#' sustituirNAs(ejemplo, "0")
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9           0
#' @export
sustituirNAs <- function(datos, sustituto = "\u00b7") {
  df <- format(datos)
  df[is.na(datos)] <- sustituto
  return(df)
}

#' imprimirNAs
#'
#' Imprime un conjunto de datos sustituyendo los NA por otro valor
#' @author Jose Alejandro Morán Pérez
#' @param datos Un conjunto de datos que deben imprimir. Se esperan dataframes.
#' @param sustituto El valor por el que se reemplazan los NA. Por defecto: vacío.
#' @export
imprimirNAs <- function(datos, sustituto = "") {
  print(sustituirNAs(datos, sustituto = sustituto))
}

#' sustituir (Deprecado)
#'
#' Sustituye los NA por otro valor en un conjunto de datos
#'
#' Deprecado: Esta función va a ser eliminada en próximas versiones. Su nombre es demasiado genérico. Mejor usar sustituirNAs
#' @author Jose Alejandro Morán Pérez
#' @param datos Un conjunto de datos que deben imprimir. Se esperan dataframes.
#' @param sustituto El valor por el que se reemplazan los NA. Por defecto: ·.
#' @references https://stackoverflow.com/questions/19516302/do-not-print-na-when-printing-data-frame
#' @return El conjunto de datos con los NAs sustituidos.
#' @seealso sustituirNAs
#' @examples
#' \dontrun{
#' ejemplo <- iris[1:2, 1:2]
#' ejemplo
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9         3.0
#' ejemplo[2, 2] <- NA
#' ejemplo
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9          NA
#' sustituir(ejemplo, "0")
#' # Sepal.Length Sepal.Width
#' #          5.1         3.5
#' #          4.9           0
#' }
sustituir <- function(datos, sustituto = "\u00b7") {
  return(sustituirNAs(datos, sustituto))
}

#' imprimir (Deprecado)
#'
#' Imprime un conjunto de datos sustituyendo los NA por otro valor
#'
#' Deprecado: Esta función va a ser eliminada en próximas versiones. Su nombre es demasiado genérico. Mejor usar imprimirNAs
#' @author Jose Alejandro Morán Pérez
#' @param datos Un conjunto de datos que deben imprimir. Se esperan dataframes.
#' @param sustituto El valor por el que se reemplazan los NA. Por defecto: vacío.
imprimir <- function(datos, sustituto = "") {
  print(imprimirNAs(datos, sustituto = sustituto))
}

#' Not in
#'
#' Operador de contrario a \%in\%
#' @source \url{http://stackoverflow.com/questions/5831794/opposite-of-in}
#' @param x uno de los operandos.
#' @param y otro operando. Con quien se compara el primero.
#' @export
"%!in%" <- function(x, y) !("%in%"(x, y))


#' formatSegundos
#'
#' Formatea un dato de segundos para que mostrar las horas y minutos que representan.
#' @author Jose Alejandro Morán Pérez
#' @param start_time Un número de segundos.
#' @return Una cadena de texto en formato \code{h:mm:ss}.
#' @examples
#' formatSegundos(605) #| Produce:0:10:05
#' @export
formatSegundos <- function(start_time) {
  formateado <- list(rep(-1, length(start_time)))
  for (time in 1:length(start_time)) {
    diff <- start_time[time]
    hr <- diff %/% 3600
    min <- round(floor(diff - hr * 3600) / 60, digits = 0)
    sec <- round(floor(diff - min * 60 - hr * 3600), digits = 0)
    if (sec < 0) {
      min <- min - 1
      sec <- sec + 60
    }
    formateado[[1]][time] <- sprintf("%d:%02.0f:%02.0f", hr, min, sec)
  }
  return(formateado[[1]])
}


#' extraerFecha
#'
#' Extrae la fecha de una cadena que tiene fecha y hora
#' @author Jose Alejandro Morán Pérez
#' @param cadena La cadena de fecha y hora.
#' @return La subcadena de la fecha.Si el formato es incorrecto devuelve una cadena vacía y lanza un aviso.
#' @examples
#' extraerFecha("2019-10-02 23:33:14") #-> "2019-10-02"
#' @export
extraerFecha <- function(cadena) {
  resultado <- ""
  # patron <- "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  patron <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}"
  correctos <- grepl(pattern = patron, cadena)
  resultado <- left(cadena, 10)
  if (length(correctos[correctos == F]) > 0) {
    incorrectos <- cadena[!correctos]
    for (incorrecto in incorrectos) {
      warning(paste0("No se ha podido extraer la Fecha de \"", cadena, "\". El formato deber\u00eda ser AAAA-MM-DD.")) #i acentuada
    }
    resultado[!correctos] <- ""
  }
  return(resultado)
}

#' extraerHora
#'
#' Extrae la hora de una cadena que tiene fecha y hora
#' @author Jose Alejandro Morán Pérez
#' @param cadena La cadena de fecha y hora.
#' @return La subcadena de la hora.Si el formato es incorrecto devuelve una cadena vacía y lanza un aviso.
#' @examples
#' extraerHora("2019-10-02 23:33:14") #-> "23:33:14"
#' @export
extraerHora <- function(cadena) {
  resultado <- ""
  # patron <- "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  patron <- "[0-9]{2}:[0-9]{2}:[0-9]{2}$"
  correctos <- grepl(pattern = patron, cadena)
  resultado <- right(cadena, 8)
  if (length(correctos[correctos == F]) > 0) {
    incorrectos <- cadena[!correctos]
    for (incorrecto in incorrectos) {
      warning(paste0("No se ha podido extraer la Hora de \"", cadena, "\". El formato deber\u00eda ser HH:MM:SS.")) #i acentuada
    }
    resultado[!correctos] <- ""
  }
  return(resultado)
}


#' getTipo
#'
#' Devuelve el tipo de archivo (extensión) a partir de su nombre
#' @author Jose Alejandro Morán Pérez
#' @param nombreDeFichero La cadena del nombre del fichero.
#' @return La subcadena de la extensión, o "" si no encuentra el punto..
#' @examples
#' getTipo("00176a223d658759746323cc1281e93d.jpg") #-> "jpg"
#' @export
getTipo <- function(nombreDeFichero) {
  resultado <- ""
  tmp <- gregexpr(pattern = "\\.", nombreDeFichero)
  posicion <- tmp[[1]][1]
  resultado <- right(nombreDeFichero, nchar(nombreDeFichero) - posicion)
  return(resultado)
}


#' haSalida
#'
#' Envía varios textos a la salida, concatenados en una sóla linea, embebidos entre tag's P de html. Es adecuada para mostrar resultados 'asis' en documentos rmarkdown.
#' @author Jose Alejandro Morán Pérez
#' @param ... Una o varias cadena de texto.
#' @return Nada, Imprime las cadenas que se el envían
#' @examples
#' incremento <- 47
#' haSalida("Incremento: ", incremento, "Seg") #-> "<p>Incremento: 47Seg<p>"
#' @export
haSalida <- function(...) {
  cat(paste0("<p>", ..., "</p>"))
}

#' aSalida
#'
#' Envía varios textos a la salida, concatenados en una sóla linea.
#' @author Jose Alejandro Morán Pérez
#'
#' @param ... Una o varias cadena de texto.
#'
#' @return Nada, Imprime las cadenas que se el envían
#' @examples
#' incremento <- 47
#' aSalida("Incremento: ", incremento, "Seg") #-> "Incremento: 47Seg"
#' @export
aSalida <- function(...) {
  print(paste0(...))
}

#' aDebug
#'
#' Envía varios textos de debug a la salida, concatenados en una sóla linea, cuando la variable DEBUG está establecida en TRUE.
#' @author Jose Alejandro Morán Pérez
#' @param ... Una o varias cadena de texto.
#' @return Nada, Imprime las cadenas que se el envían. Si DEBUG es TRUE
#' @examples
#' incremento <- 47
#' DEBUG <- TRUE
#' aDebug("Incremento: ", incremento, "Seg") #-> "Incremento: 47Seg"
#' @export
aDebug <- function(...) {
  if (exists("DEBUG")) {
    if (DEBUG == TRUE) {
      aSalida(...)
    }
  }
}


#' cuidado
#'
#' Envía varios textos al registro de warnings, concatenados en una sóla linea.
#' @author Jose Alejandro Morán Pérez
#' @param ... Una o varias cadena de texto.
#' @return La cadena de peligro.
#' @examples
#'  \dontrun{
#' numero <- 47
#' cuidado("Peligro: ", numero, "Seg") #-> "Peligro: 47Seg"
#' # "Warning message:
#' # "In cuidado("Peligro: ", numero, "Seg") : Peligro: 47Seg
#' }
#' @export
cuidado <- function(...) {
  warning(paste0(...))
}


#' tamagno
#'
#' Calcula el tamaño de un objeto.
#' @author Jose Alejandro Morán Pérez
#' @param objeto El objeto a evaluar.
#' @return El número de filas y columnas que tiene. O el número de elementos, si es una lista.
#' @examples
#' listado <- rep("a", 256)
#' tamagno(listado) #-> 256
#' @export
tamagno <- function(objeto) {
  resultado <- NULL
  resultado <- dim(objeto)
  if (is.null(resultado)) {
    resultado <- length(objeto)
  }
  return(resultado)
}

#' asegurarExistencia
#'
#' Comprueba si un archivo exixte en el sistema operativo o no.
#'
#' Si no existe, informa de ello por consola. Si existe, informa por DEBUG.
#' @author Jose Alejandro Morán Pérez
#' @param archivo La ruta completa del archivo
#' @param nombre El nombre con el que se quiere referir al archivo, si se omite se usará la ruta completa.
#' @return TRUE o FALSE según exista, o no, el archivo.
#' @examples
#'  \dontrun{
#' asegurarExistencia(nombre_archivo_csv, "ImportanTe")
#' #-> TRUE (Debug: "OK: se ha encontrado el archivo ImportanTe")
#' }
#' @export
asegurarExistencia <- function(archivo, nombre = archivo) {
  resultado <- FALSE
  if (!file.exists(archivo)) {
    aSalida("******************************************************************")
    aSalida("No existe el archivo ", nombre)
    aSalida("******************************************************************")
  } else {
    aDebug("OK: se ha encontrado el archivo ", nombre)
    resultado <- TRUE
  }
  return(resultado)
}

#' agnadirNivel
#'
#' Añade un nuevo nivel a un factor
#' @author Jose Alejandro Morán Pérez.
#' @param factor Un objeto de tipo factor
#' @param nivel Un string que será el nombre del nuevo nivel. Tambien puede ser
#'   una concatenación de strings.
#' @return El factor con un nivel más. Si el nivel ya estaba en el factor, se
#'   ignora.
#' @examples
#' agnadirNivel(factor(c("a", "b", "c")), c("d", "a")) #<- a b c (Levels: a b c d)
#' @export
agnadirNivel <- function(factor, nivel) {
  if (is.factor(factor)) {
    return(factor(factor, levels = unique(c(levels(factor), nivel))))
  }
  return(factor)
}


#' recuento
#'
#' Cuenta el número de filas que tiene una tabla
#' @author Jose Alejandro Morán Pérez
#' @param tabla Una tabla de datos medible.
#' @return El número de registros en la tabla.
#' @examples
#' recuento(c(3, 3)) #-> 2
#' @export
recuento <- function(tabla) {
  return(tamagno(tabla)[1])
}


#' hay
#'
#' Indica si una tabla contiene datos
#' @author Jose Alejandro Morán Pérez
#' @param tabla Una tabla de datos medible.
#' @return TRUE o FALSE según haya, o no, registros en la tabla.
#' @examples
#' hay(c(3, 3)) #-> TRUE
#' @export
hay <- function(tabla) {
  if (tamagno(tabla)[1] > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' sumar_fechas
#'
#' Suma una cantidad de tiempo a una, o varias fechas. Adminte números negativos, para restar.
#' @author Jose Alejandro Morán Pérez
#' @param fechas la lista de fechas a modificar.
#' @param dias el número de días a añadir (opcional).
#' @param horas el número de horas a añadir (opcional).
#' @param minutos el número de minutos a añadir (opcional).
#' @param segundos el número de segundos a añadir (opcional).
#' @return las fechas modificadas. Si no se puede convertir a fecha, lo mismo que se envíe.
#' @examples sumar_fechas("2020-12-04 14:46:47", -4, 4) #-> "2020-11-30 18:46:47"
#' @export
sumar_fechas <- function(fechas, dias = 0, horas = 0, minutos = 0, segundos = 0) {
  tipo <- class(fechas)
  resultado <- NULL
  for (fecha in fechas) {
    nueva <- tryCatch(
      {
        tmp <- as.POSIXct(fecha)
        segundos_agnadidos <- dias * 24 * 60 * 60 + horas * 60 * 60 + minutos * 60 + segundos
        tmp <- tmp + segundos_agnadidos
      },
      error = function(cond) {
        # message(cond)
        return(fecha)
      }
    )
    resultado <- c(resultado, as.character(nueva))
  }
  if (tipo == "factor") {
    return(as.factor(resultado))
  } else { # character
    return(resultado)
  }
}

#' reemplazar.nas
#'
#' Cambia los NAs de un dataframe por espacios vacíos
#' @author Jose Alejandro Morán Pérez
#' @param data El dataframe
#' @return El dataframe sin NAs
#' @examples ejemplo <- iris[1:2, 1:2]
#' #     Sepal.Length Sepal.Width
#' #              5.1         3.5
#' #              4.9         3.0
#' ejemplo[2, 2] <- NA
#' #     Sepal.Length Sepal.Width
#' #              5.1         3.5
#' #              4.9         NA
#' reemplazar.nas(ejemplo) #->
#' #     Sepal.Length Sepal.Width
#' #              5.1         3.5
#' #              4.9
#' @export
reemplazar.nas <- function(data) {
  nombres <- names(data)

  for (n in 1:length(nombres)) {
    hayNas <- FALSE
    data[[n]] <- as.factor(data[[n]])
    for (dato in data[[n]]) {
      if (is.na(dato)) {
        hayNas <- TRUE
        break
      }
    }
    if (hayNas) {
      niveles <- levels(data[[n]])
      niveles[length(niveles) + 1] <- ""
      levels(data[[n]]) <- niveles
      data[[n]][is.na(data[[n]])] <- ""
    }
  }
  return(data)
}
