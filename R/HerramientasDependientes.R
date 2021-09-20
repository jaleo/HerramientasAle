# Este archivo contiene herramientas que dependen de otras bibliotecas.

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
#' color_transparente
#'
#'  Crea un color transparente
#' @author Jose Alejandro Morán Pérez
#' @param color El nombre del color
#' @param percent Porcentaje de transparencia
#' @param name Un nombre, opcional, para el color
#' @examples
#' color_transparente("#11aaff", 25) #-> "#11AAFFBF"
#' @export
color_transparente <- function(color, percent = 50, name = NULL) {
  # 	  color = color name
  # 	percent = % transparency
  # 	   name = an optional name for the color
  ## Get RGB values for named color
  # require("grDevices")
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
    max = 255,
    alpha = (100 - percent) * 255 / 100,
    names = name
  )
  ## Save the color
  return(t.col)
}

#' crearTituloExcel
#'
#'   Crea un título formateado en una celda de una hoja de excel
#' @author Jose Alejandro Morán Pérez
#' @param hoja El objeto de hoja de excel en que poner el título
#' @param fila Coordenada de la fila de la celda en que situar el título
#' @param columna Coordenada de la columna de la celda en que situar el título
#' @param texto Texto del título. Si es nulo, no se establece el texto, sólo se cambia el formato.
#' @param estilo Estilo en que irá el título.
#' @examples
#'   \dontrun{
#'     crearTituloExcel(hoja = sheet, fila = 2, columna = 2, estilo = TITLE_STYLE)
#'     #-> ### Crea un título formateado en la celda B2
#'   }
#' @export
crearTituloExcel <- function(hoja, fila, columna, texto, estilo) {
  row <- getRows(sheet = hoja, rowIndex = fila)
  if (is.null(row) | length(row) == 0) {
    row <- createRow(sheet = hoja, rowIndex = fila)
  }
  celda <- getCells(row, colIndex = columna)
  if (is.null(celda)) celda <- createCell(row, colIndex = columna)
  if (!is.null(texto)) setCellValue(celda[[1]], texto)
  setCellStyle(celda[[1]], estilo)
}

#' modificarEstiloTituloExcel
#'
#' Modifica el estilo de las filas y columnas de una tabla. Esto es, de la primera fila y la primera columna de la tabla
#' @author Jose Alejandro Morán Pérez
#' @param hoja El objeto de hoja de excel en que poner el título. Si no es una lista, no se procesan las filas.
#' @param filas Coordenadas de las filas que se deben modificar. Si no es una lista, no se procesan las columnas.
#' @param columnas Coordenadas de las columnas que se deben modificar.
#' @param estiloFilas Estilo que se aplicará a las filas.
#' @param estiloColumnas Estilo que se aplicará a las columnas.
#' @examples
#'  \dontrun{
#'     modificarEstiloTituloExcel(sheet, 2:5, 2:8, TABLE_ROWNAMES_STYLE, TABLE_COLNAMES_STYLE)
#'     #-> ### Crea un título en la primer fila y columna de la tabla 2,2 a 5,8
#'  }
#' @export
modificarEstiloTituloExcel <- function(hoja, filas, columnas, estiloFilas, estiloColumnas) {
  if (length(filas) != 1) {
    for (x in filas) {
      # print(paste("fila",x,"columna", columnas[1]))
      crearTituloExcel(hoja, x, columnas[1], NULL, estiloFilas)
    }
  }
  if (length(columnas) != 1) {
    for (y in columnas) {
      # print(paste("fila",filas[1],"columna", y))
      crearTituloExcel(hoja, filas[1], y, NULL, estiloColumnas)
    }
  }
}


#' cualEsMiIP
#'
#' Devuelve la ip actual del ordenador.
#' @author Jose Alejandro Morán Pérez
#' @return Una cadena con la IP asignada al ordenador.
#' @examples
#' cualEsMiIP() #-> "193.146.96.2"
#' @seealso \url{https://www.ipify.org/}
#' @export
cualEsMiIP <- function() {
  ip <- rawToChar(curl::curl_fetch_memory("https://api.ipify.org?format=json")$content)
  return(jsonlite::fromJSON(ip)$ip)
}


#' guardar_tsv_utf8_con_bom
#'
#' Guarda un dataframe con fommato de archivo .tsv codificado en UTF-8 con BOM
#' @author Jose Alejandro Morán Pérez
#' @param datos El conjunto de datos a guardar.
#' @param nombre_fichero El nombre del fichero con que se guardarán los datos.
#' @return nada
#' @examples
#'  \dontrun{
#' guardar_tsv_utf8_con_bom(datos, "datos.tsv")
#' }
#' @export
guardar_tsv_utf8_con_bom <- function(datos, nombre_fichero) {
  BOM <- charToRaw("\xEF\xBB\xBF")
  con <- file(nombre_fichero, encoding = "UTF-8", "wb")
  writeBin(BOM, con, endian = "little")
  close(con)
  con <- file(nombre_fichero, encoding = "UTF-8", "a")
  rio::export(datos, file = con, format = "tsv", quote = F, na = "")
  close(con)
}


#' desconectarTodasLasBasesDeDatosMysql
#'
#' Cierra todas la conexiónes con servidores Mysql activas
#' @author Jose Alejandro Morán Pérez
#' @examples
#'  \dontrun{
#' desconectarTodasLasBasesDeDatosMysql()
#' }
#' @export
desconectarTodasLasBasesDeDatosMysql <- function() {
  conexionesActivas <- dbListConnections(RMySQL::MySQL())
  for (conexion in conexionesActivas) {
    print(paste("Desconectando la conexi\u00f3n con", dbGetInfo(conexion)$host)) # o acentuada
    dbDisconnect(conexion)
  }
}

#' extraerRutaDeWindows
#'
#' Formatéa una ruta del sistema operativo Windows para que sea posible introducirla en un script de R sin necesidad e hacer cambios adicionales.
#'
#' Esta utilidad tiene un comportamiento especial: utiliza el contendio del portapapeles, por tanto, la ruta del dirctorio debe estar en el portapapeles.
#' Al ejecutrar esta función, se imprime la ruta en el formato que se necesita
#' @author Jose Alejandro Morán Pérez
#' @param path El texto de la variable que se quiere convertir. Por defecto, la cadena almacenada en el portapapeles.
#' @param copiarAPortaPapeles Indica si copiar, o no, el resultado al portapapeles. Por defecto, si.
#' @return Una ruta compatible con R.
#' @examples extraerRutaDeWindows() #-> "G:/Unidades compartidas/EMPRESA/BBDD/JSON"
#' @export
extraerRutaDeWindows <- function(path = "clipboard", copiarAPortaPapeles = TRUE) {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Por favor, intorduce la ruta:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  if (copiarAPortaPapeles) writeClipboard(x)

  return(x)
}

#' convertirRutaDeRaFormatoWindows
#'
#' Formatéa una ruta del sistema operativo Windows para que sea posible introducirla en un script de R sin necesidad e hacer cambios adicionales.
#'
#' Esta utilidad tiene un comportamiento especial: utiliza el contenido del portapapeles, por tanto, la ruta del dirctorio debe estar en el portapapeles.
#' Al ejecutrar esta función, se imprime la ruta en el formato que se necesita
#' @author Jose Alejandro Morán Pérez
#' @param path La ruta en formato R.
#' @return Una ruta compatible con Windows.
#' @examples extraerRutaDeWindows("G:/Unidades compartidas/EMPRESA/BBDD/JSON")
#' #-> Produce: "G:\Unidades compartidas\EMPRESA\BBDD\JSON"
#' # Si se quiere elimina la doble \ se debe imporimir este resultado con cat()
convertirRutaDeRaFormatoWindows <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else if (nchar(path) > 0) {
    y <- path
  } else {
    cat("Por favor, intorduce la ruta:\n\n")
    readline()
  }
  x <- chartr("/", "\\", y)
  writeClipboard(x)
  return(x)
}
