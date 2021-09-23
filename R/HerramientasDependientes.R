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
#' @importFrom grDevices col2rgb rgb
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
  t.col <- rgb(
    red = rgb.val[1],
    green = rgb.val[2],
    blue = rgb.val[3],
    maxColorValue	= 255,
    alpha = (100 - percent) * 255 / 100,
    names = name
  )
  ## Save the color
  return(t.col)
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
#' @importFrom DBI dbListConnections dbGetInfo dbDisconnect
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
#' Formatéa una ruta del sistema operativo Windows para que sea posible
#' introducirla en un script de R sin necesidad e hacer cambios adicionales.
#'
#' Esta utilidad tiene un comportamiento especial: utiliza el contendio del
#' portapapeles, por tanto, la ruta del dirctorio debe estar en el portapapeles.
#' Al ejecutrar esta función, se imprime la ruta en el formato que se necesita
#' @author Jose Alejandro Morán Pérez
#' @param path El texto de la variable que se quiere convertir. Por defecto, la
#'   cadena almacenada en el portapapeles.
#' @param copiarAPortaPapeles Indica si copiar, o no, el resultado al
#'   portapapeles. Por defecto, si.
#' @return Una ruta compatible con R.
#' @examples
#' \dontrun{
#' clipr::write_clip("G:\u005cUnidades compartidas\u005cEMPRESA\u005cBBDD\u005cJSON",
#' allow_non_interactive = TRUE)
#' # Los caracteres unicode son barras invertidas. R es así. Es un caracter reservado
#' clipr::read_clip()
#' # Esto saca por pantalla la ruta escapada. Pero en el portapapeles se
#' # almacena bien, con una sóla barra inclinada. R no puede impirmir esto
#' # porque lo interpreta como escape de códigos unicode. (R va a tener que
#' # mejorar este comportamiento, pero mientra y no ...)
#' extraerRutaDeWindows(copiarAPortaPapeles = FALSE)
#' #-> "G:/Unidades compartidas/EMPRESA/BBDD/JSON"
#' }
#' @export
extraerRutaDeWindows <- function(path = "clipboard", copiarAPortaPapeles = TRUE) {
  y <- if (path == "clipboard") {
    # readClipboard()
    clipr::read_clip()
  } else {
    cat("Por favor, intorduce la ruta:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  # if (copiarAPortaPapeles) writeClipboard(x)
  if (copiarAPortaPapeles) clipr::write_clip(x, allow_non_interactive = TRUE)

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
#' @param copiarAPortaPapeles Indica si copiar, o no, el resultado al portapapeles. Por defecto, si.
#' @return Una ruta compatible con Windows.
#' @examples convertirRutaDeRaFormatoWindows("G:/Unidades compartidas/EMPRESA/BBDD/JSON", FALSE)
#' #-> Produce: "G:\Unidades compartidas\EMPRESA\BBDD\JSON"
#' # Si se quiere elimina la doble \ se debe imporimir este resultado con cat()
#' @export
convertirRutaDeRaFormatoWindows <- function(path = "clipboard", copiarAPortaPapeles = TRUE) {
  #REM #' @importFrom utils readClipboard writeClipboard
  y <- if (path == "clipboard") {
    # readClipboard()
    clipr::read_clip()
  } else if (nchar(path) > 0) {
    y <- path
  } else {
    cat("Por favor, intorduce la ruta:\n\n")
    readline()
  }
  x <- chartr("/", "\\", y)
  # writeClipboard(x)
  if (copiarAPortaPapeles) clipr::write_clip(x, allow_non_interactive = TRUE)
  return(x)
}
