% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HerramientasDependientes.R
\name{convertirRutaDeRaFormatoWindows}
\alias{convertirRutaDeRaFormatoWindows}
\title{convertirRutaDeRaFormatoWindows}
\usage{
convertirRutaDeRaFormatoWindows(path = "clipboard", copiarAPortaPapeles = TRUE)
}
\arguments{
\item{path}{La ruta en formato R.}

\item{copiarAPortaPapeles}{Indica si copiar, o no, el resultado al portapapeles. Por defecto, si.}
}
\value{
Una ruta compatible con Windows.
}
\description{
Formatéa una ruta del sistema operativo Windows para que sea posible introducirla en un script de R sin necesidad e hacer cambios adicionales.
}
\details{
Esta utilidad tiene un comportamiento especial: utiliza el contenido del portapapeles, por tanto, la ruta del dirctorio debe estar en el portapapeles.
Al ejecutrar esta función, se imprime la ruta en el formato que se necesita
}
\examples{
convertirRutaDeRaFormatoWindows("G:/Unidades compartidas/EMPRESA/BBDD/JSON", FALSE)
#-> Produce: "G:\Unidades compartidas\EMPRESA\BBDD\JSON"
# Si se quiere elimina la doble \ se debe imprimir este resultado con cat()
}
\author{
Jose Alejandro Morán Pérez
}
