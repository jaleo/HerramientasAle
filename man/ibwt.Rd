% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HerramientasAlpha.R
\name{ibwt}
\alias{ibwt}
\title{Realiza la operación inversa transformación de Burrows-Wheeler. Recupera el texto original}
\source{
https://gist.github.com/aaronwolen/cbdd74180a714267bf0c
}
\usage{
ibwt(x, eof = "!")
}
\arguments{
\item{x}{El texto a descifrar}

\item{eof}{Caracter de contol para marcar el número de línea que puede reconstruir el texto transformado. Debe estar contenido en el texto. Por defecto, !}
}
\description{
Realiza la operación inversa transformación de Burrows-Wheeler. Recupera el texto original
}
\examples{
ibwt("TXDSEXIIXSSMP..E.!FIII") #-> "SIX.MIXED.PIXIES.SIFT"
}
\seealso{
bwt La operación contraria
}
