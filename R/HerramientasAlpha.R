# Contiene funciones en estado alpha: no funcionan muy bien.


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
