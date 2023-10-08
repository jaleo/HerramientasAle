# HerramientasAle
Herramientas de Alejandro.

Para instalarlas puedes usar este código:

``` install.packages("remotes")```

``` remotes::install_github("jaleo/HerramientasAle")```

Alternativamente, puedes utilizar este bloque de código, que hace lo mismo, pero es más eficiente:

``` if(!require(HerramientasAle)){```

```   if(!require("remotes")) install.packages("remotes")```

```   remotes::install_github("jaleo/HerramientasAle")```

```   library(HerramientasAle)```

``` }```
