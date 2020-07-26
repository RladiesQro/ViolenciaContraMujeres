# Violencia Intrafamiliar
Script para el conversatorio, agosto 2020

## Uso del Paquete
Requiere la librería `devtools()` instalada

```r
install.package("devtools")
```

Con ello se podra instalar todo el paquete con el comando:

```r
devtools::install_github("RladiesQro/violencia_intrafamiliar")
```

Despues de la descarga se carga el paquete

```r
library(violencia_intrafamiliar)
```

La applicación shiny se podra ejecutar usando:

```r
shiny::runApp('inst/app')
```
