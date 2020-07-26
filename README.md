# Violencia contra las Mujeres
Script para el conversatorio, agosto 2020

## **Los seres humanos nacemos libres e iguales en dignidad y en derechos.**

Pese este principio por los estado de la "Declaración Universal de los Derechos Humanos", en muchos países como el nuestro. *México* existe una alta incidencia de violencia contra la mujer. Dada la situación de confinacmiento voluntario como medida sanitaria contra la pandemia por el virus SARS-CoV-2 desde marzo del 2020, en México ha habido un incremento de la *violencia familiar*. 

> La violencia familiar es un acto de poder u omisión intencional, dirigido a dominar,
> someter, controlar o agredir física, verbal, psicoemocional o sexualmente a cualquier
> integrante de la familia, dentro o fuera del domicilio familiar, por quien tenga o haya
> tenido algún parentesco por afinidad, civil, matrimonio, concubinato o a partir de
> una relación de hecho y que tenga por efecto causar un daño.

En este script se trabajan con los datos sobre la **Violencia contra la mujer** del 2015 al mayo del 2020, públicados el 31 de mayo del 2020 por Centro Nacional de Información. Los datos incluyen la incidencia delictiva y llamadas de emergencia al #911. 

## AYUDA
En la Coordinación del Programa sobre Asuntos de la Niñez y la Familia de la CNDH, podrán proporcionarte orientación ante un caso de violencia, llama al teléfono 56 31 00 40, extensiones 2105, 2119, 2135, 2313, 2314, 2327, 2333 y 2375.

La Comisión Nacional de los Derechos Humanos está facultada por la ley para recibir las quejas sobre presuntas violaciones a los derechos humanos cometidas por servidores públicos o autoridades administrativas federales.

Las quejas se pueden presentar en Periférico Sur 3469, colonia San Jerónimo Lídice, Delegación Magdalena Contreras, C. P. 10200, Ciudad de México
Teléfono:
56 81 81 25

Lada sin costo:
01 800 715 2000

Página electrónica:
www.cndh.org.mx

Si usted desea obtener más información,
acuda a la: PRIMERA VISITADURÍA GENERAL
Coordinación del Programa sobre Asuntos de la Niñez y la Familia
Carretera Picacho-Ajusco núm. 238,
2o. piso, colonia Jardines en la Montaña,
Delegación Tlalpan, C. P. 14210, Ciudad de México

Lada sin costo:
01 800 008 6900

Correo electrónico:
asuntosdelafamilia@cndh.org.mx


## Uso del Paquete
Para explorar estos datos, se ha creado una shiny-app que para su uso te pedimos que sigas las siguientes instrucciones de uso:

Requiere la librería `devtools()` instalada

```r
install.package("devtools")
```

Con ello se podra instalar todo el paquete con el comando:

```r
devtools::install_github("RladiesQro/ViolenciaContraMujeres")
```

Despues de la descarga se carga el paquete

```r
library(ViolenciaContraMujeres)
```

La applicación shiny se podra ejecutar usando:

```r
ejecutaShinyApp()
```
