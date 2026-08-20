Análisis Multivariante de la Conducta de la Ballena Jorobada (Trabajo en equipo)

Análisis de datos cinemáticos procedentes de sensores bio-loggers (DTAGs) para estandarizar y clasificar el comportamiento de la ballena jorobada (Megaptera novaeangliae) en la Península Antártica.

Los sensores remotos generan una cantidad masiva de datos de alta dimensionalidad (frecuencias de 5 Hz durante 24 horas). El objetivo de este proyecto es reducir esa "tormenta de datos" y aplicar algoritmos de Machine Learning para predecir estados biológicos reales (alimentación, descanso, viaje, exploración), descubriendo además qué hace el animal en los periodos sin etiquetar ("Unknown").

Stack Técnico y Metodología
* Lenguaje: R
* Preprocesamiento: Tidyverse (agrupación temporal a 1 Hz, creación de variables de varianza extrema como `Speed_max` y `Activity_vigor`).
* Análisis No Supervisado:
  * PCA (Análisis de Componentes Principales) para reducción de dimensionalidad.
  * Análisis Factorial.
  * K-Means Clustering para identificar agrupaciones naturales y perfilar comportamientos.
* Análisis Supervisado (Clasificación):
  * LDA (Análisis Discriminante Lineal) para validación cruzada y re-clasificación.
  * SVM (Support Vector Machines) evaluando Kernel Lineal vs. Radial.
* Visualización: `ggplot2`, `ggiraph` (gráficos interactivos) y `Shiny` (Dashboard web).

Resultados Clave
* Reducción de dimensionalidad: El modelo PCA logró resumir el 65% de la varianza total en dos ejes naturales biológicos: "Intensidad Energética" y "Perfil de Inmersión".
* Mejora predictiva no lineal: El modelo SVM con Kernel Radial alcanzó una precisión del 78.15%, superando significativamente a los modelos lineales al capturar la complejidad real de las embestidas alimenticias y los buceos profundos.
* Resolución de incertidumbre: A través de la re-clasificación mediante LDA, se demostró estadísticamente que el 88.8% de los datos "Unknown" correspondían a estados de superficie, y se logró rescatar un 10.5% de eventos de alimentación que estaban ocultos.

Equipo del Proyecto

Este proyecto ha sido ideado y desarrollado de forma colaborativa por un equipo de 3 estudiantes de Ciencia de Datos:
Martina Barbarin Urdanoz
Marta De Miguel Mendoza
Marta Goñi Vallez
