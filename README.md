# 🚜 Análisis Multivariado datos producción de leche Departamento del Vichada
---
¡Bienvenido(a) al repositorio de análisis exploratorio de datos (EDA) del **Censo Nacional Agropecuario (CNA)** de Colombia, centrado específicamente en el departamento del **Vichada**!

Este proyecto tiene como objetivo principal identificar patrones, contrastes y la interrelación entre variables clave como el uso del suelo, la tenencia de la tierra, las características étnicas y las condiciones ambientales en las Unidades Productoras Agropecuarias (UPA) del Vichada.

---

## 🎯 Objetivos del Análisis

- **Depuración y Preparación de Datos:** Realizar un preprocesamiento riguroso para manejar datos faltantes y transformar variables.
- **Análisis Cuantitativo:** Explorar la distribución, correlación y detección de *outliers* multivariantes (Distancia de Mahalanobis) en variables como área de pastos, producción de leche y área total de la UPA.
- **Análisis de Asociación Categórica:** Evaluar la relación entre variables cualitativas como **Predominancia Étnica**, **Tenencia de la Tierra** y **Fenómenos Naturales Afectantes** mediante pruebas $\chi^2$ y medidas de asociación (V de Cramér).
- **Generación de Insights:** Producir un informe conciso que resuma los hallazgos clave para la toma de decisiones en gestión territorial.

---

## 📂 Estructura del Repositorio

| Carpeta/Archivo | Descripción |
| :--- | :--- |
| `Data/input/` | Contiene los datos originales y tablas de referencia necesarias. |
| `Data/output/` | Almacena la base de datos depurada (`datos_dep.rds`, `datos_dep.csv`). |
| `Resultados/` | Guarda todos los objetos y *outputs* generados por los scripts de R (tablas, gráficos, matrices, etc.). |
| `01_Preprocesado.R` | Script de **análisis cuantitativo**. Incluye limpieza, cálculo de distancias de Mahalanobis y resúmenes descriptivos. |
| `02_Preprocesado.R` | Script de **análisis categórico**. Incluye la creación de tablas de contingencia y el cálculo de medidas de asociación. |
| `Informe.pdf` | **Documento final** con las conclusiones y resultados detallados del análisis. **¡Lectura obligatoria!** 🧐 |
| `README.rmd` | Este archivo. |

---

## 🛠️ Requisitos Técnicos

El análisis ha sido desarrollado en el entorno de **R** y requiere las siguientes librerías:

```r
# Scripts de Preprocesado
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)

# Análisis Cuantitativo
library(MVN)
library(MASS) # Para distancias de Mahalanobis
library(ggplot2)
library(GGally)
library(patchwork)

# Análisis Categórico
library(vcd) # Para medidas de asociación
library(kableExtra)
```
---
## 📞 Contacto y Autoría
Autora: Paola Angarita 

GitHub: [paoangarita02]

Correo Electrónico: [paoangarita02@gmail.com]
