
# Importar librerías ------------------------------------------------------

library(data.table)
library(readxl)
library(MVN)
library(knitr)
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)
library(GGally)
library(patchwork)
# Tablas de homologación --------------------------------------------------

tr_6 <- read_excel("Data/input/TEMATICA_DISENO DE REGISTRO CNA2014.xlsx",
                   sheet = "TABLAS_REFERENCIA",
                   range = "R2:S10") %>% 
  rename("PRED_ETNICA_C" = "Pedominancia étnica")

tr_7 <- read_excel("Data/input/TEMATICA_DISENO DE REGISTRO CNA2014.xlsx",
                   sheet = "TABLAS_REFERENCIA",
                   range = "U2:V13") %>% 
  rename("S05_TENENCIA_C" = "Predominancia de tenencia en la Unidad productora")

tr_8 <- read_excel("Data/input/TEMATICA_DISENO DE REGISTRO CNA2014.xlsx",
                   sheet = "TABLAS_REFERENCIA",
                   range = "X2:Y15") %>% 
  rename("P_S6P71_C" = "principal fenomeno que afectó los pastos sembrados")

# Construcción de dataframe -----------------------------------------------

datos <- fread("Data/input/S01_15_Unidad_productora_.csv") %>% 
  filter(TIPO_UC == 1) %>% 
  dplyr::select(TIPO_REG,
                ENCUESTA,
                COD_VEREDA,
                # Variables cualitativas
                PRED_ETNICA, # TR 6
                S05_TENENCIA, # TR 7
                P_S6P71, # TR 8
                P_S6P65, # Pastos o sabanas,
                # Variables cuantitativas
                P_S6P66, # Area de pastos
                P_S7P84F, # N. de hembras
                P_S7P85B, # Leche
                P_S12P150A, # Area total
                P_S15P158B # Personas en el predio
  ) %>% 
  na.omit() %>% 
  mutate_at(.vars = c("TIPO_REG","ENCUESTA","COD_VEREDA","PRED_ETNICA","S05_TENENCIA","P_S6P71","P_S6P65"), 
            .funs = as.character) %>% 
  left_join(tr_6, by = "PRED_ETNICA") %>% 
  left_join(tr_7, by = "S05_TENENCIA") %>% 
  left_join(tr_8, by = "P_S6P71") %>% 
  mutate(P_S6P65_c = ifelse(P_S6P65 == "1", "Si", "No"))

str(datos)


# Como supera el 5% de valores faltantes, no tiene sentido imputar
sum(is.na(datos$P_S7P84F))
sum(is.na(datos$P_S7P85B))

datos_dep <- readRDS("Data/output/datos_dep.rds")


# VARIABLES CUANTITATIVAS ----------------------------------------

# Seleccionar variables cuantitativas
vars_cuantitativas <- datos_dep %>%
  select(
    `Area pastos` = P_S6P66,
    `Numero hembras` = P_S7P84F,
    `Produccion leche` = P_S7P85B,
    `Area total` = P_S12P150A,
    `N.Personas` = P_S15P158B
  )
tab_summary_cuanti <- summary(vars_cuantitativas)
tab_summary_cuanti
# Crear vector de medias
vector_medias <- sapply(vars_cuantitativas, mean, na.rm = TRUE)

# Reemplazar nombres de variables por otros sin tildes ni caracteres especiales
nombres_limpios <- c("Area pastos", "Numero hembras", "Produccion leche", "Area total", "Numero personas")

# Crear la tabla con nombres limpios y redondear las medias
tabla_medias <- data.frame(
  Variable = nombres_limpios,
  Media = round(as.numeric(vector_medias), 2)
)

tabla_medias

#knitr::kable(vector_medias)

# Matriz de varianzas y covarianzas
matriz_cov <- cov(vars_cuantitativas, use = "complete.obs")

# Mostrar la matriz
print("Matriz de varianzas y covarianzas:")
print(matriz_cov)


# Matriz de correlaciones
matriz_cor <- round(cor(vars_cuantitativas, use = "complete.obs"),4)

# Mostrar la matriz
print("Matriz de correlaciones:")
print(matriz_cor)

# DISPERSOGRAMA 
pairs(vars_cuantitativas)

# mostrando los diagramas en la parte inferior, cambiando el color 
# de los puntos 


disp <- ggpairs(vars_cuantitativas, 
                upper = NULL,
               lower = list(continuous = wrap("points", color = "#6E8B3D")),
                diag = NULL
)
print(disp)

#GRAFICO BOXPLOT
# Crear los boxplots individualmente
p1 <- ggplot(vars_cuantitativas, aes(y = `Área pastos`)) +
  geom_boxplot(fill = "#87CEEB") +
  labs(title = "Área pastos") +
  ylab("Área (m)") + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

p2 <- ggplot(vars_cuantitativas, aes(y = `Número hembras`)) +
  geom_boxplot(fill = "#FFD700") +
  labs(title = "Número hembras") +
  ylab("Cantidad") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

p3 <- ggplot(vars_cuantitativas, aes(y = `Producción leche`)) +
  geom_boxplot(fill = "#90EE90") +
  labs(title = "Producción leche") +
  ylab("Cantidad (l)") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

p4 <- ggplot(vars_cuantitativas, aes(y = `Área total`)) +
  geom_boxplot(fill = "#FFB6C1") +
  labs(title = "Área total") +
  ylab("Área UPA (m)") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

p5 <- ggplot(vars_cuantitativas, aes(y = `N.Personas`)) +
  geom_boxplot(fill = "#D8BFD8") +
  labs(title = "Número de personas") +
  ylab("Cantidad") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))  # Centrar título

# Malla 3x2 (con una celda vacía al final)
caja <- (p1 + p2 + p3 + p4 + p5 + plot_spacer()) + 
  plot_layout(ncol = 3)

# Mostrar
caja

# MEDIDAS GLOBALES DE VARIABILIDAD

# 1. VARIANZA TOTAL. Calculando la traza de s.man
sum(diag(matriz_cov))

# 2. VARIANZA PROMEDIO. Se divide la varianza total entre el número de variables
sum(diag(matriz_cov))/5

# 3. VARIANZA GENERALIZADA. Determinante de s.man
det(matriz_cov)

# 4. DESVIACIÓN TÍPICA GENERALIZADA
sqrt(det(matriz_cov))

# 5. VARIABILIDAD PROMEDIO. Como se tiene p=5 variables, se calcula la raíz quinta
(det(matriz_cov))^(1/5)

# 6.DESVIACIÓN PROMEDIO. Se toma la raíz 2p-ésima, es decir, que para 
# este caso la raíz 10, porque es 2p = 2(5) = 10 
(det(matriz_cov))^(1/10)



# Calcular medidas globales de variabilidad
var_total <- sum(diag(matriz_cov))
var_promedio <- var_total / 5
var_generalizada <- det(matriz_cov)
desv_tip_generalizada <- sqrt(var_generalizada)
variabilidad_promedio <- var_generalizada^(1/5)
desv_promedio <- var_generalizada^(1/10)

# Crear data frame con las medidas
tabla_var_glob <- data.frame(
  Medida = c("Varianza total", "Varianza promedio", "Varianza general",
             "Desv. típica general", "Variabilidad promedio", "Desv. promedio"),
  Valor = c(var_total, var_promedio, var_generalizada,
            desv_tip_generalizada, variabilidad_promedio, desv_promedio)
)

# Redondear a 2 cifras
tabla_var_glob$Valor <- round(tabla_var_glob$Valor, 2)

print(tabla_var_glob)



# DISTANCIA DE MAHALANOBIS
dist_maha<-mahalanobis(vars_cuantitativas,vector_medias,matriz_cov) 
dist_maha

# Gráfico de las distancias de mahalanobis 
maha<-plot(dist_maha, col="blue")
maha

# Calcular distancia de Mahalanobis
dist_maha <- mahalanobis(vars_cuantitativas, vector_medias, matriz_cov)

# Crear data frame para graficar
df_maha <- data.frame(
  Observacion = 1:length(dist_maha),
  Distancia = dist_maha
)

# Gráfico estilo "plot base" con puntos
library(ggplot2)
maha <- ggplot(df_maha, aes(x = Observacion, y = Distancia)) +
  geom_point(color = "#698B22", size = 1.5) +
  geom_hline(yintercept = qchisq(0.975, df = ncol(vars_cuantitativas)), 
             linetype = "dashed", color = "black") +
  labs(title = "Distancia de Mahalanobis",
       x = "Observación", y = "Distancia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Mostrar el gráfico
maha


#COEFICIENTE DE ASIMTRIA Y CURTOSIS


# FUNCION PARA ASIMETRÍA Y KURTOSIS MULTIVARIADA ------------------------

Asi_Kur <- function(x){ 
  n <- dim(x)[1] 
  p <- dim(x)[2]
  vec.medias <- (1/n) * t(x) %*% rep(1, n)
  S <- cov(x) * (n - 1) / n 
  
  b1.ma <- matrix(NA, n, n)
  b2.ma <- rep(NA, n) 
  
  for (i in 1:n) {
    for (j in 1:n) {
      b1.ma[i, j] <- (sum(t(x[i, ] - vec.medias) %*% solve(S) %*% (x[j, ] - vec.medias)))^3 
    } 
    b2.ma[i] <- (sum(t(x[i, ] - vec.medias) %*% solve(S) %*% (x[i, ] - vec.medias)))^2 
  } 
  
  Asimetria.multi <- sum(b1.ma) / (n^2) 
  Kurtosis.multi <- sum(b2.ma) / n 
  
  list(
    "Filas" = n,
    "Columnas" = p,
    "Vector de medias" = vec.medias,
    "Matriz de covarianzas (S)" = S,
    "Asimetría multivariada" = Asimetria.multi,
    "Kurtosis multivariada" = Kurtosis.multi
  ) 
}

# DATOS CUANTITATIVOS ---------------------------------------------------

# variables están como matriz numérica
vars_cuant_mat <- as.matrix(vars_cuantitativas)

# Calcular asimetría y kurtosis multivariada
resultado_asi_kur <- Asi_Kur(vars_cuant_mat)

# Mostrar el resultado
print(resultado_asi_kur)


# Crear una tabla con los resultados
tabla_asi_kur <- data.frame(
  Medida = c("Asimetría multi.", "Curtosis multi."),
  Valor = c(
    round(resultado_asi_kur$`Asimetría multivariada`, 4),
    round(resultado_asi_kur$`Kurtosis multivariada`, 4)
  )
)

tabla_asi_kur

#COEFICIENTE DE ASIMETRIA MULTIVARIADO

# Centrar los datos
X <- scale(vars_cuantitativas, center = TRUE, scale = FALSE)  # centrado, no estandarizado

# Número de observaciones y variables
n <- nrow(X)
p <- ncol(X)

# Matriz de covarianzas
S <- cov(X)

# Inversa de la matriz de covarianza
S_inv <- solve(S)

# Cálculo del coeficiente de asimetría de Mardia
skew_mardia <- 0
for (i in 1:n) {
  for (j in 1:n) {
    diff_i <- as.numeric(X[i, ])
    diff_j <- as.numeric(X[j, ])
    skew_mardia <- skew_mardia + ((t(diff_i) %*% S_inv %*% diff_j)^3)
  }
}
skew_mardia <- skew_mardia / (n^2)

# Mostrar resultado
cat("Coeficiente de asimetría multivariante (Mardia):", skew_mardia, "\n")


# Calcular prueba de normalidad multivariada (incluye curtosis de Mardia)
mvn_result <- mvn(data = vars_cuantitativas, mvnTest = "mardia")

# Ver curtosis multivariada
print("Coeficiente de curtosis multivariante (Mardia):")
print(mvn_result$multivariateKurtosis)


# PRUEBA DE MULTINORMALIDAD 
# Primero revisión caso univariado 

# Algunos gráficos: cuantil-cuantil e histogramas 
mvn(vars_cuantitativas, univariatePlot = "qqplot")
mvn(vars_cuantitativas, univariatePlot = "histogram")


# HACIENDO LA PRUEBA PARA JUZGAR NORMALIDAD:
mvn(vars_cuantitativas,univariateTest = "Lillie")



# Ejecutar la prueba de normalidad univariada y guardar el resultado
resultado_normalidad <- mvn(vars_cuantitativas, univariateTest = "Lillie")

# Extraer solo la primera tabla (univariateNormality)
tab_norm <- resultado_normalidad$univariateNormality
tab_norm
# Para juzgar la normalidad caso univariado se observa que 
# para las variables talla y gest, el p-valor es < 0.001, entonces, 
# se rechaza la hipótesis nula de que los datos proceden de una 
# normal, mientras que el p-valor para la variable peso es de 
# 0.4728, cantidad mayor que cualquier nivel de signficancia alfa 
# entonces no se puede rechazar la hipótesis nula de que la variable 
# Prueba de Mardia (para juzgar multinormalidad) 

# Ahora la prueba de multinormalidad 
mvn(vars_cuantitativas, mvnTest = c("mardia"))
resul<-mvn(vars_cuantitativas, mvnTest = c("mardia"))
tab_mul<-resul$multivariateNormality
tab_mul
# Nota: Este es un ejemplo didáctico pues el tamaño de muestra es 
# muy pequeño, al mirar la prueba de Mardia tanto para la asimetría 
# como para la kurtosis los p-valore son muy pequeños, luego 
# se rechazan las hipótesis que la asimetría es cero y que la 
# kurtosis sea p(p+2), de manera que la muestra no se ajusta a una 
# normal trivariada. Pero quizá se llegó a esta conclusión por la 
# presencia de valores atípicos. 

# GRÁFICO CHI CUADRADO - Chi square plot 
mvn(vars_cuantitativas, multivariatePlot = "qq")

# Calcular distancias de Mahalanobis
dist_maha <- mahalanobis(vars_cuantitativas, vector_medias, matriz_cov)

# Paso 1: Distancias ordenadas
dist_maha_ord <- sort(dist_maha)

# Paso 2: Cuantiles de la Chi-cuadrado con p = 5 grados de libertad
n <- nrow(vars_cuantitativas)
p <- ncol(vars_cuantitativas)
q_chi <- qchisq((1:n - 0.5)/n, df = p)

# Paso 3: Crear el gráfico
plot(q_chi, dist_maha_ord,
     main = "Chi-Square Plot",
     xlab = paste("Cuantiles Chi-cuadrado con", p, "gl"),
     ylab = "Distancias de Mahalanobis",
     col = "#9ACD32", pch = 19)
abline(0, 1, col = "#8B4726", lwd = 2)

# Guardar el gráfico en una variable
chi_plot <- recordPlot()
chi_plot


# DETECCIÓN DE ATÍPICOS, CRITERIO DISTANCIA DE MAHALANOBIS 
# CON ESTIMADORS USUALES 

dist_maha

# Para saber si hay atípicos, usando por ejemplo alfa=0.05, hallamos 
# el percentil de la chi-cuadrado, cuya área de ese punto hacía 
# atrás sea 1-alfa, y v=p=5 grados de libertad (se tienen 5 variables) 

# como alfa=0.05, 1-alfa=0.95, así llamamos el objeto 
chi0.95<-qchisq(0.95,5) 
chi0.95

# Hacemos un plot con las distancias de mahalanobis y trazamos una 
# recta paralela al eje X con ecuación y=chi0.95, es decir, y=11.0705 

par(mfrow=c(1,1)) 
plot(dist_maha) 
abline(chi0.95,0, col="red")

# Se observa que hay varios distancias por encima de la recta de color rojo 
# es decir hay valores de la distancia mayores a 11.0705 

# Construimos una matriz de resumen para ver cuáles son esas observaciones

resumen.cuanti<-cbind(vars_cuantitativas, dist_maha)
resumen.cuanti

# RESUMEN VARIABLES CUANTITATIVAS  ----------------------------------------



# Exportamos --------------------------------------------------------------
saveRDS(tab_summary_cuanti, file = "Resultados/tab_summary_cuanti.rsd")
saveRDS(caja, file = "Resultados/caja.rds")
saveRDS(tabla_medias, file = "Resultados/tabla_medias.rsd")
saveRDS(matriz_cov, file = "Resultados/matriz_cov.rsd")
saveRDS(matriz_cor, file = "Resultados/matriz_cor.rsd")
saveRDS(disp, file = "Resultados/disp.rds")
saveRDS(tabla_var_glob, file = "Resultados/tabla_var_glob.rsd")
saveRDS(maha, file = "Resultados/maha.rds")
saveRDS(tabla_asi_kur, file = "Resultados/tabla_asi_kur.rsd")
saveRDS(tab_norm, file = "Resultados/tab_norm.rsd")
saveRDS(tab_mul, file = "Resultados/tab_mul.rsd")
saveRDS(chi_plot, file = "Resultados/chi_plot.rds")
saveRDS(datos, "Data/output/datos_dep.rds")
write.csv2(datos, "Data/output/datos_dep.csv")
