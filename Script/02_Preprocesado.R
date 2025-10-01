# Importar librerías ------------------------------------------------------

library(data.table)
library(readxl)
library(MVN)
library(knitr)
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)
library(vcd)
library(kableExtra)

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
  mutate(P_S6P65 = ifelse(P_S6P65 == "1", "Si", "No"))

str(datos)

datos_dep <- readRDS("Data/output/datos_dep.rds")

# VARIABLES CUALITATIVAS -----------------------------------------

# Seleccionar las variables

vars_cualitativas <- datos_dep %>%
  rename(
    ETNIA = PRED_ETNICA_C,
    TENENCIA = S05_TENENCIA_C,
    FENOMENO = P_S6P71_C,
    PASTOS_SABANAS = P_S6P65
  ) %>%
  mutate(
    ETNIA = recode(ETNIA,
                   "Territorios de Ocupación Colectiva de Comunidades Negras sin titulación" = "T. Sin titulacion",
                   "Ninguno de los anteriores" = "Ninguno",
                   "Asentamiento indígena" = "Asent. Ind.",
                   "Resguardo Indígena" = "Resg. Ind"
    ),
    
    FENOMENO = recode(FENOMENO,
                      "Sequía prolongada" = "Sequía",
                      "Inundación o exceso de lluvia" = "Inundación",
                      "Incendio" = "Incendio",
                      "Plagas o enfermedades" = "Plagas",
                      "Otro fenómeno" = "Otro",
                      "Ninguno" = "Ninguno",
                      "No fueron Afectados" = "No afectados",
                      "LLuvia a destiempo" = "Lluvia dest."
    ),
    TENENCIA = recode(TENENCIA,
                      "Adjudicatario o comunero" = "Comunero",
                      "Ocupación de hecho" = "Invacion",
                      "Otra forma de tenencia" = "Otra",
                      "Propiedad colectiva" = "Colectiva"
    )
  )







vars_cualitativas$P_S6P65
 # TABLAS DE CONTINGENCIA

# 1 PREDOMINANCIA ETNICA vs. PREDOMINANCIA TENENCIA 
# Tabla de frecuencia absoluta
tab_cruz1 <- table(vars_cualitativas$TENENCIA, vars_cualitativas$ETNIA)

# Porcentaje relativo al total
tab_prob1 <- prop.table(tab_cruz1) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb1 <- matrix(
  paste0(tab_cruz1, " (", round(tab_prob1, 1), "%)"),
  nrow = nrow(tab_cruz1),
  dimnames = dimnames(tab_cruz1)
)

# Convertir a data.frame
tb1_ET_TEN <- as.data.frame.matrix(tab_comb1)
tb1_ET_TEN

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas1 <- rowSums(tab_cruz1)
# Porcentaje por fila sobre total general
total_filas1_pct <- round(100 * total_filas1 / sum(tab_cruz1), 1)
# Concatenar en formato "n (xx%)"
tb1_ET_TEN$Total <- paste0(total_filas1, " (", total_filas1_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas1 <- colSums(tab_cruz1)
# Porcentaje por columna sobre total general
total_columnas1_pct <- round(100 * total_columnas1 / sum(tab_cruz1), 1)
# Concatenar en formato "n (xx%)"
fila_total1 <- paste0(total_columnas1, " (", total_columnas1_pct, "%)")

# Agregar total final (esquina inferior derecha)
total1_general <- sum(tab_cruz1)
fila_total1 <- c(fila_total1, paste0(total1_general, " (100%)"))

# Añadir la fila al data frame

tb1_ET_TEN <- rbind(tb1_ET_TEN, Total = fila_total1)
print(tb1_ET_TEN)
tb1_ET_TEN


# heatmap de la tabla 1

# Crear tabla de contingencia con frecuencia absoluta
tab_cruz1 <- as.data.frame(table(vars_cualitativas$TENENCIA, vars_cualitativas$ETNIA))
colnames(tab_cruz1) <- c("Tenencia", "Predominancia", "Freq")

# Graficar heatmap
grafico_1<- ggplot(tab_cruz1, aes(x = Predominancia, y = Tenencia, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(
    title = "P. Etnica VS Tenencia",
    x = "Etnia",
    y = "Tenencia"
  )


grafico_1



# 2 PREDOMINANCIA ETNICA vs. FENOMENO
# Tabla de frecuencia absoluta
tab_cruz2 <- table(vars_cualitativas$FENOMENO, vars_cualitativas$ETNIA)

# Porcentaje relativo al total
tab_prob2 <- prop.table(tab_cruz2) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb2 <- matrix(
  paste0(tab_cruz2, " (", round(tab_prob2, 1), "%)"),
  nrow = nrow(tab_cruz2),
  dimnames = dimnames(tab_cruz2)
)

# Convertir a data.frame
tb2_ET_FEN <- as.data.frame.matrix(tab_comb2)

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas2 <- rowSums(tab_cruz2)
# Porcentaje por fila sobre total general
total_filas2_pct <- round(100 * total_filas2 / sum(tab_cruz2), 1)
# Concatenar en formato "n (xx%)"
tb2_ET_FEN$Total <- paste0(total_filas2, " (", total_filas2_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas2 <- colSums(tab_cruz2)
# Porcentaje por columna sobre total general
total_columnas2_pct <- round(100 * total_columnas2 / sum(tab_cruz2), 1)
# Concatenar en formato "n (xx%)"
fila_total2 <- paste0(total_columnas2, " (", total_columnas2_pct, "%)")

# Agregar total final (esquina inferior derecha)
total2_general <- sum(tab_cruz2)
fila_total2 <- c(fila_total2, paste0(total2_general, " (100%)"))

# Añadir la fila al data frame
tb2_ET_FEN <- rbind(tb2_ET_FEN, Total = fila_total2)

tb2_ET_FEN

#heatmap de la tabla 2
# Convertir la tabla de frecuencias a data.frame
tab_cruz2_df <- as.data.frame(tab_cruz2)
colnames(tab_cruz2_df) <- c("Fenomeno", "Predominancia", "Freq")

# Graficar heatmap
library(ggplot2)

grafico_2<-ggplot(tab_cruz2_df, aes(x = Predominancia, y = Fenomeno, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "P. Etnica VS Fenomeno",
    x = "P. Etnica",
    y = "Fenomeno"
  )

grafico_2
# Crear tabla de contingencia con frecuencia absoluta
tab_cruz2 <- as.data.frame(table(vars_cualitativas$ETNIA, vars_cualitativas$FENOMENO))
colnames(tab_cruz2) <- c("P. Etnica", "Fenomeno", "Freq")

# Graficar heatmap



# 3 PREDOMINANCIA ETNICA vs. PASTOS_SABANAS
# Tabla de frecuencia absoluta
tab_cruz3 <- table(vars_cualitativas$ETNIA, datos$P_S6P65)

# Porcentaje relativo al total
tab_prob3 <- prop.table(tab_cruz3) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb3 <- matrix(
  paste0(tab_cruz3, " (", round(tab_prob3, 1), "%)"),
  nrow = nrow(tab_cruz3),
  dimnames = dimnames(tab_cruz3)
)

# Convertir a data.frame
tb3_PAS_ET <- as.data.frame.matrix(tab_comb3)

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas3 <- rowSums(tab_cruz3)
# Porcentaje por fila sobre total general
total_filas3_pct <- round(100 * total_filas3 / sum(tab_cruz3), 1)
# Concatenar en formato "n (xx%)"
tb3_PAS_ET$Total <- paste0(total_filas3, " (", total_filas3_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas3 <- colSums(tab_cruz3)
# Porcentaje por columna sobre total general
total_columnas3_pct <- round(100 * total_columnas3 / sum(tab_cruz3), 1)
# Concatenar en formato "n (xx%)"
fila_total3 <- paste0(total_columnas3, " (", total_columnas3_pct, "%)")

# Agregar total final (esquina inferior derecha)
total3_general <- sum(tab_cruz3)
fila_total3 <- c(fila_total3, paste0(total3_general, " (100%)"))

# Añadir la fila al data frame
tb3_PAS_ET <- rbind(tb3_PAS_ET, Total = fila_total3)
tb3_PAS_ET

#Heat map 3


# Convertir la tabla de frecuencias a data.frame
tab_cruz3_df <- as.data.frame(tab_cruz3)
colnames(tab_cruz3_df) <- c("Predominancia", "Pastos_Sabanas", "Freq")

# Graficar heatmap
library(ggplot2)

grafico_3 <- ggplot(tab_cruz3_df, aes(x = Pastos_Sabanas, y = Predominancia, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "P. Etnica VS  Pastos o Sabanas",
    x = "¿Posee Pastos/Sabanas?",
    y = "P. Etnica"
  )

grafico_3




# 4 PREDOMINANCIA TENENCIA vs. FENOMENO
# Tabla de frecuencia absoluta
tab_cruz4 <- table(vars_cualitativas$FENOMENO,vars_cualitativas$TENENCIA)

# Porcentaje relativo al total
tab_prob4 <- prop.table(tab_cruz4) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb4 <- matrix(
  paste0(tab_cruz4, " (", round(tab_prob4, 1), "%)"),
  nrow = nrow(tab_cruz4),
  dimnames = dimnames(tab_cruz4)
)

# Convertir a data.frame
tb4_TEN_FEN <- as.data.frame.matrix(tab_comb4)

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas4 <- rowSums(tab_cruz4)
# Porcentaje por fila sobre total general
total_filas4_pct <- round(100 * total_filas4 / sum(tab_cruz4), 1)
# Concatenar en formato "n (xx%)"
tb4_TEN_FEN$Total <- paste0(total_filas4, " (", total_filas4_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas4 <- colSums(tab_cruz4)
# Porcentaje por columna sobre total general
total_columnas4_pct <- round(100 * total_columnas4 / sum(tab_cruz4), 1)
# Concatenar en formato "n (xx%)"
fila_total4 <- paste0(total_columnas4, " (", total_columnas4_pct, "%)")

# Agregar total final (esquina inferior derecha)
total4_general <- sum(tab_cruz4)
fila_total4 <- c(fila_total4, paste0(total4_general, " (100%)"))

# Añadir la fila al data frame
tb4_TEN_FEN <- rbind(tb4_TEN_FEN, Total = fila_total4)
tb4_TEN_FEN



# Seleccionamos las primeras 5 columnas
tb4_TEN_FEN_1 <- tb4_TEN_FEN[, 1:5]

# Seleccionamos las últimas 5 columnas
tb4_TEN_FEN_2 <- tb4_TEN_FEN[, 6:ncol(tb4_TEN_FEN)]

# Ver las tablas resultantes
tb4_TEN_FEN_1
tb4_TEN_FEN_2

# Heat map 4

# Convertir la tabla de frecuencias a data.frame
tab_cruz4_df <- as.data.frame(tab_cruz4)
colnames(tab_cruz4_df) <- c("Fenomeno", "Tenencia", "Freq")

# Graficar heatmap
library(ggplot2)

grafico_4<- ggplot(tab_cruz4_df, aes(x = Tenencia, y = Fenomeno, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Tipo de Tenencia VS Fenomeno",
    x = "Tipo de Tenencia",
    y = "Fenomeno"
  )
grafico_4



# 5 PREDOMINANCIA TENENCIA vs. PASTOS/SABANAS
# Tabla de frecuencia absoluta
tab_cruz5 <- table(vars_cualitativas$TENENCIA, vars_cualitativas$PASTOS_SABANAS)

# Porcentaje relativo al total
tab_prob5 <- prop.table(tab_cruz5) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb5 <- matrix(
  paste0(tab_cruz5, " (", round(tab_prob5, 1), "%)"),
  nrow = nrow(tab_cruz5),
  dimnames = dimnames(tab_cruz5)
)

# Convertir a data.frame
tb5_PAS_TEN <- as.data.frame.matrix(tab_comb5)

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas5 <- rowSums(tab_cruz5)
# Porcentaje por fila sobre total general
total_filas5_pct <- round(100 * total_filas5 / sum(tab_cruz5), 1)
# Concatenar en formato "n (xx%)"
tb5_PAS_TEN$Total <- paste0(total_filas5, " (", total_filas5_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas5 <- colSums(tab_cruz5)
# Porcentaje por columna sobre total general
total_columnas5_pct <- round(100 * total_columnas5 / sum(tab_cruz5), 1)
# Concatenar en formato "n (xx%)"
fila_total5 <- paste0(total_columnas5, " (", total_columnas5_pct, "%)")

# Agregar total final (esquina inferior derecha)
total5_general <- sum(tab_cruz5)
fila_total5 <- c(fila_total5, paste0(total5_general, " (100%)"))

# Añadir la fila al data frame
tb5_PAS_TEN <- rbind(tb5_PAS_TEN, Total = fila_total5)
tb5_PAS_TEN

# heat map 5
# Convertir tabla de frecuencias a data.frame
tab_cruz5_df <- as.data.frame(tab_cruz5)
colnames(tab_cruz5_df) <- c("Tenencia", "Pastos_Sabanas", "Freq")

# Graficar heatmap
library(ggplot2)

grafico_5<- ggplot(tab_cruz5_df, aes(x = Pastos_Sabanas, y = Tenencia, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Tenencia VS Pastos ",
    x = "¿Posee Pastos/Sabanas?",
    y = "Tipo de Tenencia"
  )


grafico_5


length(vars_cualitativas$FENOMENO)
length(vars_cualitativas$P_S6P65)


# 6 FENONEMO vs. PASTOS/SABANAS
# Tabla de frecuencia absoluta
tab_cruz6 <- table(vars_cualitativas$FENOMENO, vars_cualitativas$PASTOS_SABANAS)

# Porcentaje relativo al total
tab_prob6 <- prop.table(tab_cruz6) * 100

# Crear tabla combinada: "Frecuencia (xx%)"
tab_comb6 <- matrix(
  paste0(tab_cruz6, " (", round(tab_prob6, 1), "%)"),
  nrow = nrow(tab_cruz6),
  dimnames = dimnames(tab_cruz6)
)

# Convertir a data.frame
tb6_FEN_PAS <- as.data.frame.matrix(tab_comb6)

# --- Agregar columna de totales ---
# Sumar por fila (frecuencia)
total_filas6 <- rowSums(tab_cruz6)
# Porcentaje por fila sobre total general
total_filas6_pct <- round(100 * total_filas6 / sum(tab_cruz6), 1)
# Concatenar en formato "n (xx%)"
tb6_FEN_PAS$Total <- paste0(total_filas6, " (", total_filas6_pct, "%)")

# --- Agregar fila de totales ---
# Sumar por columna (frecuencia)
total_columnas6 <- colSums(tab_cruz6)
# Porcentaje por columna sobre total general
total_columnas6_pct <- round(100 * total_columnas6 / sum(tab_cruz6), 1)
# Concatenar en formato "n (xx%)"
fila_total6 <- paste0(total_columnas6, " (", total_columnas6_pct, "%)")

# Agregar total final (esquina inferior derecha)
total6_general <- sum(tab_cruz6)
fila_total6 <- c(fila_total6, paste0(total6_general, " (100%)"))

# Añadir la fila al data frame
tb6_FEN_PAS <- rbind(tb6_FEN_PAS, Total = fila_total6)

tb6_FEN_PAS

#heatmap 6


# Convertir tabla de frecuencias a data.frame
tab_cruz6_df <- as.data.frame(tab_cruz6)
colnames(tab_cruz6_df) <- c("Fenomeno", "Pastos_Sabanas", "Freq")

# Graficar heatmap
library(ggplot2)

grafico_6<-ggplot(tab_cruz6_df, aes(x = Pastos_Sabanas, y = Fenomeno, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#BCEE68", high = "darkgreen") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Fenomeno VS  Pastos",
    x = "¿Posee Pastos/Sabanas?",
    y = "Fenomeno"
  )

grafico_6

# PERFIL FILA PARA PREDOMINANCIA ETNICA vs. PREDOMINANCIA TENENCIA

# Perfil fila: porcentaje por fila
perfil_fila <- prop.table(tab_cruz1, margin = 1) * 100

# Redondear y mostrar como tabla
kable(round(perfil_fila, 2), caption = "Perfil Fila: Porcentaje por fila", align = "c")


#GRAFICO
# Convertir a data.frame largo para ggplot
perfil_fila_df <- as.data.frame(perfil_fila) %>%
  rename(PRED_ETNICA_C = Var1,
         S05_TENENCIA_C = Var2,
         Porcentaje = Freq)
library(RColorBrewer)
display.brewer.pal(9, "Greens")  # Ver la paleta

# Gráfico de barras apiladas (perfil fila)
per_f<-ggplot(perfil_fila_df, aes(x = PRED_ETNICA_C, y = Porcentaje, fill = S05_TENENCIA_C)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución dentro de cada grupo de P. Tenencia",
       x = "TENENCIA",
       y = "Porcentaje",
       fill = "ETNIA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # texto del eje x
    axis.text.y = element_text(size = 8),                         # texto del eje y
    axis.title.x = element_text(size = 8),                        # título del eje x
    axis.title.y = element_text(size = 8),                        # título del eje y
    legend.title = element_text(size = 8),                        # título de la leyenda
    legend.text = element_text(size = 8),                         # texto de la leyenda
    plot.title = element_text(size = 8)                           # título del gráfico
  ) +
  scale_fill_brewer(palette = "Greens")
per_f

# PERFIL COLUMNA PARA PREDOMINANCIA ETNICA vs. PREDOMINANCIA TENENCIA

# Perfil columna: porcentaje por columna
perfil_columna <- prop.table(tab_cruz1, margin = 2) * 100

# Redondear y mostrar como tabla
kable(round(perfil_columna, 2), caption = "Perfil Columna: Porcentaje por columna", align = "c")


#GRAFICO

# Convertir a data.frame largo para ggplot
perfil_columna_df <- as.data.frame(perfil_columna) %>%
  rename(PRED_ETNICA_C = Var1,
         S05_TENENCIA_C = Var2,
         Porcentaje = Freq)

# Gráfico de barras apiladas (perfil columna)
per_c<-ggplot(perfil_columna_df, aes(x = S05_TENENCIA_C, y = Porcentaje, fill = PRED_ETNICA_C)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución dentro de cada grupo Étnico ",
       x = "ETNIA",
       y = "Porcentaje",
       fill = "TENENCIA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # texto del eje x
    axis.text.y = element_text(size = 8),                         # texto del eje y
    axis.title.x = element_text(size = 8),                        # título del eje x
    axis.title.y = element_text(size = 8),                        # título del eje y
    legend.title = element_text(size = 8),                        # título de la leyenda
    legend.text = element_text(size = 8),                         # texto de la leyenda
    plot.title = element_text(size = 8)                           # título del gráfico
  ) +
  scale_fill_brewer(palette = "Greens")
per_c




# PRUEBA DE INDEPENDENCIA
# Crear las tablas de contingencia
tab_cruz1 <- table(vars_cualitativas$TENENCIA, vars_cualitativas$ETNIA)
tab_cruz2 <- table(vars_cualitativas$FENOMENO, vars_cualitativas$ETNIA)
tab_cruz3 <- table(vars_cualitativas$ETNIA, vars_cualitativas$PASTOS_SABANAS)
tab_cruz4 <- table(vars_cualitativas$FENOMENO, vars_cualitativas$TENENCIA)
tab_cruz5 <- table(vars_cualitativas$TENENCIA, vars_cualitativas$PASTOS_SABANAS)
tab_cruz6 <- table(vars_cualitativas$FENOMENO, vars_cualitativas$PASTOS_SABANAS)

# Realizar la prueba Chi-cuadrado para cada tabla de contingencia
chi_squared_test1 <- chisq.test(tab_cruz1)
chi_squared_test2 <- chisq.test(tab_cruz2)
chi_squared_test3 <- chisq.test(tab_cruz3)
chi_squared_test4 <- chisq.test(tab_cruz4)
chi_squared_test5 <- chisq.test(tab_cruz5)
chi_squared_test6 <- chisq.test(tab_cruz6)

# Mostrar los resultados de la prueba para cada tabla
print(chi_squared_test1)
print(chi_squared_test2)
print(chi_squared_test3)
print(chi_squared_test4)
print(chi_squared_test5)
print(chi_squared_test6)



# MEDIDAS DE ASOCIACION

# resumen hip
aso_1<-assocstats(tab_cruz1)
aso_2<-assocstats(tab_cruz2)
aso_3<-assocstats(tab_cruz3)
aso_4<-assocstats(tab_cruz4)
aso_5<-assocstats(tab_cruz5)
aso_6<-assocstats(tab_cruz6)

aso_1
# RESUMEN VARIABLES CUALITATIVAS ------------------------------------------

#ANÁLISIS EXPLORATORIO

# Función para extraer los valores relevantes desde assocstats
extraer_medidas <- function(aso_obj, nombre) {
  pearson <- aso_obj$chisq_tests["Pearson", ]
  
  data.frame(
    Comparación = nombre,
    Chi_squared = pearson["X^2"],
    df = pearson["df"],
    p_value = pearson["P(> X^2)"],
    Contingency_Coeff = aso_obj$contingency,
    Cramers_V = aso_obj$cramer
  )
}

# Aplicar la función a todos los objetos
resumen_aso <- rbind(
  extraer_medidas(aso_1, "TENENCIA vs ETNIA"),
  extraer_medidas(aso_2, "FENOMENO vs ETNIA"),
  extraer_medidas(aso_3, "ETNIA vs PASTOS_SABANAS"),
  extraer_medidas(aso_4, "FENOMENO vs TENENCIA"),
  extraer_medidas(aso_5, "TENENCIA vs PASTOS_SABANAS"),
  extraer_medidas(aso_6, "FENOMENO vs PASTOS_SABANAS")
)

# Ver resultado
print(resumen_aso)









# Función para extraer medidas y redondear
extraer_medidas <- function(aso_obj, nombre) {
  pearson <- aso_obj$chisq_tests["Pearson", ]
  
  data.frame(
    Comparación = nombre,
    Chi_squared = round(as.numeric(pearson["X^2"]), 4),
    df = pearson["df"],
    p_value = round(as.numeric(pearson["P(> X^2)"]), 4),
    Contingency_Coeff = round(aso_obj$contingency, 4),
    Cramers_V = round(aso_obj$cramer, 4)
  )
}

# Aplicar la función a todos los resultados
resumen_aso <- rbind(
  extraer_medidas(aso_1, "TENENCIA vs ETNIA"),
  extraer_medidas(aso_2, "FENOMENO vs ETNIA"),
  extraer_medidas(aso_3, "ETNIA vs PASTOS_SABANAS"),
  extraer_medidas(aso_4, "FENOMENO vs TENENCIA"),
  extraer_medidas(aso_5, "TENENCIA vs PASTOS_SABANAS"),
  extraer_medidas(aso_6, "FENOMENO vs PASTOS_SABANAS")
)

# Resetear los nombres de fila para evitar "X^2", etc.
rownames(resumen_aso) <- NULL

# Ver resultado
print(resumen_aso)

# Guardar a CSV si lo necesitas
write.csv(resumen_aso, "resumen_asociaciones.csv", row.names = FALSE)

# Exportar a CSV si lo deseas
write.csv(resumen_aso, "resumen_asociaciones.csv", row.names = FALSE)


# Exportamos --------------------------------------------------------------

saveRDS(datos, "Data/output/datos_dep.rds")

saveRDS(tb1_ET_TEN, "Resultados/tb1_ET_TEN.rds")
saveRDS(grafico_1, "Resultados/grafico_1.rds")

saveRDS(grafico_2, "Resultados/grafico_2.rds")
write.csv2(datos, "Data/output/datos_dep.csv")
saveRDS(tb2_ET_FEN, "Resultados/tb2_ET_FEN.rds")
saveRDS(tb3_PAS_ET, "Resultados/tb3_PAS_ET.rds")
saveRDS(grafico_3, "Resultados/grafico_3.rds")
saveRDS(tb4_TEN_FEN_1, "Resultados/tb4_TEN_FEN_1.rds")
saveRDS(tb4_TEN_FEN_2, "Resultados/tb4_TEN_FEN_2.rds")
saveRDS(grafico_4, "Resultados/grafico_4.rds")
saveRDS(tb5_PAS_TEN, "Resultados/tb5_PAS_TEN.rds")
saveRDS(grafico_5, "Resultados/grafico_5.rds")
saveRDS(tb6_FEN_PAS, "Resultados/tb6_FEN_PAS.rds")
saveRDS(grafico_6, "Resultados/grafico_6.rds")
saveRDS(per_c, "Resultados/per_c.rds")
saveRDS(per_f, "Resultados/per_f.rds")
saveRDS(resumen_aso, "Resultados/resumen_aso.rds")
