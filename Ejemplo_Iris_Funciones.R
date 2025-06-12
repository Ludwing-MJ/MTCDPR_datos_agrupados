# UNIVERSIDAD DE SAN CARLOS DE GUATEMALA
# FACULTAD DE AGRONOMÍA
# CENTRO DE TELEMÁTICA -CETE-
# EJEMPLO PARA ESTADISTICA DESCRIPTIVA PARA DATOS AGRUPADOS
# P. Agr. Ludwing Isaí Marroquín Jiménez

# Enlace Github Ejemplo:
# https://ludwing-mj.github.io/R-para-el-analisis-estadistico-de-datos-

# Repositorio Github Script: 
# https://github.com/Ludwing-MJ/MTCDPR_datos_agrupados.git

# Instalación y carga de paquetes necesarios
## Para manipulación y visualización de datos
if (!require(tidyverse)) install.packages("tidyverse")
## Para exportar archivos en excel
if (!require(writexl)) install.packages("writexl")
## Para importar archivos en excel
if (!require(readxl)) install.packages("readxl")

# Cargar el dataset iris
data(iris)

# Extraer la variable longitud de pétalo
longitud_petalo <- iris$Petal.Length

# Aplicar función para calcular parámetros de agrupamiento
parametros <- calcular_parametros_agrupamiento(longitud_petalo)
# Visualizar el resultado
parametros

# Construir tabla de frecuencias
tabla_freq <- construir_tabla_frecuencias(longitud_petalo, parametros)

# Mostrar tabla
tabla_freq

# Exportar la tabla de frecuencias
write_xlsx(tabla_freq, "tabla_frecuencias.xlsx")

# Calcular medidas
tendencia <- calcular_tendencia_central(tabla_freq, parametros)

# Mostrar resultados 
tendencia

# Calcular medidas de dispersión
dispersion <- calcular_dispersion(tabla_freq, parametros, tendencia$media)

# Mostrar los resultados
dispersion

# Calcular Q1 y P80
Q1 <- calcular_posicion_relativa(tabla_freq, parametros, 1, "cuartil");Q1
P80 <- calcular_posicion_relativa(tabla_freq, parametros, 80, "percentil");P80
