# Instalación y carga de paquetes necesarios
## Para manipulación y visualización de datos
if (!require(tidyverse)) install.packages("tidyverse")
## Para exportar archivos en excel
if (!require(writexl)) install.packages("writexl")
## Para importar archivos en excel
if (!require(readxl)) install.packages("readxl")

# Importar datos ejercicio 1
ejercicio1 <- read_excel("HT.xlsx", sheet = 1)

# Importar datos ejercicio 2
ejercicio2 <- read_excel("HT.xlsx", sheet = 2)

# Importar datos ejercicio 3
ejercicio3 <- read_excel("HT.xlsx", sheet = 3)

# ejercicio 3
# Aplicar función para calcular parámetros de agrupamiento
parametros_3 <- calcular_parametros_agrupamiento(ejercicio3$Peso)
# Visualizar el resultado
parametros_3

# Construir tabla de frecuencias
tabla_freq_3 <- construir_tabla_frecuencias(ejercicio3$Peso, parametros_3)

# Mostrar tabla
tabla_freq_3

# Calcular medidas
tendencia <- calcular_tendencia_central(tabla_freq, parametros)

# Mostrar resultados 
tendencia

# Calcular medidas de dispersión
dispersion <- calcular_dispersion(tabla_freq, parametros, tendencia$media)

# Mostrar los resultados
dispersion