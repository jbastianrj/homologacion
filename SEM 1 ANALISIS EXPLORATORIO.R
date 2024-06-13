# 1 ANALISIS EXPLORATORIO Y PRIMERAS IMPRESIONES

install.packages("tidyverse")
install.packages("readxl")
install.packages("rlang")
install.packages("stringr") #Uso de str_length
install.packages("quanteda") #Uso de dfr_trim
install.packages("wordcloud") #Uso de worldcloud

library(tidyverse)
library(readxl)
library(rlang)
library(stringr)
library(quanteda)
library(wordcloud)
library(ggplot2)


# Identificar dirección
getwd()
setwd("C:/Users/ramir/Desktop/MCE CURSOS/CONSULTORIA")

# Carga los datos SHF 
load('01_datos_shf_estado_14.RData')
shf_data <- data

# Carga el archivo de tipos de asentamiento
load("02_settlement_types.RData")
settlement_types <- settlement.types

# Carga los datos SEPOMEX
sepomex_data <- read_xls("sepomex_jalisco_23012024.xls", sheet = 2)


# Exploración inicial de los datos SHF
summary(shf_data)
str(shf_data)
head(shf_data)

#La columna de interés es settlement

#Todos son datos de caracteres salvo date_appraisal, latitude y longitude
#latitude y longitud podrian estar alterados de acuerdo a consultor
#zip_code podria no ser correcto
#state, municipality, zip_code, code_appraisal pueden cambiarse a número
#cambiar todo a mayusculas, eliminar acentos, medir la longitud del campo


# Exploración inicial de los datos SEPOMEX
summary(sepomex_data)
str(sepomex_data)
head(sepomex_data)

#La columna de interés es d_asenta esta ayuda a la homologación

#Todos son carateres, hay que cambiar las posibles a número
#c_CP me llama la atención por ser logical y tener NA
#Cambiar a número las columnas d_codigo, d_CP, c_estado, c_oficina, c_tipo_asenta 
#c_mnpio, id_asenta_cpcons, c_cve_ciudad             
#Hay que validar columna c_CP con NA
#ver relación de d_tipo_asenta [21] contra id_asenta_cpcons, o en su defecto combinarlos 
#o estandarizar juntar con settlement_types[13] que es el data siguiente
#d_CP sin datos faltantes
#id_asenta_cpcons sirve para identificar a cada registro son 637, 
# por lo que podria haber registros repetidos

# Exploración inicial de los tipos de asentamiento
summary(settlement_types)
str(settlement_types)
head(settlement_types)

# Una sola columna sobre los tipos de asentamientos, 13 tipos
# "Colonia" "Pueblo" "Barrio" "Equipamiento" entre otros 
# Manejo de los que no estén en settlement_types

# Análisis específico de la columna de nombres de colonias
nombres_colonias <- shf_data$settlement

# Verificación de valores únicos de nombres de colonias
unique_values <- unique(nombres_colonias)
cat("Valores Únicos:", length(unique_values), "\n")
#De 285,993 pasamos a 29,485  aprox un 10%

# PENDIENTE: Realizar un analisis sobre los cp

# MEJORAR: El siguiente gráfico no es de utilidad porque de la mayoría tenemos pocos registros

# Visualización de la distribución de nombres de colonias
ggplot(data = data.frame(Nombres = nombres_colonias[c(1:20)]), aes(x = Nombres)) +
  geom_bar(stat = "count", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Nombres de Colonias")

# Crear un data frame con la frecuencia de ocurrencia de cada cantidad de registros
frecuencia_registros <- table(table(nombres_colonias))
# El primer table indica frecuencia de cada uno de los registros
# mientras que el segundo indica la frecuencia de cuántas veces aparece cada frecuencia en la tabla de frecuencia original.

# Convertir a data frame para facilitar la visualización
df_frecuencia <- as.data.frame(frecuencia_registros)
names(df_frecuencia) <- c("Cantidad_Registros", "Frecuencia")

head(frecuencia_registros)
# Existen 17,387 registros únicos 
# 3 904 registros que se repiten 2 veces
# ...

# Existen registros que se repiten varias veces, llegando a tener registros que se repiten hasta 7755


tail(frecuencia_registros)


# Sumando todos los registros obtenemos los registros únicos 29, 485
# que coincide con lo obtenido de unique_values

registros_unicos <- sum(frecuencia_registros)
registros_unicos


# Mostrar el resultado

# Campos vacíos y NA

# Verificar si hay registros vacíos
registros_vacios <- sum(nchar(nombres_colonias) == 0)

# Verificar si hay registros con NA
registros_con_na <- sum(is.na(nombres_colonias))

# Mostrar los resultados
cat("Registros vacíos:", registros_vacios, "\n")
cat("Registros con NA:", registros_con_na, "\n")

# No hay vacíos ni NAs


# Análisis de longitud de nombres en caracteres
lengths <- str_length(nombres_colonias)
summary(lengths)
#Existe unas colonias de hasta 102 caracteres (son 24) y algunas de muy pocas letras

head(table(lengths))
# Hay 5 registros con un caracter, 
# 1 con 2 caracteres,
#..
tail(table(lengths))
# Hay 212 con 100 caracteres, etc

max(table(lengths))

# Encuentra la longitud más frecuente
registro <- table(lengths)[table(lengths) == 24698]

# La longitud de registro más frecuente es de 18 con 24698 registros

# Encuentra el/los registro con 102 caracteres
registro_102_caracteres <- shf_data$settlement[lengths == 102]
cat("Registro con 102 caracteres:", registro_102_caracteres, "\n")


length(registro_102_caracteres)

# Hay 24 registros con la longitud máxima de 102
# P.e "FRACCIONAMIENTO PENAL ORIENTE SEGUNDA SECCIÓN, ACTUALMENTE FRACCIONAMIENTO POTRERO ALTO SECTOR LIBER"
# Son registros que explican algun cambio en el nombre de la colina, eg. 
# "Fraccionamiento El Palomar Country Club, hoy El Cielo Country Club Condominio Vistas de las Lagunas" eg. 
# O también que naturalmente son un nombre largo, 
# Fraccionamiento Acueducto San Javier, situado a inmediaciones del poblado de San Agustin R. Casilla eg
# PENDIENTE: Faltaría validar que tan largo son los nombres de la SEPOMEX

# Encuentra una lista de registros con más caracteres
umbral_caracteres <- 100  # Puedes ajustar este umbral según tu necesidad
registros_mas_largos <- shf_data$settlement[lengths > umbral_caracteres]
cat("Registros con más de", umbral_caracteres, "caracteres:", "\n")
head(registros_mas_largos)

# Encuentra una lista de registros con menos caracteres
umbral_caracteres <- 4
registros_mas_cortos <- shf_data$settlement[lengths < umbral_caracteres]
cat("Registros con menos de", umbral_caracteres, "caracteres:", "\n")
head(registros_mas_cortos)
# Contiene abreviaciones o algunos registros que son un número o estpan llenadas con cero o punto para llenar el campo
#  "6-B" "ONU" "ONU" "9"   "DLV" "AVA" "AVA" "ORO" "0"   "0"   "UAG" "Oro" "9"   "UAG" "."   "DLV" "4X4" "14" 

# Grafico por cantidad de carateres, desde cero hasta 102

# Crear un data frame con la longitud de los nombres de colonias
df_longitudes <- data.frame(Longitud = as.numeric(names(table(lengths))), Frecuencia = as.numeric(table(lengths)))

# Crear un gráfico de barras
ggplot(df_longitudes, aes(x = Longitud, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Longitudes de Nombres de Colonias", x = "Longitud", y = "Número de Registros")


# Se observa una distribución más cercana a los registros entre 1 y 50 caracteres aprox
# El gráfico tiene una cola pesada por registros con muchos caracteres

# Ahora vamos a estudiar por tokens en vez de por registro

# Ejemplo de n-gramas para analizar la frecuencia de palabras
ngramas <- tokens(nombres_colonias, what = "word", n = 1) %>%
  tokens_ngrams(n = 1) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 1)  # Se puede ajustar el umbral para eliminar los menos frecuentes

# Visualización de n-gramas más frecuentes
top_ngramas <- topfeatures(ngramas, 50)
wordcloud(words = names(top_ngramas), freq = top_ngramas, scale = c(2, 0.5), colors = brewer.pal(8, "Dark2"))

# Ejemplo de n-gramas para analizar la frecuencia de palabras
ngramas <- tokens(nombres_colonias, what = "word", n = 1) %>%
  tokens_ngrams(n = 2) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 1)  # Se puede ajustar el umbral para eliminar los menos frecuentes

# Visualización de n-gramas más frecuentes
top_ngramas <- topfeatures(ngramas, 50)
wordcloud(words = names(top_ngramas), freq = top_ngramas, scale = c(2, 0.5), colors = brewer.pal(8, "Dark2"))

# Se observan muchas palabras que son simbolos o conectores validarlo de nuevo despues de limpieza


# Realizar mismo analisis pero para la otra columna de SEPOMEX

# sepomex_data

head(sepomex_data$d_asenta)

# Análisis específico de la columna de nombres de colonias
nombres_colonias <- sepomex_data$d_asenta


# Verificación de valores únicos de nombres de colonias
unique_values <- unique(nombres_colonias)
cat("Valores Únicos:", length(unique_values), "\n")
#De 6072 pasamos a 4667  aprox un 24% menos

# PENDIENTE: Realizar un analisis sobre los cp de SEPOMEX

# MEJORAR: El siguiente gráfico no es de utilidad porque de la mayoría tenemos pocos registros

# Visualización de la distribución de nombres de colonias
ggplot(data = data.frame(Nombres = nombres_colonias[c(1:20)]), aes(x = Nombres)) +
  geom_bar(stat = "count", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Nombres de Colonias")

# Crear un data frame con la frecuencia de ocurrencia de cada cantidad de registros
frecuencia_registros <- table(table(nombres_colonias))
# El primer table indica frecuencia de cada uno de los registros
# mientras que el segundo indica la frecuencia de cuántas veces aparece cada frecuencia en la tabla de frecuencia original.

# Convertir a data frame para facilitar la visualización
df_frecuencia <- as.data.frame(frecuencia_registros)
names(df_frecuencia) <- c("Cantidad_Registros", "Frecuencia")

head(frecuencia_registros)
# Existen 4075 registros únicos 
# 340 registros que se repiten 2 veces
# ...

# Existen registros que se repiten varias veces, llegando a tener registros que se repiten hasta 17 veces

tail(frecuencia_registros)


# Sumando todos los registros obtenemos los registros únicos 29, 485
# que coincide con lo obtenido de unique_values

registros_unicos <- sum(frecuencia_registros)
registros_unicos


# Mostrar el resultado

# Campos vacíos y NA

# Verificar si hay registros vacíos
registros_vacios <- sum(nchar(nombres_colonias) == 0)

# Verificar si hay registros con NA
registros_con_na <- sum(is.na(nombres_colonias))

# Mostrar los resultados
cat("Registros vacíos:", registros_vacios, "\n")
cat("Registros con NA:", registros_con_na, "\n")

# No hay vacíos ni NAs


# Análisis de longitud de nombres en caracteres
lengths <- str_length(nombres_colonias)
summary(lengths)
#Existe unas colonias de 3 hasta 49 caracteres y algunas de muy pocas letras

head(table(lengths))
# Hay 2 registros con 3 caracter, 
# 13 con 4 caracteres,
#..
tail(table(lengths))
# Hay 1 con 49 caracteres, etc

max(table(lengths))

# Encuentra la longitud más frecuente
registro <- table(lengths)[table(lengths) == 666]

# La longitud de registro más frecuente es de 10 con 666 registros

# Encuentra el/los registro con 102 caracteres
registro_49_caracteres <- sepomex_data$d_asenta[lengths == 49]
cat("Registro con 49 caracteres:", registro_49_caracteres, "\n")


length(registro_49_caracteres)

# Hay 1 registros con la longitud máxima de 49


# Registro con 49 caracteres: Las Víboras (Fraccionamiento Valle de las Flores) 

# Encuentra una lista de registros con más caracteres
umbral_caracteres <- 45  # Puedes ajustar este umbral según tu necesidad
registros_mas_largos <- sepomex_data$d_asenta[lengths > umbral_caracteres]
cat("Registros con más de", umbral_caracteres, "caracteres:", "\n")
head(registros_mas_largos)

# Encuentra una lista de registros con menos caracteres
umbral_caracteres <- 6
registros_mas_cortos <- sepomex_data$d_asenta[lengths < umbral_caracteres]
cat("Registros con menos de", umbral_caracteres, "caracteres:", "\n")
head(registros_mas_cortos)
# [1] "Arcos" "Deitz" "Real"  "2001"  "Sutaj" "Atlas"
# Grafico por cantidad de carateres, desde 2 hasta 49

# Crear un data frame con la longitud de los nombres de colonias
df_longitudes <- data.frame(Longitud = as.numeric(names(table(lengths))), Frecuencia = as.numeric(table(lengths)))

# Crear un gráfico de barras
ggplot(df_longitudes, aes(x = Longitud, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Longitudes de Nombres de Colonias", x = "Longitud", y = "Número de Registros")


# Se observa una distribución más cercana a los registros entre 1 y 50 caracteres aprox
# El gráfico tiene una cola pesada por registros con muchos caracteres

# Ahora vamos a estudiar por tokens en vez de por registro

# Ejemplo de n-gramas para analizar la frecuencia de palabras
ngramas <- tokens(nombres_colonias, what = "word", n = 1) %>%
  tokens_ngrams(n = 1) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 1)  # Se puede ajustar el umbral para eliminar los menos frecuentes

# Visualización de n-gramas más frecuentes
top_ngramas <- topfeatures(ngramas, 50)
wordcloud(words = names(top_ngramas), freq = top_ngramas, scale = c(2, 0.5), colors = brewer.pal(8, "Dark2"))


# Convertir a data frame para facilitar la manipulación
df_word_counts <- as.data.frame(top_ngramas)



# Graficar el resultado usando ggplot2
ggplot(df_word_counts, aes(x = row.names(df_word_counts))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Palabras más frecuentes", x = "Palabra", y = "Frecuencia") +
  theme(axis.text.y = element_text(size = 8))  # Ajustar el tamaño del texto en el eje y si es necesario


# Ejemplo de n-gramas para analizar la frecuencia de palabras
ngramas <- tokens(nombres_colonias, what = "word", n = 1) %>%
  tokens_ngrams(n = 2) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 1)  # Se puede ajustar el umbral para eliminar los menos frecuentes

# Visualización de n-gramas más frecuentes
top_ngramas <- topfeatures(ngramas, 50)
wordcloud(words = names(top_ngramas), freq = top_ngramas, scale = c(2, 0.5), colors = brewer.pal(8, "Dark2"))

# Se observan muchas palabras que son simbolos o conectores validarlo de nuevo despues de limpieza


# NUEVAS IDEAS O PENDIENTES
# Limpieza de datos para NLP
# Realizar un diccionario de datos/ de variables
# Validar la estructura de los nombres de colonias del catalogo de la SEPOMEX (analisis previo para otras columnas, colonias o CP)
# Validar si se pueden usar otras fuentes de información o validación como la SEDATU, INEGI, etc. 
# Conjunto de validación de métricas/rendimientos
# Uso de geoespacial/referencia/topologicos, etc

# IMPLEMENTACIÓN LEVENSHTEIN DISTANCE

text <- c("casa", "miedo", "luz")
text2 <- c("cosa", "caso", "casa")

dist_matrix <- adist(text2, text)
dist_matrix

closest_index <- apply(dist_matrix, 1, which.min)
closest_index

homologated_data <- data.frame(
  text = text,
  text2 = text2,
  text2new = text[closest_index],
  dist_edition = dist_matrix[cbind(seq_len(nrow(dist_matrix)), closest_index)]
)

homologated_data

# Calcular la matriz de distancias de edición
dist_matrix <- adist(unique(tolower(shf_data$settlement))[1:50],unique(tolower(sepomex_data$d_asenta))[1:40])

# Encontrar el índice de la columna más cercana para cada fila
closest_index <- apply(dist_matrix, 1, which.min)

# Crear un nuevo dataframe homologado
homologated_data <- data.frame(
  sepomex_asenta = unique(tolower(sepomex_data$d_asenta))[1:40],
  shf_settlement = unique(tolower(shf_data$settlement))[1:50],
  new_shf_settlement = tolower(sepomex_data$d_asenta)[closest_index],
  dist_edition = dist_matrix[cbind(seq_len(nrow(dist_matrix)), closest_index)]
)

homologated_data

#Datos completos, minusculas, sin limpieza NLP

# Calcular la matriz de distancias de edición
dist_matrix <- adist(unique(tolower(shf_data$settlement)),unique(tolower(sepomex_data$d_asenta)))

# Encontrar el índice de la columna más cercana para cada fila
closest_index <- apply(dist_matrix, 1, which.min)

# Crear un nuevo dataframe homologado
homologated_data2 <- data.frame(
  #sepomex_asenta = unique(tolower(sepomex_data$d_asenta))[1:2],
  shf_settlement = unique(tolower(shf_data$settlement)),
  new_shf_settlement = unique(tolower(sepomex_data$d_asenta))[closest_index],
  dist_edition = dist_matrix[cbind(seq_len(nrow(dist_matrix)), closest_index)]
)


# Crea un histograma de las distancias de edición
ggplot(homologated_data2, aes(x = dist_edition)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución de Distancias de Edición", x = "Distancia de Edición", y = "Frecuencia")

table(homologated_data2$dist_edition)
