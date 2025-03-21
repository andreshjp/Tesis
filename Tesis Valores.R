# Paquetes necesarios:
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tibble)
library(purrr)
library(countrycode)   
library(lmtest)
library(plm)
library(car)

# Datos de la CEPAL
# Importar xlsx a RStudio
CEPAL <- read_excel("Datos CEPAL.xlsx")

# Mantener solo las comlumnas que serán utilizadas
CEPAL <- CEPAL[, c("País__ESTANDAR", "Gasto público por función", "Años__ESTANDAR", "value")]

# Renombramos las columnas
CEPAL <- CEPAL %>% 
  rename(
    país = País__ESTANDAR,
    función = 'Gasto público por función',
    año = Años__ESTANDAR,
    valor = value)

# Vistazo general
glimpse(CEPAL)

# Cambiamos el formato de las variable 
CEPAL <- CEPAL %>% 
  mutate(across(c(año, valor), as.numeric))

# Transformamos esto a un formato de datos de panel
# Primero debenos completar los años faltantes para algunos países
# Crear un vector con todos los años
años <- min(CEPAL$año):max(CEPAL$año)

# Completar los años faltantes y luego usar pivot_wider 
CEPAL_panel <- CEPAL %>%
  complete(país, función, año = años) %>%
  pivot_wider(names_from = año, values_from = valor)



# Datos del FMI
# Importar xlsx a RStudio
FMI <- read_excel("Datos FMI.xlsx")

# Convertimos los. 0 en N/A
FMI[FMI == 0] <- NA

# Eliminar columnas que no serán utilizadas
FMI <- FMI %>% 
  select(-c('Indicator ID','Attribute 2', 'Attribute 3',Partner, 'Economy ISO3'))

# Renombramos las columnas
FMI <- FMI %>% 
  rename(
    país_inglés = 'Economy Name',
    función = Indicator,
    'sección gobierno' = 'Attribute 1')

# Vistazo general
glimpse(FMI)
head(FMI,66) %>% 
  print(n=66)

# Ahora debemos filtrar para solo manetener aquellas funciones del gasto a estudiar
FMI <- FMI %>% 
  filter(función %in% c("Expenditure on economic affairs, Domestic currency",
               "Expenditure on defense, Domestic currency",
               "Expenditure on education, Domestic currency",
               "Expenditure on general public services, Domestic currency",
               "Expenditure on housing & community amenities, Domestic currency",
               "Expenditure on health, Domestic currency",
               "Expenditure on environment protection, Domestic currency",
               "Expenditure on public order & safety, Domestic currency",
               "Expenditure on recreation, culture, & religion, Domestic currency",
               "Expenditure on social protection, Domestic currency",
               "Expenditure, Domestic currency"))

# Ahora debemos unificar el gasto de los distitnos tipo de gobierno para aquellos cuyos datos sobre el gobierno general no están disponibles
# Definir las secciones de gobierno a sumar cuando general governmet no esté diponible
secciones_a_sumar <- c("Budgetary central government", 
                       "Central government (incl. social security funds)", 
                       "Extrabudgetary central government", 
                       "Social security funds", 
                       "State governments")

# Filtrar y sumar
FMI_consolidado <- FMI %>%
  group_by(país_inglés, función) %>%
  summarise(across(`1972`:`2022`, ~ {
    if ("General government" %in% `sección gobierno` && !is.na(.x[`sección gobierno` == "General government"])) {
      .x[`sección gobierno` == "General government"]
    } else {
      sum(.x[`sección gobierno` %in% secciones_a_sumar], na.rm = TRUE)
    }
  })) %>%
  ungroup()

FMI_final <- FMI_consolidado %>%
  pivot_longer(cols = `1972`:`2022`, names_to = "Año", values_to = "Gasto") %>%
  pivot_wider(names_from = función, values_from = Gasto)

# Expandir al formato de panel
FMI_panel <- FMI_consolidado %>%
  mutate(`sección gobierno` = "General government") %>%  # Agregar la columna de sección de gobierno
  pivot_longer(cols = `1972`:`2022`, names_to = "Año", values_to = "Gasto") %>%  # Convertir años en filas
  pivot_wider(names_from = Año, values_from = Gasto)  # Volver a formato wide (años como columnas)

# Cambiamos los nombres de las funciones para que coincidan con las demás bases
FMI_panel <- FMI_panel %>%
  mutate(función = recode(función,
                          "Expenditure on defense, Domestic currency" = "Defensa",
                          "Expenditure on economic affairs, Domestic currency" = "Asuntos económicos",
                          "Expenditure on education, Domestic currency" = "Educación",
                          "Expenditure on environment protection, Domestic currency" = "Protección del medio ambiente",
                          "Expenditure on general public services, Domestic currency" = "Servicios públicos generales",
                          "Expenditure on health, Domestic currency" = "Salud",
                          "Expenditure on housing & community amenities, Domestic currency" = "Vivienda y servicios comunitarios",
                          "Expenditure on public order & safety, Domestic currency" = "Orden público y seguridad",
                          "Expenditure on recreation, culture, & religion, Domestic currency" = "Actividades recreativas, cultura y religión",
                          "Expenditure on social protection, Domestic currency" = "Protección social",
                          "Expenditure, Domestic currency" = "Erogaciones totales"))

# Eliminamos la columna de sección de gobierno puesto que no será necesaria
FMI_panel <- FMI_panel[, -3]

# Traducimos los países a Español 
FMI_panel$país <- countrycode(FMI_panel$país_inglés, origin = "country.name", destination = "cldr.short.es")

# Reorganizar las columnas para que país quede en la segunda posición
FMI_panel <- FMI_panel %>%
  select(país, everything())

# Eliminamos la columna de países en inglés puesto que no será necesaria
FMI_panel <- FMI_panel[, -2]




# Datos del Eurostat
# Cómo esta viene en varias hojas de excel, se creará una función para procesar cada hoja y seguir el mismo procedimiento en cada una
funcion_Eurostat <- function(hojas, funciones) {
  # Importar xlsx a RStudio
  Eurostat_Total <- read_excel("Datos Eurostat.xlsx", sheet = hojas)
  
  # Eliminamos el encabezado y otras filas que no serán utilizadas 
  Eurostat_Total <- Eurostat_Total %>%
    slice(-c(1:9, 11:14, 45:n()))
  
  # Mantenemos solo las columnas que no están vacías y que no tienen valores "p" y "b"
  Eurostat_Total <- Eurostat_Total %>%
    select_if(function(x) {
      !all(is.na(x)) &&
        !(any(x %in% c("p","b"), na.rm = TRUE) && sum(is.na(x)) == (length(x) - sum(x %in% c("p","b"), na.rm = TRUE)))
    })
  
  # Colocamos la primera fila como los nombres de las columnas y la eliminamos 
  # Verificamos que la primera fila no contenga NA antes de usarla como nombres
  nombres_columnas <- as.character(Eurostat_Total[1, ])
  nombres_columnas[is.na(nombres_columnas)] <- "Columna_Sin_Nombre"  # Reemplazar NA con un nombre válido
  Eurostat_Total <- Eurostat_Total[-1, ]
  colnames(Eurostat_Total) <- nombres_columnas
  
  # Renombramos las columnas 
  Eurostat_Total <- Eurostat_Total %>% 
    rename(país_inglés = TIME) 
  
  # Añadimos la función respectiva
  Eurostat_Total <- Eurostat_Total %>%
    add_column(función = funciones, .after = 1)
  
  return(Eurostat_Total)
}

# Creamos una lista con los nombres de las hojas de excel a procesar y las funciones respectivas a cada hoja
hojas <- c("Sheet 1", "Sheet 2", "Sheet 11", "Sheet 17", "Sheet 24", "Sheet 34", "Sheet 41", "Sheet 48", "Sheet 55", "Sheet 62", "Sheet 71")
funciones <- c("Erogaciones totales", "Servicios públicos generales", "Defensa", "Orden público y seguridad", "Asuntos económicos", "Protección del medio ambiente", "Vivienda y servicios comunitarios", "Salud", "Actividades recreativas, cultura y religión", "Educación", "Protección social")

# Iteramos sobre los vectores hechos anteriormente
Eurostat_Completo <- map2(hojas, funciones, funcion_Eurostat)              

# Finalmente, combinamos los dataframes resultantes en uno solo
Eurostat <- bind_rows(Eurostat_Completo)

# Vistazo general
glimpse(Eurostat)

# Traducimos los países a Español 
Eurostat$país <- countrycode(Eurostat$país_inglés, origin = "country.name", destination = "cldr.short.es")

# Reorganizar las columnas para que país quede en la segunda posición
Eurostat <- Eurostat %>%
  select(país, everything())

# Eliminamos la columna de países en inglés puesto que no será necesaria
Eurostat <- Eurostat[, -2]


# Ahora se unificará la información de las anteriores bases de datos 
# Primero debemos filtar de la base del FMI los países que hay en las otras bases
# Obtener los países únicos de CEPAL y Eurostat
paises_cepal <- unique(CEPAL_panel$país)

paises_eurostat <- unique(Eurostat$país)

# Combinar los países únicos de ambas bases de datos
paises_filtrar <- unique(c(paises_cepal, paises_eurostat))

# Filtrar la base de datos del FMI
FMI_panel <- FMI_panel %>%
  anti_join(tibble(país = paises_filtrar), by = "país")

# Primero unificamos las de la CEPAL, FMI y Eurostat
# Antes de esto colocamos las columnas del mismo formato
CEPAL_panel <- CEPAL_panel %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

FMI_panel <- FMI_panel %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

Eurostat <- Eurostat %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

# Unificamos estas tres bases
Datos <- bind_rows(Eurostat, CEPAL_panel, FMI_panel)

# Eliminamos los años que no aplican para el análsis
Datos <- Datos[, -(37:54)]



# Ya que tenemos el gasto, ahora deberemos incluir el crecimeinto económico y, con este, otras variables explicativas para el modelo
# Específicamente la inversión, el consumo final de los hogares, las exportaciones y las importaciones.
# Importamos los csv saltando las filas innecesarias
PIB <- read_csv("Datos PIB.csv", skip = 3)
consumo <- read_csv("Datos Consumo.csv", skip = 3)
inversion <- read_csv("Datos FBKF.csv", skip = 3)
exportaciones <- read_csv("Datos Exportaciones.csv", skip = 3)
importaciones <- read_csv("Datos Importaciones.csv", skip = 3)

# Eliminamos las columnas que no serán utilizadas
PIB <- PIB[, -(2:4)]
consumo <- consumo[, -(2:4)]
inversion <- inversion[, -(2:4)]
exportaciones <- exportaciones[, -(2:4)]
importaciones <- importaciones[, -(2:4)]

# Agregamos la columna función que para cada caso y colocamos el nombre para que coincida con las otras bases de datos
PIB <- PIB %>% 
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>% 
  rename(país = 'Country Name') %>% 
  mutate(función = "PIB") %>% 
  relocate(función, .after = 1)
  
consumo <- consumo %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(función = "Consumo final de los hogares") %>% 
  relocate(función, .after = 1)

inversion <- inversion %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(función = "Formación bruta de capital fijo") %>% 
  relocate(función, .after = 1)

exportaciones <- exportaciones %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(función = "Exportaciones") %>% 
  relocate(función, .after = 1)

importaciones <- importaciones %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(función = "Importaciones") %>% 
  relocate(función, .after = 1)

# Debemos filtar los países para los cuales seleccionaremos el crecimiento económico
# Obtener los países únicos en Datos
paises_en_datos <- unique(Datos$país)

# Filtrar para incluir solo los países que están en Datos
PIB_filtrado <- PIB %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

consumo_filtrado <- consumo %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

inversion_filtrado <- inversion %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

exportaciones_filtrado <- exportaciones %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

importaciones_filtrado <- importaciones %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

# Unificar Datos con las nuevas variables
Datos_completos <- bind_rows(Datos, PIB_filtrado, consumo_filtrado, inversion_filtrado, exportaciones_filtrado, importaciones_filtrado)

# Eliminar columnas innecesarios
Datos_completos <- Datos_completos[, -(37:67)]

# Exportar la base de datos final
write_csv(Datos_completos, file = "Datos Completos.csv")

# Transformar a formato largo
Datos_long <- Datos_completos %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "año", 
               values_to = "valor") %>%
  mutate(año = as.numeric(año))

# Verificamos datos faltantes
colSums(is.na(Datos_long))

# Eliminamos datos faltantes ya que son una fracción del total
Datos_long <- Datos_long %>%
  filter(!is.na(país))

Datos_long <- Datos_long %>%
  filter(!is.na(valor))

# Separar el PIB y las variables explicativas
pib <- Datos_long %>%
  filter(función == "PIB") %>%
  select(país, año, valor) %>%
  rename(pib = valor)

variables_explicativas <- Datos_long %>%
  filter(función != "PIB") %>%
  pivot_wider(names_from = función, values_from = valor)

# Unir las dos bases de datos
datos_regresion <- pib %>%
  left_join(variables_explicativas, by = c("país", "año"))

# Incluir el comercio neto 
datos_regresion <- datos_regresion %>%
  mutate(Comercio = Exportaciones - Importaciones)

# Eliminar filas con valores faltantes en la variable dependiente
datos_regresion <- datos_regresion %>%
  filter(!is.na(pib))

# Exportar la base de datos final para la regresión
write_csv(datos_regresion, file = "Datos Regresión.csv")

# Convertir a un objeto de panel
datos_panel <- pdata.frame(datos_regresion, index = c("país", "año"))

# Eliminar filas con valores faltantes en las variables independientes
datos_panel <- datos_panel %>%
  drop_na()


# Antes de proceder con los modelos dividiremos los paíes según su nivel de desarrollo
# Lista de países desarrollados
paises_desarrollados <- c(
  "Australia", "Austria", "Bélgica", "Canadá", "Suiza", "Chipre", "Alemania",
  "Dinamarca", "España", "Estonia", "Finlandia", "Francia", "Grecia", "Hong Kong",
  "Croacia", "Hungría", "Irlanda", "Islandia", "Israel", "Italia", "Japón",
  "Luxemburgo", "Malta", "Países Bajos", "Noruega", "Portugal", "San Marino",
  "Singapur", "Eslovenia", "Suecia"
)

# Lista de países en desarrollo
paises_en_desarrollo <- c(
  "Albania", "Argelia", "Argentina", "Armenia", "Azerbaiyán", "Bahamas",
  "Barbados", "Belice", "Bolivia", "Bosnia y Herzegovina", "Brasil", "Bulgaria",
  "Cabo Verde", "Chile", "China", "Colombia", "Costa Rica", "Cuba", "Dominica",
  "República Dominicana", "Ecuador", "Egipto", "Emiratos Árabes Unidos",
  "Georgia", "Indonesia", "India", "Irán", "Jamaica", "Jordania", "Kazajistán",
  "Kirguistán", "Líbano", "Libia", "Macedonia del Norte", "Malasia", "Maldivas",
  "Mauricio", "México", "Mongolia", "Montenegro", "Marruecos", "Namibia",
  "Nicaragua", "Omán", "Panamá", "Paraguay", "Perú", "Filipinas", "Polonia",
  "Rumania", "Rusia", "Serbia", "Seychelles", "Sri Lanka", "Tailandia", "Túnez",
  "Turquía", "Ucrania", "Uruguay"
)

# Lista de países menos desarrollados
paises_menos_desarrollados <- c(
  "Afganistán", "Angola", "Bangladesh", "Benin", "Burkina Faso", "Burundi",
  "Camboya", "Camerún", "República Centroafricana", "Chad", "Congo",
  "Costa de Marfil", "Yibuti", "Guinea Ecuatorial", "Eritrea", "Etiopía",
  "Gambia", "Ghana", "Guinea", "Haití", "Honduras", "Kenia", "Kiribati", "Laos",
  "Lesoto", "Liberia", "Madagascar", "Malaui", "Malí", "Mauritania",
  "Mozambique", "Myanmar", "Nepal", "Níger", "Nigeria", "Pakistán",
  "Papúa Nueva Guinea", "Ruanda", "Santo Tomé y Príncipe", "Senegal",
  "Sierra Leona", "Islas Salomón", "Somalia", "Sudán del Sur", "Sudán", "Siria",
  "Tanzania", "Timor-Leste", "Togo", "Tonga", "Uganda", "Uzbekistán", "Vanuatu",
  "Vietnam", "Yemen", "Zambia", "Zimbabue"
)

# Filtrar datos para cada grupo
datos_desarrollados <- datos_panel %>% 
  filter(país %in% paises_desarrollados)

datos_en_desarrollo <- datos_panel %>% 
  filter(país %in% paises_en_desarrollo)

datos_menos_desarrollados <- datos_panel %>% 
  filter(país %in% paises_menos_desarrollados)


# Ahora procedemos a hacer los modelos
# Recrear datos de panel como un pdata.frame
datos_panel <- pdata.frame(datos_regresion, index = c("país", "año"))
datos_desarrollados <- pdata.frame(datos_desarrollados, index = c("país", "año"))
datos_en_desarrollo <- pdata.frame(datos_en_desarrollo, index = c("país", "año"))
datos_menos_desarrollados  <- pdata.frame(datos_menos_desarrollados , index = c("país", "año"))

# Verificar que es un pdata.frame válido
class(datos_panel)



# Ajustar el primer modelo: Variables macroeconómicas + Erogaciones totales, para todo el dataset
modelo_1 <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                  Comercio + Erogaciones.totales, 
                data = datos_panel, 
                model = "within")

# Resumen del primer modelo
summary(modelo_1)
summary(modelo_1, vcov = vcovHC(modelo_1, method = "arellano"))

# Modelo para países desarrollados
modelo_desarrollados <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                              Comercio + Erogaciones.totales, 
                            data = datos_desarrollados, 
                            model = "within")

# Resumen del modelo con errores estándar robustos
summary(modelo_desarrollados, vcov = vcovHC(modelo_desarrollados, method = "arellano"))

# Modelo para países en desarrollo
modelo_en_desarrollo <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                              Comercio + Erogaciones.totales, 
                            data = datos_en_desarrollo, 
                            model = "within")

# Resumen del modelo con errores estándar robustos
summary(modelo_en_desarrollo, vcov = vcovHC(modelo_en_desarrollo, method = "arellano"))

# Modelo para países menos desarrollados
modelo_menos_desarrollados <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                                    Comercio + Erogaciones.totales, 
                                  data = datos_menos_desarrollados, 
                                  model = "within")

# Resumen del modelo con errores estándar robustos
summary(modelo_menos_desarrollados, vcov = vcovHC(modelo_menos_desarrollados, method = "arellano"))


# Ahora separamos el gasto en sus distintas funciones
# Modelo para el dataset completo con funciones desagregadas
modelo_2 <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                  Comercio + Defensa + Asuntos.económicos + Educación + 
                  Protección.del.medio.ambiente + Servicios.públicos.generales + 
                  Salud + Vivienda.y.servicios.comunitarios + Orden.público.y.seguridad + 
                  Actividades.recreativas..cultura.y.religión + Protección.social, 
                data = datos_panel, 
                model = "within")

# Resumen del modelo con errores estándar robustos
summary(modelo_2)
summary(modelo_2, vcov = vcovHC(modelo_2, method = "arellano"))

















# Modelo para países desarrollados con funciones desagregadas
modelo_desarrollados_2 <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                                Comercio + Defensa + Asuntos.económicos + Educación + 
                                Salud + Vivienda.y.servicios.comunitarios +  
                                Actividades.recreativas..cultura.y.religión + Protección.social, 
                              data = datos_desarrollados, 
                              model = "within")

# Resumen del modelo con errores estándar robustos
summary(modelo_desarrollados_2, vcov = vcovHC(modelo_desarrollados_2, method = "arellano"))






