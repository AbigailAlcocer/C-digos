#---------------------------------------------------------------------------------------
# CARGAR LIBRERÃAS
#---------------------------------------------------------------------------------------

## LibrerÃ­a
library("tidyverse")
library("data.table") # Manipular bases
#library("assertive") # Corroborar clases de variables
#library("visdat") # Visualizar dataset para corroborar errores 
#library("stringdist") # Calcular distancia en strings
#library("fuzzyjoin") # Realizar merge de strings 
#library("reclin") # Comparar strings en dos datasets 
#library("skimr") # Resumen estadÃ­stico de variables (similar a glimpse)
#library("janitor") # Limpiar el nombre de las variables en la base
#library("moderndive") # Trabajar con inferencia estadÃ­stica (regresiones)

library("inegiR") # INEGI
library("siebanxicor") # Banxico
library("OECD") # OECD
library("WDI") # World Bank: World Development Indicators
library("fredr") # API FRED
library("eurostat") # API EUROSTAT

library("readxl") # Leer Excel 1 (en desarrollo)
library("XLConnect") # Leer Excel 2 (interfaz para leer Excel en R)
library("writexl") # Exportar archivos Excel
library("gdata") # Leer Excel 3

library("lubridate") # Manipular fechas
library("zoo") # Manipular datos

library("ggthemes") # Formato de grÃ¡ficas
library("leaflet") # GrÃ¡fica de mapas interactivos
#library("leafle.extras") # Funcionalidades extras de leaflet
library("ggmap") # Visializaciones espaciales con ggplot2
library("htmltools") # Herramientas HTML
library("maps") # Visualizar mapas
library("mapproj") # Proyecciones de mapas
library("mapdata") # Bases de datos en mapas
#library("mxmaps") # Base de mapa de MÃ©xico

library("httr") # Trabajar con HTML 1
library("rvest") # Scrape Web pages 2
library("xml2") # Analizar estructuras en HTML
#library("DBI") # Trabajar bases en SQL
#library("jsonlite") # Trabajar APIs en formato JSON
#library("haven") # Trabajar con datos de SAS, STATA SPSS
#library("foreign") # Trabajar con datos de SAS, STATA SPSS
library(usethis)
#---------------------------------------------------------------------------------------
# LIMPIAR ENTORNO, DEFINIR TOKEN Y RANGO DE FECHA
#---------------------------------------------------------------------------------------

# Limpiar entorno
rm(list = ls())

# Establecer KEY API
usethis::edit_r_environ()
fredr_set_key("7d98537f9e96546789d942d0e9b4954e")

#---------------------------------------------------------------------------------------
# CREAR FUNCIONES DE TRABAJO
#---------------------------------------------------------------------------------------

# FunciÃ³n obtener datos FRED
obtener <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2010-01-31")) %>%
    mutate(value = value/1000) %>% select(fecha = date, value)
}

obtener_2 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("1996-01-31")) %>%
    select(fecha = date, value) %>% mutate(value = round(value, 4))
}


obtener_3 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("1999-01-31")) %>%
    select(fecha = date, value) %>% mutate(value = round(value, 4))
}


obtener_4 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2015-01-31")) %>%
    select(fecha = date, value) %>% mutate(value = round(value, 1))
}


obtener_5 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2010-01-31")) %>%
    select(fecha = date, value) %>% mutate(value = round(value, 3))
}


obtener_6 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2014-01-01")) %>%
    select(fecha = date, value) %>% mutate(value = round(value, 3))
}


#---------------------------------------------------------------------------------------
#------------------------------------DATA INF ------------------------------------------
#------------------------------------DATA INF ------------------------------------------
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# IDs DE TRABAJO: USA PCE
#---------------------------------------------------------------------------------------

# PERSONAL CONSUMPTION INDEX
id_pce_gen <- c("PCEPI") #  Índice general	
id_pce_sub <- c("PCEPILFE") # Índice subyacente


pce_gen <- obtener_2(id_pce_gen) %>% rename(`PCE_Índice general` = value) %>% #  Índice general	, #¿como saber el id de la variable?
  transmute(fecha, 
            `PCE_Índice general`,
            `PCE_Índice general_mom` =  round(((`PCE_Índice general`/lag(`PCE_Índice general`,1))-1)*100,1),
            `PCE_Índice general_yoy` =  round(((`PCE_Índice general`/lag(`PCE_Índice general`,12))-1)*100,1),
            )

pce_sub <- obtener_2(id_pce_sub) %>% rename(`PCE_Índice subyacente` = value) %>%  # Índice subyacente
  transmute(fecha, 
            `PCE_Índice subyacente`,
            `PCE_Índice subyacente_mom` =  round(((`PCE_Índice subyacente`/lag(`PCE_Índice subyacente`,1))-1)*100,1),
            `PCE_Índice subyacente_yoy` =  round(((`PCE_Índice subyacente`/lag(`PCE_Índice subyacente`,12))-1)*100,1),
  )


#---------------------------------------------------------------------------------------
# IDs DE TRABAJO: USA INFLACIÓN: CPI
#---------------------------------------------------------------------------------------
# Índice IPC
id_inf_gen <- c("CPIAUCNS") #  Índice general
id_inf_sub <- c("CPILFENS") # Índice subyacente


inf_gen <- obtener_2(id_inf_gen) %>% rename(`CPI_Índice general` = value) %>% #  Índice general	
  transmute(fecha, 
            `CPI_Índice general`,
            `CPI_Índice general_mom` =  round(((`CPI_Índice general`/lag(`CPI_Índice general`,1))-1)*100,1),
            `CPI_Índice general_yoy` =  round(((`CPI_Índice general`/lag(`CPI_Índice general`,12))-1)*100,1),
  )

inf_sub <- obtener_2(id_inf_sub) %>% rename(`CPI_Índice subyacente` = value) %>% # Índice subyacente
  transmute(fecha, 
            `CPI_Índice subyacente`,
            `CPI_Índice subyacente_mom` =  round(((`CPI_Índice subyacente`/lag(`CPI_Índice subyacente`,1))-1)*100,1),
            `CPI_Índice subyacente_yoy` =  round(((`CPI_Índice subyacente`/lag(`CPI_Índice subyacente`,12))-1)*100,1),
  )


#---------------------------------------------------------------------------------------
# IDs DE TRABAJO: USA INFLACIÓN: PPI
#---------------------------------------------------------------------------------------
# Índice IPC
id_ppi_gen <- c("PPIFID") #  Producer Price Index by Commodity: Final Demand, Not Seasonally Adjusted
id_ppi_cor <- c("PPICOR") #  Producer Price Index by Commodity: Final Demand: Final Demand Less Foods and Energy, Not Seasonally Adjusted


ppi_gen <- obtener_2(id_ppi_gen) %>% rename(`PPI_Índice general` = value) %>% #  Índice general	
  transmute(fecha, 
            `PPI_Índice general`,
            `PPI_Índice general_mom` =  round(((`PPI_Índice general`/lag(`PPI_Índice general`,1))-1)*100,1),
            `PPI_Índice general_yoy` =  round(((`PPI_Índice general`/lag(`PPI_Índice general`,12))-1)*100,1),
  )

ppi_cor <- obtener_2(id_ppi_cor) %>% rename(`PPI_Índice subyacente` = value) %>% # Índice subyacente
  transmute(fecha, 
            `PPI_Índice subyacente`,
            `PPI_Índice subyacente_mom` =  round(((`PPI_Índice subyacente`/lag(`PPI_Índice subyacente`,1))-1)*100,1),
            `PPI_Índice subyacente_yoy` =  round(((`PPI_Índice subyacente`/lag(`PPI_Índice subyacente`,12))-1)*100,1),
  )


#---------------------------------------------------------------------------------------
# BASE DE TRABAJO: ATLANTA WAGE GROWTH TRACKER 
#---------------------------------------------------------------------------------------
atlanta_url <- "https://www.atlantafed.org/-/media/documents/datafiles/chcs/wage-growth-tracker/wage-growth-data.xlsx"

GET(atlanta_url, write_disk(data_atlanta <- tempfile(fileext = ".xlsx")))

## Atlanta Wage Growth Tracker
atlanta_wage <- read_excel(data_atlanta, sheet = "data_overall", skip = 1) %>% 
  transmute(fecha = ymd(`...1`),
            `Atlanta Fed Wage Growth` = as.numeric(`Weighted Overall`)) %>% 
  fill(`Atlanta Fed Wage Growth`, .direction = "up") 


#---------------------------------------------------------------------------------------
# BASE DE TRABAJO: FED CLEVELAND: INFLATION EXPECTATIONS
#---------------------------------------------------------------------------------------
clevelanda_url <- "https://www.clevelandfed.org/en/our-research/indicators-and-data/~/media/content/our%20research/indicators%20and%20data/inflation%20expectations/ie%20latest/ie%20xls.xls"

GET(clevelanda_url, write_disk(data_cleveland <- tempfile(fileext = ".xls")))

## Cleveland: Infation Expectation
clevelanda_expec <- read_excel(data_cleveland, sheet = "Expected Inflation") %>% 
  transmute(fecha = ymd(`Model Output Date`),
            `Fed Cleveland: 1 AÃ±o` = round(`1 year Expected Inflation`*100,1),
            `Fed Cleveland: 2 AÃ±os` = round(`2 year Expected Inflation`*100,1),
            `Fed Cleveland: 3 AÃ±os` = round(`3 year Expected Inflation`*100,1),
            )


#---------------------------------------------------------------------------------------
# BASE DE TRABAJO: FED NEW YORK INFLATION EXPECTATIONS
#---------------------------------------------------------------------------------------
ny_url <- "https://www.newyorkfed.org/medialibrary/interactives/sce/sce/downloads/data/frbny-sce-data.xlsx"

GET(ny_url, write_disk(data_ny <- tempfile(fileext = ".xlsx")))

## Cleveland: Infation Expectation
ny_expec <- read_excel(data_ny, sheet = "Inflation expectations", skip = 3) %>% 
  transmute(fecha = ym(`...1`),
            `Fed New York: 1 AÃ±o` = round(`Median one-year ahead expected inflation rate`,1),
            `Fed New York: 3 AÃ±os` = round(`Median three-year ahead expected inflation rate`,1),
  )


#---------------------------------------------------------------------------------------
# BASE DE TRABAJO: UNIVERSITY OF MICHIGAN INFLATION EXPECTATIONS
#---------------------------------------------------------------------------------------
uofm_url <- "http://www.sca.isr.umich.edu/files/tbmpx1px5.xls"

GET(uofm_url, write_disk(data_uofm <- tempfile(fileext = ".xls")))

## Cleveland: Infation Expectation
uofm_expec <- read_excel(data_uofm, skip = 3) %>% drop_na() %>%  
  transmute(fecha = my(str_c(`DATE OF SURVEY`, '-', `...2`)),
            `UofM: 1 AÃ±o` = round(`NEXT YEAR`,1),
            `UofM: 5 AÃ±os` = as.numeric(`NEXT 5 YEARS`)
  ) %>% 
  fill(`UofM: 5 AÃ±os`, .direction = "up")


#---------------------------------------------------------------------------------------
# CONSOLIDADO INFLACIÃN
#---------------------------------------------------------------------------------------

INF1 <- inf_gen %>% left_join(inf_sub, by ="fecha") %>% left_join(pce_gen, by = "fecha") %>% 
  left_join(pce_sub, by = "fecha") %>% left_join(ppi_gen, by = "fecha") %>% left_join(ppi_cor, by = "fecha") %>% 
  left_join(atlanta_wage, by = "fecha") %>% mutate(fecha = ceiling_date(fecha, "month") %m-% days(1))
INF1 <- fill(INF1, names(INF1),.direction = "up")

INF2 <- clevelanda_expec %>% left_join(ny_expec, by = "fecha") %>% left_join(uofm_expec, by = "fecha") %>% 
  mutate(fecha = ceiling_date(fecha, "month") %m-% days(1))
INF2 <- fill(INF2, names(INF2),.direction = "up")

#---------------------------------------------------------------------------------------
# CONSOLIDADOS DE EXCEL
#---------------------------------------------------------------------------------------

# Crear base Tracker
write_xlsx(
  list(
    "Medidas Económicas" = INF1,
    "Expectativas de Inflación" = INF2
  ),
  #"/Users/antoniogarcia/Desktop/R/8. Trackers/1. US_inflación.xlsx",
  "C:/Users/abiga/OneDrive/Escritorio/SERVICIO SOCIAL SHCP/usa_inf_tracker/1. US_inflación.xlsx",
  col_names = T
)
