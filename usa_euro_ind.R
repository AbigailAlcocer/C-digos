#---------------------------------------------------------------------------------------
# CARGAR LIBRERÍAS
#---------------------------------------------------------------------------------------

## Librería
library("tidyverse")
library("data.table") # Manipular bases
#library("assertive") # Corroborar clases de variables
#library("visdat") # Visualizar dataset para corroborar errores 
#library("stringdist") # Calcular distancia en strings
#library("fuzzyjoin") # Realizar merge de strings 
#library("reclin") # Comparar strings en dos datasets 
#library("skimr") # Resumen estadístico de variables (similar a glimpse)
#library("janitor") # Limpiar el nombre de las variables en la base
#library("moderndive") # Trabajar con inferencia estadística (regresiones)

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

library("ggthemes") # Formato de gráficas
library("leaflet") # GrÃ¡fica de mapas interactivos
#library("leafle.extras") # Funcionalidades extras de leaflet
library("ggmap") # Visializaciones espaciales con ggplot2
library("htmltools") # Herramientas HTML
library("maps") # Visualizar mapas
library("mapproj") # Proyecciones de mapas
library("mapdata") # Bases de datos en mapas
#library("mxmaps") # Base de mapa de México

library("httr") # Trabajar con HTML 1
library("rvest") # Scrape Web pages 2
library("xml2") # Analizar estructuras en HTML
#library("DBI") # Trabajar bases en SQL
#library("jsonlite") # Trabajar APIs en formato JSON
#library("haven") # Trabajar con datos de SAS, STATA SPSS
#library("foreign") # Trabajar con datos de SAS, STATA SPSS

#---------------------------------------------------------------------------------------
# LIMPIAR ENTORNO, DEFINIR TOKEN Y RANGO DE FECHA
#---------------------------------------------------------------------------------------

# Limpiar entorno
rm(list = ls())

# Establecer KEY API
fredr_set_key("d4e7d1cc46dd3f7893af956c88eedfb7")

#---------------------------------------------------------------------------------------
# CREAR FUNCIONES DE TRABAJO
#---------------------------------------------------------------------------------------

# Función obtener datos FRED
obtener <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2013-01-31")) %>%
    mutate(value = value/1000) %>% select(fecha = date, value)
}

obtener_2 <- function(id_1){
  fredr(series_id = id_1,
        observation_start = ymd("2012-01-31")) %>%
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


obtener_A <- function(base_A,region,id_1,id_2){
  base_A %>% 
    filter(geo == region,
           s_adj == id_1,
           unit == "I15",
           nace_r2 == id_2,
           time >= "2013-01-31",
    ) %>% select(fecha = time, values)
}


obtener_B <- function(base_B,region,id_1,medida,id_2){
  base_B %>% 
    filter(geo == region,
           s_adj == id_1,
           unit == medida,
           nace_r2 == id_2,
           time >= "2013-01-31",
    ) %>% select(fecha = time, values)
}


obtener_C <- function(base_C,region,medida,id_1){
  base_C %>% 
    filter(geo == region,#
           unit == medida,#
           indic == id_1,#
           time >= "2013-01-31",
    ) %>% select(fecha = time, values)
}


obtener_D <- function(base_D,region,medida,id_1,sea_adj){
  base_D %>% 
    filter(geo == region,#
           unit == medida,#
           na_item == id_1,#
           s_adj == sea_adj,
           time >= "2010-03-31",
    ) %>% select(fecha = time, values)
}


#---------------------------------------------------------------------------------------
#------------------------------------DATA FRED------------------------------------------
#------------------------------------DATA FRED------------------------------------------
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# IDs DE TRABAJO: USA VENTAS AL MENUDEO
#---------------------------------------------------------------------------------------

# Ventas al menudeo
id_rs_total <- c("RSAFS") # Advance Retail Sales: Retail Trade and Food Services, Seasonally Adjusted
id_rs_gas <- c("RSGASS") # Advance Retail Sales: Gasoline Stations, Seasonally Adjusted
id_rs_fsdp <- c("RSFSDP") # Advance Retail Sales: Food Services and Drinking Places, Seasonally Adjusted
id_rs_sport <- c("RSSGHBMS") # Advance Retail Sales: Sporting Goods, Hobby, Musical Instrument, and Book Stores, Seasonally Adjusted
id_rs_mercha <- c("RSGMS") # Advance Retail Sales: General Merchandise Stores, Seasonally Adjusted
id_rs_fbstore <- c("RSDBS") # Advance Retail Sales: Food and Beverage Stores, Seasonally Adjusted
id_rs_misce <- c("RSMSR") # Advance Retail Sales: Miscellaneous Store Retailers, Seasonally Adjusted
id_rs_furni <- c("RSFHFS") # Advance Retail Sales: Furniture and Home Furnishings Stores, Seasonally Adjusted
id_rs_nonstore <- c("RSNSR") # Advance Retail Sales: Nonstore Retailers, Seasonally Adjusted
id_rs_health <- c("RSHPCS") # Advance Retail Sales: Health and Personal Care Stores, Seasonally Adjusted
id_rs_clothing <- c("RSCCAS") # Advance Retail Sales: Clothing and Clothing Accessory Stores, Seasonally Adjusted
id_rs_motor <- c("RSMVPD") # Advance Retail Sales: Motor Vehicle and Parts Dealers, Seasonally Adjusted
id_rs_electro <- c("RSEAS") # Advance Retail Sales: Electronics and Appliance Stores, Seasonally Adjusted
id_rs_building <- c("RSBMGESD") # Advance Retail Sales: Building Materials, Garden Equipment and Supplies Dealers, Seasonally Adjusted

#---------------------------------------------------------------------------------------
# DESCARGAR BASES FRED: VENTAS AL MENUDEO
#---------------------------------------------------------------------------------------

# Total
rs_total <- obtener(id_rs_total) %>% rename(Total = value)
# Gasolina
rs_gas <- obtener(id_rs_gas) %>% rename(Gasolina = value)
# Servicios de Comida y Bebida
rs_fsdp <- obtener(id_rs_fsdp) %>% rename(`Servicios de comida y bebida` = value)
# Bienes Deportivos 
rs_sport <- obtener(id_rs_sport) %>% rename(`Bienes deportivos` = value)
# MercancÃ­a General 
rs_mercha <- obtener(id_rs_mercha) %>% rename(`Mercancía general` = value)
# Comida y Bebida
rs_fbstore <- obtener(id_rs_fbstore) %>% rename(`Comida y bebida` = value)
# Miscelánea
rs_misce <- obtener(id_rs_misce) %>% rename(`Miscelánea` = value)
# Muebles para el hogar
rs_furni <- obtener(id_rs_furni) %>% rename(`Muebles para el hogar` = value)
# Comercio electrónico
rs_nonstore <- obtener(id_rs_nonstore) %>% rename(`Comercio electrónico` = value)
# Salud y cuidado personal
rs_health <- obtener(id_rs_health) %>% rename(`Salud y cuidado personal` = value)
# Ropa y accesorios
rs_clothing <- obtener(id_rs_clothing) %>% rename(`Ropa y accesorios` = value)
# Autopartes
rs_motor <- obtener(id_rs_motor) %>% rename(`Autopartes` = value)
# Electrónicos
rs_electro <- obtener(id_rs_electro) %>% rename(`Electrónicos` = value)
# Materiales de construcción
rs_building <- obtener(id_rs_building) %>% rename(`Materiales de construcción` = value)


# Consolidado
RS <- rs_total %>% left_join(rs_gas, by = "fecha") %>% left_join(rs_fsdp, by = "fecha") %>% 
  left_join(rs_sport, by = "fecha") %>% left_join(rs_mercha, by = "fecha") %>% left_join(rs_fbstore, by = "fecha") %>% 
  left_join(rs_misce, by = "fecha") %>% left_join(rs_furni, by = "fecha") %>% left_join(rs_nonstore, by = "fecha") %>% 
  left_join(rs_health, by = "fecha") %>% left_join(rs_clothing, by = "fecha") %>% left_join(rs_motor, by = "fecha") %>% 
  left_join(rs_electro, by = "fecha") %>% left_join(rs_building, by = "fecha")


# Crear base
write_xlsx(
  RS,
  "/Users/antoniogarcia/Desktop/R/2.0 Macro/0. Datos/1.1 RS_USA.xlsx",
  col_names = T
)


#---------------------------------------------------------------------------------------
# IDs DE TRABAJO: USA PRODUCCIÓN INDUSTRIAL
#---------------------------------------------------------------------------------------

# Producción industrial
id_ip_total <- c("INDPRO") #  Industrial Production: Total Index , Seasonally Adjusted

id_ip_manu <- c("IPMANSICS") # Industrial Production: Manufacturing (SIC), Seasonally Adjusted
id_ip_manu_naics <- c("IPMAN") # Industrial Production: Manufacturing (NAICS), Seasonally Adjusted
id_ip_dg_naics <- c("IPDMAN") # Industrial Production: Durable Manufacturing (NAICS), Seasonally Adjusted
id_ip_dg_wood <- c("IPG321S") # Industrial Production: Manufacturing: Durable Goods: Wood Product (NAICS = 321), Seasonally Adjusted
id_ip_dg_nmet <- c("IPG327S") # Industrial Production: Manufacturing: Durable Goods: Nonmetallic Mineral Product (NAICS = 327), Seasonally Adjusted
id_ip_dg_prim <- c("IPG331S") # Industrial Production: Manufacturing: Durable Goods: Primary Metal (NAICS = 331), Seasonally Adjusted
id_ip_dg_famet <- c("IPG332S") # Industrial Production: Manufacturing: Durable Goods: Fabricated Metal Product (NAICS = 332), Seasonally Adjusted
id_ip_dg_mach <- c("IPG333S") # Industrial Production: Manufacturing: Durable Goods: Machinery (NAICS = 333), Seasonally Adjusted
id_ip_dg_coel <- c("IPG334S") # Industrial Production: Manufacturing: Durable Goods: Computer and Electronic Product (NAICS = 334), Seasonally Adjusted
id_ip_dg_elequ <- c("IPG335S") # Industrial Production: Manufacturing: Durable Goods: Electrical Equipment, Appliance, and Component (NAICS = 335), Seasonally Adjusted
id_ip_dg_motor <- c("IPG3361T3S") # Industrial Production: Manufacturing: Durable Goods: Motor Vehicles and Parts (NAICS = 3361-3), Seasonally Adjusted
id_ip_dg_aero <- c("IPG3364T9S") # Industrial Production: Manufacturing: Durable Goods: Aerospace and Miscellaneous Transportation Equipment (NAICS = 3364-9), Seasonally Adjusted
id_ip_dg_furn <- c("IPG337S") # Industrial Production: Manufacturing: Durable Goods: Furniture and Related Product (NAICS = 337), Seasonally Adjusted
id_ip_dg_misc <- c("IPG339S") # Industrial Production: Manufacturing: Durable Goods: Miscellaneous (NAICS = 339), Seasonally Adjusted
id_ip_ndg_naics <- c("IPNMAN") # Industrial Production: Non-Durable Manufacturing (NAICS), Seasonally Adjusted
id_ip_ndg_food <- c("IPG311A2S") # Industrial Production: Manufacturing: Non-Durable Goods: Food, Beverage, and Tobacco (NAICS = 311,2), Seasonally Adjusted
id_ip_ndg_text <- c("IPG313A4S") # Industrial Production: Manufacturing: Non-Durable Goods: Textiles and Products (NAICS = 313,4), Seasonally Adjusted
id_ip_ndg_appa <- c("IPG315A6S") # Industrial Production: Manufacturing: Non-Durable Goods: Apparel and Leather Goods (NAICS = 315,6), Seasonally Adjusted
id_ip_ndg_paper <- c("IPG322S") # Industrial Production: Manufacturing: Non-Durable Goods: Paper (NAICS = 322), Seasonally Adjusted
id_ip_ndg_print <- c("IPG323S") # Industrial Production: Manufacturing: Non-Durable Goods: Printing and Related Support Activities (NAICS = 323), Seasonally Adjusted
id_ip_ndg_petro <- c("IPG324S") # Industrial Production: Manufacturing: Non-Durable Goods: Petroleum and Coal Products (NAICS = 324), Seasonally Adjusted
id_ip_ndg_chem <- c("IPG325S") # Industrial Production: Manufacturing: Non-Durable Goods: Chemical (NAICS = 325), Seasonally Adjusted
id_ip_ndg_plastic <- c("IPG326S") # Industrial Production: Manufacturing: Non-Durable Goods: Plastics and Rubber Products (NAICS = 326), Seasonally Adjusted
id_ip_otherma <- c("IPGMFOS") # Industrial Production: Other Manufacturing, Seasonally Adjusted

id_ip_mini <- c("IPMINE") # Industrial Production: Mining, Quarrying, and Oil and Gas Extraction: Mining (NAICS = 21), Seasonally Adjusted

id_ip_utili <- c("IPUTIL") # Industrial Production: Utilities: Electric and Gas Utilities, Seasonally Adjusted
id_ip_utili_power <- c("IPG2211S") # Industrial Production: Utilities: Electric Power Generation, Transmission, and Distribution (NAICS = 2211), Seasonally Adjusted
id_ip_utili_gas <- c("IPG2212S") # Industrial Production: Utilities: Natural Gas Distribution, Seasonally Adjusted

#---------------------------------------------------------------------------------------
# DESCARGAR BASES FRED: PRODUCCIÓN INDUSTRIAL
#---------------------------------------------------------------------------------------

# Total
ip_total <- obtener_2(id_ip_total) %>% rename(Total = value)

# Índice manufacturero
ip_manu <- obtener_2(id_ip_manu) %>% rename(`Índice manufacturero` = value)
# Índice Manufacturero (NAICS)
ip_manu_naics <- obtener_2(id_ip_manu_naics) %>% rename(`Índice Manufacturero (NAICS)` = value)
# Índice Bienes Durables
ip_dg_naics <- obtener_2(id_ip_dg_naics) %>% rename(`Índice Bienes Durables` = value)
# Índice Productos de Madera
ip_dg_wood <- obtener_2(id_ip_dg_wood) %>% rename(`Índice Productos de Madera` = value)
# Índice Productos No MetÃ¡licos
ip_dg_nmet <- obtener_2(id_ip_dg_nmet) %>% rename(`Índice Productos No Metálicos` = value)
# Índice Metales Primarios
ip_dg_prim <- obtener_2(id_ip_dg_prim) %>% rename(`Índice Metales Primarios` = value)
# Índice Productos MetÃ¡licos Fabricados
ip_dg_famet <- obtener_2(id_ip_dg_famet) %>% rename(`Índice Productos Metálicos Fabricados` = value)
# Índice Maquinaria
ip_dg_mach <- obtener_2(id_ip_dg_mach) %>% rename(`Índice Maquinaria` = value)
# Índice Productos ElectrÃ³nicos y de CÃ³mputo
ip_dg_coel <- obtener_2(id_ip_dg_coel) %>% rename(`Índice Productos Electrónicos y de Cómputo` = value)
# Índice Equipo ElÃ©ctrico y Componentes
ip_dg_elequ <- obtener_2(id_ip_dg_elequ) %>% rename(`Índice Equipo Eléctrico y Componentes` = value)
# Índice Vehículos y Autopartes
ip_dg_motor <- obtener_2(id_ip_dg_motor) %>% rename(`Índice Vehículos y Autopartes` = value)
# Índice Equipo Aeroespacial
ip_dg_aero <- obtener_2(id_ip_dg_aero) %>% rename(`Índice Equipo Aeroespacial` = value)
# Índice Muebles
ip_dg_furn <- obtener_2(id_ip_dg_furn) %>% rename(`Índice Muebles` = value)
# Índice Productos Durables GenÃ©ricos
ip_dg_misc <- obtener_2(id_ip_dg_misc) %>% rename(`Índice Productos Durables Genéricos` = value)
# Índice Bienes No Durables
ip_ndg_naics <- obtener_2(id_ip_ndg_naics) %>% rename(`Índice Bienes No Durables` = value)
# Índice Alimentos, Bebidas y Tabaco
ip_ndg_food <- obtener_2(id_ip_ndg_food) %>% rename(`Índice Alimentos, Bebidas y Tabaco` = value)
# Índice Textiles
ip_ndg_text <- obtener_2(id_ip_ndg_text) %>% rename(`Índice Textiles` = value)
# Índice Ropa y Cuero
ip_ndg_appa <- obtener_2(id_ip_ndg_appa) %>% rename(`Índice Ropa y Cuero` = value)
# Índice Papel
ip_ndg_paper <- obtener_2(id_ip_ndg_paper) %>% rename(`Índice Papel` = value)
# Índice ImpresiÃ³n y Soporte
ip_ndg_print <- obtener_2(id_ip_ndg_print) %>% rename(`Índice Impresión y Soporte` = value)
# Índice de PetrÃ³leo y CarbÃ³n
ip_ndg_petro <- obtener_2(id_ip_ndg_petro) %>% rename(`Índice de Petróleo y Carbón` = value)
# Índice Químicos
ip_ndg_chem <- obtener_2(id_ip_ndg_chem) %>% rename(`Índice Químicos` = value)
# Índice Plásticos y Caucho
ip_ndg_plastic <- obtener_2(id_ip_ndg_plastic) %>% rename(`Índice Plásticos y Caucho` = value)
# Índice Otras Manufacturas
ip_otherma <- obtener_2(id_ip_otherma) %>% rename(`Índice Otras Manufacturas` = value)

# Índice minero
ip_mini <- obtener_2(id_ip_mini) %>% rename(`Índice minero` = value)

# Índice servicios pÃºblicos
ip_utili <- obtener_2(id_ip_utili) %>% rename(`Índice servicios públicos` = value)
# Índice GeneraciÃ³n de Energía
ip_utili_power <- obtener_2(id_ip_utili_power) %>% rename(`Índice Generación de Energía` = value)
# Índice Gas Natural
ip_utili_gas <- obtener_2(id_ip_utili_gas) %>% rename(`Índice Gas Natural` = value)


# Consolidado
IP_USA <- ip_total %>% left_join(ip_manu, by = "fecha") %>% left_join(ip_manu_naics, by = "fecha") %>% left_join(ip_dg_naics, by = "fecha") %>% left_join(ip_dg_wood, by = "fecha") %>% 
  left_join(ip_dg_nmet, by = "fecha") %>% left_join(ip_dg_prim, by = "fecha") %>% left_join(ip_dg_famet, by = "fecha") %>% left_join(ip_dg_mach, by = "fecha") %>% left_join(ip_dg_coel, by = "fecha") %>% 
  left_join(ip_dg_elequ, by = "fecha") %>% left_join(ip_dg_motor, by = "fecha") %>% left_join(ip_dg_aero, by = "fecha") %>% left_join(ip_dg_furn, by = "fecha") %>% left_join(ip_dg_misc, by = "fecha") %>% 
  left_join(ip_ndg_naics, by = "fecha") %>% left_join(ip_ndg_food, by = "fecha") %>% left_join(ip_ndg_text, by = "fecha") %>% left_join(ip_ndg_appa, by = "fecha") %>% left_join(ip_ndg_paper, by = "fecha") %>% 
  left_join(ip_ndg_print, by = "fecha") %>% left_join(ip_ndg_petro, by = "fecha") %>% left_join(ip_ndg_chem, by = "fecha") %>% left_join(ip_ndg_plastic, by = "fecha") %>% left_join(ip_otherma, by = "fecha") %>% 
  left_join(ip_mini, by = "fecha") %>% left_join(ip_utili, by = "fecha") %>% left_join(ip_utili_power, by = "fecha") %>% left_join(ip_utili_gas, by = "fecha")


# Crear base
write_xlsx(
  IP_USA,
  #"/Users/antoniogarcia/Desktop/R/2.0 Macro/0. Datos/1.2 IP_USA.xlsx",
  "/Users/abiga/OneDrive/Escritorio/SERVICIO SOCIAL SHCP/Códigos de R/1.IP_US.xlsx",
  col_names = T
)
