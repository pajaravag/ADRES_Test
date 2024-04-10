#######################################################################################################
## Instalación de Librerías
install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr")
install.packages("ggplot2")

install.packages("stringdist")
install.packages("ggplot2")
install.packages("gt")

## Carga de Librerías
library(stringdist)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

library(stringr)
library(tidyverse)
library(readr)
library(ggplot2)
library(gt)
library(knitr)

## Conexión a base de datos
cd <-  getwd()

prestadores_tibble <- file.path(cd, 'DB_Test.db') %>%
  dbConnect(RSQLite::SQLite(), .) %>%
  dbGetQuery("SELECT * FROM Prestadores") %>%
  as_tibble() 

municipios_tibble <- file.path(cd, 'DB_Test.db') %>%
  dbConnect(RSQLite::SQLite(), .) %>%
  dbGetQuery("SELECT * FROM Municipios") %>%
  as_tibble() 

dbDisconnect(dbConnect(RSQLite::SQLite(), cd))

head(municipios_tibble)
summary(municipios_tibble)

head(prestadores_tibble)
summary(prestadores_tibble)

info_dane <- read_delim(file.path(cd, 'Info_DANE.csv'), delim = ";")

### Casting Data

find_similar_string <- function(target_string, reference_strings) {
  distances <- stringdist::stringdist(target_string, reference_strings, method = "lv")
  closest_match <- reference_strings[which.min(distances)]
  return(closest_match)
}

lista_departamentos <- tolower(unique(info_dane$DEPARTAMENTO))
lista_municipios <- tolower(unique(info_dane$MUNICIPIO))

table(prestadores_tibble$depa_nombre)

### Pipelines de Transformación
prestadores <- prestadores_tibble %>%
  mutate(depa_nombre = str_replace_all(depa_nombre, "[^[:alnum:][:space:]]", ""),
         muni_nombre = str_replace_all(muni_nombre, "[^[:alnum:][:space:]]", ""),
         nombre_prestador = str_replace_all(nombre_prestador, "[^[:alnum:][:space:]]", ""),
         razon_social = str_replace_all(razon_social, "[^[:alnum:][:space:]]", "")) %>%
  mutate(depa_nombre = case_when(
    depa_nombre == "Barranquilla" ~ "Atlantico",
    depa_nombre == "Buenaventura" ~ "Valle del Cauca",
    depa_nombre == "Cali" ~ "Valle del Cauca",
    depa_nombre == "Cartagena" ~ "Bolivar",
    depa_nombre == "BogotC" ~ "Bogota D.C.",
    depa_nombre == "Cba" ~ "Cordoba",
    depa_nombre == "San Andr鳠y Providencia" ~ "San Andres y Providencia",
    depa_nombre == "Santa Marta" ~ "Magdalena",
    TRUE ~ depa_nombre  
  ))  %>%
  mutate(across(c(depa_nombre, muni_nombre, nombre_prestador, razon_social), tolower)) %>%
  mutate(across(c(depa_nombre, muni_nombre, nombre_prestador, razon_social), str_trim)) %>%
  mutate(depa_nombre = str_replace_all(depa_nombre, "_([a-z])", "\\U\\1"),
         muni_nombre = str_replace_all(muni_nombre, "_([a-z])", "\\U\\1"),
         nombre_prestador = str_replace_all(nombre_prestador, "_([a-z])", "\\U\\1"),
         razon_social = str_replace_all(nombre_prestador, "_([a-z])", "\\U\\1")) %>%
  mutate(depa_nombre = sapply(depa_nombre, function(x) find_similar_string(x, lista_departamentos)),
         muni_nombre = sapply(muni_nombre, function(x) find_similar_string(x, lista_municipios))) %>%
  mutate(depa_nombre = str_to_title(depa_nombre),
         muni_nombre = str_to_title(muni_nombre),
         nombre_prestador = str_to_title(nombre_prestador),
         razon_social = str_to_title(razon_social)) %>%
  mutate(depa_nombre = case_when(
    muni_nombre == "San Andrés" ~ "San Andrés Y Providencia ",
    muni_nombre == "San Pablo" ~ "Bolívar",
    muni_nombre == "Bello" ~ "Antioquia",
    muni_nombre == "Buenavista" ~ "Sucre",
    muni_nombre == "Suan" ~ "Atlántico",
    muni_nombre == "Yondó" ~ "Antioquia",
    TRUE ~ depa_nombre
  ))

  
municipios <- municipios_tibble %>%
  mutate(Departamento = str_replace_all(Departamento, "[^[:alnum:][:space:]]", ""),
         Municipio = str_replace_all(Municipio, "[^[:alnum:][:space:]]", "")) %>%
  mutate(across(c(Departamento, Municipio), tolower)) %>%
  mutate(across(c(Departamento, Municipio), str_trim)) %>%
  mutate(Departamento = sapply(Departamento, function(x) find_similar_string(x, lista_departamentos)),
         Municipio = sapply(Municipio, function(x) find_similar_string(x, lista_municipios))) %>%
  mutate(Departamento = str_to_title(str_replace_all(Departamento, "_([a-z])", "\\U\\1")),
         Municipio = str_to_title(str_replace_all(Municipio, "_([a-z])", "\\U\\1")))

## Unión de Dataframes
df <- inner_join(prestadores, municipios, by = c("depa_nombre" = "Departamento", 
                                                 "muni_nombre" = "Municipio"),
                 relationship = "many-to-many") %>% filter(!is.na(numero_sede_principal)) %>%
  distinct(depa_nombre, muni_nombre, codigo_habilitacion, nombre_prestador,.keep_all = TRUE)

summary(df)

## Plots
ggplot(df, aes(x = reorder(depa_nombre, -table(depa_nombre)[depa_nombre]), fill = depa_nombre)) +
  geom_bar(width = 0.5) +
  ggtitle("Frecuencia de cada Departamento") +
  xlab("Departamentos") +
  ylab("Frecuencia") +
  theme_minimal() +
  labs(fill = "Departamentos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))


agg_df_muni <- df %>%
  group_by(muni_nombre) %>%
  summarise(
    count = n(),
    Poblacion = max(Poblacion, na.rm = TRUE)
  ) %>%
  mutate(
    ratio = (count / Poblacion) * 100
  ) %>%
  arrange(desc(ratio)) %>%
  slice_head(n = 20)

ggplot(agg_df_muni, aes(x = muni_nombre, y = ratio, fill = muni_nombre)) +
  geom_bar(stat = "identity", width = 0.5) +
  ggtitle("Top 20 Municipios (Número de Prestadores / Población Municipal)") +
  xlab("Municipio") +
  ylab("Porcentajes (%)") +  # Change y-axis label to show percentage
  theme_minimal() +
  labs(fill = "Municipios") +
  scale_x_discrete(limits = agg_df_muni$muni_nombre) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

head(df) %>% gt()

region_count <- df %>%
  count(Region) %>%
  mutate(Porcentaje = n / sum(n) * 100)

ggplot(region_count, aes(x = "", y = Porcentaje, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +  
  coord_polar(theta = "y") +  
  ggtitle("Gráfico de Pastel de las Regiones") +
  xlab(NULL) +  
  ylab(NULL) +  
  theme_minimal() +
  labs(fill = "Region", y = "Porcentaje (%)")


knitr::kable

calculate_category_stats <- function(df, category_column) {

  category_counts <- df %>%
    count({{ category_column }}) %>%
    mutate(
      percentage = n / sum(n) * 100,
      percentage = sprintf("%.2f%%", percentage)  
    ) %>%
    arrange(desc(percentage))
  
  return(category_counts)
}

display_category_stats <- function(df, category_column) {

  category_counts <- calculate_category_stats(df, {{ category_column }})
  
  caption_text <- paste("Cateogría basas en: ", deparse(substitute(category_column)))
  
  # Display the table using knitr::kable
  kable(category_counts, 
        caption = caption_text, 
        col.names = c("Categoría", "Frecuencia", "Porcentaje (%)"))
}


display_category_stats(df, Region)
display_category_stats(df, clpr_nombre)
display_category_stats(df, clase_persona)









