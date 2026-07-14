# ============================================================
# clean_enemdu_evolucion.R
# Extrae y armoniza el promedio de horas trabajadas por sector y sexo (2007-2026)
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "purrr", "stringr", "readr"))

input_dir <- "data/raw/enemdu/evolucion"
out_path_rds <- "data/processed/enemdu_horas_sector_2007_2026.rds"
out_path_csv <- "data/processed/enemdu_horas_sector_2007_2026.csv"


archivos <- list.files(input_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)


procesar_anio <- function(ruta) {
  
  
  anio_actual <- as.numeric(stringr::str_extract(basename(ruta), "\\d{4}"))
  
  
  df <- haven::read_sav(ruta)
  
  df_limpio <- df %>%
    filter(p03 >= 15) %>%
    select(p02, p03, p24, secemp, p05a, p05b) %>%
    mutate(
      
      secemp_new = case_when(
        !is.na(secemp) ~ secemp,
        anio_actual == 2007 & p05a == 9 & p05b == 9 ~ 2,
        anio_actual %in% c(2008, 2009, 2016) & p05a == 9 & p05b == 8 ~ 2,
        anio_actual %in% c(2010:2015, 2017) & p05a == 10 & p05b == 9 ~ 2,
        anio_actual %in% c(2018, 2022) & p05a == 7 & p05b == 6 ~ 2,
        anio_actual == 2019 & p05a == 7 & p05b == 7 ~ 2,
        anio_actual == 2020 & p05a == 8 & p05b == 5 ~ 2,
        anio_actual == 2021 & p05a == 9 & p05b == 6 ~ 2,
        anio_actual %in% 2023:2026 & p05a == 7 & p05b == 5 ~ 2,
        is.na(secemp) ~ 1, 
        TRUE ~ secemp
      ),
      sexo = ifelse(p02 == 1, "Hombres", "Mujeres"),
      sector_desc = case_when(
        secemp_new == 1 ~ "Sector Formal",
        secemp_new == 2 ~ "Sector Informal",
        secemp_new == 3 ~ "Empleo Doméstico",
        secemp_new == 4 ~ "No clasificados por sector",
        TRUE ~ "Otro"
      )
    )
  
  
  resumen <- df_limpio %>%
    filter(sector_desc != "Otro") %>%
    group_by(anio = anio_actual, sexo, sector_desc) %>%
    summarise(
      horas_promedio = mean(as.numeric(p24), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(resumen)
}


message("Procesando bases de datos. Esto puede tomar un par de minutos...")
datos_consolidados <- purrr::map_dfr(archivos, procesar_anio)


saveRDS(datos_consolidados, out_path_rds)
readr::write_csv(datos_consolidados, out_path_csv)

message("¡Éxito! Datos consolidados guardados en: ", out_path_rds)