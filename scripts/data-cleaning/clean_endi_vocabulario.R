# ==========================================================================
# clean_endi_vocabulario.R
# Limpieza, cruce y estandarización de la ENDI 2022 para el análisis de 
# brechas de vocabulario infantil.
# Entrada: data/raw/endi/BDD_ENDI_R1_f1_personas.csv
#          data/raw/endi/BDD_ENDI_R1_f3_desarrollo_inf.csv
# Salida:  data/processed/endi_vocabulario_muestra.rds
# ==========================================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "stringr", "readr"))


ruta_f1 <- "data/raw/endi/BDD_ENDI_R1_f1_personas.csv"
ruta_f3 <- "data/raw/endi/BDD_ENDI_R1_f3_desarrollo_inf.csv"

f1 <- read_delim(ruta_f1, delim = ";", 
                 col_types = cols(.default = col_character()),
                 locale = locale(encoding = "UTF-8"))
f3 <- read_delim(ruta_f3, delim = ";", 
                 col_types = cols(.default = col_character()),
                 locale = locale(encoding = "UTF-8"))


names(f1) <- str_trim(names(f1))
names(f3) <- str_trim(names(f3))
f1 <- f1 %>% 
  mutate(across(everything(), str_trim))
f3 <- f3 %>% 
  mutate(across(everything(), str_trim))


f1 <- f1 %>% 
  mutate(
    across(
      c(id_upm, id_viv, id_hogar, id_per), as.character)
    )
f3 <- f3 %>% 
  mutate(across
         (c(id_upm, id_viv, id_hogar, id_per), as.character)
         )
if ("id_cuid" %in% names(f3)) {
  f3 <- f3 %>% 
    mutate(
      id_cuid = as.character(id_cuid)
      )
} else {
  stop("Error estructural: f3 no contiene la columna id_cuid.")
}


hog_ninos <- f1 %>%
  filter(as.numeric(f1_s1_3_1) < 5) %>%
  count(id_upm, id_viv, id_hogar, name = "tot_men5")


caract_nino <- f1 %>%
  filter(as.numeric(f1_s1_3_1) < 5) %>%
  select(id_upm, id_viv, id_hogar, id_per, quintil_of = quintil) %>%
  mutate(
    quintil_of = na_if(quintil_of, "."),
    quintil_of = factor(quintil_of, levels = c("Quintil 1","Quintil 2","Quintil 3", "Quintil 4","Quintil 5"))
  )


educ_cuidador <- f1 %>%
  select(id_upm, id_viv, id_hogar, id_per, niv_inst = f1_s1_15_1) %>%
  distinct(id_upm, id_viv, id_hogar, id_per, .keep_all = TRUE)


base <- f3 %>%
  left_join(hog_ninos, by = c("id_upm", "id_viv", "id_hogar")) %>%
  left_join(caract_nino, by = c("id_upm", "id_viv", "id_hogar", "id_per")) %>%
  left_join(educ_cuidador, by = c("id_upm", "id_viv", "id_hogar", "id_cuid" = "id_per"))

base <- base %>%
  mutate(
    edad_meses      = as.numeric(f3_s0_1c_anios) * 12 + as.numeric(f3_s0_1c_meses),
    tvip_raw        = suppressWarnings(as.numeric(f3_s6_608)),
    realizo_tvip    = f3_s6_601 == "Si",
    lengua_indigena = f3_s6_600 %in% c("Español y lengua indígena?", "Solo lengua indígena?"),
    horas_solo      = suppressWarnings(as.numeric(f3_s1_107)),
    tot_men5        = if_else(is.na(tot_men5), 0L, as.integer(tot_men5)),
    act_b = as.integer(f3_s1_108_b == "Sí"),
    act_g = as.integer(f3_s1_108_g == "Sí"),
    educ_madre = case_when(
      niv_inst == "Ninguno" ~ "Sin educación",
      niv_inst %in% c("Centro de desarrollo infantil/Creciendo con nuestros hijos/Guardería",
                      "Educación Inicial/Preescolar/SAFPI",
                      "Alfabetización (EBJA)",
                      "Primaria",
                      "Educación General Básica (EGB)") ~ "Básica",
      niv_inst %in% c("Secundaria", "Bachillerato") ~ "Media",
      niv_inst %in% c("Ciclo Postbachillerato (no superior)",
                      "Educación Técnica o Tecnológica Superior (institutos superiores técnicos y tecnológicos)",
                      "Educación Superior (universidades, escuelas politécnicas)",
                      "Maestría/Especialización",
                      "PHD/Doctorado") ~ "Superior",
      TRUE ~ NA_character_
    ),
    educ_madre = factor(educ_madre, levels = c("Sin educación", "Básica", "Media", "Superior"))
  ) %>%
  select(-niv_inst)


muestra <- base %>%
  filter(
    realizo_tvip,
    !is.na(tvip_raw),
    !is.na(edad_meses),
    !is.na(quintil_of),
    !is.na(educ_madre),
    !is.na(horas_solo),
    !is.na(tot_men5),
    !is.na(act_b),
    !is.na(act_g),
    lengua_indigena == FALSE
  )

muestra <- muestra %>%
  group_by(edad_meses) %>%
  mutate(
    n_edad = n(),
    z_tvip = if (n() >= 5) (tvip_raw - mean(tvip_raw)) / sd(tvip_raw) else NA_real_
  ) %>%
  ungroup() %>%
  filter(!is.na(z_tvip))

saveRDS(muestra, "data/processed/endi_vocabulario_muestra.rds")
write_csv(muestra, "data/processed/endi_vocabulario_muestra.csv")

message("Datos procesados y guardados en formatos RDS y CSV exitosamente en data/processed/")
