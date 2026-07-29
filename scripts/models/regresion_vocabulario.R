# ==========================================================================
# regresion_vocabulario.R
# Estimación del modelo parsimonioso para brechas de vocabulario infantil.
# Entrada: data/processed/endi_vocabulario_muestra.rds
# Salida:  data/processed/coeficientes_modelo.rds
#          outputs/tables/tabla_regresion_vocabulario.html
# ==========================================================================

source("scripts/packages.R")

ensure_packages(c("dplyr", "survey", "car", "broom", "stargazer"))


muestra <- readRDS("data/processed/endi_vocabulario_muestra.rds")


muestra <- muestra %>%
  mutate(fexp_di = suppressWarnings(as.numeric(fexp_di))) %>%
  filter(!is.na(fexp_di), is.finite(fexp_di), fexp_di > 0)

options(survey.lonely.psu = "adjust")
diseno <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp_di,
  data = muestra,
  nest = TRUE
)

modelo_final <- svyglm(
  z_tvip ~ quintil_of + educ_madre + tot_men5 + horas_solo + act_b + act_g,
  design = diseno
)

resultados_tidy <- tidy(modelo_final, conf.int = TRUE) %>%
  filter(term != "(Intercept)")
saveRDS(resultados_tidy, "data/processed/coeficientes_modelo.rds")

stargazer(modelo_final, 
          type = "html", 
          out = "outputs/tables/tabla_regresion_vocabulario.html",
          title = "Impacto en el Desarrollo del Vocabulario Infantil (Ecuador)",
          dep.var.labels = c("Z-Score de Vocabulario (TVIP)"),
          covariate.labels = c("Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5",
                               "Madre: Educ. Básica", "Madre: Educ. Media", "Madre: Educ. Superior",
                               "Menores de 5 años en hogar", "Horas a solas",
                               "Estimulación: Contó cuentos", "Estimulación: Nombró objetos"),
          notes = "Errores estándar entre paréntesis.",
          header = FALSE, 
          single.row = FALSE) 

message("Modelo ejecutado. Tabla académica guardada en outputs/tables/")