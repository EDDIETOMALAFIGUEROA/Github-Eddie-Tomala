# ============================================================
# plot_endi_coeficientes_regresion.R
# Visualización de los coeficientes del modelo parsimonioso
# sobre el desarrollo del vocabulario infantil (ENDI 2022)
# Requiere: data/processed/coeficientes_modelo.rds
# Guarda:   outputs/figures/impacto_vocabulario_coeficientes.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr", "ggtext"))

input_path <- "data/processed/coeficientes_modelo.rds"
out_path <- "outputs/figures/impacto_vocabulario_coeficientes.png"

resultados_tidy <- readRDS(input_path)

resultados_tidy <- resultados_tidy %>%
  mutate(
    term_limpio = case_when(
      term == "quintil_ofQuintil 2" ~ "Quintil 2",
      term == "quintil_ofQuintil 3" ~ "Quintil 3",
      term == "quintil_ofQuintil 4" ~ "Quintil 4",
      term == "quintil_ofQuintil 5" ~ "Riqueza:\nQuintil 5",
      term == "educ_madreBásica"    ~ "Básica",
      term == "educ_madreMedia"     ~ "Media",
      term == "educ_madreSuperior"  ~ "Madre:\nEduc. Superior",
      term == "tot_men5"            ~ "Hermanos\n(<5 años)",
      term == "horas_solo"          ~ "Horas a solas",
      term == "act_b"               ~ "Contó cuentos",
      term == "act_g"               ~ "Estimulación:\nNombró objetos",
      TRUE ~ term
    ),
    term_limpio = factor(term_limpio, levels = rev(c(
      "Madre:\nEduc. Superior", "Media", "Básica",
      "Riqueza:\nQuintil 5", "Quintil 4", "Quintil 3", "Quintil 2",
      "Estimulación:\nNombró objetos", "Contó cuentos",
      "Hermanos\n(<5 años)", 
      "Horas a solas"
    )))
  ) %>%
  filter(!is.na(term_limpio))

title_raw <- "El impacto de la educación materna y la estimulación temprana en el vocabulario de los niños"
subtitle_raw <- "Un vistazo a los factores del entorno familiar que suman o restan palabras al desarrollo de los niños."

caption_raw <- paste0(
  "Fuente: Encuesta Nacional de Desnutrición Infantil (ENDI 2022), INEC. ",
  "Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE. ",
  "Los puntos representan el cambio esperado en el Z-Score de vocabulario (TVIP) ",
  "por cada unidad de cambio en la variable independiente manteniendo el resto constante."
)

title_txt <- stringr::str_wrap(title_raw, width = 45)
subtitle_txt <- stringr::str_wrap(subtitle_raw, width = 55)
caption_txt <- stringr::str_wrap(caption_raw, width = 75)

build_chart <- function() {
  ggplot(resultados_tidy, aes(x = estimate, y = term_limpio)) +

    geom_vline(xintercept = 0, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +

    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, color = "#3498DB", linewidth = 1.2) +
    geom_point(color = "#2980B9", size = 3.5) +

    scale_x_continuous(
      breaks = seq(-0.3, 1.8, by = 0.3), 
      expand = expansion(mult = c(0.05, 0.05)) 
    ) +
    
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "Efecto en el Z-Score de Vocabulario\n(Desviaciones Estándar)",
      y = NULL,
      caption = caption_txt
    ) +
    
    theme_quantificador() +
    theme(
      axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#333333", size = 9),
      axis.text = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 7, face = "bold", margin = margin(r = 5), lineheight = 0.8),
      
      panel.grid.major.x = element_line(color = "gray85", linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(t = 15, r = 25, b = 10, l = 15)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", x = 0.82, y = 0.12)
dest <- out_path

ggsave(
  filename = dest,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado exitosamente: ", dest)