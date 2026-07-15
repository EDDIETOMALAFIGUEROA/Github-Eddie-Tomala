# ============================================================
# plot_mdi_desaparecidos_fatalidad.R
# Genera un gráfico de líneas mostrando la evolución de las
# desapariciones y fatalidad (2017-2025).
# Requiere: data/raw/mdi/mdi_personasdesaparecidas_pm_2017_2025.xlsx
# Guarda:   outputs/figures/evolucion_desaparecidos_fatalidad_2017_2025.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_mdi_desaparecidos_fatalidad.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "lubridate", "ragg", "stringr", "ggtext", "readxl"))

input_path <- "data/raw/mdi/mdi_personasdesaparecidas_pm_2017_2025.xlsx"
out_path <- "outputs/figures/evolucion_desaparecidos_fatalidad_2017_2025.png"

chart_data <- readxl::read_excel(input_path, sheet = "1")

plot_df <- chart_data %>%
  mutate(anio = year(ymd(fecha_desaparicion))) %>%
  filter(!is.na(anio), !is.na(situacion_actual)) %>%
  group_by(anio, situacion_actual) %>%
  summarise(cantidad = n(), .groups = 'drop') %>%
  group_by(anio) %>%
  mutate(porcentaje = cantidad / sum(cantidad)) %>%
  ungroup() %>%
  filter(situacion_actual %in% c("DESAPARECIDO", "FALLECIDO"))

title_raw <- "La crisis de las desapariciones revela menos hallazgos y más fatalidad"
subtitle_txt <- "Porcentaje de casos que permanecen sin resolver o\nculminan en muerte (2017-2025)"

caption_raw <- paste0(
  "Fuente: Portal de Datos Abiertos y Subsecretaría de Estudios y Estadística de la Seguridad del Ministerio del Interior. ",
  "Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE. ",
  "El universo de análisis comprende el total de denuncias oficiales ingresadas al sistema. ",
  "El porcentaje refleja la proporción de casos anuales que permanecen en estado DESAPARECIDO o culminan en FALLECIDO indistintamente de la motivación o tipificación final del hecho. "
)

caption_txt <- stringr::str_wrap(caption_raw, width = 70)

palette_color <- c(
  "DESAPARECIDO" = "#FF7F00", 
  "FALLECIDO" = "#E41A1C"
)

build_chart <- function() {
  spec <- house_spec("portrait") 
  title_txt <- stringr::str_wrap(title_raw, width = 42)
  caption_use <- caption_txt
  
  ggplot(plot_df, aes(x = anio, y = porcentaje, color = situacion_actual)) +
    
    geom_hline(yintercept = 0, color = "#222222", linewidth = 0.8) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1.5) +
    
    scale_color_manual(values = palette_color) +
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1), 
      limits = c(0, 0.12),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(breaks = seq(2017, 2025, by = 1)) +
    
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "Año del Reporte",
      y = "Porcentaje sobre el total de denuncias",
      color = NULL, 
      caption = caption_use
    ) +
    
    theme_quantificador() +
    theme(
      axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#333333"),
      axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#333333", size = 9),
      axis.text = element_text(size = 10, color = "black"),
      
      legend.position = "top",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 9, face = "bold"),
      legend.margin = margin(b = 10),
      
      panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
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

message("Guardado: ", dest)