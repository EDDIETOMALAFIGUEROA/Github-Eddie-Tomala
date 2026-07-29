# ============================================================
# plot_endi_educacion_madre.R
# Distribución del puntaje de vocabulario (Z-Score) según el
# nivel de instrucción de la madre (ENDI 2022)
# Requiere: data/processed/endi_vocabulario_muestra.rds
# Guarda:    outputs/figures/distribucion_vocabulario_educacion_madre.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr", "ggtext"))

input_path <- "data/processed/endi_vocabulario_muestra.rds"
out_path <- "outputs/figures/distribucion_vocabulario_educacion_madre.png"

muestra <- readRDS(input_path)

title_raw <- "Mayor educación materna se traduce en un mejor desarrollo del vocabulario infantil"
subtitle_raw <- "Distribución del puntaje de vocabulario estandarizado por la edad en meses del menor, comparado según el nivel de instrucción de la madre."

caption_raw <- paste0(
  "Fuente: Encuesta Nacional de Desnutrición Infantil (ENDI 2022), INEC. ",
  "Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE. ",
  "Nota metodológica Muestra restringida a niños hispanohablantes evaluados con la prueba TVIP. ",
  "El puntaje estandarizado Z expresa desviaciones respecto a la media de cada grupo etario."
)

title_txt <- stringr::str_wrap(title_raw, width = 45)
subtitle_txt <- stringr::str_wrap(subtitle_raw, width = 55)
caption_txt <- stringr::str_wrap(caption_raw, width = 75)

palette_educ <- c(
  "Sin educación" = "#95A5A6",
  "Básica"        = "#3498DB",
  "Media"         = "#2ECC71",
  "Superior"      = "#9B59B6"
)

build_chart <- function() {
  ggplot(muestra, aes(x = educ_madre, y = z_tvip, fill = educ_madre)) +
    
    geom_hline(yintercept = 0, color = "#222222", linewidth = 0.8, linetype = "dashed") +

    geom_boxplot(alpha = 0.85, width = 0.5, outlier.shape = NA) +
    
    scale_fill_manual(values = palette_educ) +
    
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    
    scale_y_continuous(
      breaks = seq(-3, 3, by = 1),
      limits = c(-3.5, 3.5),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = "Desviaciones Estándar\n(Z-Score)", 
      caption = caption_txt
    ) +
    
    theme_quantificador() +
    theme(
      legend.position = "none",
      axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#333333", size = 9, hjust = 0.5),
      axis.text = element_text(size = 10, color = "black"),
      axis.text.x = element_text(face = "bold", margin = margin(t = 5, b = 10)),
      
      panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),
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