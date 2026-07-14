# ============================================================
# plot_enemdu_reduccion_horas.R
# Genera un gráfico de barras mostrando la variación porcentual 
# de horas trabajadas (2007 vs 2026) por sector y sexo.
# Requiere: data/processed/enemdu_horas_sector_2007_2026.rds
# Guarda:   outputs/figures/variacion_horas_2007_2026.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_reduccion_horas.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "ragg", "stringr", "ggtext"))

input_path <- "data/processed/enemdu_horas_sector_2007_2026.rds"
out_path <- "outputs/figures/variacion_horas_2007_2026.png"


chart_data <- readRDS(input_path)


plot_df <- chart_data %>%
  filter(anio %in% c(2007, 2026), 
         sector_desc %in% c("Sector Formal", "Sector Informal")) %>%
  pivot_wider(names_from = anio, values_from = horas_promedio, names_prefix = "anio_") %>%
  mutate(
    Variacion = (anio_2026 / anio_2007) - 1,
    sexo = factor(sexo, levels = c("Mujeres", "Hombres")),
    sector_desc = factor(sector_desc, levels = c("Sector Formal", "Sector Informal"))
  )


title_raw <- "Sin importar el sector, los ecuatorianos trabajan cada vez menos horas"
subtitle_txt <- "Cambio en la jornada laboral promedio a lo largo de las últimas \ndos décadas (2007-2026)"

caption_raw <- paste0(
  "Fuente: ENEMDU - INEC, marzo 2026. Cálculos de Eddie Tomalá para El Quantificador de ",
  "Laboratorio LIDE. Las barras hacia abajo indican el porcentaje de horas que se han reducido. ",
  "El análisis comprende a la población de 15 años o más. La variación se calcula sobre el promedio ",
  "de horas de trabajo en la semana de referencia. Las omisiones en la clasificación del sector laboral ",
  "fueron imputadas utilizando la tenencia de seguridad social como proxy de formalidad."
)


caption_txt <- stringr::str_wrap(caption_raw, width = 70)


palette_fill <- c(
  "Mujeres" = "#e79f43", 
  "Hombres" = "#688a9e"
)


build_chart <- function() {
  spec <- house_spec("portrait")
  title_txt <- stringr::str_wrap(title_raw, width = 42)
  caption_use <- caption_txt
  
  ggplot(plot_df, aes(x = sector_desc, y = Variacion, fill = sexo)) +
    geom_hline(yintercept = 0, color = "#222222", linewidth = 0.8) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.65) +
    geom_text(
      aes(
        label = scales::percent(Variacion, accuracy = 0.1, decimal.mark = ","), 
        vjust = 1.5 
      ),
      position = position_dodge(width = 0.8),
      size = 3, 
      fontface = "bold", 
      color = "#222222"
    ) +
    scale_fill_manual(values = palette_fill) +
    scale_y_continuous(
      labels = label_percent_intl(accuracy = 1), 
      expand = expansion(mult = c(0.15, 0.05)) 
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL, 
      y = "Variación porcentual (%)",
      fill = NULL,
      caption = caption_use
    ) +
    theme_quantificador() +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(color = "#222222", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text.x = element_text(color = "black", size = 9, face = "bold", margin = margin(b = 10)), 
      axis.title.y = element_text(color = "gray30", size = 8, margin = margin(r = 10)),
      legend.position = "top",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 8, face = "bold"),
      legend.margin = margin(b = 15),
      plot.margin = margin(t = 10, r = 15, b = 5, l = 15)
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