# ============================================================
# plot_endi_trayectorias_aprendizaje.R
# Trayectorias de aprendizaje del vocabulario (LOESS)
# según el quintil de riqueza del hogar (ENDI 2022)
# Requiere: data/processed/endi_vocabulario_muestra.rds
# Guarda:   outputs/figures/trayectorias_vocabulario_quintiles.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr", "ggtext", "viridis"))

input_path <- "data/processed/endi_vocabulario_muestra.rds"
out_path <- "outputs/figures/trayectorias_vocabulario_quintiles.png"

muestra <- readRDS(input_path)

title_raw <- "¿Creciendo con desigualdad? Evolución de las habilidades de vocabulario por quintil económico"
subtitle_raw <- "Puntaje directo del Test de Vocabulario en Imágenes Peabody (TVIP) en infantes de 43 a 59 meses"

caption_raw <- paste0(
  "Fuente: Encuesta Nacional de Desnutrición Infantil (ENDI 2022), INEC. ",
  "Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE. ",
  "Las curvas representan el ajuste suavizado local (LOESS) ",
  "del puntaje crudo del Test de Vocabulario en Imágenes Peabody (TVIP)."
)

title_txt <- stringr::str_wrap(title_raw, width = 45)
subtitle_txt <- stringr::str_wrap(subtitle_raw, width = 55)
caption_txt <- stringr::str_wrap(caption_raw, width = 75)

build_chart <- function() {
  ggplot(muestra, aes(x = edad_meses, y = tvip_raw, color = quintil_of)) +
    
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
    
    scale_color_viridis_d(option = "plasma", end = 0.9) + 
    
    scale_x_continuous(
      breaks = seq(36, 60, by = 2), 
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    scale_y_continuous(
      breaks = seq(15, 60, by = 5), 
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "Edad (Meses)",
      y = "Puntaje",
      color = "Estrato Socioeconómico",
      caption = caption_txt
    ) +
    
    guides(color = guide_legend(
      nrow = 2, 
      byrow = TRUE, 
      title.position = "top", 
      title.hjust = 0.5        
    )) +
    
    theme_quantificador() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 8, margin = margin(b = 2)),
      legend.text = element_text(size = 7.5),
      legend.key.width = unit(0.7, "cm"), 
      legend.margin = margin(t = -10),    

      axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "#333333", size = 9, hjust = 0.5),
      axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#333333", size = 9, hjust = 0.5),
      axis.text = element_text(size = 10, color = "black"),
      
      panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
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