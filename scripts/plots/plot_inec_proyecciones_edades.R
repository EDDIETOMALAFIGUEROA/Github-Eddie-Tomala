# ============================================================
# plot_inec_proyecciones_edades.R
# Genera un gráfico de líneas sobre la evolución estructural 
# de la población ecuatoriana por grupos de edad (1950-2050)
# Requiere: data/raw/inec/Proyecciones/tabul_nac_edad_sim_1950-2050.xlsx
# Guarda:   outputs/figures/evolucion_proyecciones_inec.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_inec_proyecciones_edades.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "ragg", "stringr", "ggtext", "readxl"))

input_path <- "data/raw/inec/Proyecciones/tabul_nac_edad_sim_1950-2050.xlsx"
out_path <- "outputs/figures/evolucion_proyecciones_inec.png"

poblacion_ancha <- readxl::read_excel(
  input_path, 
  sheet = "población_ambos_sexos", 
  range = "B18:CY118", 
  col_names = FALSE
)
names(poblacion_ancha) <- c("edad", 1950:2050)

datos_composicion <- poblacion_ancha %>%
  pivot_longer(cols = -edad, names_to = "anio", values_to = "poblacion") %>%
  mutate(
    anio = as.numeric(anio),
    edad = as.numeric(edad)
  ) %>%
  mutate(
    grupo_edad = case_when(
      edad < 15 ~ "0-14 años",
      edad >= 15 & edad <= 24 ~ "15-24 años",
      edad >= 25 & edad <= 54 ~ "25-54 años",
      edad >= 55 & edad <= 64 ~ "55-64 años",
      edad >= 65 ~ "65 años y más"
    )
  ) %>%
  group_by(anio, grupo_edad) %>%
  summarise(pob_grupo = sum(poblacion, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(porcentaje = pob_grupo / sum(pob_grupo)) %>%
  ungroup() %>%
  mutate(
    grupo_edad = factor(grupo_edad, levels = c("0-14 años", "15-24 años", "25-54 años", "55-64 años", "65 años y más"))
  )

title_raw <- "La proyección a 2050 en Ecuador confirma menos niños y más jubilados"
subtitle_raw <- "Proporción de cada grupo etario respecto a la población total (1950 - 2050)"

caption_raw <- paste0(
  "Fuente: Instituto Nacional de Estadística y Censos (INEC), Proyecciones poblacionales. ",
  "Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE. ",
  "Las trayectorias reflejan el peso relativo de cada segmento etario sobre el total nacional integrando los registros históricos con las proyecciones hacia 2050."
)
title_txt <- stringr::str_wrap(title_raw, width = 45)
subtitle_txt <- stringr::str_wrap(subtitle_raw, width = 55)
caption_txt <- stringr::str_wrap(caption_raw, width = 75)

palette_color <- c(
  "0-14 años" = "#9B59B6",
  "15-24 años" = "#E74C3C",
  "25-54 años" = "#F39C12",
  "55-64 años" = "#3498DB",
  "65 años y más" = "#2ECC71"
)

build_chart <- function() {
  ggplot(datos_composicion, aes(x = anio, y = porcentaje, color = grupo_edad)) +
    
    geom_hline(yintercept = 0, color = "#222222", linewidth = 0.8) +
    geom_line(linewidth = 1.2) +
    
    scale_color_manual(values = palette_color) +
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1), 
      limits = c(0, max(datos_composicion$porcentaje) * 1.1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(
      breaks = seq(1950, 2050, by = 10),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    
    guides(color = guide_legend(ncol = 3, byrow = TRUE, keywidth = unit(1.5, "lines"), keyheight = unit(0.5, "lines"))) +
    
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL, 
      y = "Porcentaje de la Población",
      color = NULL, 
      caption = caption_txt
    ) +
    
    theme_quantificador() +
    theme(
      axis.title.y = element_text(margin = margin(r = 10), face = "bold", color = "#333333", size = 9),
      axis.text = element_text(size = 10, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(b = 10)), 
      
      legend.position = "top",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 8, face = "bold"),
      
      legend.margin = margin(t = 5, r = 0, b = 0, l = -35),
      legend.spacing.y = unit(0.1, "cm"),
      
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