############################################################
# PROYECTO FINAL – OPCIÓN 1: EMPRESA MINORISTA GLOBAL
# Análisis de datos y toma de decisiones (RStudio)
############################################################

# 0) PAQUETES -------------------------------------------------------------
library(readxl)
library(tidyverse)
library(car)
library(lmtest)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
# 1) CARGA Y PREPARACIÓN DE LOS DATOS -------------------------------------

datos_raw <- read_excel("~/Metodos Estadisticos 3/Analisis de datos y toma de decisiones/Prueba Proyecto/Retail_Sales (1).xlsx")

datos <- datos_raw %>%
  clean_names() %>% 
  # AQUÍ renombramos las variables macro para que EXISTAN fuel_price e ipc
  rename(
    fuel_price = gasolina_precio,
    ipc        = cpi
  ) %>%
  mutate(
    # Tienda como factor con etiquetas
    tienda = factor(
      tienda,
      levels = c(1, 2, 3, 4),
      labels = c("Munich", "Dubai", "Londres", "Nueva_York")
    ),
    
    # Variable festivo/no festivo (robusta a 0/1, "0"/"1", TRUE/FALSE)
    is_holiday = case_when(
      es_navidad %in% c(1, "1", TRUE)  ~ "Festivo",
      es_navidad %in% c(0, "0", FALSE) ~ "No_festivo",
      TRUE                             ~ NA_character_
    ),
    is_holiday = factor(is_holiday, levels = c("No_festivo", "Festivo")),
    
    # Rebajas y ventas (por claridad)
    md1         = mark_down1,
    md2         = mark_down2,
    md3         = mark_down3,
    md4         = mark_down4,
    md5         = mark_down5,
    total_sales = total_ventas,
    
    # Semana
    semana   = senaba,
    
    # Rebaja total
    md_total = md1 + md2 + md3 + md4 + md5,
    
    # Trimestre aproximado (según semana calendario)
    trimestre = case_when(
      semana >= 1  & semana <= 13 ~ "Q1",
      semana >= 14 & semana <= 26 ~ "Q2",
      semana >= 27 & semana <= 39 ~ "Q3",
      semana >= 40 & semana <= 52 ~ "Q4",
      TRUE ~ NA_character_
    ),
    trimestre = factor(trimestre, levels = c("Q1","Q2","Q3","Q4"))
  )

# Chequeo rápido
str(datos)
names(datos)


summary(select(
  datos,
  total_sales, md_total, temperatura,
  fuel_price, ipc, desempleo
))

##Analisis EDA ##

#1) PREGUNTA:
#Como se distribuyen las ventas semanales durante el año?

# Histograma ventas
hist(datos$total_sales,
     breaks = 30,
     main = "Distribución de ventas totales semanales",
     xlab = "Ventas totales semanales")


#2) PREGUNTA:
#¿Las tiendas tienen niveles de ventas comparables?

# Boxplot por tienda
boxplot(datos$total_sales ~ datos$tienda,
        main = "Ventas totales por tienda",
        xlab = "Tienda", ylab = "Ventas totales")

#3) PREGUNTA:
#¿Se observan diferencias descriptivas entre semanas festivas y no festivas?

# Boxplot festivo vs no festivo
boxplot(datos$total_sales ~ datos$is_holiday,
        main = "Ventas: semanas festivas vs no festivas",
        xlab = "Tipo de semana", ylab = "Ventas totales")



#4) PREGUNTA:
#¿Como evolucionan las ventas a lo largo del año en cada tienda?

# Evolución semanal por tienda
ggplot(datos, aes(x = semana, y = total_sales,
                  colour = tienda, group = tienda)) +
  geom_line(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Tendencia semanal de ventas por tienda",
       x = "Semana", y = "Ventas totales")


#Testeo de resultados
aggregate(total_sales ~ is_holiday, data = datos, mean)
#--------------------------------------------------------------------

#--------------------------------------------------------------------


#5) PREGUNTA:
#¿Que relaciones lineales basicas existen entre ventas y variables, seran 
#positivas(ambas suben) o negativas(baja una y aumenta otra)?

# Correlaciones
num_vars <- datos %>%
  select(total_sales, md_total, temperatura,
         fuel_price, ipc, desempleo)

cor(num_vars, use = "pairwise.complete.obs")



cor_mat <- cor(num_vars, use = "pairwise.complete.obs")

cor_long <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "corr"
  )

ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = round(corr, 2)), size = 3) +
  scale_fill_gradient2(
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    name = "Correlación"
  ) +
  labs(
    title = "Mapa de calor de correlaciones",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#6) PREGUNTA:
#¿La tempreatura tiene un efecto lineal sobre las ventas?

# Regresión simple
reg_temp <- lm(total_sales ~ temperatura, data = datos)
stargazer(reg_temp, title = "Regresion Simple", type = "text")
summary(reg_temp)


ggplot(datos, aes(x = temperatura, y = total_sales)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre temperatura y ventas semanales",
    x = "Temperatura",
    y = "Ventas totales semanales"
  ) +
  theme_minimal()



#7) PREGUNTA:
#¿Que tan bien explican las variables macro y la temperatura las ventas en 
#conjunto?

# Regresión múltiple
reg_mult <- lm(
  total_sales ~ temperatura + ipc + desempleo + fuel_price,
  data = datos
)
summary(reg_mult)
vif(reg_mult)
stargazer(reg_mult, title = "Regresion Multiple", type = "text")



datos_reg_mult <- datos %>%
  mutate(
    fitted_mult    = fitted(reg_mult),
    residuals_mult = resid(reg_mult)
  )

# 7.1 Observado vs predicho
ggplot(datos_reg_mult, aes(x = fitted_mult, y = total_sales)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Ventas observadas vs predichas (modelo múltiple)",
    x = "Ventas predichas por el modelo",
    y = "Ventas observadas"
  ) +
  theme_minimal()

# 7.2 Residuos vs predicho
ggplot(datos_reg_mult, aes(x = fitted_mult, y = residuals_mult)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.6) +
  labs(
    title = "Residuos vs valores predichos",
    x = "Ventas predichas",
    y = "Residuos"
  ) +
  theme_minimal()




#8) PREGUNTA:
#¿Las semanas festivas venden distinto que las semanas no festivas?

# t-test festivo vs no festivo
t_festivo <- t.test(total_sales ~ is_holiday, data = datos)
t_festivo


ggplot(datos, aes(x = is_holiday, y = total_sales)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size  = 3) +
  labs(
    title = "Ventas en semanas festivas vs no festivas",
    x = "Tipo de semana",
    y = "Ventas totales semanales"
  ) +
  theme_minimal()







#9) PREGUNTA:
#¿Las tiendas tienen promedios de ventas distintas entre si?
# ANOVA entre tiendas
mod_anova_tienda <- aov(total_sales ~ tienda, data = datos)
leveneTest(total_sales ~ tienda, data = datos)
summary(mod_anova_tienda)
TukeyHSD(mod_anova_tienda)


ggplot(datos, aes(x = tienda, y = total_sales)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size  = 3) +
  labs(
    title = "Distribución de ventas por tienda",
    x = "Tienda",
    y = "Ventas totales semanales"
  ) +
  theme_minimal()




#-------------------------------------------------------------------------------







