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

storytelling:
Hoy presentamos un análisis de datos orientado a toma de decisiones para una empresa minorista global con cuatro tiendas físicas: Múnich, Dubái, Londres y Nueva York. Trabajamos con un histórico de ventas semanales por tienda durante un año, más variables de contexto: temperatura, IPC (CPI), desempleo y precio del combustible, además de un indicador de semana festiva.

El objetivo fue transformar este histórico en criterios concretos de decisión para gerencia, respondiendo tres preguntas:

¿Qué tan diferente es el desempeño entre tiendas y entre semanas festivas versus no festivas?

¿Cómo se relacionan las ventas con variables de contexto (clima y macroeconomía)?

¿Qué acciones tácticas se desprenden para optimizar promociones, inventario y esfuerzo comercial?

1) Preparación y una aclaración clave del dato

Primero, estandarizamos nombres, construimos la variable Festivo/No festivo, la semana y un trimestre aproximado (Q1–Q4).

Una nota importante para interpretar correctamente: en esta base, la variable de “ventas totales” coincide exactamente con la suma de MarkDown1 a MarkDown5. En otras palabras, los “markdowns” aparecen como componentes de la venta, no como un “driver” independiente. Por eso, para explicar variaciones de ventas de manera útil para decisiones, el foco está en festividad, tienda y contexto macro/ambiental.

2) Hallazgos del EDA (qué está pasando en las ventas)

Cuando miramos la distribución de ventas semanales, vemos un patrón muy claro: la mayoría de las semanas se concentra en un rango medio, pero existe una cola larga con pocas semanas de ventas extremadamente altas.

La mediana de ventas semanales es aproximadamente 15.064.

El 50% central de semanas (entre percentil 25 y 75) cae entre 8.869 y 24.984.

El percentil 95 está cerca de 56.315, y el máximo llega a 134.774.

En términos ejecutivos: una parte relevante del resultado anual se juega en pocas semanas “pico”. Esto tiene implicancias directas en inventario, dotación y presupuesto comercial: si esas semanas se gestionan mal, el año completo sufre.

Comparación entre tiendas

Al comparar tiendas, aparece un diagnóstico contundente:

Londres es consistentemente la tienda de menor desempeño: su venta promedio semanal es cercana a 7.095.

Múnich queda en un nivel intermedio: promedio 20.766.

Dubái y Nueva York se ubican arriba: promedios 27.622 y 26.508, respectivamente.

En términos simples: Londres está estructuralmente rezagada, mientras que Múnich, Dubái y Nueva York compiten en una “liga” superior.

3) Semanas festivas vs no festivas (impacto comercial)

Luego comparamos semanas festivas versus no festivas.

Promedio en no festivo: ~18.346

Promedio en festivo: ~46.326

Diferencia: ~27.980 adicionales por semana (festivo vs no festivo)

Esta diferencia no es anecdótica: un t-test (Welch) entrega p ≈ 0,003, indicando que el aumento en semanas festivas es estadísticamente robusto, y el tamaño del efecto es muy alto (no es un cambio pequeño).

Además, el patrón se repite por tienda: incluso con ventas bajas, Londres también sube en festivos, pero sigue por debajo de las demás:

Festivo: Dubái ~61.092; Nueva York ~56.819; Múnich ~43.998; Londres ~23.395

No festivo: Dubái ~24.833; Nueva York ~23.982; Múnich ~18.830; Londres ~5.737

Mensaje ejecutivo: la temporada festiva es el “motor” del año y requiere planificación diferenciada.

4) Diferencias entre tiendas: ANOVA y comparación múltiple

Para confirmar formalmente las diferencias entre tiendas, aplicamos ANOVA:

Resultado: p ≈ 2,4×10⁻⁸ (muy significativo).
Esto implica que no todas las tiendas tienen la misma media de ventas.

Luego, con la comparación de Tukey:

Londres vende significativamente menos que Múnich, Dubái y Nueva York.

Entre Múnich, Dubái y Nueva York, no aparecen diferencias estadísticamente significativas entre sí.

Traducción para gerencia: el problema comparativo está concentrado en Londres; el resto de tiendas se mueve en un rango de desempeño similar.

5) Relaciones con variables de contexto: correlaciones y regresiones
Correlaciones

Al revisar correlaciones con ventas:

Temperatura vs ventas: ~–0,30 (moderada y negativa)

Precio combustible vs ventas: ~–0,23 (negativa)

IPC vs ventas: ~–0,19 (negativa)

Desempleo vs ventas: ~–0,12 (negativa)

Y aparece un punto crucial:

IPC y desempleo tienen correlación ~0,94, es decir, se mueven prácticamente juntos. Esto anticipa colinealidad en modelos con ambas variables.

Regresión simple: ventas ~ temperatura

Ajustamos un modelo simple para cuantificar el efecto:

Coeficiente de temperatura ≈ –434 por cada 1 grado (significativo, p < 0,001)

R² ≈ 0,087: la temperatura explica ~9% de la variación de ventas

Interpretación de negocio: semanas más calurosas tienden a vender menos, de manera consistente. No es el único driver, pero sí un factor útil para metas y planificación.

Regresión múltiple: ventas ~ temperatura + IPC + desempleo + combustible

Al incorporar contexto macro/ambiental, el modelo:

Es globalmente significativo (p < 0,001)

Sube a R² ≈ 0,162: estas variables explican ~16% de la variación

Resultados principales (signos coherentes con intuición):

Temperatura mantiene efecto negativo (≈ –248 por grado)

IPC entra negativo (≈ –342 por punto)

Combustible entra negativo (≈ –19.786 por unidad)

Desempleo aparece positivo, pero aquí hay que ser rigurosos: IPC y desempleo tienen VIF ~9,5–9,7, indicando colinealidad alta. En la práctica, es más correcto hablar de “entorno macro” conjunto, y no interpretar cada coeficiente por separado como causal.

Además, el test de heterocedasticidad sugiere que conviene reportar errores robustos o considerar transformaciones si el objetivo fuera inferencia más estricta.

Mensaje ejecutivo: cuando el contexto se tensiona (inflación y combustible altos) y cuando sube la temperatura, vender se vuelve más difícil; por tanto, metas y estrategia deben adaptarse por escenario.

6) Recomendaciones tácticas y decisiones para gerencia

A partir de los resultados, proponemos tres líneas de acción:

(1) Jugar a ganar en semanas pico y festivas

Las semanas festivas venden, en promedio, más del doble que las normales.

Decisión: priorizar inventario, dotación, logística y marketing en esas ventanas, incluyendo semanas previas y posteriores (efecto arrastre).

Objetivo: capturar el pico sin “sobre-promocionar” todo el año y destruir margen.

(2) Plan de recuperación específico para Londres

Londres está significativamente por debajo del resto de tiendas.

Decisión: abrir un diagnóstico local con foco en: mix de productos, pricing, ejecución en tienda, competencia, estacionalidad local y efectividad comercial.

Acción sugerida: pilotos controlados (campañas locales y ajustes de surtido) con medición antes/después.

(3) Incorporar clima y macroeconomía en forecast y metas

Temperatura y variables macro muestran efectos consistentes.

Decisión: integrar estas señales a un modelo de forecast y a un esquema de planificación por escenarios (por trimestre y por tienda).

Resultado esperado: metas más realistas y decisiones anticipadas sobre promociones o ajuste de costos cuando el contexto sea adverso.

7) Supuestos, límites y próximos pasos

Supuestos principales:

El año analizado es representativo (sin shocks extraordinarios).

Las mediciones son comparables entre tiendas.

Límites relevantes:

En esta base, “ventas” coincide con la suma de MarkDown1..5; para decisiones de pricing/promociones, falta separar claramente ventas base, descuentos y margen.

Próximos pasos recomendados:

Incorporar margen, rotación de inventario, stockouts, y canal online.

Agregar más años para robustez y estacionalidad.

Testear campañas con A/B testing o diseños cuasi-experimentales para estimar impacto real de acciones comerciales.

Con esto cerramos: el análisis muestra un negocio altamente dependiente de semanas pico, con un rezago estructural en Londres,
y con sensibilidad detectable al contexto climático y macroeconómico. La recomendación es priorizar ejecución en picos, levantar 
Londres con pilotos medibles e integrar señales externas en planificación y forecast.






