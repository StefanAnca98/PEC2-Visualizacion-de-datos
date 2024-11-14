# Datos obtenidos de: https://www.kaggle.com/datasets/muhammadroshaanriaz/students-performance-dataset-cleaned?resource=download
# Carga de datos y librerías
data <- read.csv("Cleaned_Students_Performance.csv")
library(dplyr)
library(ggplot2)

# Preparación de los datos en el formato adecuado para visualizar con ggplot
data_summary <- data %>%
  group_by(parental_level_of_education) %>%
  summarize(Total = mean(average_score))

# Visualización
ggplot(data_summary, aes(x = parental_level_of_education, y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Rendimiento escolar según nivel educativo de los padres") +
  xlab("Nivel educativo de los padres") +
  ylab("Puntuación promedio en examenes")