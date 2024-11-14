# Datos obtenidos de: https://www.kaggle.com/datasets/valakhorasani/gym-members-exercise-dataset

# Carga de datos y cambio de nombres de las columnas a unos nombres más cortos
data <- read.csv("gym_members_exercise_tracking.csv")
data <- data[c(3,6,7,8,9,11,12,13,14,15)]
colnames(data) <- c("Weight", "Calories_Burned", "Water_Intake", "Experience", "Height", "Avg_BPM", "Session_Duration", "Fat_Percent", "Workout_Freq", "Body Mass Index")

result <- cor(as.matrix(data))

# Obtener todas las correlaciones en forma de pares de variables
adjacency_list <- which(upper.tri(result), arr.ind = TRUE)

# Creación de un data frame con los nombres de variables y sus correlaciones
df <- data.frame(
  var1 = rownames(result)[adjacency_list[,1]],
  var2 = colnames(result)[adjacency_list[,2]],
  correlation = result[adjacency_list]
)


# Visualización
# Para poder crear la visualización he seguido la siguiente guia: https://medium.com/@putri.a.purwono/visualizing-correlation-analysis-results-through-a-chord-diagram-using-the-circlize-package-on-r-6a2d76f65a6d
library(circlize)


col_fun_pos = colorRamp2(range(0:1), c("#FFEEEE", "#FF0000"), transparency = 0.5)
col_fun_neg = colorRamp2(range(-1:0), c("#f0feff", "#0017d1"), transparency = 0.5)

# Configurar el archivo de salida en alta resolución (300 DPI)
png("chord_diagram.png", width = 10, height = 10, units = "in", res = 300)

# Ajustar los márgenes (mar = c(abajo, izquierda, arriba, derecha))
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))

# Positive correlation
chordDiagram(df, 
             col = col_fun_pos, 
             grid.col = 1:10, 
             link.visible = df$correlation > 0,
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title("Positive correlations")

# Negative correlation
chordDiagram(df, 
             col = col_fun_neg, 
             grid.col = 1:10, 
             link.visible = df$correlation < 0,
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
title("Negative correlations")

# Cerrar el dispositivo gráfico para guardar la imagen
dev.off()