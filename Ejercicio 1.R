
library(writexl)

# Leer los datos 
datos <- read.csv("datos_muestras.csv", sep=";")

# Calcular medias muestrales y intervalos de confianza
resultados <- data.frame(
  muestra = datos$muestra,
  media_muestral = rowMeans(datos[,2:10]),  # Calcular media para cada fila (X1 a X9)
  lower_ci = NA,
  upper_ci = NA,
  dentro_intervalo = NA
)

# Calcular intervalos de confianza
# Para nivel de confianza del 95%, z = 1.96
# n es el tamaño de cada muestra (9 en este caso)
z <- qnorm(0.05/2, lower.tail = FALSE)
n <- 9
sigma <- 4
error_estandar <- sigma/sqrt(n)

resultados$lower_ci <- resultados$media_muestral - z * error_estandar
resultados$upper_ci <- resultados$media_muestral + z * error_estandar

# Verificar si la media poblacional (μ = 50) está dentro del intervalo
resultados$dentro_intervalo <- resultados$lower_ci <= 50 & 50 <= resultados$upper_ci

# Redondear los resultados numéricos a 2 decimales
resultados$media_muestral <- round(resultados$media_muestral, 2)
resultados$lower_ci <- round(resultados$lower_ci, 2)
resultados$upper_ci <- round(resultados$upper_ci, 2)

# Mostrar los primeros resultados
head(resultados)

# Calcular el porcentaje de intervalos que contienen la media poblacional
porcentaje_exito <- mean(resultados$dentro_intervalo) * 100
cat("\nPorcentaje de intervalos que contienen la media poblacional:", 
    round(porcentaje_exito, 2), "%\n")



#  Gráfico de los intervalos
# Ordenar resultados por media muestral para mejor visualización
resultados <- resultados[order(resultados$media_muestral),]

# Crear el gráfico
par(mar=c(5,4,4,2) + 0.1)  # Ajustar márgenes
plot(resultados$media_muestral, 1:100, 
     xlim=c(min(resultados$lower_ci), max(resultados$upper_ci)),
     ylim=c(0, 101),
     pch=19,
     col=ifelse(resultados$dentro_intervalo, "blue", "red"),
     xlab="Valor",
     ylab="Número de intervalo",
     main="Intervalos de Confianza del 95%")

# Agregar líneas de los intervalos
for(i in 1:100) {
  lines(c(resultados$lower_ci[i], resultados$upper_ci[i]), 
        c(i,i),
        col=ifelse(resultados$dentro_intervalo[i], "blue", "red"))
}

# Agregar línea vertical para μ = 50
abline(v=50, col="darkgreen", lwd=2, lty=2)

legend("topright", 
       legend=c("Contiene μ", "No contiene μ", "μ = 50"),
       col=c("blue", "red", "darkgreen"),
       lty=c(1,1,2),
       pch=c(19,19,NA),
       cex=0.7,            
       pt.cex=0.7,        
       bty="y",       
       inset=c(0,0.0), 
       y.intersp=0.8) 
#Exportar los resultados a formato excel
write_xlsx(resultados, "resultados_intervalos_confianza.xlsx")