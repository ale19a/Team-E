# funcion para leer los csv de eventos de windows
leer <- function(x) { r <- read.csv(x, stringsAsFactors=FALSE, row.names = NULL); return(r); }

seguridad <- leer("Seguridad.csv")
sistemas <- leer("Sistema.csv")
instalacion <- leer("Instalacion.csv")
aplicacion <- leer("Aplicacion.csv")


print("plot sizes.png")
png(filename="sizes.png")
barplot(c(nrow(seguridad), nrow(sistemas), nrow(instalacion), nrow(aplicacion)), names.arg=c("Seguridad", "Sistemas", "Instalacion", "Aplicacion"))
dev.off()

library("dplyr")
# funcion para vagos
id_barplot <- function(df) {
  df.count <- count(df, Id..del.evento)
  ylim <- c(0, 1.1*max(df.count$n))
  x <- df.count[order(df.count$n, decreasing=T),]
  xx <- barplot(x$n, main="LOL", ylab="Cantidad", ylim = ylim, cex.axis = 1.2)
  text(x = xx, y = x$n, label = x$n, pos = 3, cex = 1.0, col = "red")
  axis(1, at = xx, labels = x$Id..del.evento, tick = F, las = 2, line = -0,5, cex.axis=1.2)
}

png(filename="aplicacion.id.png", width=1980, height = 1080)
id_barplot(aplicacion)
dev.off()
png(filename="sistema.id.png")
id_barplot(sistemas)
dev.off()
png(filename="instalacion.id.png")
id_barplot(instalacion)
dev.off()
png(filename="seguridad.id.png", width=1024, height = 768)
id_barplot(seguridad)
dev.off()
