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
