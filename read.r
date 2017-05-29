library("lubridate")
setClass("DateTime")
setAs("character", "DateTime", function(from) lubridate::dmy_hms(from))

#' Read windows events csv
#'
#' This is a wrapper around read.csv
#'
#' @param file csv file to read
#' @return Dataframe with the csv parsed
#'
leerwincsv <- function(file) {
  r <- read.csv(file, stringsAsFactors=FALSE
                , col.names = c("Level","Date","Origin","Id","Category","Description")
                , skip = 1
                , header = F
                , colClasses = c("factor", "DateTime", "factor", "integer", "factor", "character"));
  return(r);
}

readbunch <- function(files) {
    names <- files
    mylist <- list()
    for (i in seq_along(files)) {
        mylist[[i]] <- leerwincsv(files[i])
        attr(mylist[[i]], "name") <- names[i] # Add a name to the dataframe: Nice trick for later
    }
    return(mylist)
}

CSV <- readbunch(c("Seguridad.csv", "Sistema.csv", "Instalacion.csv", "Aplicacion.csv"))
# Nos los guardamos en una lista para trabajar mejor con ellos

print("plot sizes.png")
png(filename="sizes.png")
barplot(sapply(CSV,(function(x) nrow(x))), names.arg=sapply(CSV,(function(x) attr(x, "name", exact=T))))
dev.off()

library("dplyr")
# funcion para hacer los plots
id_barplot <- function(df) {
  df.count <- count(df, Id) # contar la cantidad de eventos por Id
  ylim <- c(0, 1.1*max(df.count$n))
  x <- df.count[order(df.count$n, decreasing=T),] # df.count ordenado
  xx <- barplot(x$n, main=attr(df, "name", exact=T), ylab="Cantidad", ylim = ylim, cex.axis = 1.2)
  text(x = xx, y = x$n, label = x$n, pos = 3, cex = 1.0, col = "red")
  axis(1, at = xx, labels = x$Id, tick = F, las = 2, line = -0,5, cex.axis=1.2)
}

png(filename="id.count.png", width=1920, 1080)
par(mfrow=c(2,2))
lapply(CSV, id_barplot)
#id_barplot(sistemas) # FIXME: Demasiados id
#id_barplot(instalacion) # FIXME: los id los pone entre las barras
#id_barplot(seguridad)
#id_barplot(aplicacion) # FIXME: Demasiados id
dev.off()

png(filename="id.count.top3.png", width=1920, 1080)
par(mfrow=c(2,2))
lapply(CSV, function(df) { 
  df.count <- count(df, Id) # contar la cantidad de eventos por Id
  ylim <- c(0, 1.1*max(df.count$n))
  x <- df.count[order(df.count$n, decreasing=T),] # df.count ordenado
  x <- x[1:3,] # TOP 3
  xx <- barplot(x$n, main=attr(df, "name", exact=T), ylab="Cantidad", ylim = ylim, cex.axis = 1.2)
  text(x = xx, y = x$n, label = x$n, pos = 3, cex = 1.0, col = "red")
  axis(1, at = xx, labels = x$Id, tick = F, las = 2, line = -0,5, cex.axis=1.2)
})
dev.off()

