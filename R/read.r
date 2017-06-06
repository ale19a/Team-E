library("lubridate")
library("dplyr")

setClass("DateTime")
setAs("character", "DateTime", function(from) lubridate::dmy_hms(from))

#' Read windows events csv
#'
#' This is a wrapper around read.csv
#'
#' @param file csv file to read
#' @return Dataframe with the csv parsed
#' @export
#'
leerwincsv <- function(file) {
  r <- read.csv(file, stringsAsFactors=FALSE
                , col.names = c("Level","Date","Origin","Id","Category","Description")
                , skip = 1
                , header = F
                , colClasses = c("factor", "DateTime", "factor", "integer", "factor", "character"));
  return(r);
}

#' Read a list of windows events csv using leerwincsv
#'
#' Returns a list with all the dataframes.
#' Each dataframe has the attribute name with its name
#'
#' @param files vector of csv to read using leerwincsv
#' @return list of all the read dataframes
#' @export
#'
readbunch <- function(files) {
    mylist <- list()
    for (i in seq_along(files)) {
        mylist[[i]] <- leerwincsv(files[i])
        attr(mylist[[i]], "name") <- files[i] # Add a name to the dataframe: Nice trick for later
    }
    names(mylist) <- files # nombres para el CSV
    return(mylist)
}

#' Wrapper para barplot "bonitos"
#'
#' @param y vector con los valores para hacer barplot
#' @param x vector con los nombres de los valores para mostrar en el eje de las x
#' @param yname Nombre para las y
#' @param xname Nombre para las x
#' @param col Colores para las columnas
#' @examples
#'  mi_barplot(c(1,10,2), c("A", "B", "C"), yname="Cantidad", xname="Cantidad de cada tipo")
#' @export
mi_barplot <- function(y, x, yname = "", xname = "", col = NULL) {
    ylim <- c(0, 1.1*max(y))
    xx <- barplot(y, main=xname, ylab=yname, ylim = ylim, cex.axis = 1.2, col = col)
    text(x = xx, y = y, label = y, pos = 3, cex = 1.0, col = "black")
    axis(1, at = xx, labels = x, tick = F, las = 2, line = -0.5, cex.axis=1.2)
}


#' Invisilbe lapply
#'
#' @description lapply without any output.
#'
#' @param X iterable of elements
#' @param FUN function to apply to each element
#' @param ... lapply other params
#'
#' @return NULL
#' @export
#' @seealso lapply
ilapply <- function(X, FUN, ...) { invisible(lapply(X, FUN, ...)) }
