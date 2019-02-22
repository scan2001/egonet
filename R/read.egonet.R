#' Reads egocentric graph in table format and creates a matrix suitable for SNA. 
#' 
#' Reads egocentric graph in table format and creates a matrix suitable for SNA
#' Reads text table and creates ego-centric graph from it, with the ego actor in the first row and column. The result is a square matrix with common input/output connection' names.
#' 
#' @aliases read.egonet
#' @param file the name of the file which the data are to be read from. It must be a square matrix, with node's names in the first row and column. See also file of read.table
#' @param sep as in read.table
#' @param dec as in read.table
#' @param ego.name column and row name given to the ego subject. The default is \code{EGO}
#' 
#' @return  A square matrix containing the ego-centric network of the data in the file.
#' 
#' Per tutte le altre funzioni, l'output e' un \code{vector} della stessa
#' lunghezza di \code{testo} ma con testi normalizzati.
#' @note %% ~~further notes~~
#' @author Livio Finos, Andrea Sciandra
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' dat <- read.egonet(system.file("extdata", "q1.csv", package="ARI"))
#' dat
#' 
#' @export read.egonet
read.egonet <- function(file, sep=",", dec=".", ego.name="EGO") {
dat <- read.csv(file, header=T, sep= sep )
dat <- dat[!is.na(dat[,ego.name]), , drop = FALSE]

rownms <- dat[,1]
colnms <- rownms ## rows ans columns are forced to have the same order
dat <- as.matrix(dat[,-1, drop = FALSE])
rownames(dat) <- rownms
colnames(dat) <- colnms

idego <- which(rownames(dat)==ego.name)
dat <- dat[c(idego,setdiff(1:dim(dat)[1],idego)), , drop = FALSE]
idego <- which(colnames(dat)==ego.name)
dat <- dat[,c(idego,setdiff(1:dim(dat)[1],idego)), drop = FALSE]

# make dat a square matrix with common input/output connection' names
keep <- intersect(rownames(dat), colnames(dat))
dat <- dat[keep,keep, drop = FALSE]
dat
}
