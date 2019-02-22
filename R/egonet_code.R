# Reads ego-centred network in table format and creates a matrix suitable for SNA
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


# Burt's effective size index
.effsize <- function(dati, ego.name){
  n <- dim(dati)[1]
  if(n < 2) return(NaN)
  Sj <- 0
  for( y in 2:n)          Sj <- Sj + (dati[ego.name,y] + dati[y,ego.name])
  ris <- 0
  for ( j in 2: n){
    cont1 <- setdiff(1:n,j)
    Vetmax <- rep(NA,(length(cont1)))
    for (k in setdiff(1:n,j)){ Vetmax[k] <- (dati[j,k] +dati[k,j])}
    massimo <- max(Vetmax,na.rm=T)
    sumPM <- 0
    for(f in setdiff(2:n,j) ){ #f=q in formula originale
      Piq <- (dati[ego.name, f] + dati[f,ego.name])/ Sj
      Mjq <- (dati[j,f] + dati[f,j])/ massimo  
      sumPM <- sumPM +Piq * Mjq    
    }
    ris <- ris + (1 - (sumPM))
  }
  ris    
}
    
# Burt's aggregate constraint index
.constraint <- function(dati, ego.name) {
  idego <- which(rownames(dati)==ego.name)
  n <- dim(dati)[1]
  S <- rep(0,n)
  for( i in 1:n)  for( y in setdiff(1:n,i))          S[i] <- S[i] + (dati[i,y] + dati[y,i])
 
  Pij <- dati
  Pij[,] <- NA
  for(i in 1:n ){
    for(j in setdiff(1:n,i) ){
      Pij[i,j] <- (dati[i,j] + dati[j,i])/ S[i]
    }
  }

  pp <- rep(0,n)
  for (j in setdiff(1:n,idego) )
    for (q in setdiff(1:n,c(idego,j) ) )
      pp[j] <- pp[j] + Pij[idego,q]*Pij[q,j]
  sum((Pij[idego,] + pp)^2,na.rm=T)
}


#' @name index.egonet
#' @aliases index.egonet
#' @title  Compute Burt's indexes (effective size and aggregate constraint) and other measure from sna package
#' @description Compute Burt's indexes (effective size and aggregate constraint) and other measure from  \code{\link[sna:sna]{sna}} package.  
#' It also allow to restrict the analysis to one social circle identified by the name of the alters.
#' @param dat graph to be analyzed having row and column names in the same order. 
#' @param index is a list containing prespecified index functions. 
#'     \code{effsize} and  \code{constraint} are defined in as in Burt (1992) \\
#'      \code{outdegree} and \code{indegree} make a call of \code{degree(dat, cmode="outdegree")} and \code{degree(dat, cmode="indegree")} and take the element corresponding to \code{ego.name}\\
#'      \code{ego.gden} applies \code{gden} in a matrix without \code{ego.name} connections.  \\
#'      Other indeces are call of the homonimous functions of package \code{\link[sna:sna]{sna}}. \\ 
#'      If the generic element \code{is.call(index[[h]])} is \code{TRUE}, it will be evaluated in the global environment: \code{eval(index[[h]],envir =.GlobalEnv)}  (see also example below)
#' @param subset Restricts the analysis to the nodes with at least one among the elements of \code{vector} in the name. If \code{vector = NULL} - the default - all the nodes of the graph are comprised.
#' @param ego.name column and row name given to the ego subject. The default is \code{EGO} 
#' @return The requested indices.
#' @references Burt, R.S. (1992) ''Structural Holes. The Social Structure of Competition'', Cambridge (MA), Harvard University Press.
#' @author A. Sciandra, F. Gioachin, L. Finos  
#' @export index.egonet
#' @import sna
#' @examples
#'     # make a toy dataset
#'      egomat <-  matrix(c(0,1,1,1,0,0,1,0,0),3,3)
#'    colnames(egomat) <- rownames(egomat) <- c("EGO", "1P", "1A")
#'    
#'    index.egonet(egomat)
#'    
#'    # an example with self defined index 
#'    my.outdegree <- function(dat) degree(dat,cmod="outdegree")[1]
#'    index.egonet(egomat,index=c("effsize","constraint",
#'    "outdegree","indegree","efficiency", "centralization", 
#'    "gden", "ego.gden", my.outdegree=call("my.outdegree",dat=egomat)))
#'    
#'    #Restricts the \code{outdegree} and \code{efficiency} to "EGO" and nodes with "P" in the name
#'    index.egonet(egomat,index=c("outdegree","efficiency"),subset="P")
#'    


index.egonet <- function( dat, index= list("effsize","constraint","outdegree","indegree","efficiency", "hierarchy", "centralization", "gden", "ego.gden"), subset=NULL, ego.name="EGO"){
  if(!is.null(subset)) dat <- dat[unique(c(ego.name, rownames(dat)[unlist(sapply(subset, grep, rownames(dat)))])),unique(c(ego.name, colnames(dat)[unlist(sapply(subset, grep, colnames(dat)))])), drop=FALSE]
  
  if (is.null(names(index))) names(index) <- index
  index <- as.list(index)
  ego.id <- which(colnames(dat)==ego.name)
  ris <- rep(NA,length(index))
  names(ris) <- names(index)
  names(ris)[names(ris)==""] <- index[names(ris)==""]
  # require("sna")
  # library(sna)
  for(h in 1:length(index)){
    if(is.call(index[[h]])) {
	    ris[h] <- eval(index[[h]],envir =.GlobalEnv)
	    names(ris)[h] <- names(index)[h] }
    else  ris[h] <- switch (unlist(index[[h]]),
      outdegree = {grado <- sna::degree(dat, cmode="outdegree"); as.matrix(grado[ego.id])},
	  indegree = {grado <- sna::degree(dat, cmode="indegree"); as.matrix(grado[ego.id])},
      efficiency = sna::efficiency(dat),
      hierarchy = hierarchy(dat),
      centralization = centralization(dat,degree),
      gden = gden(dat), 
      ego.gden = ifelse(!is.null(dim(dat[-ego.id,-ego.id] )), gden(dat[-ego.id,-ego.id,drop=FALSE]),NaN),
      effsize = .effsize(dat,ego.name),
      constraint =.constraint(dat,ego.name) )}
  return(ris)
}
