#' A package for egocentric measures in Social Network Analysis
#' 
#' A package for egocentric measures in Social Network Analysis
#' 
#' \tabular{ll}{ Package: \tab egonet\cr Type: \tab Package\cr Version:
#' \tab 1.2\cr Date: \tab 2018-10-18\cr License: GPL (>= 2) }
#' 
#' @name egonet-package
#' @aliases egonet-package egonet
#' @docType package
#' @author Andrea Sciandra, Livio Finos
#' 
#' Maintainer: livio finos <livio@stat.unipd.it>
#' @keywords package
#' @examples

## This is a list of file' names containing the ego-network
ff <- c("q1.csv","q2.csv","q3.csv")
files  <- paste("http://www.egonet.associazionerospo.org/egonetdata/",ff,sep="")
names(files) <- ff

##if all your data are in a directory (eg. egonets, containing all files with the same extension), 
##you can easily get this list with the following commands
# ff <- dir("./egonets",pattern=".csv")
# files  <- paste("./egonets/",ff,sep="")
# names(files) <- ff


# Here is a dataset with demographic informations and the names of files containing the ego-network
#we now want to add network indeces to this data.frame
data <- data.frame( gender=c("F","F","M"), age =(2:4)*10, filename=ff)
data


#import all the egonets
mats <- lapply(files,read.egonet)

#compute (default) indices on the first subject
index.egonet(mats[[1]])

#compute indices on all adjacency matrices
idx <- sapply(mats,index.egonet)

#reshape idx in a data.frame and add the file names
idx <- as.data.frame(t(idx))
idx <- cbind(idx,filename=rownames(idx))

#now merge demographic informations with the indices
data <- merge(data,idx,by="filename")
data

#Compute the \code{effsize} and \code{constraint} to the restricted network with "EGO" and nodes with "P" in the name (i.e. the relatives social circle)
idx <- sapply(mats,index.egonet, subset = "P",index = c("effsize","constraint"))
rownames(idx) <- paste(rownames(idx),"P",sep=".")
idx <- as.data.frame(t(idx))
idx <- cbind(idx,filename=rownames(idx))

#and merge them to the dataset
data <- merge(data,idx,by="filename")
data
