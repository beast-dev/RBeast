.hide_until_formatted_properly <- function() {

###########
# PURPOSE #
###########
# Read BEAST-style annotated tree, drop tip(s), and write back to file
#
# Builds on function posted by Liam Revell at
# http://blog.phytools.org/2011/07/writing-phylo-object-to-newick-string.html,
# APE's drop.tip function and parts of the write.nexus function from APE are
# used for writing to file.
#
# This script comes with no guarantees. Test carefully and use at own risk. But feel free to
# treat me on a beer should we ever meet.
#
# Happy using,
# Bram Vrancken

############
# PACKAGES #
############
library(OutbreakTools)
library(geiger)
library(ape)

##################
# USER SPECIFIED #
##################
#toy example:
setwd("...")
inTREE = "example.tre"
outTREE <- "example_out.tre"

toDropTip <- "XXX"

############
# FUNCTION #
############

drop.tip.annotated.tree <- function(phy, tipNames, trim.internal = TRUE, root.edge = 0, subtree = FALSE) {

  if (!inherits(phy, "phylo")) {stop('object "phy" is not of class "phylo"')}

  Ntip <- length(phy$tip.label)
  ## find the tips to drop:
  if (is.character(tipNames)){
    tip <- which(phy$tip.label %in% tipNames)
  } else {
    stop('tips must be of type CHARACTER')
  }
  if (any(tip > Ntip)){
    stop("some tip numbers were higher than the number of tips")
  }

  rooted = ape::is.rooted(phy)
  if (!rooted && subtree) {
    phy <- ape::root(phy, (1:Ntip)[-tip][1])
    root.edge <- 0
  }

  phy <- reorder(phy) # ensure it is in cladewise order
  NEWROOT <- ROOT <- Ntip + 1
  Nnode <- phy$Nnode
  Nedge <- dim(phy$edge)[1]

  wbl <- !is.null(phy$edge.length) # test for at at least 2 taxa
  edge1 <- phy$edge[, 1] # local copies
  edge2 <- phy$edge[, 2] #
  keep <- !logical(Nedge)
  #phy$root.annotation perhaps needs an update!
  parentNodeNrIndex <- which(phy$edge[,2] == tip)
  parentNode <- phy$edge[parentNodeNrIndex,1]
  rootUpdate <- geiger::is.root(node = parentNode, phy = phy)

  if(rootUpdate){
    #new root:
    firstNodeEdgeIndexes <- which(phy$edge[,1] == parentNode)
    newRootNodeEdgeIndex <- firstNodeEdgeIndexes[which(!(firstNodeEdgeIndexes %in% parentNodeNrIndex))]

    rootAnnots <- names(phy$root.annotation)
    nodeAnnots <- names(phy$annotation[[newRootNodeEdgeIndex]])
    indexes <- which(nodeAnnots %in% rootAnnots)

    for (a in 1:length(indexes)){
      currentAnnot <- nodeAnnots[indexes[a]]
      phy$root.annotation[[currentAnnot]] <- phy$annotations[[newRootNodeEdgeIndex]][currentAnnot][[1]]
    }
  }


  ## delete the terminal edges given by `tip', and also use for annotations:
  keep[match(tip, edge2)] <- FALSE

  if (trim.internal) {
    ints <- edge2 > Ntip
    ## delete the internal edges that do not have anymore
    ## descendants (ie, they are in the 2nd col of `edge' but
    ## not in the 1st one)
    repeat {
      sel <- !(edge2 %in% edge1[keep]) & ints & keep
      if (!sum(sel)) break
      keep[sel] <- FALSE
    }
    if (subtree) {
      ## keep the subtending edge(s):
      subt <- edge1 %in% edge1[keep] & edge1 %in% edge1[!keep]
      keep[subt] <- TRUE
    }
    if (root.edge && wbl) {
      degree <- tabulate(edge1[keep])
      if (degree[ROOT] == 1) {
        j <- integer(0) # will store the indices of the edges below the new root
        repeat {
          i <- which(edge1 == NEWROOT & keep)
          j <- c(i, j)
          NEWROOT <- edge2[i]
          degree <- tabulate(edge1[keep])
          if (degree[NEWROOT] > 1) break
        }
        keep[j] <- FALSE
        if (length(j) > root.edge) j <- 1:root.edge
        NewRootEdge <- sum(phy$edge.length[j])
        if (length(j) < root.edge && !is.null(phy$root.edge))
          NewRootEdge <- NewRootEdge + phy$root.edge
        phy$root.edge <- NewRootEdge
      }
    }
  }

  if (!root.edge) phy$root.edge <- NULL

  ## drop the edges and annotations:
  phy$edge <- phy$edge[keep, ]

  if (wbl){
    phy$edge.length <- phy$edge.length[keep]
  } #end if(wbl)

  ## find the new terminal edges (works whatever 'subtree' and 'trim.internal'):
  TERMS <- !(phy$edge[, 2] %in% phy$edge[, 1])

  ## get the old No. of the nodes and tips that become tips:
  oldNo.ofNewTips <- phy$edge[TERMS, 2]

  #drop the annotations: use the copy of the old edge matrix, edge2
  toKeepAnnotations <- which(edge2 %in% oldNo.ofNewTips)
  phy$annotations <- phy$annotations[toKeepAnnotations]

  ## in case some tips are dropped but kept because of 'subtree = TRUE':
  if (subtree) {
    i <- which(tip %in% oldNo.ofNewTips)
    if (length(i)) {
      phy$tip.label[tip[i]] <- "[1_tip]"
      tip <- tip[-i]
    }
  }

  n <- length(oldNo.ofNewTips) # the new number of tips in the tree

  ## the tips may not be sorted in increasing order in the
  ## 2nd col of edge, so no need to reorder $tip.label
  phy$edge[TERMS, 2] <- rank(phy$edge[TERMS, 2])
  phy$tip.label <- phy$tip.label[-tip]

  ## make new tip labels if necessary:
  if (subtree || !trim.internal) {
    ## get the numbers of the nodes that become tips:
    node2tip <- oldNo.ofNewTips[oldNo.ofNewTips > Ntip]
    new.tip.label <- if (subtree) {
      paste("[", N[node2tip], "_tips]", sep = "")
    } else {
      if (is.null(phy$node.label)) rep("NA", length(node2tip))
      else phy$node.label[node2tip - Ntip]
    }
    if (!is.null(phy$node.label))
      phy$node.label <- phy$node.label[-(node2tip - Ntip)]
    phy$tip.label <- c(phy$tip.label, new.tip.label)
  }

  ## update node.label if needed:
  if (!is.null(phy$node.label))
    phy$node.label <- phy$node.label[sort(unique(phy$edge[, 1])) - Ntip]

  phy$Nnode <- dim(phy$edge)[1] - n + 1L # update phy$Nnode

  ## The block below renumbers the nodes so that they conform
  ## to the "phylo" format -- same as in root()
  newNb <- integer(n + phy$Nnode)
  newNb[NEWROOT] <- n + 1L
  sndcol <- phy$edge[, 2] > n
  ## executed from right to left, so newNb is modified before phy$edge:
  phy$edge[sndcol, 2] <- newNb[phy$edge[sndcol, 2]] <-
    (n + 2):(n + phy$Nnode)
  phy$edge[, 1] <- newNb[phy$edge[, 1]]
  storage.mode(phy$edge) <- "integer"
  ape::collapse.singles(phy)

  #for testing purposes:
  #    f <- ape::collapse.singles(phy)


}

#get the newick string with annotations:
get.annotated.newick.string<-function(tree){
  tree<-ape::reorder.phylo(tree,"cladewise")
  n<-length(tree$tip)
  #prepare for writing:
  string<-vector(); string[1]<-"("
  j<-2

  for(i in 1:nrow(tree$edge)){ # 3

    nodeNr <- tree$edge[i,2]

    if (nodeNr<=n){ #test whether we start from a tip

      #for NEXUS format: need to replace the taxon name with the corresponding node nr
      #string[j]<-tree$tip.label[tree$edge[i,2]]
      string[j]<-nodeNr
      j<-j+1

      #insert colon and start of annotation info:
      string[j]<-paste(":[&", collapse=""); j<-j+1
      #fetch annotations
      traitIndex <- i
      annotCount <- length(tree$annotations[[i]])

      for (a in 1:annotCount){
        annotName <- names(tree$annotations[[i]])[a]
        annotValue <- tree$annotations[[i]][a][[1]]
        if(is.numeric(annotValue)){
          string[j]<-paste(annotName, "=", annotValue ,collapse="")
        } else {
          string[j]<-paste(annotName, '="', annotValue, '"' ,collapse="")
        }
        j<-j+1
        if (a < annotCount){
          string[j]<-paste("," ,collapse="")
          j<-j+1
        }
      }

      #plug in branch length info
      string[j]<-paste(c("]",round(tree$edge.length[i],10)), collapse="")
      j<-j+1

      #fetch the annot info up to the root
      v<-which(tree$edge[,1]==tree$edge[i,1]); k<-i

      while(length(v)>0&&k==v[length(v)]){

        string[j]<-")"; j<-j+1

        if(!(geiger::is.root(node = tree$edge[k,1], phy = tree))){
          w<-which(tree$edge[,2]==tree$edge[k,1])

          #when immediately landing at root: w = integer(0)
          #text <- paste("i:", i, " k:", k, " w:", w, sep="")
          #print(text)


          nodeNr <- tree$edge[w,2]
          #insert colon and start of annotation info:
          string[j]<-paste(":[&", collapse=""); j<-j+1

          #fetch annotations:
          annotCount <- length(tree$annotations[[w]])

          for (a in 1:annotCount){
            annotName <- names(tree$annotations[[w]])[a]
            annotValue <- tree$annotations[[w]][a][[1]]
            if(is.numeric(annotValue)){
              string[j]<-paste(annotName, "=", annotValue ,collapse="")
            } else {
              string[j]<-paste(annotName, '="', annotValue, '"' ,collapse="")
            }
            j<-j+1
          }
          #plug in branch length info
          string[j]<-paste(c("]",round(tree$edge.length[w],10)), collapse="")
          j<-j+1

        } # k not root node
        v<-which(tree$edge[,1]==tree$edge[w,1]); k<-w
      }
      string[j]<-","; j<-j+1
    } else if(tree$edge[i,2]>=n){
      string[j]<-"("; j<-j+1
    }
  }

  #still need to plug in the root node annotations and end with semicolon:

  if(is.null(tree$edge.length)){

    #remove redundant komma from end of string and open root node annotation:
    string<-c(string[1:(length(string)-1)], "[&")
    j <- length(string) + 1

    annotCount <- length(tree$root.annotation)
    for (a in 1:annotCount){
      annotName <- names(tree$root.annotation)[a]
      annotValue <- tree$root.annotation[a][[1]]
      if(is.numeric(annotValue)){
        string[j]<-paste(annotName, "=", annotValue ,collapse="")
      } else {
        string[j]<-paste(annotName, '="', annotValue, '"' ,collapse="")
      }
      j<-j+1
      if (a < annotCount){
        string[j]<-paste("," ,collapse="")
        j<-j+1
      }
    }
    #close annot info:
    string[j]<-paste("];", collapse="")

  } else {

    #as above: remove redundant stuff from end and open root node annotation:
    string<-c(string[1:(length(string)-1)],"[&")
    j <- length(string) + 1

    annotCount <- length(tree$root.annotation)
    for (a in 1:annotCount){
      annotName <- names(tree$root.annotation)[a]
      annotValue <- tree$root.annotation[a][[1]]
      if(is.numeric(annotValue)){
        string[j]<-paste(annotName, "=", annotValue ,collapse="")
      } else {
        string[j]<-paste(annotName, '="', annotValue, '"' ,collapse="")
      }
      j<-j+1
      if (a < annotCount){
        string[j]<-paste("," ,collapse="")
        j<-j+1
      }
    }
    #close annot info and end tree:
    string[j]<-paste("];", collapse="")

  }
  string<-paste(string,collapse="")
  return(string)
}

#################################
# WRITE TO FILE IN NEXUS FORMAT #
#################################
#read tree:
tree <- OutbreakTools::read.annotated.nexus(file = inTREE)
apeTree <- ape::read.nexus(file = inTREE)

#remove a tip:
tree2 <- drop.tip.annotated.tree(phy = tree, tipNames = toDropTip)
apeTree2 <- ape::drop.tip(phy = apeTree, tip = toDropTip) # Unsure if ape's or geiger's drop.tip

#write to file:
cat("#NEXUS\n", file = outTREE)
cat(paste("[based on write.nexus function from R-package APE and a function by Liam Revell at http://blog.phytools.org/]\n\n", sep = ""), file = outTREE, append = TRUE)
cat("BEGIN TAXA;\n", file = outTREE, append = TRUE)
N <- length(apeTree$tip.label)
cat(paste("\tDIMENSIONS NTAX = ", N, ";\n", sep = ""), file = outTREE, append = TRUE)
cat("\tTAXLABELS\n", file = outTREE, append = TRUE)
cat(paste("\t\t", apeTree$tip.label, sep = ""), sep = "\n", file = outTREE, append = TRUE)
cat("\t;\n", file = outTREE, append = TRUE)
cat("END;\n\n", file = outTREE, append = TRUE)
cat("BEGIN TREES;\n", file = outTREE, append = TRUE)
cat("\tTRANSLATE\n", file = outTREE, append = TRUE)
tmp <- checkLabel(apeTree$tip.label)
X <- paste("\t\t", 1:N, "\t", tmp, ",", sep = "")
## We remove the last comma:
X[length(X)] <- gsub(",", "", X[length(X)])
cat(X, file = outTREE, append = TRUE, sep = "\n")
cat("\t;\n", file = outTREE, append = TRUE)
#token <- as.character(1:N)
#names(token) <- apeTree$tip.label
#apeTree$tip.label <- token
#create newick string for writing to file:
tr <- get.annotated.newick.string(tree2)
#remove spaces before writing to file:
tr <- gsub(pattern = " ", replacement = "", x = tr)
if (ape::is.rooted(apeTree)){
  cat("\tTREE * UNTITLED = [&R] ", file = outTREE, append = TRUE)
  cat(tr, "\n", sep = "", file = outTREE, append = TRUE)
  } else {
  cat("\tTREE * UNTITLED = [&U] ", file = outTREE, append = TRUE)
  cat(tr, "\n", sep = "", file = outTREE, append = TRUE)
}
cat("END;\n", file = outTREE, append = TRUE)

######################################## THE END ########################################

}
