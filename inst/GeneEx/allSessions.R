# Data and functions shared across all sessions in the same R process
# Utility functions not depending on output and input
# The database object




useShinyjs()
# Utility functions
.sracipePlotDensity = function(plotData,binCount=40, plotColor=NULL, label1 = "x",
                               label2 = "y", ...){
  if(is.null(plotColor)){
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    plotColor <- rf(32)
  }
  colnames(plotData[,1:2]) <- c(label1, label2)
  h1 <- hist(as.numeric(plotData[,1]), breaks=binCount, plot=F)
  h2 <- hist(as.numeric(plotData[,2]), breaks=binCount, plot=F)
  top <- max(h1$counts, h2$counts)
  kernelDensity <- kde2d(plotData[,1], plotData[,2], n=binCount)

  oldpar <- par()
  par(mgp=c(2,1,0),mar=c(3,3,1,1))
  layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
  image(kernelDensity, cex = 2, cex.axis = 1, cex.lab = 1, 
        xlab= label1, ylab=label2) 
  par(mar=c(0,2,1,0))
  barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
  par(mar=c(2,0,0.5,1))
  barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)
  
}

.sracipePlotHeatmapInt <- function(plotData, nSamples = 500, plotColor = NULL, ...) {
  if(is.null(plotColor)){
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    plotColor <- rf(32)
  }
  hmap <- heatmaply(plotData, col=plotColor)
}



.sracipePlotHeatmap <- function(plotData, nSamples = 500, plotColor = NULL, 
                                nClusters = 3, 
                                assignedClusters = NULL, ...) {
  plotData <- plotData[,1:min(nSamples,ncol(plotData))]
  if(is.null(plotColor)){
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    plotColor <- rf(32)
  }

  if(is.null(assignedClusters)){
  #ref_cor <- cor((plotData), method = "spearman")
  #distance <- as.dist((1 - ref_cor) / 2)
  distance <- dist(t(plotData))
  clusters <- hclust(distance, method = "ward.D2")
  assignedClusters <- cutree(clusters, nClusters)
  }
    clustNames <- unique(assignedClusters)
    nClusters <- length(clustNames)
    clustColors <- numeric(length(assignedClusters))
    for(tmp1 in seq_len(length(clustColors))){
      clustColors[tmp1] <- which(clustNames == assignedClusters[tmp1] )
    }
    col2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7")
    clustColors <- col2[clustColors]
   names(clustColors) <- assignedClusters
  gplots::heatmap.2(plotData, col=plotColor, 
                    hclustfun = function(x) hclust(x,method = 'ward.D2'), 
                 #   distfun=function(x) as.dist((1-cor(t(x), method = "spear"))/2), 
                    trace="none", ColSideColors = clustColors,
                 ...)
}

.sracipePlotPca <- function(plotData, pca = NULL, ...){
  if(is.null(pca)) {pca = prcomp(t(plotData), scale. = FALSE)}
  pcaData <- data.frame(x=pca$x[,1],y=pca$x[,2])
  .sracipePlotDensity(pcaData, 
                      label1 = paste0("PC1(",100*summary(pca)$importance[2,1],"%)"),
                      label2 = paste0("PC2(",100*summary(pca)$importance[2,2],"%)"),
                      ...)
}

.sracipePlotTS <- function(plotData, ...){
  sexprs <- stack(as.data.frame(plotData))
  colnames(sexprs) <- c("geneExp", "Gene")
  sexprs$time <- rep(as.numeric(rownames(plotData)), ncol(plotData))
  theme_set(theme_bw(base_size = 18))
  ggplot2::qplot(time, geneExp, data = sexprs, group = Gene, colour = Gene,
                 geom = "line", ylab = "Gene Expression", xlab = "time" )
}

rf <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')))
plotColor <- rf(32)
