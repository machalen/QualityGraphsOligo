makePCA <-
function(est_noctrls, picname, conditions=NULL, colors=NULL, dist=2, resDir=NULL){
    #est_noctrls: Matriu amb les intensitats normalitzades sense controls
    #labels: Array names
    #picname: Name of the picture
    #conditions: Vector with the different conditions
    #colors: Vector with the colors assigned to each condition (in order of the unique(conditions))
    #dist: Distància de les labels respecte els punts
    #resDir: Directori de resultats, per defecte resultsDir
    if (!is.null(resDir)){
        resDir=resultsDir
    }
    require(scatterplot3d)
    labels <- colnames(est_noctrls)
    parameters <- setparam(labels)
    summary(pca.filt <- prcomp(t(est_noctrls))) #38% 
    png(file.path(resDir,paste(picname,"PCA.png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
    
    if (is.null(conditions)) {
        pca3d<-scatterplot3d(x=pca.filt$x[,1],y=pca.filt$x[,2],z=pca.filt$x[,3],  xlab='PC1', ylab='PC2', zlab='PC3', main='PCA',col.grid="lightblue", cex.symbols=parameters$ce+0.2)
        ##per canviar la dist?ncia del text amb els punts s'ha de sumar a les coordenades pca3d$xyz.convert(pca.filt$x)
        text(pca3d$xyz.convert(pca.filt$x+dist), labels=rownames(pca.filt$x), cex=parameters$ce+0.2)
        
    }else if(is.null(colors)){
        
        list1 <- unique(conditions)
        ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, "Paired")) #20 colors en total
        list2 <- ColVect[1:length(unique(conditions))]
        map = setNames(list2, list1)
        colors <- map[conditions]
        pca3d<-scatterplot3d(x=pca.filt$x[,1],y=pca.filt$x[,2],z=pca.filt$x[,3],  xlab='PC1', ylab='PC2', zlab='PC3', main='PCA',col.grid="lightblue", cex.symbols=parameters$ce+0.2, color= colors, pch=16)
        ##per canviar la dist?ncia del text amb els punts s'ha de sumar a les coordenades pca3d$xyz.convert(pca.filt$x)
        text(pca3d$xyz.convert(pca.filt$x+dist), labels=rownames(pca.filt$x), cex=parameters$ce+0.2, col= colors)
        legend("bottomright",legend=list1,col=list2,pch=16,ncol=1,cex=0.9)
        
    } else {
        list1 <- unique(conditions)
        list2 <- colors
        map = setNames(list2, list1)
        colors <- map[conditions]
        pca3d<-scatterplot3d(x=pca.filt$x[,1],y=pca.filt$x[,2],z=pca.filt$x[,3],  xlab='PC1', ylab='PC2', zlab='PC3', main='PCA',col.grid="lightblue", cex.symbols=parameters$ce+0.2, color= colors, pch=16)
        ##per canviar la dist?ncia del text amb els punts s'ha de sumar a les coordenades pca3d$xyz.convert(pca.filt$x)
        text(pca3d$xyz.convert(pca.filt$x+dist), labels=rownames(pca.filt$x), cex=parameters$ce+0.2, col= colors)
        legend("bottomright",legend=list1,col=list2,pch=16,ncol=1,cex=0.9)
    }
    dev.off()
}
