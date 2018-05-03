BoxplotRaw_oligo <-
function(ds,strt=NULL,nd=NULL, parameters, lab, conditions=NULL, colors=NULL){
    #ds: GeneFeatureSet obtinguda per: RawData <- read.celfiles(celFiles.s, sampleNames= celnames.s)
    #strt: first sample to be analyzed
    #nd: last sample to be analyzed
    #conditions: Vector with the different conditions
    #colors: Vector with the colors assigned to each condition (in order of the unique(conditions))
    
    
    if (is.null(strt) & is.null(nd)) {
        strt <- 1
        nd <- length(ds)
    } else if (is.null(strt)){
        strt <- 1
    } else if (is.null(nd)) {
        nd <- length(ds)
    }
    
    RwData <- log2(exprs(ds))
    
    if (is.null(conditions)) {
        boxplot(RwData,main="Boxplot rawdata ",xaxt="n")
        axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
    }else if(is.null(colors)){
        CondNames <- mixedsort(lab)
        #CondNames <- lab
        CondTable <- data.frame(CondNames,conditions)
        conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
        list1 <- unique(as.character(sort(conditions.o))) #Es necessari el as.character per a que els colors surtin be!!
        ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, "Paired")) #20 colors en total
        list2 <- ColVect[1:length(unique(conditions))]
        map = setNames(list2, list1)
        colors <- map[conditions.o]
        boxplot(RwData,main="Boxplot rawdata ",xaxt="n", col=colors)
        axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
        legend("topright",legend=list1, cex=0.5,fill=list2)
    }else{
        CondNames <- mixedsort(lab)
        #CondNames <- lab
        CondTable <- data.frame(CondNames,conditions)
        conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
        list1 <- unique(as.character(sort(conditions.o)))#Es necessari el as.character per a que els colors surtin be!!
        list2 <- colors
        map = setNames(list2, list1)
        colors <- map[conditions.o]
        boxplot(RwData,main="Boxplot rawdata ",xaxt="n", col=colors)
        axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
        legend("topright",legend=list1, cex=0.5,fill=list2)
    }
}
