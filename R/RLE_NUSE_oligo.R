RLE_NUSE_oligo <-
function (Pset, kind, parameters, lab, conditions=NULL, colors=NULL) {
    #Pset: PLM object obtained from fitProbeLevelModel: Pset <- fitProbeLevelModel(RawData)
    #Kind: NUSE o RLE, no es poden fer els dos a la vegada
    #conditions: Vector with the different conditions
    #colors: Vector with the colors assigned to each condition (in order of the unique(conditions))
    
    if (kind == "NUSE"){
        if (is.null(conditions)) {
            oligo::NUSE(Pset,main=paste(kind, "plot"),xaxt="n")
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
        }else if(is.null(colors)){
            #CondNames <- mixedsort(lab)
            CondNames <- lab
            CondTable <- data.frame(CondNames,conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
            list1 <- unique(as.character(sort(conditions.o)))#Es necessari el as.character per a que els colors surtin be!!
            ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, "Paired")) #20 colors en total
            list2 <- ColVect[1:length(unique(conditions))]
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::NUSE(Pset,main=paste(kind, "plot"),xaxt="n", col=colors)
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
            legend("topright",legend=list1, cex=parameters$ce+0.2, fill=list2)
        } else {
            #CondNames <- mixedsort(lab)
            CondNames <- lab
            CondTable <- data.frame(CondNames,conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
            list1 <- unique(as.character(sort(conditions.o)))#Es necessari el as.character per a que els colors surtin be!!
            list2 <- colors
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::NUSE(Pset,main=paste(kind, "plot"),xaxt="n", col=colors)
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
            legend("topright",legend=list1, cex=parameters$ce+0.2, fill=list2)
        }
    } else if(kind == "RLE") {
        if (is.null(conditions)) {
            oligo::RLE(Pset,main=paste(kind, "plot"),xaxt="n")
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
        }else if(is.null(colors)){
            #CondNames <- mixedsort(lab)
            CondNames <- lab
            CondTable <- data.frame(CondNames,conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
            list1 <- unique(as.character(sort(conditions.o)))#Es necessari el as.character per a que els colors surtin be!!
            ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, "Paired")) #20 colors en total
            list2 <- ColVect[1:length(unique(conditions))]
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::RLE(Pset,main=paste(kind, "plot"),xaxt="n", col=colors)
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
            legend("topright",legend=list1, cex=parameters$ce+0.2, fill=list2)
        } else {
            #CondNames <- mixedsort(lab)
            CondNames <- lab
            CondTable <- data.frame(CondNames,conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames),"conditions"]
            list1 <- unique(as.character(sort(conditions.o)))#Es necessari el as.character per a que els colors surtin be!!
            list2 <- colors
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::RLE(Pset,main=paste(kind, "plot"),xaxt="n", col=colors)
            axis(1,at=1:length(lab),labels=lab,cex.axis=parameters$ce,las=2)
            legend("topright",legend=list1, cex=parameters$ce+0.2, fill=list2)
        }
    }
    
}
