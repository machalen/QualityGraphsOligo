densityplotAll_oligo <-
function(ds, labels, picname, groupn=NULL, all=FALSE, resDir=NULL) {
    #Es necessita la funci? setparam
    #Es necessita la funci? densityplot_oligo()
    #ds: GeneFeatureSet obtinguda per la funció (RawData <- read.celfiles(celFiles.s, sampleNames= celnames.s))
    #labels: array names
    #picname: output file name
    #groupn: subset to represent in the density plot
    #all: If TRUE a summary with all the density plots in the same picture is generated
    
    if (is.null(resDir)){
        resDir=resultsDir
    }
    
    labels <- rownames(protocolData(ds))
    if (is.null(groupn)) {
        groupn <- length(labels)
    } 
    parameters <- setparam(labels[1:groupn])
    
    for (i in seq(1,length(labels), groupn)) {
        png(file.path(resDir,paste(picname, i, i+groupn-1, ".png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
        densityplot_oligo(ds, strt=i, nd=(i+groupn-1), labels[i:(i+groupn-1)], parameters)
        dev.off()
    }
    
    #Fer un plot amb tots els density!
    if (all) {
        plots <- length(labels)/groupn
        if (plots > 3){
            ro <- 2
            le <- ceiling(plots/2)
        } else {
            ro <- 1
            le <- plots
        }
        #Els par?metres height, res i width s'haurien d'acabar doptimitzar
        
        
        png(file=file.path(resDir, paste(picname, "all",".png", sep="_")), width=(parameters$wid*le),height=(parameters$hei*ro), res=parameters$res)
        par(mfrow=c(ro,le))
        for (i in seq(1,length(labels), groupn)) {
            densityplot_oligo(ds, strt=i, nd=(i+groupn-1), labels[i:(i+groupn-1)], parameters)
        }
        dev.off()
    }
    
}
