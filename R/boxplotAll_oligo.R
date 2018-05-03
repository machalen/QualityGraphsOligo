boxplotAll_oligo <-
function(ds, est_noctrls=NULL,Pset=NULL, picname, labels=NULL, conditions=NULL,
                              colors=NULL, raw.bp=TRUE, RMA.bp=TRUE, RLE=TRUE, NUSE=TRUE, 
                              resDir=NULL){
    #ds: GeneFeatureSet obtinguda per: RawData <- read.celfiles(celFiles.s, sampleNames= celnames.s)
    #est_noctrls: Normalized matrix
    #Pset: PLM object obtained from fitProbeLevelModel: Pset <- fitProbeLevelModel(RawData)
    #labels: Array names
    #picname: Name of the picture
    #conditions: Vector with the different conditions
    #raw.bp: If FALSE doesn't plot the raw intensities boxplot
    #RMA.bp: If false doesn't plot normalized boxplot
    #RLE: If false doesn't plot the RLE plot
    #NUSE: If false doesn't plot the NUSE plot
    
    labels <- colnames(exprs(ds))
    parameters <- setparam(labels)
    
    if (!is.null(resDir)){
        resultsDir=resDir
    }
    
    if (is.null(Pset)) {
        #Extraiem els Pset per fer el RLE i el NUSE
        Pset <- fitProbeLevelModel(RawData)
    }
    
    #Boxplot rawdata
    if (raw.bp){
        png(file.path(resultsDir, paste(picname,"Boxplots.png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
        BoxplotRaw_oligo(ds, parameters=parameters, lab =labels, conditions=conditions, colors=colors)
        dev.off()
    }
    
    #Boxplot RMA
    if (RMA.bp){
        png(file.path(resultsDir,paste(picname, "BoxplotsRMA.png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
        BoxplotNorm_oligo(est_noctrls=est_noctrls, parameters=parameters, conditions=conditions, colors=colors)
        dev.off()
    }
    #RLE plot
    if (RLE) {
        png(file.path(resultsDir,paste(picname, "RLE.png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
        RLE_NUSE_oligo(Pset, "RLE", parameters=parameters, lab=labels, conditions=conditions, colors=colors)
        dev.off()
    }
    
    #NUSE plot
    if(NUSE) {
        png(file.path(resultsDir,paste(picname, "NUSE.png", sep="_")),width=parameters$wid,height=parameters$hei,res=parameters$res)
        RLE_NUSE_oligo(Pset, "NUSE", parameters=parameters, lab=labels, conditions=conditions, colors=colors)
        dev.off()
    }
    
}
