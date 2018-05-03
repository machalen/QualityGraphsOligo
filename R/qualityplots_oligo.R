qualityplots_oligo <-
function(ds, picname, estimates_m=NULL, est_noctrls, labels=NULL, Pset=NULL, 
                               conditions=NULL,colors=NULL, groupn=NULL, all=TRUE, Density=TRUE, 
                               Boxplots=TRUE, Clusters=TRUE, PCA=TRUE, estimates=FALSE, 
                               noctrls=TRUE, resDir=NULL) {
    library(RColorBrewer)
    require(gtools)
    #ds: GeneFeatureSet obtinguda per: RawData <- read.celfiles(celFiles.s, sampleNames= celnames.s)
    #picname: Name of the picture
    #estimates_m: matriu amb les intensitats normalitzades
    #est_noctrls: Matriu amb les intensitats normalitzades sense controls
    #Pset: PLM object obtained from fitProbeLevelModel: Pset <- fitProbeLevelModel(RawData)
    #conditions: Vector with the different conditions. It has to be a vector of characters!!
    #colors: Vector with the colors assigned to each condition (in order of the unique(conditions))
    #groupn: subset to represent in the density plot if all=TRUE
    #all: If TRUE a summary with all the density plots in the same picture is generated
    #Density: If TRUE the density plots are generated
    #Boxplots: If TRUE the boxplots are generated
    #Clusters: If TRUE clusters are generated
    #PCA: If TRUE PCA are generated
    #estimates: If TRUE clusters with all the estimates are generated
    #noctrls: If TRUE clusters with no controls are generated
    #resDir: Per defecte Null, es el directori on es guarden els resultats
    if (is.null(resDir)){
        resDir=resultsDir
    }
    #Density plots per separat (subsets of 16 samples) i tots en una mateixa imatge
    if(Density){
        densityplotAll_oligo(ds, picname=picname, groupn= groupn, all=all, resDir=resDir)
    }
    #Boxplots
    if(Boxplots){
        boxplotAll_oligo(ds, est_noctrls=est_noctrls, Pset=Pset, labels=labels, picname=picname, 
                         conditions=conditions, colors=colors, resDir=resDir)
    }
    #Clusters with and without controls
    if(Clusters){
        clusterdend(estimates_m=estimates_m, est_noctrls=est_noctrls, picname=picname, conditions=conditions, 
                    colors=colors, estimates=estimates, noctrls=noctrls, resDir=resDir)
    }
    
    # PCA 
    if(PCA){
        makePCA(est_noctrls, picname=picname, conditions=conditions, colors=colors, resDir=resDir)
    }
}
