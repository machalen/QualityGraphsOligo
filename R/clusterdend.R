clusterdend <-
function(estimates_m=NULL, est_noctrls=NULL, picname, conditions=NULL, colors=NULL, 
                        estimates=FALSE, noctrls=TRUE, resDir=NULL) {
    #picname: Name of the picture
    #conditions: Vector with the different conditions
    #estimates_m: matriu amb les intensitats normalitzades
    #est_noctrls: Matriu amb les intensitats normalitzades sense controls
    #colors: Vector with the colors assigned to each condition (in order of the unique(conditions))
    if (!is.null(resDir)){
        resDir=resultsDir
    }
    
    if(estimates) {
        labels <- colnames(estimates_m)
        parameters <- setparam(labels)
        many.clusters_V(estimates_m,resDir,paste(picname, "ClustersWithCtrls", sep="_"),"Clusters with ctrls", parameters=parameters, conditions=conditions, colors=colors)
    } 
    
    if(noctrls) {
        labels <- colnames(est_noctrls)
        parameters <- setparam(labels)
        many.clusters_V(est_noctrls,resDir,paste(picname, "ClustersNoCtrls", sep="_"),"Clusters no ctrls", parameters=parameters, conditions=conditions, colors=colors)
    }
}
