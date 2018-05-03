densityplot_oligo <-
function(ds,strt=NULL,nd=NULL, lab, parameters){
    #NOTA: si no es posa nd i strt sha de posar lab= i parameters=
    #ds: Aroma affimetrix object
    #strt: first sample to be analyzed
    #nd: last sample to be analyzed
    #lab: labels of the sample
    if (is.null(strt) & is.null(nd)) {
        strt <- 1
        nd <- length(lab)
    } else if (is.null(strt)){
        strt <- 1
    } else if (is.null(nd)) {
        nd <- length(lab)
    }
    
    rwData <- log2(exprs(ds))
    #############################Set the ylim in the graph #################
    #Agafem el valor d'intensitat m?s elevat i l'arrodonim a l'al?a
    yvalues <- apply(rwData, 2, function(x) ceiling(max(density(x)$y)))
    
    ###########################################################
    colors <-rainbow(nd-strt+1)
    #Dibuixar la primera linia
    
    plot(density(rwData[,strt]), col=colors[1], main=paste("Samples", strt, "to", nd), xlab= "log2(y)", ylim=c(0,max(yvalues)), lty=strt, ylab="")
    
    for (e in (strt+1):nd){
        lines(density(rwData[,e]), col=colors[(e-strt)+1], lty = e)
    }
    
    if (length(lab) < 15){
        legend("topright",legend=lab, cex=0.6,col=colors[1:nd], lty= strt:nd, lwd=1.6)
    } else {
        legend("topright",legend=lab, cex=parameters$ce,col=colors[1:nd], lty= strt:nd, lwd=1.6)
    }
    
    
}
