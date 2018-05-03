\name{densityplot_oligo}
\alias{densityplot_oligo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to make density plots with raw microarray data in log2 scale.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
densityplot_oligo(ds, strt = NULL, nd = NULL, lab, parameters)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{strt}{
%%     ~~Describe \code{strt} here~~
}
  \item{nd}{
%%     ~~Describe \code{nd} here~~
}
  \item{lab}{
%%     ~~Describe \code{lab} here~~
}
  \item{parameters}{
%%     ~~Describe \code{parameters} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ds, strt = NULL, nd = NULL, lab, parameters) 
{
    if (is.null(strt) & is.null(nd)) {
        strt <- 1
        nd <- length(lab)
    }
    else if (is.null(strt)) {
        strt <- 1
    }
    else if (is.null(nd)) {
        nd <- length(lab)
    }
    rwData <- log2(exprs(ds))
    yvalues <- apply(rwData, 2, function(x) ceiling(max(density(x)$y)))
    colors <- rainbow(nd - strt + 1)
    plot(density(rwData[, strt]), col = colors[1], main = paste("Samples", 
        strt, "to", nd), xlab = "log2(y)", ylim = c(0, max(yvalues)), 
        lty = strt, ylab = "")
    for (e in (strt + 1):nd) {
        lines(density(rwData[, e]), col = colors[(e - strt) + 
            1], lty = e)
    }
    if (length(lab) < 15) {
        legend("topright", legend = lab, cex = 0.6, col = colors[1:nd], 
            lty = strt:nd, lwd = 1.6)
    }
    else {
        legend("topright", legend = lab, cex = parameters$ce, 
            col = colors[1:nd], lty = strt:nd, lwd = 1.6)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
