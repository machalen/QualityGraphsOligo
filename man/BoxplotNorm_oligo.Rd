\name{BoxplotNorm_oligo}
\alias{BoxplotNorm_oligo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to make boxplots with microarray normalized data.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BoxplotNorm_oligo(est_noctrls, strt = NULL, nd = NULL, parameters, conditions = NULL, colors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est_noctrls}{
%%     ~~Describe \code{est_noctrls} here~~
}
  \item{strt}{
%%     ~~Describe \code{strt} here~~
}
  \item{nd}{
%%     ~~Describe \code{nd} here~~
}
  \item{parameters}{
%%     ~~Describe \code{parameters} here~~
}
  \item{conditions}{
%%     ~~Describe \code{conditions} here~~
}
  \item{colors}{
%%     ~~Describe \code{colors} here~~
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
function (est_noctrls, strt = NULL, nd = NULL, parameters, conditions = NULL, 
    colors = NULL) 
{
    lab <- colnames(est_noctrls)
    if (is.null(strt) & is.null(nd)) {
        strt <- 1
        nd <- length(ncol(est_noctrls))
    }
    else if (is.null(strt)) {
        strt <- 1
    }
    else if (is.null(nd)) {
        nd <- length(ncol(est_noctrls))
    }
    if (is.null(conditions)) {
        boxplot(est_noctrls, main = "Boxplot RMA all probe sets", 
            xaxt = "n")
        axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
            las = 2)
    }
    else if (is.null(colors)) {
        CondNames <- mixedsort(lab)
        CondTable <- data.frame(CondNames, conditions)
        conditions.o <- CondTable[match(lab, CondTable$CondNames), 
            "conditions"]
        list1 <- unique(as.character(sort(conditions.o)))
        ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, "Paired"))
        list2 <- ColVect[1:length(unique(conditions))]
        map = setNames(list2, list1)
        colors <- map[conditions.o]
        boxplot(est_noctrls, main = "Boxplot RMA all probe sets", 
            xaxt = "n", col = colors)
        axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
            las = 2)
        legend("topright", legend = list1, cex = 0.5, fill = list2)
    }
    else {
        CondNames <- mixedsort(lab)
        CondTable <- data.frame(CondNames, conditions)
        conditions.o <- CondTable[match(lab, CondTable$CondNames), 
            "conditions"]
        list1 <- unique(as.character(sort(conditions.o)))
        list2 <- colors
        map = setNames(list2, list1)
        colors <- map[conditions.o]
        boxplot(est_noctrls, main = "Boxplot RMA all probe sets", 
            xaxt = "n", col = colors)
        axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
            las = 2)
        legend("topright", legend = list1, cex = 0.5, fill = list2)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
