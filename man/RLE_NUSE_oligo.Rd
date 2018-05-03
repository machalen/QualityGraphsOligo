\name{RLE_NUSE_oligo}
\alias{RLE_NUSE_oligo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to make NUSE and RLE boxplots.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RLE_NUSE_oligo(Pset, kind, parameters, lab, conditions = NULL, colors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Pset}{
%%     ~~Describe \code{Pset} here~~
}
  \item{kind}{
%%     ~~Describe \code{kind} here~~
}
  \item{parameters}{
%%     ~~Describe \code{parameters} here~~
}
  \item{lab}{
%%     ~~Describe \code{lab} here~~
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
function (Pset, kind, parameters, lab, conditions = NULL, colors = NULL) 
{
    if (kind == "NUSE") {
        if (is.null(conditions)) {
            oligo::NUSE(Pset, main = paste(kind, "plot"), xaxt = "n")
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
        }
        else if (is.null(colors)) {
            CondNames <- mixedsort(lab)
            CondTable <- data.frame(CondNames, conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames), 
                "conditions"]
            list1 <- unique(as.character(sort(conditions.o)))
            ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, 
                "Paired"))
            list2 <- ColVect[1:length(unique(conditions))]
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::NUSE(Pset, main = paste(kind, "plot"), xaxt = "n", 
                col = colors)
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
            legend("topright", legend = list1, cex = parameters$ce + 
                0.2, fill = list2)
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
            oligo::NUSE(Pset, main = paste(kind, "plot"), xaxt = "n", 
                col = colors)
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
            legend("topright", legend = list1, cex = parameters$ce + 
                0.2, fill = list2)
        }
    }
    else if (kind == "RLE") {
        if (is.null(conditions)) {
            oligo::RLE(Pset, main = paste(kind, "plot"), xaxt = "n")
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
        }
        else if (is.null(colors)) {
            CondNames <- mixedsort(lab)
            CondTable <- data.frame(CondNames, conditions)
            conditions.o <- CondTable[match(lab, CondTable$CondNames), 
                "conditions"]
            list1 <- unique(as.character(sort(conditions.o)))
            ColVect <- c(brewer.pal(8, "Dark2"), brewer.pal(12, 
                "Paired"))
            list2 <- ColVect[1:length(unique(conditions))]
            map = setNames(list2, list1)
            colors <- map[conditions.o]
            oligo::RLE(Pset, main = paste(kind, "plot"), xaxt = "n", 
                col = colors)
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
            legend("topright", legend = list1, cex = parameters$ce + 
                0.2, fill = list2)
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
            oligo::RLE(Pset, main = paste(kind, "plot"), xaxt = "n", 
                col = colors)
            axis(1, at = 1:length(lab), labels = lab, cex.axis = parameters$ce, 
                las = 2)
            legend("topright", legend = list1, cex = parameters$ce + 
                0.2, fill = list2)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
