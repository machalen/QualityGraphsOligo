\name{boxplotAll_oligo}
\alias{boxplotAll_oligo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BoxPlots for quality control
}
\description{
Function to make all boxplots required for quality control.
}
\usage{
boxplotAll_oligo(ds, est_noctrls = NULL, Pset = NULL, picname, labels = NULL, conditions = NULL, colors = NULL, raw.bp = TRUE, RMA.bp = TRUE, RLE = TRUE, NUSE = TRUE, resDir = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
GeneFeatureSet object obtained with: RawData <- read.celfiles(celFiles.s, sampleNames= celnames.s)
}
  \item{est_noctrls}{
Normalized expression matrix.
}
  \item{Pset}{
PLM object obtained from fitProbeLevelModel: Pset <- fitProbeLevelModel(RawData)
}
  \item{picname}{
Character with the output file name.
}
  \item{labels}{
Optional. Vector with sample names. Samples are in the same order as in the ds object. Default get sample names from the ds object.
}
  \item{conditions}{
Optional, vector with sample condition in the same order as samples appear in the ds object.
}
  \item{colors}{
Optional, vector with the colours corresponding to each condition.
}
  \item{raw.bp}{
Plot boxplots with raw data at log2 scale. Default TRUE.
}
  \item{RMA.bp}{
Plot boxplots with normalized log2 data. Default TRUE.
}
  \item{RLE}{
Plot boxplots with relative log expression values (RLE). Default TRUE.
}
  \item{NUSE}{
Plot boxplots with normalized unscaled standard errors (NUSE). Default TRUE.
}
  \item{resDir}{
Output results directory. Default is ResultsDir.
}
}
\details{
This function outputs boxplots for quality assessment in microarray data using objects obtained with oligo package pre-processing. A boxplot with raw data, and a boxplot with Normalized data. Additionally reports relative log expression values (RLE), and normalized unscaled standard errors (NUSE).
}
\value{
This function outputs plots in .png format in the resDir directory.
}
\references{

}
\author{
Magdalena Arnal Segura <marnal@imim.es>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
