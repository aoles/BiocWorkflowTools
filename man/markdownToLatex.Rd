% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markdownToLatex.R
\name{markdownToLatex}
\alias{markdownToLatex}
\title{Convert R markdown to F1000 latex}
\usage{
markdownToLatex(input, output = NULL, compress = TRUE)
}
\arguments{
\item{input}{path to Rmd file to be converted}

\item{output}{Specifies the folder where the output should be written.  If 
left NULL this defaults to the same folder as the input file.}

\item{compress}{If TRUE a zip file of the output directory is created, which
can be uploaded to Overleaf}
}
\value{
No value is returned, but a tex file is written to disk, and is 
given an identical name to the input Rmd except for the file extension.  
Accompanying figures are copied to the output directory long with style 
files and figures required to match the F1000 Research format.

Optionally the output directory can be compressed into a zip archive, 
which can then be uploaded to Overleaf either manually, or by passing it 
to the function \code{\link{uploadToOverleaf}}.
}
\description{
Turn Rmarkdown version of workflow document into latex file that can be 
uploaded to F1000 Overleaf
}
\examples{
example_Rmd <- system.file('examples/f1000_software_example.Rmd', 
                           package = "BiocWorkflowTools")
output_dir <- file.path(tempdir(), 'example')
markdownToLatex(input = example_Rmd, output = output_dir, 
                compress = TRUE)

}

