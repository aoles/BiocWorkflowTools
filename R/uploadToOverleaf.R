#' Upload a LaTeX project to Overleaf
#' 
#' @param files Character vector of file names to upload.  If the first entry
#' is a zip file this is uploaded directly.  Otherwise the files will be added
#' to a zip archive and then uploaded.
#' @param openInBrowser Boolean determining whether to open a browser at the 
#' created Overleaf project or not. Default value is FALSE.
#'
#' @return No value is returned.  The URL where the uploaded project can be 
#' accessed is printed to the screen.  If the argument \code{openInBrowser} is 
#' set to \code{TRUE}, then the default browser will automatically open at the 
#' Overleaf project page.
#' 
#' @examples 
#' example_Rmd <- system.file('examples/f1000_software_example.Rmd', 
#'                            package = "BiocWorkflowTools")
#' output_dir <- file.path(tempdir(), 'example')
#' markdownToLatex(input = example_Rmd, output = output_dir, 
#'                 compress = TRUE)
#' \dontrun{
#' ## don't run this code chunk in the example as we don't want to spam Overleaf
#' zip_file <- paste0(output_dir, '.zip')
#' uploadToOverleaf(files = zip_file, openInBrowser = TRUE)
#' }                
#' 
#' @importFrom stringr str_replace_all
#' @importFrom utils browseURL zip head
#' @importFrom tools file_ext
#' @importFrom httr POST upload_file content
#' 
#' @export
uploadToOverleaf <- function(files = NULL, openInBrowser = FALSE) {
 
    if(is.null(files)) {
        stop("No file(s) specified")
    } else if(file_ext(files[1]) != "zip") {
        ## zip the files up, even if there's only one
        tf <- tempfile(fileext = "zip")
        zip(zipfile = tf, files = files)
        files[1] <- tf
    } 

    ## this is an irritating two step process. First we upload the zip file
    ## to a free file host. then we pass the URL for this to the overleaf API.
    ## Maybe we can improve this in the future?
    uploaded <- POST(url = 'https://transfer.sh/',
                     body = list(zip_file = upload_file(files[1])))
    ## strip new line
    zip_url <- str_replace_all(
        httr::content(uploaded, encoding = "UTF-8", as = "parsed"),
        "[\r\n]" , "")
    ## post to overleaf
    tmp <- POST(url = 'https://www.overleaf.com/docs',
                body = list(zip_uri = zip_url))
    overleaf_url <- head(tmp)$url
    
    message("Overleaf project created at:\n\t", overleaf_url)
    
    if(openInBrowser){
        browseURL(url = overleaf_url)
    }
}