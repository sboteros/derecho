# Cargar los archivos necesarios
library(stringr)
library(knitr)

# Funci�n de eliminaci�n de archivos innecesarios
cleanUp <- function(tmpExtensions) {
  filesInDir <- 
    unlist(str_split(list.files(), pattern = "[[:space:]]+"))
  
  sapply(filesInDir, function(file) {
    extension <- str_split(file, pattern="[.]", n=2)[[1]][2]
    if (extension %in% tmpExtensions) {
      cat("Removing", file, "...\n")
      file.remove(file)
    }
    invisible(file)
  })
  
  invisible()
}

# Funci�n para hacer el PDF y correr Biber 
rmd2pdf <- function(file='prueba.Rmd', template='../tools/latex/plantilla_tarea.tex',
                    arguments = ' --metadata-file=header.yaml',
                    biber=TRUE, rmTmpFiles=TRUE) {
  if (str_length(Sys.which('xelatex')) == 0) {
    stop(str_c('Must have xelatex installed and accesible from the command line',
               ' to run this function.')
    )
  }
  
  if (biber && str_length(Sys.which('biber')) == 0) {
    stop(str_c('Must have biber installed and accesible from the command line',
               ' to run this function.')
    )
  }
  
  if (str_length(Sys.which('pandoc')) == 0) {
    stop(str_c('Must have pandoc installed and accesible from the command line',
               ' to run this function.')
         )
  }
  
  cat("Working in", getwd(), "...\n")
  
  filename <- tail(
                str_split(
                  string = str_split(file, ".(R|r)md", n=2)[[1]][1],
                  pattern = "/")[[1]],
                n=1)
  
  cat("Making ./tmp/ directory to hold temporary files...\n")
  dir.create('./tmp')
  
  mdFile <- str_c("./tmp/", filename, ".md")
  
  cat("knit'ing", file, "into", mdFile, "...\n") 
  knitr::knit(input = file, output = mdFile)
  
  texFile <- str_c("./tmp/", filename, ".tex")
  
  # make pandoc command string
  pandocCmd <- str_c("pandoc ", shQuote(mdFile),
                     " -t latex -o ", shQuote(texFile),
                     " --self-contained --number-sections", arguments,
                     " --template=", shQuote(template))
  
  cat("pandoc'ing", mdFile, "into", texFile, "...\n")
  system(pandocCmd)

  # run xelatex 
  xelatexCmd <- str_c("xelatex ", texFile)
  
  cat("xelatex'ing", texFile, "...\n")
  xelatexFail <- system(xelatexCmd)
  
  if (biber) {
    cat("\n\nRunning biber on intermediate files...\n\n")
    system(str_c("biber ", filename))
    
    cat("\n\nRe-running xelatex to clean up cross-references...\n\n")
    xelatexFail <- system(xelatexCmd)
  }
  
  # Es m�s seguro definir una lista restringida de terminaciones para eliminar, 
  # que de terminaciones para conservar.
  tmpExtensions = c("aux", "bbl", "bcf", "blg", "lof",
                    "log", "lot", "run.xml", "toc", "out")
  
  if (xelatexFail) {
    stop("\n\nxelatex failed to compile a PDF from the rendered .tex file...\n")
    stop("Cleaning up temporary files...\n\n")
    cleanUp(tmpExtensions)
  } 
  
  if (rmTmpFiles) cleanUp(tmpExtensions)
  
  return(str_c(filename, ".pdf"))
}
