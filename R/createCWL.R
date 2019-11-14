###################################################
##   Create CommandLineTool from commandLine   ##
###################################################
createCLT <- function(targets=NULL, commandLine, results_path="./results", module_load="baseCommand", file = "default", 
                     overwrite = FALSE, cwlVersion = "v1.0", class = "CommandLineTool",
                     make_stwf = TRUE, fileprefix = "clt_", dbg=0){
  ## TODO if is not default, module load and file
  if(!class(commandLine)=="list") stop("'commandLine' needs to be object of class 'list'.")  
  if(any(!c("id", "header", "baseCommand", "inputs", "outputs") %in% names(commandLine))) stop("Argument 'commandLine' needs to be assigned at least: 'id', 'header', 'baseCommand', 'input' and 'output'.")
  if(all(!c("CommandLineTool") %in% class)) stop("Class slot in '<wf_file>.cwl' needs to be 'Workflow' or 'CommandLineTool'.")
  if(dir.exists(results_path)==FALSE) dir.create(path=results_path)
  ## module_load 
  ## 1.  module_load="baseCommand" will use the name of the software 
  ## 2.  module_load=c("ncbi-blast/2.2.30+", "hisat2/2.1.0") will use the specific version and names
  if(module_load == "baseCommand"){
    module_load <- commandLine$baseCommand[[1]]
  } else {
    module_load <- module_load
  }
  ## File Path
  ## 1. file = "descriptive_filename" # use commandLine$id$path_to_template_store and id$descriptive_filename
  ## 1. file = "default"
  ## 2. file = c("test.cwl", "test.yml")
  if (dbg>9) browser()
  if("descriptive_filename" %in% file) {
    if(dir.exists(commandLine$id$path_in_template_store)==FALSE) dir.create(path=commandLine$id$path_in_template_store, recursive = TRUE)
    file.cwl <- paste0(commandLine$id$path_in_template_store, "/", fileprefix, commandLine$id$descriptive_filename, ".cwl")
    file.yml <- paste0(commandLine$id$path_in_template_store, "/", fileprefix, commandLine$id$descriptive_filename, ".yml")
  } else if("default" %in% file){ # if you want to use baseCommand, you must strip off brackets []
    if(dir.exists(paste("param/cwl/", commandLine$baseCommand, sep=""))==FALSE) dir.create(path=paste("param/cwl/", commandLine$baseCommand, sep=""), recursive = TRUE)
    file.cwl <- paste("param/cwl/", commandLine$baseCommand, "/", commandLine$baseCommand, ".cwl", sep="")
    file.yml <- paste("param/cwl/", commandLine$baseCommand, "/", commandLine$baseCommand, ".yml", sep="")
  } else {
    for(i in seq_along(file)){
      extension <- sub('.*\\.', '', file[[i]])
      if(!c("cwl") %in% extension & !c("yml") %in% extension) stop ("Argument 'file' needs to be assigned as a character vector with the names of the two param file. For example, 'test.cwl' and 'test.yml'.")
      if(c("yml") %in% extension){
        file.yml <- file[[i]]
      } else if(c("cwl") %in% extension){
        file.cwl <- file[[i]]
      }
    }
  }
  print("Gordon c1")
  print(file.cwl)
  if(file.exists(file.cwl) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               file, "or set 'overwrite=TRUE', or provide a different name in the 'file' argument"))
  if(file.exists(file.yml) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               file, "or set 'overwrite=TRUE', or provide a different name in the 'file' argument"))
  ## class("CommandLineTool", "Workflow")
  #if (dbg>9) browser()
  WF.temp <- SYScreate()
  WF.temp <- as(WF.temp, "list")
  ## TODO: Expand to write.WF()
  WF.temp$wf <- list()
  WF.temp$clt <- write.clt.cwl(commandLine, cwlVersion, class, file.cwl) 
  WF.temp$yamlinput <- write.yml(commandLine, file.yml, results_path, module_load) 
  WF.temp$modules <- WF.temp$yamlinput$ModulesToLoad
  WF.temp$cmdlist <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$input <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$output <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$cwlfiles <- list(cwl=normalizePath(file.path(file.cwl)), yml=normalizePath(file.path(file.yml)), steps=names(WF.temp$clt))
  ## targets
  if(!is.null(targets)) {
    #if (dbg>9) browser()
    mytargets <- read.delim(normalizePath(file.path(targets)), comment.char = "#")
    mytargets <- targets.as.list(mytargets)
    targetsheader <- readLines(normalizePath(file.path(targets)))
    targetsheader <- targetsheader[grepl("^#", targetsheader)]
    WF.temp <- c(list(targets=mytargets, targetsheader=list(targetsheader=targetsheader)), WF.temp)
  } else {
    WF.temp <- c(list(targets=data.frame(), targetsheader=list()), WF.temp)
  }
  return(as(WF.temp, "SYSargs2"))
}

## Usage: 
## Example commandLine
# "hisat2 -S ./results/_SampleName_.sam  -x ./data/tair10.fasta  -k 1  --min-intronlen 30  --max-intronlen 3000 --threads 4 -U _FASTQ_PATH1_"
## Provide a list 
# baseCommand <- list(baseCommand="hisat2")
# inputs <- list(
#   "S"=list(type="File", prefix="-S", yml="./results/_SampleName_.sam"),
#   "x"=list(type="File", prefix="-x", yml="./data/tair10.fasta"),
#   "k"= list(type="int", prefix="-k", yml= 1L),
#   "threads"= list(type="int", prefix="-threads", yml=4L),
#   "min-intronlen"= list(type="int", prefix="--min-intronlen", yml= 30L),
#   "max-intronlen"= list(type="int", prefix="--max-intronlen", yml=3000L),
#   "U"=list(type="File", prefix="-U", yml="./data/_FASTQ_PATH1_") )
# outputs <- list("hisat2_sam"=list(type="File", "./results/_SampleName_.sam"))
# commandLine <- list(baseCommand=baseCommand, inputs=inputs, outputs=outputs)
# 
## Creates a SYSargs2 object, populate all the command-line, and creates *.cwl and *.yml files
# targets <- system.file("extdata", "targets.txt", package="systemPipeR")
# WF <- createCLT(targets=targets, commandLine, results_path="./results", module_load="baseCommand",
#                file = "default", overwrite = FALSE, cwlVersion = "v1.0",
#                class = "CommandLineTool")
# WF <- renderWF(WF, inputvars=c(FileName="_FASTQ_PATH1_", SampleName="_SampleName_"))

###################################################
##   Create Workflow from WFdescription          ##
###################################################
createWF <- function(targets=NULL, WFdescription, results_path="./results", module_load="NA", file = "descriptive_filename", 
                     overwrite = FALSE, cwlVersion = "v1.0", class = "Workflow",
                     make_stwf = NA, fileprefix = "wf_", dbg=0){
  ## TODO if is not default, module load and file
  if(!class(WFdescription)=="list") stop("'WFdescription' needs to be object of class 'list'.")  
  if(any(!c("id", "header", "steps") %in% names(WFdescription))) stop("Argument 'WFdescription' needs to be assigned at least: 'id', 'header' and 'steps'.")
  if(all(!c("Workflow") %in% class)) stop("Class slot in '<wf_file>.cwl' needs to be 'Workflow' or 'CommandLineTool'.")
  if(dir.exists(results_path)==FALSE) dir.create(path=results_path)
  ## module_load 
  ## 1.  module_load="baseCommand" will use the name of the software 
  ## 2.  module_load=c("ncbi-blast/2.2.30+", "hisat2/2.1.0") will use the specific version and names
  if(module_load == "baseCommand"){
    module_load <- commandLine$baseCommand[[1]]
  } else {
    module_load <- module_load
  }
  ## File Path
  ## 1. file = "descriptive_filename" # use WFdescription$id$path_to_template_store and id$descriptive_filename
  ## 1. file = "default"
  ## 2. file = c("test.cwl", "test.yml")
  #if (dbg>9) browser()
  if("descriptive_filename" %in% file) {
    if(dir.exists(WFdescription$id$path_in_template_store)==FALSE) dir.create(path=WFdescription$id$path_in_template_store, recursive = TRUE)
    file.cwl <- paste0(WFdescription$id$path_in_template_store, "/", fileprefix, WFdescription$id$descriptive_filename, ".cwl")
    file.yml <- paste0(WFdescription$id$path_in_template_store, "/", fileprefix, WFdescription$id$descriptive_filename, ".yml")
  } else if("default" %in% file){ # if you want to use baseCommand, you must strip off brackets []
    if(dir.exists(paste("param/cwl/", commandLine$baseCommand, sep=""))==FALSE) dir.create(path=paste("param/cwl/", commandLine$baseCommand, sep=""), recursive = TRUE)
    file.cwl <- paste("param/cwl/", commandLine$baseCommand, "/", commandLine$baseCommand, ".cwl", sep="")
    file.yml <- paste("param/cwl/", commandLine$baseCommand, "/", commandLine$baseCommand, ".yml", sep="")
  } else {
    for(i in seq_along(file)){
      extension <- sub('.*\\.', '', file[[i]])
      if(!c("cwl") %in% extension & !c("yml") %in% extension) stop ("Argument 'file' needs to be assigned as a character vector with the names of the two param file. For example, 'test.cwl' and 'test.yml'.")
      if(c("yml") %in% extension){
        file.yml <- file[[i]]
      } else if(c("cwl") %in% extension){
        file.cwl <- file[[i]]
      }
    }
  }
  print("Gordon c1")
  print(file.cwl)
  print(fileprefix)
  print(dbg)
  if (dbg>9) browser()
  if(file.exists(file.cwl) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               file, "or set 'overwrite=TRUE', or provide a different name in the 'file' argument"))
  if(file.exists(file.yml) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               file, "or set 'overwrite=TRUE', or provide a different name in the 'file' argument"))
  ## class("CommandLineTool", "Workflow")
  #if (dbg>9) browser()
  WF.temp <- SYScreate()
  WF.temp <- as(WF.temp, "list")

  ## TODO: Expand to write.WF()
  #if (dbg>9) browser()
  ## get steps from WFdescription
  #steps <- sapply(names(WFdescription$steps), function(x) list())
  steps <- names(WFdescription$steps)
  ## load cltlist
  #cwlfiles$steps <- steps # not used? # object of type 'closure' is not subsettable 
  cltpaths <- sapply(seq_along(steps), function(x) normalizePath(steps[x])) # was wf$steps[[steps[x]]]$run
  cltlist <- sapply(cltpaths, function(x) yaml::read_yaml(file.path(x)), simplify = F) 
  names(cltlist) <- sapply(seq_along(steps), function(x) steps[x]) # was wf$steps[[steps[x]]]$run
  WF.temp$clt <- cltlist # cltlist complete
  if (dbg>9) browser()
  
  WF.temp$wf <- write.wf.cwl(WF.temp, WFdescription, cwlVersion, class, file.cwl, file.yml)
#  WF.temp$wf <- yaml::read_yaml(file.path(dir_path, wf_file))
#  WF.temp$wf <- yaml::read_yaml(file.cwl) # apparently the return from write.wf.cwl() is inadequate, this line is a good bandaid, something about wf: 6 when print(WF)
  WF.temp$yamlinput <- write.wf.yml(WF.temp, WFdescription, file.yml, results_path, module_load) 
  WF.temp$modules <- WF.temp$yamlinput$ModulesToLoad
  WF.temp$cmdlist <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$input <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$output <- sapply(names(WF.temp$clt), function(x) list(NULL))
  WF.temp$cwlfiles <- list(cwl=normalizePath(file.path(file.cwl)), yml=normalizePath(file.path(file.yml)), steps=names(WF.temp$clt))
  ## targets
  if(!is.null(targets)) {
    #if (dbg>9) browser()
    mytargets <- read.delim(normalizePath(file.path(targets)), comment.char = "#")
    mytargets <- targets.as.list(mytargets)
    targetsheader <- readLines(normalizePath(file.path(targets)))
    targetsheader <- targetsheader[grepl("^#", targetsheader)]
    WF.temp <- c(list(targets=mytargets, targetsheader=list(targetsheader=targetsheader)), WF.temp)
  } else {
    WF.temp <- c(list(targets=data.frame(), targetsheader=list()), WF.temp)
  }
  return(as(WF.temp, "SYSargs2"))
}

## Usage: 
## Example WFdescription
# workflow-hisat2/wf_hisat2-mapping-se_samtools.cwl
# workflow-hisat2/wf_hisat2-mapping-se_samtools.yml
# "hisat2 -S ./results/_SampleName_.sam  -x ./data/tair10.fasta  -k 1  --min-intronlen 30  --max-intronlen 3000 --threads 4 -U _FASTQ_PATH1_"
# "samtools view -bS -o ./results/V12B.bam  ./results/V12B.Aligned.out.sam " 
# "samtools sort -o ./results/V12B.sorted.bam  ./results/V12B.bam  -@ 4" 
# "samtools index -b ./results/V12B.sorted.bam  ./results/V12B.sorted.bam.bai  ./results/V12B.sorted.bam "

## Provide a list 
steps <- list(
"param/cwl/hisat2/hisat2-mapping-se-intronlen30-3000.cwl" = list(),
"param/cwl/samtools/samtools-view.cwl" =       list("samtools_sam"= "hisat2-mapping-se-intronlen30-3000/Aligned_out_sam"),
"param/cwl/samtools/samtools-sort.cwl" =       list("samtools_bam"= "samtools-view/samtools_bam"),
"param/cwl/samtools/samtools-index.cwl" =      list("samtools_sort_bam"= "samtools-sort/samtools_sort_bam")
)
# WFdescription <- list(id=id, header=header, steps=steps)
# 
## Creates a SYSargs2 object, populate all the command-line, and creates *.cwl and *.yml files
# targets <- system.file("extdata", "targets.txt", package="systemPipeR")
# WF <- createWF(targets=targets, WFdescription, results_path="./results", module_load="NA",
#                file = "descriptive_filename", overwrite = FALSE, cwlVersion = "v1.0",
#                class = "Workflow")
# WF <- renderWF(WF, inputvars=c(FileName1="fq1", FileName2="fq2", SampleName="SampleName"))

######################################################
##   Create Single-Tool Workflow from existing CLT  ##
######################################################
createSTWF <- function( CLTnames = "", overwrite = FALSE, fileprefix = "stwf_", dbg=0 ) { 

## FUNCTION
## basically a wrapper for createWF() that makes the WFdescription from an existing clt_*.cwl
##      since there is just one step this is trivial
## ARGUMENTS
## passed in CLTnames identify CLT to make an STWF for.
##      output names stwf_*.cwl are calculated from that,
##      if the CLTnames have fileprefix "clt_",
##          it will be removed before prepending "stwf_" to the output names
## besides the extension, the basename()'s of the CLTnames must be the same
##      this is because write_wf_yml() expects to find the ymlpath's from WFdescription$steps
## STEPS
## confirm that CLT .cwl + .yml files exist
## see if STWF file already exists
## do I need to handle a module_load?
##      if so, see createWF() for example
## setup the WFdescription
## call createWF()
## return with WF loaded, for consistency because createCLT() and createWF() do this

## BEGIN
## confirm that CLT .cwl + .yml files exist
  for(i in seq_along(CLTnames)){
    extension <- sub('.*\\.', '', CLTnames[[i]])
    if(!c("cwl") %in% extension & !c("yml") %in% extension) stop ("Argument 'CLTnames' needs to be assigned as a character vector with the names of the two param file. For example, 'test.cwl' and 'test.yml'.")
    if(c("yml") %in% extension){
      file.yml <- CLTnames[[i]]
    } else if(c("cwl") %in% extension){
      file.cwl <- CLTnames[[i]]
    }
  }
  if(!file.exists(file.cwl)) 
    stop(paste(file.cwl, " does not exist, provide a different name in the 'file' argument"))
  if(!file.exists(file.yml)) 
    stop(paste(file.yml, " does not exist, provide a different name in the 'file' argument"))
## see if STWF file already exists
  print("Gordon stwf2")
  print(file.cwl)
  STWFnames = c(paste0(dirname(file.cwl), "/", fileprefix, gsub("clt_", "", basename(file.cwl))), paste0(dirname(file.yml), "/", fileprefix, gsub("clt_", "", basename(file.yml)))) # remove fileprefix "clt_" if present
  print(STWFnames[1])
  print(STWFnames)
  print(overwrite)
  if (dbg>9) browser()
  if(file.exists(STWFnames[1]) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               STWFnames, "or set 'overwrite=TRUE', or provide a different name in the 'CLTnames' argument"))
  if(file.exists(STWFnames[2]) & overwrite == FALSE) 
    stop(paste("I am not allowed to overwrite files; please delete existing file:", 
               STWFnames, "or set 'overwrite=TRUE', or provide a different name in the 'CLTnames' argument"))
## do I need to handle a module_load?
##      if so, see createWF() for example
## setup the WFdescription
  ## Provide a list 
  ## only contains info not available in the clt_*.cwl + .yml's
  ## make descriptive_filename
  path_tmp <- dirname(file.cwl)
  file_tmp <- basename(file.cwl) 
  file_tmp <- gsub("\\.cwl$", "", file_tmp)
  file_tmp <- gsub("clt_", "", file_tmp) # remove fileprefix "clt_" if present
  print("================= descriptive_filename ================")
  print(file_tmp)
  id <- list(
    "description"=file_tmp, # recommend this be used for descriptive_filename as well
    # descriptive_filename should adequately describe any tool configuration choices that affect the .cwl file
    #     specifically, are the outputs different? besides value are the inputs different?
    #     edits to "constant" arguments? Et cetera
    # Note: it is safe and recommended to use dashes (rather than underscores) in descriptive_filename
    #     since we use underscores to separate steps in workflow filenames
    #     this is not a strict rule, just a suggestion for readability
    "descriptive_filename"=file_tmp, # will be used for clt_*.cwl, *.yml and stwf_*.cwl
    #"path_in_template_store"="./param/cwl/workflow-hisat2",
    "path_in_template_store"=path_tmp,
    "author"="systemPipeR",
    "last_update"=Sys.Date() # should be constant when published
  )
  header <- list(
    "cwlVersion"="v1.0",
    "class"="Workflow"
  )
  requirements <- list(
    "InitialWorkDirRequirement"=list("listing"=list("$(inputs.results_path)"))
  )
## steps are like a mini cltlist, with replacement input mappings for inputs from prev steps
  steps <- list(
     "./clt_hisat2-mapping-se-intronlen30-3000.cwl" = list() # only one clt in STWF
  )
  WFdescription <- list(id=id, header=header, requirements=requirements, steps=steps)
  rm(list=c("id","header","requirements","steps"), inherits=T) # make sure these don't get referenced directly later
  WFdescription
  names(WFdescription)
  WFdescription[1]
  WFdescription[[1]]


## call createWF()
## createWF() - Creates a SYSargs2 object, populate the WF, and creates *.cwl and *.yml files
  targets <- system.file("extdata", "targets.txt", package="systemPipeR")
  targets
  WF <- createWF(targets=targets, WFdescription, results_path="./results", module_load="NA",
               file = "descriptive_filename", overwrite = TRUE, cwlVersion = "v1.0",
               class = "Workflow", fileprefix=fileprefix, dbg=dbg)
## return with WF loaded, for consistency because createCLT() and createWF() do this
  return(WF)
}


## Usage: 
# 
## basically a wrapper for createWF() that makes the WFdescription from an existing clt_*.cwl
## Creates a SYSargs2 object, populate all the command-line, and creates *.cwl and *.yml files
# targets <- system.file("extdata", "targets.txt", package="systemPipeR")
# WF <- createSTWF(file = c("path_to/clt_existing.cwl", "path_to/clt_existing.yml"), overwrite = FALSE, fileprefix = "stwf_", dbg=0 )
# WF <- renderWF(WF, inputvars=c(FileName="_FASTQ_PATH1_", SampleName="_SampleName_"))


##################################
## Unexported helper functions ##
##################################

##############################
##   .fixup_wf_write_yml    ##
##############################
## code to handle features not supported by write_yaml()
.fixup_wf_write_yaml <- function(wf, file.cwl) {
  banner1 <- c("#!/usr/bin/env cwltool",
	       paste0("#Created by systemPipeR on ",Sys.Date()),
	       strrep("#",60),
               paste0("##  ", file.cwl), # , strrep(" ",60-6-nchar(file.cwl)), "##"), 
               strrep("#",60), ""  )
  banner2 <- c("", strrep("#",60),
               paste0("##          ", "baseCommand and Arguments Definitions", strrep(" ",60-14-37), "##"),
               strrep("#",60), ""  )
  banner3 <- c("", strrep("#",60),
               paste0("##          ", "Inputs and Outputs Settings", strrep(" ",60-14-27), "##"),
               strrep("#",60), ""  )
  banner4 <- c("", strrep("#",60),
               paste0("##          ", "Workflow Steps Definitions", strrep(" ",60-14-26), "##"),
               strrep("#",60), ""  )

  dbg <- 0
  if (dbg>9) browser()
  print(yaml::as.yaml(x=wf, indent.mapping.sequence=TRUE))
  wf.textstring <- yaml::as.yaml(x=wf, indent.mapping.sequence=TRUE)
  # these 2 patches remove single quotes from array notation not supported by as.yaml()
  wf.textstring <- gsub("\'\\[ ", "[ ", wf.textstring)
  wf.textstring <- gsub(" \\]\'", " ]", wf.textstring)
  # these n patches are only needed for rewrites of cwl files, first writes are correct if we are willing to list arrays instead of bracketing them
  wf.textstring <- gsub("version: 2.1.0", "version: [ 2.1.0 ]", wf.textstring)
  wf.textstring <- gsub("version: 2.5.3a", "version: [ 2.5.3a ]", wf.textstring)
  wf.textstring <- gsub("listing: \\$\\(inputs.results_path\\)", "listing: [ $(inputs.results_path) ]", wf.textstring)
  wf.textstring <- gsub("listing: \\$\\(inputs.idx_star_idx_basedir\\)", "listing: [ $(inputs.idx_star_idx_basedir) ]", wf.textstring)
  wf.textstring <- gsub("out: hisat2_sam", "out: [ hisat2_sam ]", wf.textstring)
  wf.textstring <- gsub("out: samtools_bam", "out: [ samtools_bam ]", wf.textstring)
  wf.textstring <- gsub("out: samtools_sort_bam", "out: [ samtools_sort_bam ]", wf.textstring)
  wf.textstring <- gsub("out: samtools_index", "out: [ samtools_index ]", wf.textstring)

  wf.textvec <- unlist(strsplit(wf.textstring, split="\\n"))
  print(wf.textvec) 
  bannerloc <- c(1, which(grepl("^inputs:",wf.textvec)),
                    which(grepl("^steps:",wf.textvec))  )
  wf.textvec <- c(banner1, wf.textvec[bannerloc[1]:bannerloc[2]-1],
                   banner3, wf.textvec[(bannerloc[2]+1):bannerloc[3]-1],
                   banner4, wf.textvec[bannerloc[3]:length(wf.textvec)] )
  print(wf.textvec)
  writeLines(wf.textvec, file.cwl)
}
## Usage:
# .fixup_wf_write_yaml(wf, file.cwl)

##############################
##   .fixup_clt_write_yml   ##
##############################
## code to handle features not supported by write_yaml()
.fixup_clt_write_yaml <- function(clt, file.cwl) {
  banner1 <- c("#!/usr/bin/env cwltool",
	       paste0("#Created by systemPipeR on ",Sys.Date()),
	       strrep("#",60),
               paste0("##  ", file.cwl), # , strrep(" ",60-6-nchar(file.cwl)), "##"), 
               strrep("#",60), ""  )
  banner2 <- c("", strrep("#",60),
               paste0("##          ", "baseCommand and Arguments Definitions", strrep(" ",60-14-37), "##"),
               strrep("#",60), ""  )
  banner3 <- c("", strrep("#",60),
               paste0("##          ", "Inputs and Outputs Settings", strrep(" ",60-14-27), "##"),
               strrep("#",60), ""  )
  banner4 <- c("", strrep("#",60),
               paste0("##          ", "Workflow Steps Definitions", strrep(" ",60-14-26), "##"),
               strrep("#",60), ""  )

  dbg <- 0
  if (dbg>9) browser()
  print(yaml::as.yaml(x=clt, indent.mapping.sequence=TRUE))
  clt.textstring <- yaml::as.yaml(x=clt, indent.mapping.sequence=TRUE)
  # these 2 patches remove single quotes from array notation not supported by as.yaml()
  clt.textstring <- gsub("\'\\[ ", "[ ", clt.textstring)
  clt.textstring <- gsub(" \\]\'", " ]", clt.textstring)
  # these 3 patches are only needed for rewrites of cwl files, first writes are correct if we are willing to list arrays instead of bracketing them
  clt.textstring <- gsub("version: 2.1.0", "version: [ 2.1.0 ]", clt.textstring)
  clt.textstring <- gsub("version: 2.5.3a", "version: [ 2.5.3a ]", clt.textstring)
  clt.textstring <- gsub("listing: \\$\\(inputs.results_path\\)", "listing: [ $(inputs.results_path) ]", clt.textstring)
  clt.textstring <- gsub("listing: \\$\\(inputs.idx_star_idx_basedir\\)", "listing: [ $(inputs.idx_star_idx_basedir) ]", clt.textstring)
  clt.textvec <- unlist(strsplit(clt.textstring, split="\\n"))
  print(clt.textvec) 
  bannerloc <- c(1, which(grepl("^baseCommand:",clt.textvec)),
                    which(grepl("^inputs:",clt.textvec))  )
  clt.textvec <- c(banner1, clt.textvec[bannerloc[1]:bannerloc[2]-1],
                   banner2, clt.textvec[(bannerloc[2]+1):bannerloc[3]-1],
                   banner3, clt.textvec[bannerloc[3]:length(clt.textvec)] )
  print(clt.textvec)
  writeLines(clt.textvec, file.cwl)
}

## Usage:
# .fixup_clt_write_yaml(wf, file.cwl)

##############################
##   .fixup_yml_write_yml   ##
##############################
## code to handle features not supported by write_yaml()
.fixup_yml_write_yaml <- function(yml, file.yml) {
  banner1 <- c("#!/usr/bin/env cwltool",
	       paste0("#Created by systemPipeR on ",Sys.Date()),
               strrep("#",60),
               paste0("##  ", file.yml), # , strrep(" ",60-6-nchar(file.yml)), "##"),
               strrep("#",60), ""  )
  yml.textstring <- yaml::as.yaml(x=yml, indent.mapping.sequence=TRUE)
  # these n patches are only needed for rewrites of cwl files, first writes are correct if we are willing to list arrays instead of bracketing them
  dbg <- 0
  if (dbg>9) browser()
  yml.textstring <- gsub("outSAMtype: SAM", "outSAMtype: [ SAM ]", yml.textstring)
  yml.textvec <- unlist(strsplit(yml.textstring, split="\\n"))
  yml.textvec <- c(banner1, yml.textvec[1:length(yml.textvec)] )
  writeLines(yml.textvec, file.yml)
}

## Usage:
#  .fixup_yml_write_yaml(yamlinput_yml, file.yml)

######################
##   write.wf.cwl   ##
######################
## builds wf from WFdescription, WF.temp$clt and clt files
## writes wf to file.cwl
## does not write yamlinput to file.yml (see write.wf.yml())
## returns wf
write.wf.cwl <- function(WF.temp, WFdescription, cwlVersion, class, file.cwl, file.yml, dbg = 0) {
  dbg <- 1 # override arg for testing
  cwlVersion <- cwlVersion 
  class <- class
  ## REQUIREMENTS
  if(is.null(WFdescription$requirements)){
    dump <- "do nothing" 
  } else {
    ## works ok as is
#    if (WFdescription$requirements[[1]]$[1] == "listing") {
#      requirements[[1]][InitialWorkDirRequirement] <- list(list("testlist"))
 #   }
  }  


  ## load wf for WF

  ## PULL OUT REPLACEMENTS INFO FROM WFdescription, needed for steps and also to filter inputs_list
    ## need vectors for replacements with stepnames
    ##   replacement_value_vec
    ##   replacement_stepnames_vec
    ##   replacement_names_vec
  replacement_value_vec = NULL
  replacement_stepnames_vec <- NULL
  replacement_names_vec = NULL
  for(clt_index in seq_along(names(WFdescription$steps))) { # for each clt/step in the WFdescription
    if (dbg>9) print("write.wf.cwl - loop for each clt")
    if (dbg>9) print(names(WF.temp$clt[clt_index])) # cltlist names and indexes are same as WF$description$steps
    if (dbg>9) print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    for(input_index in seq_along(WFdescription$steps[[clt_index]])) {
      if (dbg>1) print("write.wf.cwl - loop for each keypair in each clt")
      if (dbg>1) print(names(WFdescription$steps[clt_index]))
      if (dbg>1) print(WFdescription$steps[[clt_index]][input_index])
      if (dbg>1) print(names(WFdescription$steps[[clt_index]][input_index]))
      replacement_value_vec <- c(replacement_value_vec, WFdescription$steps[[clt_index]][input_index])
      replacement_stepnames_vec <- c(replacement_stepnames_vec, names(WFdescription$steps[clt_index]))
      replacement_names_vec <- c(replacement_names_vec, names(WFdescription$steps[[clt_index]][input_index]))
    }
  }
  dbg <- 1

  ## code to extract base filename to use as a step name
  for (i in seq_along(replacement_stepnames_vec)) {
    replacement_stepnames_vec[i] <- gsub("\\.cwl$", "", replacement_stepnames_vec[i]) # drop the extension
    name <- strsplit(replacement_stepnames_vec[i], split="\\/")[[1]] # split on / to drop the folder names
    replacement_stepnames_vec[i] <- name[length(name)] # keep the base filename w/o extension
  }
  if (dbg>0) print(replacement_stepnames_vec)
  print(replacement_names_vec)
  if (dbg>0) print("end of replacements")
  if (dbg>9) browser("end of replacements")


  ## INPUTS

  input_type_vec = NULL
  input_stepnames_vec <- NULL
  input_names_vec = NULL
  for(clt_index in seq_along(names(WF.temp$clt))) { # for each clt in the WF we are building
    if (dbg>9) print("write.wf.cwl - loop for each clt")
    if (dbg>9) print(names(WF.temp$clt[clt_index])) # cltlist names and indexes are same as WF$description$steps
    if (dbg>9) print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    for(input_index in seq_along(WF.temp$clt[[clt_index]]$inputs)) {
      if (dbg>1) print("write.wf.cwl - loop for each input in each clt")
      if (dbg>1) print(names(WF.temp$clt[clt_index]))
      if (dbg>1) print(WF.temp$clt[[clt_index]]$inputs[[input_index]]$type)
      if (dbg>1) print(names(WF.temp$clt[[clt_index]]$inputs[input_index]))
      input_type_vec <- c(input_type_vec, WF.temp$clt[[clt_index]]$inputs[[input_index]]$type)
      input_stepnames_vec <- c(input_stepnames_vec, names(WF.temp$clt[clt_index]))
      input_names_vec <- c(input_names_vec, names(WF.temp$clt[[clt_index]]$inputs[input_index]))
    }
  }
  dbg <- 1

  ## code to extract base filename to use as a step name
  for (i in seq_along(input_stepnames_vec)) {
    input_stepnames_vec[i] <- gsub("\\.cwl$", "", input_stepnames_vec[i]) # drop the extension
    name <- strsplit(input_stepnames_vec[i], split="\\/")[[1]] # split on / to drop the folder names
    input_stepnames_vec[i] <- name[length(name)] # keep the base filename w/o extension
  }
  if (dbg>0) print(input_stepnames_vec)
  if (dbg>9) browser()
  
  ## create inputlist from vectors
  names(input_type_vec) <- input_names_vec
  inputs_list <- sapply(names(input_type_vec), function(x) list(input_type_vec[[x]]))  
#  inputs_list <- sapply(input_names_vec, function(x) list(input_type_vec[[x]]))  
  if (dbg>0) print(inputs_list)
  ## need to drop duplicates here, but keep detail for steps section later
  if (dbg>0) print(duplicated(names(inputs_list)))
  inputs_list_nodups <- inputs_list[!duplicated(names(inputs_list))] 
  inputs_list_nodups_noreplacements <- inputs_list_nodups[!names(inputs_list_nodups) %in% replacement_names_vec]
  if (dbg>0) print(inputs_list_nodups_noreplacements)
  print("SHOW ME THE INPUTS W/ DUPLICATES AND REPLACEMENTS REMOVED")
  if (dbg>9) browser()
  
  if (dbg>0) inputs_toplevel_list <- list(inputs_list) # make cwl toplevel container 
  if (dbg>0) names(inputs_toplevel_list) <- "inputs" # called outputs
  if (dbg>0) wf.textstring <- yaml::as.yaml(inputs_toplevel_list, indent.mapping.sequence=TRUE)
  if (dbg>0) cat(wf.textstring)
  if (dbg>0) print("end of inputs")
  if (dbg>9) browser("end of inputs")

  ## OUTPUTS
    ## need vectors for outputs with stepnames
    ##   output_type_vec        output type
    ##   output_stepnames_vec   output stepnames
    ##   output_names_vec       output names

  output_type_vec <- NULL
  output_stepnames_vec <- NULL
  output_names_vec <- NULL
  dbg <- 2
  for(clt_index in seq_along(names(WF.temp$clt))) { # for each clt in the WF we are building
    if (dbg>9) print("write.wf.cwl - loop for each clt")
    if (dbg>9) print(names(WF.temp$clt[clt_index])) # cltlist names and indexes are same as WF$description$steps
    if (dbg>9) print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    for(output_index in seq_along(WF.temp$clt[[clt_index]]$outputs)) { # for each output in the clt
      if (dbg>1) print("write.wf.cwl - loop for each output in each clt")
      if (dbg>1) print(names(WF.temp$clt[clt_index]))
      if (dbg>1) print(names(WF.temp$clt[[clt_index]]$outputs[output_index]))
      if (dbg>1) print(WF.temp$clt[[clt_index]]$outputs[[output_index]]$type)
      output_type_vec <- c(output_type_vec, WF.temp$clt[[clt_index]]$outputs[[output_index]]$type)
      output_stepnames_vec <- c(output_stepnames_vec, names(WF.temp$clt[clt_index]))
      output_names_vec <- c(output_names_vec, names(WF.temp$clt[[clt_index]]$outputs[output_index]))
    }
  }
  dbg <- 1

  ## code to extract base filename to use as a step name
  for (i in seq_along(output_stepnames_vec)) {
    output_stepnames_vec[i] <- gsub("\\.cwl$", "", output_stepnames_vec[i]) # drop the extension
    name <- strsplit(output_stepnames_vec[i], split="\\/")[[1]] # split on / to drop the folder names
    output_stepnames_vec[i] <- name[length(name)] # keep the base filename w/o extension
  }
  
  ## create outputlist from vectors
  outputs_list <- sapply(output_names_vec, function(x) list()) # make them a list AND give them lists
  if (dbg>1) print(outputs_list)
  for (i in seq_along(outputs_list)) { # for each output, populate the attribute list
    outputs_list[[i]] = list(outputSource=paste0(output_stepnames_vec[[i]], "/", output_names_vec[[i]]), type=output_type_vec[[i]]) # list becomes special cwl syntax "step/output" and type
  }
  if (dbg>1) print(outputs_list)

  if (dbg>0) outputs_toplevel_list <- list(outputs_list) # make cwl toplevel container 
  if (dbg>0) names(outputs_toplevel_list) <- "outputs" # called outputs
  if (dbg>0) wf.textstring <- yaml::as.yaml(outputs_toplevel_list, indent.mapping.sequence=TRUE)
  if (dbg>0) cat(wf.textstring)
  if (dbg>0) print("end of outputs")
  if (dbg>9) browser("end of outputs")
  
  ## STEPS
  ## we already know all the stepnames
  ## we already know all the inputs
  ## we already know all the outputs
  ## run: is the same as step
  
  .stepname <- function(filename) {
    name <- gsub("\\.cwl$", "", filename) # drop the extension
    name <- strsplit(name, split="\\/")[[1]] # split on / to drop the folder names
    name <- name[length(name)] # keep the basename w/o extension    
  }


  ## loop over steps to make in, out & run
  this_step_list <- list()
  this_step_vec <- NULL
  for(clt_index in seq_along(names(WF.temp$clt))) { # for each clt in the WF we are building
    print("Gordon wf1")
    print(names(WF.temp$clt[clt_index])) # cltlist names and indexes are same as WF$description$steps
    print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    this_step_name <- .stepname( names(WFdescription$steps[clt_index]) )
    cat("Processing ", this_step_name, "\n")
    ## filter inputs and outputs for this step
    ## need filter expression
#    print("line 1633")
#    if (dbg>9) browser()
    #input_stepnames_vec %in% "clt_hisat2-mapping-se-intronlen30-3000" 
    input_stepnames_vec %in% this_step_name
  
    filter_expr <- input_stepnames_vec %in% this_step_name
    filter_expr
    inputs_list
    ## use the filter expression
    inputs_list2 <- inputs_list[filter_expr] # works on the list
    inputs_list2
    input_names_vec_filtered <- input_names_vec[filter_expr] # works on the vector
    input_names_vec_filtered
  
    ## make in_list
#    cat("line 1654")
#    if (dbg>9) browser()
    
    names(input_names_vec_filtered) <- input_names_vec_filtered
    names(input_names_vec_filtered)
    in_list <- sapply(names(input_names_vec_filtered), function(x) list(input_names_vec_filtered[[x]]))
    in_list
  
    ## make substitutions
    # Example: x[["name"]] <- "Clair"; x
#    cat("line 1661")
#    if (dbg>9) browser()
    for (subst_index in seq_along(names(WFdescription$steps[clt_index][[1]]))) { # subst list can be empty
      in_list[[names(WFdescription$steps[clt_index][[1]][subst_index])]] <- WFdescription$steps[clt_index][[1]][[subst_index]]
    }
  
    ## make out_list
#    print("BROWSING AT OUT_LIST $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
#    if (dbg>9) browser()
    filter_expr <- output_stepnames_vec %in% this_step_name
    output_names_vec_filtered <- output_names_vec[filter_expr] # works on the vector
    output_names_vec_filtered
    out_list <- list(output_names_vec_filtered)
    out_list
  
    ## make bundle with run:
    this_step_list[[clt_index]] <- list()
#    this_step_list[[clt_index]] <- c(this_step_list[[clt_index]], list(inputs=in_list, out=out_list, run=names(WFdescription$steps[clt_index])))
    this_step_list[[clt_index]] <- c( list("in"=in_list, out=out_list, run=names(WFdescription$steps[clt_index])))

    this_step_vec <- c(this_step_vec, this_step_name)
    if (dbg>0) wf.textstring <- yaml::as.yaml(this_step_list[[clt_index]], indent.mapping.sequence=TRUE)
    if (dbg>0) cat(wf.textstring)
  }
  cat("line 1678")
  if (dbg>9) browser()
  names(this_step_list) <- this_step_vec # names(WFdescription$steps) # c("Step1", "Step2","Step3","Step4")
  print(this_step_list)
  steps_list <- c(this_step_list)

  if (dbg>1) print(steps_list)

  if (dbg>0) steps_toplevel_list <- list(steps_list) # make cwl toplevel container 
  if (dbg>0) names(steps_toplevel_list) <- "steps" # called steps
  if (dbg>0) wf.textstring <- yaml::as.yaml(steps_toplevel_list, indent.mapping.sequence=TRUE)
  if (dbg>0) cat(wf.textstring)
  if (dbg>0) print("END OF STEPS")
  if (dbg>9) browser("end of steps")
  


  ## prepare the wf object for return

  wf_object <- list()
  #wf_object <- list(cwlVersion=cwlVersion, class=class)  # NOT SURE IF IT'S BEST TO USE THESE OR THOSE IN header
  #if (dbg>9) browser()
  wf_object <- c(wf_object, list(cwlVersion=WFdescription$header[[1]],class=WFdescription$header[[2]])) # make this more generic
  wf_object <- c(wf_object, list(requirements=WFdescription$requirements, inputs=inputs_list_nodups_noreplacements, outputs=outputs_list, steps=steps_list))
  yaml::write_yaml(x=wf_object, file = file.cwl, indent.mapping.sequence=TRUE, handlers = list(
#	 logical = function(x) {   # example code from package, we don't use this
#		   result <- ifelse(x, "true", "false")
#		   class(result) <- "verbatim"
#		   return(result)
#		   }, 
	NOTcharacter = function(x) { # in hisat, character kills quotes needed for 3 arguments, but doesn't kill bad quotes on  version or listing
	result <- x
#        cat(class(result), result)
	class(result) <- "verbatim"
	return(result)
	}
	))
  print("READY TO FIXUP")
  if (dbg>9) browser()
  ## new code to handle features not supported by write_yaml()
  .fixup_wf_write_yaml(wf_object, file.cwl)

  # Gordon test lines follow
  ## quick test to see how read_yaml() and write_yaml() handle new wf_*.cwl file
  ######################################################################wf.read <- yaml::read_yaml(file.cwl) # error Duplicate map key: 'SampleName'
  #cat(unlist(wf.read), "\n")
  #yaml::write_yaml(wf.read, "hisat2.clt.rd.cwl", indent.mapping.sequence=TRUE)
  ######################################################################.fixup_wf_write_yaml(wf.read, "wf_hisat2-mapping-se_samtools.wf.read.cwl")

  ## end test

  wflist <- list(wf_object)
  #names(wflist) <- baseCommand # not as good a name as basename
  names(wflist) <- basename(file.cwl)
  print(names(wflist)) # look for the extension still present !@!
  if (dbg>9) browser()
  return(wf_object)
}

## Usage:
# wf_cwl <- write.wf.cwl(WFdescription, cwlVersion, class, file.cwl) 

#######################
##   write.clt.cwl   ##
#######################
write.clt.cwl <- function(commandLine, cwlVersion, class, file.cwl) {
  dbg <- 0
  cwlVersion <- cwlVersion 
  class <- class
  baseCommand <- commandLine$baseCommand[[1]]
  ## REQUIREMENTS
  if(is.null(commandLine$requirements)){
    dump <- "do nothing" 
  } else {
#    if (commandLine$requirements[[1]]$[1] == "listing") {
#      requirements[[1]][InitialWorkDirRequirement] <- list(list("testlist"))
 #   }
  }  
## ARGUMENTS
  if(is.null(commandLine$arguments)){
    dump <- "do nothing"
  } else {
#    arguments <- sapply(names(commandLine$arguments), function(x) list())
    arguments <- sapply(seq_along(commandLine$arguments), function(x) list())
    for(i in seq_along(commandLine$arguments)){
      arguments[[i]]["prefix"] <- commandLine$arguments[[i]]$prefix
      arguments[[i]]["valueFrom"] <- commandLine$arguments[[i]]$valueFrom
#      cat(class(arguments[[i]]["valueFrom"]))
    }
  }
  ## INPUTS
  if(any(names(commandLine$inputs)=="")) stop("Each element of the list 'commandLine' needs to be assigned a name")
  if(is.null(names(commandLine$inputs))) stop("Each element of the list 'commandLine' needs to be assigned a name")
  inputs <- sapply(names(commandLine$inputs), function(x) list())
  for(i in seq_along(commandLine$inputs)){
    #if (dbg>9) browser()
    if(any(c("label", "secondaryFiles", "doc", "default", "format", "streamable") %in% names(commandLine$inputs[[i]]))){
#      for(j in which(c("label", "secondaryFiles", "doc", "default", "format", "streamable") %in% names(commandLine$inputs[[i]]))){
#        inputs[[i]][names(commandLine$inputs[[i]][j])] <- commandLine$inputs[[i]][names(commandLine$inputs[[i]])][[j]]
#      }
      for(j in which( names(commandLine$inputs[[i]])  %in% c("label", "secondaryFiles", "doc", "default", "format", "streamable")   )){
        inputs[[i]][names(commandLine$inputs[[i]][j])] <- commandLine$inputs[[i]][names(commandLine$inputs[[i]])][[j]]
      }
    }
    if("type" %in% names(commandLine$inputs[[i]])){
      if(!c("type") %in% names(commandLine$inputs[[i]])) stop("Each element of the sublist 'inputs' in 'commandLine' needs to be defined the type of the argument, for example: type='Directory' or type='File' or type='int' or type='string'")
      inputs[[i]]["type"] <-commandLine$inputs[[i]]$type 
    } 
    if("prefix" %in% names(commandLine$inputs[[i]])){
      if(commandLine$inputs[[i]]$prefix==""){
        inputs[[i]]["inputBinding"] <- list(list(prefix=NULL))
      } else{
        if ("itemSeparator" %in% names(commandLine$inputs[[i]])) {
          inputs[[i]]["inputBinding"] <- list(list(prefix=commandLine$inputs[[i]]$prefix, itemSeparator=commandLine$inputs[[i]]$itemSeparator))
        } else {
          inputs[[i]]["inputBinding"] <- list(list(prefix=commandLine$inputs[[i]]$prefix))
        }
      }
    } 
  }
  ## OUTPUTS
  outputs <- sapply(names(commandLine$outputs), function(x) list())
  if(!c("type") %in% names(commandLine$inputs[[i]])) stop("Each element of the sublist 'outputs' in 'commandLine' needs to be defined the type of the argument, for example: type='Directory' or type='File'.")
  if(all(!c("File", "Directory") %in% commandLine$outputs[[1]]$type)) stop("Each element of the sublist 'outputs' in 'commandLine' needs to be defined the type = 'Directory', 'File'.")
  for(i in seq_along(commandLine$outputs)){
    outputs[[i]]["type"] <- commandLine$outputs[[i]]$type
    outputs[[i]]["outputBinding"] <- list(list(glob=commandLine$outputs[[i]][[2]]))
  }
  clt <- list()
  #clt <- list(cwlVersion=cwlVersion, class=class)  # NOT SURE IF IT'S BEST TO USE THESE OR THOSE IN header
#  if(exists("requirements")){
#    clt <- c(clt, list(requirements=requirements))
#  }
#  if(exists("arguments")){
#    clt <- c(clt, list(arguments=arguments))
#  }
  #if (dbg>9) browser()
  header <- commandLine$header
  clt <- c(clt, list(cwlVersion=header[[1]],class=header[[2]],doc=header[[3]],hints=header[[4]])) # make this more generic
  clt <- c(clt, list(baseCommand=baseCommand, requirements=commandLine$requirements, arguments=arguments, inputs=inputs, outputs=outputs))
  yaml::write_yaml(x=clt, file = file.cwl, indent.mapping.sequence=TRUE, handlers = list(
#	 logical = function(x) {   # example code from package, we don't use this
#		   result <- ifelse(x, "true", "false")
#		   class(result) <- "verbatim"
#		   return(result)
#		   }, 
	NOTcharacter = function(x) { # in hisat, character kills quotes needed for 3 arguments, but doesn't kill bad quotes on  version or listing
	result <- x
#        cat(class(result), result)
	class(result) <- "verbatim"
	return(result)
	}
	))
  ## new code to handle features not supported by write_yaml()
  .fixup_clt_write_yaml(clt, file.cwl)

  # Gordon 11 test lines follow
  ## quick test to see how read_yaml() and write_yaml() handle existing star-mapping-pe.cwl file
  clt.rd <- yaml::read_yaml(file.cwl)
  #cat(unlist(clt.rd), "\n")
  #yaml::write_yaml(clt.rd, "hisat2.clt.rd.cwl", indent.mapping.sequence=TRUE)
  .fixup_clt_write_yaml(clt.rd, "clt_hisat2.clt.rd.cwl")

  clt.rd <- yaml::read_yaml("./param/cwl/star/star-idx/star-index.cwl")
  .fixup_clt_write_yaml(clt.rd, "clt_star-index.cwl")
  clt.rd <- yaml::read_yaml("./param/cwl/star/star-pe/star-mapping-pe.cwl")
  .fixup_clt_write_yaml(clt.rd, "clt_star-mapping-pe.cwl")
  ## end test

  cltlist <- list(clt)
  #names(cltlist) <- baseCommand # not as good a name as basename
  names(cltlist) <- basename(file.cwl)
  return(cltlist)
}

## Usage:
# clt_cwl <- write.clt.cwl(commandLine, cwlVersion, class, file.cwl) 

###################
##   write.wf.yml   ##
###################
## Write the yaml file  
write.wf.yml <- function(WF.temp, WFdescription, file.yml, results_path, module_load){
  dbg <- 0
  steps <- names(WFdescription$steps)
  steps
  ymlsteps <- list()
  for (i in seq_along(steps)) {
  #ymlsteps <- rapply(ymlsteps, function(x) gsub("\\.cwl$", ".yml", ymlsteps[[i]]), how = "replace")
    ymlsteps[i] <- gsub("\\.cwl$", ".yml", steps[[i]])
  }
  print(ymlsteps)
  print(typeof(ymlsteps))
  ymlsteps <- unlist(ymlsteps) 
  print(ymlsteps)
  ymlpaths <- sapply(seq_along(ymlsteps), function(x) normalizePath(ymlsteps[x])) # was wf$steps[[steps[x]]]$run
  ymllist <- sapply(ymlpaths, function(x) yaml::read_yaml(file.path(x)), simplify = F) 
  names(ymllist) <- sapply(seq_along(steps), function(x) steps[x]) # was wf$steps[[steps[x]]]$run
  ymllist  
  print(names(ymllist))  
  #####################################################################################################

  ## VALUES
  
  WF.temp$ymllist <- ymllist # we will not keep this unless Daniela wants to, similar to WF.temp$clt (list)
dbg = 2
  input_value_vec = NULL
  input_stepnames_vec <- NULL
  input_names_vec = NULL
  final_ymllist = list()
  nn <- 1 # counter for module_list
  for(clt_index in seq_along(names(WF.temp$ymllist))) { # for each file.yml in the WF we are building
    if (dbg>9) print("write.wf.yml - loop for each file.yml")
    if (dbg>9) print(names(WF.temp$ymllist[clt_index])) # cltlist names and indexes are same as WF$description$steps
    if (dbg>9) print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    for(input_index in seq_along(WF.temp$ymllist[[clt_index]])) {
      if (dbg>1) print("write.wf.yml - loop for each value in each file.yml")
      if (dbg>1) print(names(WF.temp$ymllist[clt_index]))
      if (dbg>1) print(WF.temp$ymllist[[clt_index]][[input_index]][[1]])
      if (dbg>1) print(names(WF.temp$ymllist[[clt_index]][input_index]))
      input_value_vec <- c(input_value_vec, WF.temp$ymllist[[clt_index]][[input_index]][[1]])
      input_stepnames_vec <- c(input_stepnames_vec, names(WF.temp$ymllist[clt_index]))
      input_names_vec <- c(input_names_vec, names(WF.temp$ymllist[[clt_index]][input_index]))
#      cat("browser in value loop")
#      browser("browser in value loop")
      if ( names(WF.temp$ymllist[[clt_index]][input_index]) == "ModulesToLoad") {
        module_load[nn] <- WF.temp$ymllist[[clt_index]][[input_index]][[1]]
        nn = nn + 1
      }
      else { # everything else
        # this method kills the duplicates right here - good
        # but remember some duplicate id's like SampleName, results_path may have different values in different steps
        #   I believe this only bothers the cwltool, not runCommandline()
        if (isTRUE(final_ymllist[[names(WF.temp$ymllist[[clt_index]][input_index])]] > "")) { # first settings are best so far
          # do nothing
        } else {
          final_ymllist[[names(WF.temp$ymllist[[clt_index]][input_index])]] <- WF.temp$ymllist[[clt_index]][[input_index]]
        }
      }
    }
  }
  print(final_ymllist)
  print(module_load)
  dbg <- 1
  if (dbg>0) cat("line1898")
  if (dbg>9) browser()
  
  ## code to extract base filename to use as a step name
  for (i in seq_along(input_stepnames_vec)) {
    input_stepnames_vec[i] <- gsub("\\.cwl$", "", input_stepnames_vec[i]) # drop the extension
    name <- strsplit(input_stepnames_vec[i], split="\\/")[[1]] # split on / to drop the folder names
    input_stepnames_vec[i] <- name[length(name)] # keep the base filename w/o extension
  }
  if (dbg>0) print(input_stepnames_vec)
  if (dbg>9) browser()
  
  ## create inputlist from vectors
  names(input_value_vec) <- input_names_vec
  inputs_list <- sapply(names(input_value_vec), function(x) list(input_value_vec[[x]]))  
#  inputs_list <- sapply(input_names_vec, function(x) list(input_value_vec[[x]]))  
  if (dbg>0) print(inputs_list)
  ## need to drop duplicates here, but keep detail for steps section later
  if (dbg>0) print(duplicated(names(inputs_list)))
  inputs_list_nodups <- inputs_list[!duplicated(names(inputs_list))] 
  
  ############################
    ## PULL OUT REPLACEMENTS INFO FROM WFdescription, needed for steps and also to filter inputs_list
    ## need vectors for replacements with stepnames
    ##   replacement_value_vec
    ##   replacement_stepnames_vec
    ##   replacement_names_vec
  replacement_value_vec = NULL
  replacement_stepnames_vec <- NULL
  replacement_names_vec = NULL
  for(clt_index in seq_along(names(WFdescription$steps))) { # for each clt/step in the WFdescription
    if (dbg>9) print("write.wf.cwl - loop for each clt")
    if (dbg>9) print(names(WF.temp$clt[clt_index])) # cltlist names and indexes are same as WF$description$steps
    if (dbg>9) print(names(WFdescription$steps[clt_index])) # recall the cltlist was built from the WF$description$steps in createWF()
    for(input_index in seq_along(WFdescription$steps[[clt_index]])) {
      if (dbg>1) print("write.wf.cwl - loop for each keypair in each clt")
      if (dbg>1) print(names(WFdescription$steps[clt_index]))
      if (dbg>1) print(WFdescription$steps[[clt_index]][input_index])
      if (dbg>1) print(names(WFdescription$steps[[clt_index]][input_index]))
      replacement_value_vec <- c(replacement_value_vec, WFdescription$steps[[clt_index]][input_index])
      replacement_stepnames_vec <- c(replacement_stepnames_vec, names(WFdescription$steps[clt_index]))
      replacement_names_vec <- c(replacement_names_vec, names(WFdescription$steps[[clt_index]][input_index]))
    }
  }

  final_ymllist <- final_ymllist[!names(final_ymllist) %in% replacement_names_vec]
  
  ############################
  
  
  inputs_list_nodups_noreplacements <- inputs_list_nodups[!names(inputs_list_nodups) %in% replacement_names_vec]
  if (dbg>0) print(inputs_list_nodups_noreplacements)
  print("SHOW ME THE INPUTS W/ DUPLICATES AND REPLACEMENTS REMOVED")
  if (dbg>9) browser()
  
#junk  if (dbg>0) inputs_toplevel_list <- list(inputs_list) # make cwl toplevel container 
#junk  if (dbg>0) names(inputs_toplevel_list) <- "values" # called values


  if (dbg>0) inputs_toplevel_list <- final_ymllist # only for debug print
  if (dbg>0) wf.textstring <- yaml::as.yaml(inputs_toplevel_list, indent.mapping.sequence=TRUE)
  if (dbg>0) cat(wf.textstring)
  if (dbg>0) print("end of values")
  if (dbg>9) browser("end of values")






  #####################################################################################################

  yamlinput_yml <- final_ymllist # reconnect to old code
  ## results_path
  yamlinput_yml[["results_path"]]["class"] <- list("Directory")
  yamlinput_yml[["results_path"]]["path"] <- list(results_path)
  ## moduleload
  module_load_nodups <- module_load[!duplicated(module_load)] 
  for(i in seq_along(module_load_nodups)){
    yamlinput_yml[["ModulesToLoad"]][paste0("module", i)] <- list(module_load_nodups[[i]])
  }
  ## write out the '.yml' file
  yaml::write_yaml(x=yamlinput_yml, file = file.yml) 
  ## new code to handle features not supported by write_yaml()
  .fixup_yml_write_yaml(yamlinput_yml, file.yml)
  return(yamlinput_yml)
}

## Usage: 
# yamlinput_yml <- write.wf.yml(WF.temp, WFdescription, file.yml, results_path, module_load) 

###################
##   write.clt.yml   ##
###################
## Write the yaml file  
write.clt.yml <- function(commandLine, file.yml, results_path, module_load){
  dbg <- 0
  inputs <- commandLine$inputs
  if(any(names(inputs)=="")) stop("Each element of the list 'commandLine' needs to be assigned a name")
  if(is.null(names(inputs))) stop("Each element of the list 'commandLine' needs to be assigned a name")
  ##yamlinput_yml 
  yamlinput_yml <- sapply(names(inputs), function(x) list())
  for(i in seq_along(inputs)){
    if(!c("type") %in% names(inputs[[i]])) stop("Each element of the sublist 'inputs' in 'commandLine' needs to be defined the type of the argument, for example: type='Directory' or type='File' or type='int' or type='string'")
    if("type" %in% names(inputs[[i]])){
      if(any(c("File", "Directory") %in% inputs[[i]])){
        yamlinput_yml[[i]]["class"] <- inputs[[i]]$type
        yamlinput_yml[[i]]["path"] <- inputs[[i]]$yml
      } else if (any(c("int", "string") %in% inputs[[i]])){
        yamlinput_yml[[i]]  <- inputs[[i]]$yml
      }
    } else {
      print("do something")
    }
  }
  ## results_path
  yamlinput_yml[["results_path"]]["class"] <- list("Directory")
  yamlinput_yml[["results_path"]]["path"] <- list(results_path)
  ## moduleload
  for(i in seq_along(module_load)){
    yamlinput_yml[["ModulesToLoad"]][paste0("module", i)] <- list(module_load[[i]])
  }
  ## write out the '.yml' file
  yaml::write_yaml(x=yamlinput_yml, file = file.yml) 
  ## new code to handle features not supported by write_yaml()
  .fixup_yml_write_yaml(yamlinput_yml, file.yml)
  return(yamlinput_yml)
}

## Usage: 
# yamlinput_yml <- write.clt.yml(commandLine, file.yml, results_path, module_load) 


