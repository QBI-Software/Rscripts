#!/usr/bin/env Rscript
# *******************************************************************************
#   
#   QBI tracking analysis : zerovelocity
# 
# *******************************************************************************
#   Description: zerovelocity.R parses a file called "Links in tracks statistics_<ID>.xls"
#   Output: CSV file with "Trackid, zero velocity count, total spot count, percentage"
#
#   R will need the following packages installed:
#          1. plyr
#          2. optparse
#          3. xlsx
#   To run: 
#     1. Save this file to the directory above your data files
#     2. The default input data directory is data\\input
#     3. In RStudio, open this file then if necessary modify the directory path, 
#          filename or output suffix at "CONFIGURE HERE", save and click "Source"
#
#   To run from commandline: >Rscript zerovelocity.R --help
#   
#   Copyright (C) 2017  QBI Software, The University of Queensland
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#################################################################################
library(plyr)
library(optparse)
library(xlsx)

option_list = list(
  make_option(c("-d", "--dir"), type="character", default="data\\input", 
              help="dataset directory [default= %default]", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="_ZeroVelocity.csv", 
              help="output file extension [default= %default]", metavar="character"),
  make_option(c("-h","--help"), type="store_true")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
if (opt$help){
  print_help(opt_parser)
  stop("Several arguments can be supplied or just accept defaults", call.=FALSE)
}
##### CONFIGURE HERE ############
#Input directory 
#(Note, if relative path then make sure current working directory is set to script's dir 
# with Session->set working directory -> To source location
# check with "getwd()" at the console)
# eg datadir <- "D:\\Projects\\Meunier_tracking\\vanessa\\data\\input"
datadir <- opt$dir

#Output directory
outputdir <- file.path(datadir, "output")

#File must start with this in the name
linkspattern <-"Links in tracks statistics"

#Output files with this suffix
# eg suffix <- "_ZeroVelocity.csv"
suffix <- opt$out

##########Methods################

pc <- function(zerocount,totalcount){
  pc<- (zerocount/totalcount) * 100
}

fileid <- function(linksfile){
  parts <- strsplit(linksfile,'\\.')
  fileparts <- strsplit(parts[[1]],'_')
  fileid <- fileparts[[1]][2]
}
##########Process data###########
#check folders exist
if (file.exists(datadir) && file.exists(outputdir)){
#####Loop through directory
for (linksfile in list.files(datadir)){
  if (grepl(linkspattern,linksfile)){
    #extract ID
    fid <- fileid(linksfile)
    outputfilename <- paste(fid, suffix, sep="")
    #####Calculate frequency of zero velocity in tracks
    df_links <- read.delim(file.path(datadir, linksfile),  header = TRUE, sep = '\t')
    ## Get unique IDs
    trackid <- unique(df_links$TRACK_ID)
    df_trackids <- data.frame(trackid)
    ## Cross table of track velocity
    y = xtabs(~ TRACK_ID + VELOCITY, df_links)
    df_trackids$countzerov <- y[,1]
    df_trackids$totalspots <- rowSums(y)
    df_trackids$percentage <- mapply(pc,df_trackids$countzerov,df_trackids$totalspots)
    #Output file as csv and excel (combined)
    write.csv(df_trackids, file = file.path(outputdir, outputfilename))
    write.xlsx(x = df_trackids, file = file.path(outputdir,"all_zerovelocity.xlsx"),
               sheetName = fid, row.names = FALSE,append=TRUE)
    print(paste("Saving file: ", outputfilename))
  }
}
  
print("Processing complete")
}else{
  print(paste("Input OR output directory is not found: ", datadir, outputdir))
  
}


