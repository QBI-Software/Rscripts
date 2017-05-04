#!/usr/bin/env Rscript
# *******************************************************************************
#   
#   QBI tracking analysis : allmeanspeed
# 
# *******************************************************************************
#   Description: this script parses a file called "Track statistics_<ID>.xls"
#   Output: Appends to a CSV file with "<ID>_TRACK_ID, <ID>_TRACK_MEAN_SPEED" for each file
#
#   R will need the following packages installed:
#          1. plyr
#          2. optparse
#          3. gdata
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
library(gdata)
library(optparse)

option_list = list(
  make_option(c("-d", "--dir"), type="character", default="data\\input", 
              help="dataset directory [default= %default]", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="Allmeanspeed.csv", 
              help="output filename [default= %default]", metavar="character"),
  make_option(c("-h","--help"), type="store_true")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
if (opt$help){
  print_help(opt_parser)
  stop("Several arguments can be supplied or just accept defaults", call.=FALSE)
}
##### CONFIGURE HERE ############
#Input directory (edit this if necessary or accept default)
# eg datadir <- "D:\\Projects\\Meunier_tracking\\vanessa\\data\\input"
datadir <- opt$dir

#File must start with this in the name
linkspattern <-"Track statistics"

#Output files with this suffix
outputfilename <- opt$out
outputfile <- file.path(datadir, "output", outputfilename)
##########Methods################

pc <- function(zerocount,totalcount){
  pc<- (zerocount/totalcount) * 100
}

##########Process data###########
#####Loop through directory
for (linksfile in list.files(datadir)){
  if (grepl(linkspattern,linksfile)){
    print(paste("Loading file: ", linksfile))
    #extract ID
    parts <- strsplit(linksfile,'.xls')
    fileid <- strsplit(parts[[1]],'_')
    fileid <- fileid[[1]][2]
    ##### Read file data
    df_links <- read.delim(file.path(datadir, linksfile),  header = TRUE, sep = '\t')
    ## Get unique IDs
    df_trackids <- data.frame(df_links$TRACK_ID,df_links$TRACK_MEAN_SPEED)
    colnames(df_trackids) <- c(paste(fileid,"_TRACK_ID", sep=""), paste(fileid,"_TRACK_MEAN_SPEED", sep=""))
    ## Append to existing file if it exists
    if (file.exists(outputfile)){
      all_trackids <- read.csv(outputfile, header=TRUE, check.names = FALSE, as.is = TRUE)
      df_trackids <- cbindX(all_trackids,df_trackids)
    }
    write.csv(df_trackids, file = outputfile, row.names = FALSE)
    print(paste("Saving file: ", outputfile))
  }
}
  


