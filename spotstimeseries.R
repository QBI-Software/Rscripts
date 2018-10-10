#!/usr/bin/env Rscript
# *******************************************************************************
#   
#   QBI tracking analysis : spotstimeseries
# 
# *******************************************************************************
#   Description: spotstimeseries.R parses a file called "All Spots statistics_<ID>.xls"
#     and produces a time series histogram of POSITION_T
#   Output: CSV file with "timeseries, x (POSITION_T time), freq (spot count per POSITION_T)"
#     and a plot of x and freq over time.  Anomalies in spots per frame should be apparent as 
#     large peaks or troughs.
#
#   R will need the following packages installed:
#          1. plyr
#          2. optparse
#
#   To run: 
#     1. Save this file to the directory above your data files 
#   - the input and output folders must exist
#     2. The default input data directory is data\\input
#     3. The default output data directory is data\\input\\output
#     3. In RStudio, open this file then if necessary modify the directory path, 
#          filename or output suffix at "CONFIGURE HERE", save and click "Source"
#
#   To run from commandline: >Rscript spotstimeseries.R --help
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
library("optparse")

option_list = list(
  make_option(c("-d", "--dir"), type="character", default="data\\input", 
              help="dataset directory [default= %default]", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="_spotsFreq.csv", 
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
#Input directory (edit this if necessary or accept default)
# eg datadir <- "D:\\Projects\\Meunier_tracking\\vanessa\\data\\input"
datadir <- opt$dir

#Output directory
outputdir <- file.path(datadir, "output")

#File must start with this in the name
linkspattern <-"All Spots statistics"

#Output files with this suffix
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
      print(paste("Loading file: ", linksfile))
      #extract ID
      fid <- fileid(linksfile)
      outputfilename <- paste(fid, suffix, sep="")
      #####Calculate frequency of zero velocity in tracks
      ftype <- strsplit(linksfile,'\\.')[[1]][2]
      if (ftype == 'xls'){
        separator = '\t';
      }else{
        separator = ',';
      }
      df_links <- read.delim(file.path(datadir, linksfile),  header = TRUE, sep = separator)
      # Create Timeseries for POSITION_T
      post_count <- count(df_links$POSITION_T)
      positionTimeseries <- ts(post_count)
      plot(positionTimeseries, main=outputfilename)
      #Output file as csv
      write.csv(post_count, file = file.path(outputdir, outputfilename), append=FALSE)
      print(paste("Saving file: ", outputfilename))
    }
  }
  print("Processing complete")
}else{
  print(paste("Input OR output directory is not found: ", datadir, outputdir))

}

  



