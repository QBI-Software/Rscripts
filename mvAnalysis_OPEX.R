#libraries

library("car")
library("FactoMineR")
library("factoextra")
library("corrplot")

# Analysing XNAT OPEX data
#http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining
datadir <- "D:/Dropbox/worktransfer/opex/mva"
datafile <- c("opex_subjects.csv","opex_cantab_dms.csv","opex_cantab_swm.csv",
              "opex_cantab_pal.csv","opex_cantab_mot.csv")
df_participants <- read.table(file.path(datadir, datafile[1]), sep=",", header = TRUE)
df_dms <- read.table(file.path(datadir, datafile[2]), sep=",", header = TRUE)
df_swm <- read.table(file.path(datadir, datafile[3]), sep=",", header = TRUE)
df_pal <- read.table(file.path(datadir, datafile[4]), sep=",", header = TRUE)
df_mot <- read.table(file.path(datadir, datafile[5]), sep=",", header = TRUE)

##Function - Filtering
filtertype <-function(mydf,datatype)
{
  mydf[grepl(datatype, names(mydf))]
}
# for (mydf in c(df_participants, df_dms, df_swm, df_pal, df_mot)){
#   mydf[which(mydf["Group"] != 'withdrawn'),]
# }
df_participants <-df_participants[which(df_participants$Group != "withdrawn"),]
df_dms <-df_dms[which(df_dms$Group != "withdrawn" & df_dms$sample_quality != "Poor" & df_dms$data_valid != "invalid" & df_dms$status != "SYSTEM_ERROR"),]
df_swm <-df_swm[which(df_swm$Group != "withdrawn" & df_swm$sample_quality != "Poor" & df_swm$data_valid != "invalid" & df_swm$status != "SYSTEM_ERROR"),]
df_pal <-df_pal[which(df_pal$Group != "withdrawn" & df_pal$sample_quality != "Poor" & df_pal$data_valid != "invalid" & df_pal$status != "SYSTEM_ERROR"),]
df_mot <-df_mot[which(df_mot$Group != "withdrawn" & df_mot$sample_quality != "Poor" & df_mot$data_valid != "invalid" & df_mot$status != "SYSTEM_ERROR"),]

#Get baseline
df_dms0 <-df_dms[which(df_dms$interval == 0),]
df_swm0 <-df_swm[which(df_swm$interval == 0),]
df_pal0 <-df_pal[which(df_pal$interval == 0),]
df_mot0 <-df_mot[which(df_mot$interval == 0),]
df_dms0$Group <- NULL
df_swm0$Group <- NULL
df_pal0$Group <- NULL
df_mot0$Group <- NULL

dropcols <- c(-1,-3,-4,-5,-6,-7,-8,-9,-10)
df_cantab0 <- merge(df_dms0[dropcols], df_swm0[dropcols], by='Subject', suffixes=c(".DMS",".SWM"))
df_cantab0 <- merge(df_cantab0, df_pal0[dropcols], by='Subject')
df_cantab0 <- merge(df_cantab0, df_mot0[dropcols], by='Subject')
df_age <- merge(df_cantab0, df_dms0[c(2,5)], by='Subject')
#Scatterplot matrix correlation
scatterplotMatrix(df_cantab0[2:10], diagonal="boxplot")

#Get interval 1m
#df_dms1 <-df_dms[which(df_dms$interval == 1),]
#Calculating Summary Statistics
summary(df_cantab0)

#Remove zero stddev
mask = lapply(df_cantab0, function(x) sd(x, na.rm = TRUE) > 0)
df_cantab_filtered <- df_cantab0[, names(which(unlist(mask)))]
#sapply(df_cantab0,mean) #can also do sd, range, quantile, etc

#Function for testing one col vs all others
agecorrelated <-function(age,mydfvars,topten)
{
  cormatrix <- cor(age,mydfvars, use="pairwise.complete.obs")
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("Age", "Var2","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=topten)
  
}
#Function for finding only significant correlations
sigcorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
  
}

#Function for finding diff from zero correlations
diffcorrelated <-function(mydf,cols,topten)
{
  cormatrix <- cortest.mat(mydf)
}

#Run correlation tests
cor.mat <- cor(df_cantab_filtered[c(-1)],use="pairwise.complete.obs")
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)

print("**Pearson's product-moment correlation - Top matches**")
agecorrelated(df_age$Age,df_cantab_filtered[c(-1)],10)
sigcorrelated(df_cantab_filtered[c(-1)],20) 
