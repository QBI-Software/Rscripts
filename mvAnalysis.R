#libraries

library("car")
library("FactoMineR")
library("factoextra")
library("corrplot")

# Analysing XNAT OPEX data
#http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining
datadir <- "D:/Data/XNATData/baseline/"
datafile <- "DMS_baseline.csv"
df_mv <- read.table(file.path(datadir, datafile), sep=",", header = TRUE)
df_mv
length(df_mv)
####FUNCTION for filtering data columns
filtertype <-function(mydf,datatype)
{
  mydf[grepl(datatype, names(mydf))]
}

df_mv1 <- filtertype(df_mv,"DMS")
#df_mv1$Age <- df_mv$Age

df_mvf <-df_mv[12:18] #subset only as too big
#Plot each var against each other - middle are histograms
scatterplotMatrix(df_mvf)

#Calculating Summary Statistics
summary(df_mvf)
sapply(df_mvf,mean) #can also do sd, range, quantile, etc


#######
### FUNCTIONS for Calculating Correlations for Multivariate Data
cor.test(df_mvf$DMSCC, df_mvf$DMSL0SD) #specify columns
cor.test(df_mv$Age, df_mv$DMSL0SD)
#Function for testing one col vs all others
agecorrelated <-function(mydf,mydfvars,topten)
{
  cormatrix <- cor(mydf$Age,mydfvars, use="pairwise.complete.obs")
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
cor.mat <- cor(df_mv1,use="pairwise.complete.obs")
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)

print("**Pearson's product-moment correlation - Top matches**")
agecorrelated(df_mv,df_mv1,10)
sigcorrelated(df_mv1,20) 

#Principal Component Analysis ...

