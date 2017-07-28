# Parsing Multiplex data
datadir <- "D:\\Data\XNATData\\"
datafile <- "baseline_cantabDMS.csv"
df_mv <- read.table(file.path(datadir, datafile), sep=",", header = TRUE)
df_mv
df_mvf <-df_mv[12:18]

scatterplotMatrix(df_mvf)

df_mvf.summary()


