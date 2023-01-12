#read file
file <- read.csv(file = 'Data/geoMap.csv', header = FALSE, 
                 stringsAsFactors = FALSE, skip = 3)
names(file) <- c('Region','GB','GG')

#Q1
file$GG[file$GG == "<1"] <- 0
file

#Q2
file <- transform(file, GG = as.numeric(GG))
q2 <- file$GB > file$GG
table(q2)

#Q3
file$Region[file$GG+10 > file$GB]

#Q4
length(file$Region[file$GG+10 > file$GB])/length(file$Region)

#Q5
file$GG[file$Region=="New Hampshire"]/file$GB[file$Region=="New Hampshire"]

#Q6
barplot(t(file[c('GG','GB')]),beside=TRUE, ylim=c(0,max(file[c('GG','GB')])),
  names.arg=file$Region, xlab='region', ylab='gift', legend.text=c('GG','GB'))

