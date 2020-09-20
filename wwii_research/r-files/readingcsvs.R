setwd('/home/dougliebe/Documents/CoD/csvs')
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv, header = F)

complete <- data.frame()
for(i in 1:length(myfiles)) {
  # print(ncol(myfiles[[i]]))
  complete <- rbind(complete,myfiles[[i]])
}
complete <- subset(complete, complete$V8 == 'gib')