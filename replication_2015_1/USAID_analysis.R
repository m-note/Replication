rm(list=ls())


setwd("/Users/S/Dropbox/Study/Data")
library(foreign)
data <- read.spss("USAID_DG_Programs.sav", to.data.frame=TRUE)

# How much money is spent for democracy & governance in 2004
sum(subset(data$AID100, data$year=="2004"))

# How much aid is paid each year?
year_sum <-  rep(0:0,15)
for (i in 1990:2004){year_sum[i-1989]<-sum(subset(data$AID100, data$year==i))}
year_sum
mean(year_sum)

# Trend in Polity Score (Average)
year_sum <-  rep(0:0,15)
for (i in 1990:2004){year_sum[i-1989]<-mean(subset(data$DG01, data$year==i), na.rm=TRUE)}
year_sum
plot(1990:2004,year_sum)
hist(data$DG01)

# Trend in Polity Score (Extract each year)
year_Polity <- as.list(NULL) 
for (i in 1990:2004){year_Polity[[i-1989]]<-subset(data$DG01, data$year==i)}

hist(year_Polity[[1]])
for (i in 2:15){hist(year_Polity[[i]],add=T)}

hist(data$DG01,breaks=20)

# Trend in Freedom House Score (Average)
hist(data$DG02,breaks=14)

# Extract data (1996-2004 only) --> Manually renamed country name to keep consistency with USAID data
    # Following data in USAID can't be found in WGI: Saint Lucia, Yugoslavia (Serbia-Montenegro), Zaire
data_extract <- rbind(data[data$year==1996,],data[data$year==1998,],data[data$year==2000,],data[data$year==2002,],data[data$year==2003,],data[data$year==2004,])
rownames(data_extract) <- NULL
    #write.csv(data_extract,"data_extract.csv")   # After saving, sorted with name in Excel
data_extract <- read.csv("data_extract.csv", header=TRUE)
    # Check for sure --> results are almost same as above
        hist(data_extract$DG01)
        hist(data_extract$DG02)

# Match WGI data to USAID data
data_WGI <- read.csv("/Users/S/Dropbox/Study/６学期/論文：国際政治経済の諸問題/Data/WGI/WGI_Data_Renamed.csv", header=TRUE)
  # 欠損値はExcelでNAにしている
#colnames(data_WGI)[4] <- 1996
#colnames(data_WGI)[5] <- 1998
#colnames(data_WGI)[6] <- 2000
#colnames(data_WGI)[7] <- 2002
#colnames(data_WGI)[8] <- 2003
#colnames(data_WGI)[9] <- 2004

    # Prepare Empty Column
    data_extract$WGI_PV <- NA
    data_extract$WGI_RL <- NA
    data_extract$WGI_RQ <- NA
    data_extract$WGI_CC <- NA
    data_extract$WGI_VA <- NA
    data_extract$WGI_GE <- NA
    data_extract <- data_extract[,c(1:5,541:546,6:540)]  # 行の入れ替え

    # Delete Unwanted White space
    library(stringi)

length_AID <- length(data_extract$year)
length_WGI <- length(data_WGI$cname)

#--------------------------------------

for (i in 1:length_AID){ #PV用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s1 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s1]), pattern = "\\P{Wspace}")
    
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s1]), pattern = "\\P{Wspace}")
      
      if (grepl("PV.EST", seriesWGI)==1) {  # grepl has the return value TRUE or FALSE / grep returns the point the letter starts
        data_extract$WGI_PV[i]<-as.numeric(as.character(data_WGI[s1,co]))
        #print(as.numeric(as.character(data_WGI[s1,co])))
        next 
      } else {next}
    }
  }
}  

#-----------------------------------------------
for (i in 1:length_AID){ #RL用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s2 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s2]), pattern = "\\P{Wspace}")
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s2]), pattern = "\\P{Wspace}")
      if (grepl("RL.EST", seriesWGI)==1) {
        data_extract$WGI_RL[i]<-as.numeric(as.character(data_WGI[s2,co]))
        #print(as.numeric(as.character(data_WGI[s,co])))
        next
      }
    } else {next}
  }
} 

#-----------------------------------------
for (i in 1:length_AID){ #RQ用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s3 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s3]), pattern = "\\P{Wspace}")
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s3]), pattern = "\\P{Wspace}")
      if (grepl("RQ.EST", seriesWGI)) {
        data_extract$WGI_RQ[i]<-as.numeric(as.character(data_WGI[s3,co]))
        next
      }
    } else {next}
  }
} 
#---------------------------------------
for (i in 1:length_AID){ #CC用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s4 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s4]), pattern = "\\P{Wspace}")
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s4]), pattern = "\\P{Wspace}")
      if (grepl("CC.EST", seriesWGI)) {
        data_extract$WGI_CC[i]<-as.numeric(as.character(data_WGI[s4,co]))
        next
      }
    } else {next}
  }
} 
#---------------------------------------
for (i in 1:length_AID){ #VA用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s5 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s5]), pattern = "\\P{Wspace}")
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s5]), pattern = "\\P{Wspace}")
      if (grepl("VA.EST", seriesWGI)) {
        data_extract$WGI_VA[i]<-as.numeric(as.character(data_WGI[s5,co]))
        next
      }
    } else {next}
  }
} 
#---------------------------------------
for (i in 1:length_AID){ #GE用
  country <- stri_trim_right(as.character(data_extract$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data_extract$year[i]), pattern = "\\P{Wspace}"))
  
  # 上手く見つかった場合に、年とdata_WGIのcolumの対応関係を考える
  co <- switch(year,  #switch文は文字列比較
               "1996" = 4,
               "1998" = 5,
               "2000" = 6,
               "2002" = 7,
               "2003" = 8,
               "2004" = 9,
               stop("Year not matched")
  )
  
  for (s6 in 1:length_WGI){
    countryWGI <- stri_trim_right(as.character(data_WGI$cname[s6]), pattern = "\\P{Wspace}")
    if (country==countryWGI){
      seriesWGI <- stri_trim_right(as.character(data_WGI$Series.Code[s6]), pattern = "\\P{Wspace}")
      if (grepl("GE.EST", seriesWGI)) {
        data_extract$WGI_GE[i]<-as.numeric(as.character(data_WGI[s6,co]))
        next
      }
      
    } else {next}
  } #close for(s)
} 

# Data Check
hist(data_extract$WGI_PV,breaks=30)
hist(data_extract$WGI_RL, breaks=30)
hist(data_extract$WGI_RQ, breaks=30)
hist(data_extract$WGI_CC, breaks=30)
hist(data_extract$WGI_VA, breaks=20)
hist(data_extract$WGI_GE, breaks=30)
#write.csv(data_extract,"data_wWGI.csv")

# Analysis / Seemingly Unrelated Regression (http://www.ats.ucla.edu/stat/r/faq/sureg.htm)
#基本的に想定している回帰式同士のIVは異なるものみたい: http://kamome.lib.ynu.ac.jp/dspace/bitstream/10131/7752/1/7-Takeuchi.pdf
library(stringi) # Delete Unwanted White space
data <- read.csv("data_wWGI.csv", header=TRUE)

test <- data$AID110[7:12]
acf(data$AID110)
pacf(data$AID110)

# Time Series, ACF and PACF
setwd("/Users/S/Dropbox/Study/６学期/論文：国際政治経済の諸問題/Analysis/ACF_PACF")
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )

attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    filename <- paste("tsWGI",currcty,".pdf",sep="") 
    pdf(filename) 
    par(mfrow=c(3,1)) 
    plot(WGI_PV[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)],type="l",ylab="score",xlab="Time", main = paste("Country: ",currcty), ylim=c(-3,2), xlim=c(1,6) ) 
    plot(WGI_RL[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)],type="l",ylab="score",xlab="Time", main = paste("Country: ",currcty), ylim=c(-3,2), xlim=c(1,6) ) 
    plot(WGI_RQ[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)],type="l",ylab="score",xlab="Time", main = paste("Country: ",currcty), ylim=c(-3,2.5), xlim=c(1,6) ) 
    dev.off() 
  }, error=function(e){print(currcty)}) #just skip the error
}


for (i in 1:length(list_country)) {
  tryCatch({
    currcty <- list_country[i] 
    filename <- paste("acf_pacf",currcty,".pdf",sep="") 
    pdf(filename,width=10,height=5) 
    par(mfrow=c(3,2)) 
    acf(WGI_PV[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="PV_ACF") 
    pacf(WGI_PV[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="PV_PACF")
    acf(WGI_RL[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="RL_ACF")
    pacf(WGI_RL[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="RL_PACF")
    acf(WGI_RQ[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="RQ_ACF")
    pacf(WGI_RQ[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)], main="RQ_PACF")
    dev.off()  
  }, error=function(e){}) #just skip the error
}
detach(data)
setwd("/Users/S/Dropbox/Study/６学期/論文：国際政治経済の諸問題/Data")

# WGI_PVのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_PVlag <- 1
data <- data[,c(1:7,548,8:547)]  # 行の入れ替え
WGI_PVtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack/ 下のfo loopでは埋まった次の行から新しい国を埋めて行かねばならない
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]) #今見ている国が何年分あるか
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_PVtemp[1] <- NA
    WGI_PVtemp[2:list_end] <- WGI_PV[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_PVtemp <- WGI_PVtemp[1:number]
    data$WGI_PVlag[start:track] <- WGI_PVtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
summary(WGI_PVlag[year==1996])
detach(data)

# WGI_RLのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_RLlag <- 1
data <- data[,c(1:9,549,10:548)]  # 行の入れ替え
WGI_RLtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_RLtemp[1] <- NA
    WGI_RLtemp[2:list_end] <- WGI_RL[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_RLtemp <- WGI_RLtemp[1:number]
    data$WGI_RLlag[start:track] <- WGI_RLtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
summary(WGI_RLlag[year==1996])
detach(data)

# WGI_VAのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_VAlag <- 1
data <- data[,c(1:13,550,14:549)]  # 行の入れ替え
WGI_VAtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_VAtemp[1] <- NA
    WGI_VAtemp[2:list_end] <- WGI_VA[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_VAtemp <- WGI_VAtemp[1:number]
    data$WGI_VAlag[start:track] <- WGI_VAtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$WGI_VAlag[year==1996])

# AID110のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID110lag <- 1
data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID110temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID110temp[1] <- NA
    AID110temp[2:list_end] <- AID110[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID110temp <- AID110temp[1:number]
    data$AID110lag[start:track] <- AID110temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID110lag[year==1996])

# RAID110のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID110lag <- 1
data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID110temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID110temp[1] <- NA
    RAID110temp[2:list_end] <- RAID110[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID110temp <- RAID110temp[1:number]
    data$RAID110lag[start:track] <- RAID110temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID110lag[year==1996])

# AID120のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID120lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID120temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID120temp[1] <- NA
    AID120temp[2:list_end] <- AID120[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID120temp <- AID120temp[1:number]
    data$AID120lag[start:track] <- AID120temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID120lag[year==1996])

# RAID120のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID120lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID120temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID120temp[1] <- NA
    RAID120temp[2:list_end] <- RAID120[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID120temp <- RAID120temp[1:number]
    data$RAID120lag[start:track] <- RAID120temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID120lag[year==1996])

# AID140のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID140lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID140temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID140temp[1] <- NA
    AID140temp[2:list_end] <- AID140[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID140temp <- AID140temp[1:number]
    data$AID140lag[start:track] <- AID140temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID140lag[year==1996])

# RAID140のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID140lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID140temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID140temp[1] <- NA
    RAID140temp[2:list_end] <- RAID140[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID140temp <- RAID140temp[1:number]
    data$RAID140lag[start:track] <- RAID140temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID140lag[year==1996])

# WGI_CCのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_CClag <- 1
#data <- data[,c(1:13,550,14:549)]  # 行の入れ替え
WGI_CCtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_CCtemp[1] <- NA
    WGI_CCtemp[2:list_end] <- WGI_CC[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_CCtemp <- WGI_CCtemp[1:number]
    data$WGI_CClag[start:track] <- WGI_CCtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$WGI_CClag[year==1996])

# WGI_RQのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_RQlag <- 1
WGI_RQtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_RQtemp[1] <- NA
    WGI_RQtemp[2:list_end] <- WGI_RQ[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_RQtemp <- WGI_RQtemp[1:number]
    data$WGI_RQlag[start:track] <- WGI_RQtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$WGI_RQlag[year==1996])


# WGI_GEのlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$WGI_GElag <- 1
WGI_GEtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    WGI_GEtemp[1] <- NA
    WGI_GEtemp[2:list_end] <- WGI_GE[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    WGI_GEtemp <- WGI_GEtemp[1:number]
    data$WGI_GElag[start:track] <- WGI_GEtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$WGI_GElag[year==1996])

# AID130のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID130lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID130temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID130temp[1] <- NA
    AID130temp[2:list_end] <- AID130[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID130temp <- AID130temp[1:number]
    data$AID130lag[start:track] <- AID130temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID130lag[year==1996])

# RAID130のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID130lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID130temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID130temp[1] <- NA
    RAID130temp[2:list_end] <- RAID130[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID130temp <- RAID130temp[1:number]
    data$RAID130lag[start:track] <- RAID130temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID130lag[year==1996])

# AID121のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID121lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID121temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID121temp[1] <- NA
    AID121temp[2:list_end] <- AID121[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID121temp <- AID121temp[1:number]
    data$AID121lag[start:track] <- AID121temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID121lag[year==1996])

# RAID121のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID121lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID121temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID121temp[1] <- NA
    RAID121temp[2:list_end] <- RAID121[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID121temp <- RAID121temp[1:number]
    data$RAID121lag[start:track] <- RAID121temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID121lag[year==1996])

# SAID121のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$SAID121lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
SAID121temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    SAID121temp[1] <- NA
    SAID121temp[2:list_end] <- SAID121[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    SAID121temp <- SAID121temp[1:number]
    data$SAID121lag[start:track] <- SAID121temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$SAID121lag[year==1996])

# AID000のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID000lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID000temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID000temp[1] <- NA
    AID000temp[2:list_end] <- AID000[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID000temp <- AID000temp[1:number]
    data$AID000lag[start:track] <- AID000temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID000lag[year==1996])

# RAID100のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID100lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID100temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID100temp[1] <- NA
    RAID100temp[2:list_end] <- RAID100[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID100temp <- RAID100temp[1:number]
    data$RAID100lag[start:track] <- RAID100temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID100lag[year==1996])

# SAID100のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$SAID100lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
SAID100temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    SAID100temp[1] <- NA
    SAID100temp[2:list_end] <- SAID100[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    SAID100temp <- SAID100temp[1:number]
    data$SAID100lag[start:track] <- SAID100temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$SAID100lag[year==1996])

# RAID000のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$RAID000lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
RAID000temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    RAID000temp[1] <- NA
    RAID000temp[2:list_end] <- RAID000[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    RAID000temp <- RAID000temp[1:number]
    data$RAID000lag[start:track] <- RAID000temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$RAID000lag[year==1996])

# SAID000のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$SAID000lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
SAID000temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    SAID000temp[1] <- NA
    SAID000temp[2:list_end] <- SAID000[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    SAID000temp <- SAID000temp[1:number]
    data$SAID000lag[start:track] <- SAID000temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$SAID000lag[year==1996])

#write.csv(data,"data_wWGI_lag.csv")

# US VotingについてはデータがRを使える形式でないので手打ち
#data <- read.csv("data_wWGIL_UNvote.csv", header=TRUE)
data$Inc_Consensus<- as.numeric(as.character(data$Inc_Consensus))
str(data$Inc_Consensus)

# Human RightsについてはCIRIのデータを使う
hrData <- read.csv("/Users/S/Dropbox/Study/６学期/論文：国際政治経済の諸問題/Data/CIRI/CIRI Data 1981_2011 2014.04.14.csv", header=TRUE)

data$CIRI_HR <- NA  # Prepare Empty Column
data <- data[,c(1:3,562,4:561)]
library(stringi)  # Delete Unwanted White space

length_AID <- length(data$year)
length_CIRI <- length(hrData$YEAR)

for (i in 1:length_AID){ #CIRI_Human Rights用にデータをマッチさせる
  country <- stri_trim_right(as.character(data$cname[i]), pattern = "\\P{Wspace}")  # 国名についた余計なスペースを除去
  year <- as.character(stri_trim_right(as.numeric(data$year[i]), pattern = "\\P{Wspace}"))
  
  
  for (s1 in 1:length_CIRI){
    countryCIRI <- stri_trim_right(as.character(hrData$CTRY[s1]), pattern = "\\P{Wspace}")
    
    if (country==countryCIRI){
      yearCIRI <- as.character(stri_trim_right(as.numeric(hrData$YEAR[s1]), pattern = "\\P{Wspace}"))
      
      if (grepl(year, yearCIRI)==1) {  # grepl has the return value TRUE or FALSE / grep returns the point the letter starts
        data$CIRI_HR[i]<-as.numeric(as.character(hrData$NEW_EMPINX[s1]))
        #print(as.numeric(as.character(data_WGI[s1,co])))
        next 
      } else {next}
    }
  }
}  


list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") ) # HRのlagを考える
data$CIRI_HRlag <- 1
data <- data[,c(1:4,563,5:562)]  # 行の入れ替え
CIRI_HRtemp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    CIRI_HRtemp[1] <- NA
    CIRI_HRtemp[2:list_end] <- CIRI_HR[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    CIRI_HRtemp <- CIRI_HRtemp[1:number]
    data$CIRI_HRlag[start:track] <- CIRI_HRtemp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$CIRI_HRlag[year==1996])

#write.csv(data,"data_WGIlag_UN_CIRI_v7.csv")

# Seemingly Unrelated Regression
#data <- read.csv("data_WGIlag_UN_CIRI_v7.csv", header=TRUE)
#data$Inc_Consensus<- as.numeric(as.character(data$Inc_Consensus)) # numericに変換
WGI_VAdiff <- data$WGI_VA-data$WGI_VAlag
WGI_RLdiff <- data$WGI_RL-data$WGI_RLlag
WGI_CCdiff <- data$WGI_CC-data$WGI_CClag
WGI_GEdiff <- data$WGI_GE-data$WGI_GElag
WGI_RQdiff <- data$WGI_RQ-data$WGI_RQlag
CIRI_HRdiff <- data$CIRI_HR-data$CIRI_HRlag

library(systemfit)                           # World Governance Indicatonr = USAID category 
r1 <- WGI_VAdiff ~ AID110 + AID130 + AID000 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
          #PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
          #AID000: Total Investment in Other Sectors than DG, 

r1 <- WGI_VAdiff ~ AID110lag + AID130lag + AID000lag + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)


fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)

# with regime type
r7 <- WGI_VAdiff ~ AID110 + AID130 + factor(DG09)*AID110*AID130 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r8 <- WGI_RLdiff ~ AID120 + PRF01 + factor(DG09)*AID120 + SOC09 + SOC10  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r9 <- WGI_CCdiff ~ AID140 + PRF01 + factor(DG09)*AID140 + SOC09 + SOC10  + Inc_Consensus # Control of Corruption = Governance(AID140)
r10 <- WGI_GEdiff ~ AID140 + PRF01 + factor(DG09)*AID140 + SOC09 + SOC10  + Inc_Consensus # Government Effectiveness = Governance(AID140)
r11 <- WGI_RQdiff ~ AID140 + PRF01 + factor(DG09)*AID140 + SOC09 + SOC10  + Inc_Consensus # Regulatory Quality = Governance(AID140)
r12 <- CIRI_HRdiff ~ AID121 + PRF01 + factor(DG09)*AID140 + SOC09 + SOC10  + Inc_Consensus # Human Rights = CIRI (AID121)
#PRF01: Economic growth

fitsur_r <- systemfit(list(VA = r7, RL = r8, CC = r9, GE = r10, RQ = r11, HR = r12), data=data)
summary(fitsur_r)



# Robustness Test
## Polityの準備

# DG01(combined polity)のlag
library(stringi)  # Delete Unwanted White space
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$DG01lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
DG01temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    DG01temp[1] <- NA
    DG01temp[2:list_end] <- DG01[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    DG01temp <- DG01temp[1:number]
    data$DG01lag[start:track] <- DG01temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$DG01lag[year==1996])

# AID100のlagを考える
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$AID100lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
AID100temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    AID100temp[1] <- NA
    AID100temp[2:list_end] <- AID100[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    AID100temp <- AID100temp[1:number]
    data$AID100lag[start:track] <- AID100temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$AID100lag[year==1996])


## Freedom Houseの準備
# DG02のlag
library(stringi)  # Delete Unwanted White space
list_country <- unique(stri_trim_right(as.character(data$cname), pattern = "\\P{Wspace}") )
data$DG02lag <- 1
#data <- data[,c(1:18,551,19:550)]  # 行の入れ替え
DG02temp <- c(1,1,1,1,1,1)
track <- 0 #どこまで列を埋めたかをtrack 
attach(data)
for (i in 1:length(list_country)) { 
  tryCatch({
    currcty <- list_country[i] 
    number <- length(year[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)])
    start <- track + 1
    track <- track + number
    list_end <- number + 1
    DG02temp[1] <- NA
    DG02temp[2:list_end] <- DG02[stri_trim_right(as.character(cname), pattern = "\\P{Wspace}")==as.character(currcty)]
    DG02temp <- DG02temp[1:number]
    data$DG02lag[start:track] <- DG02temp
  }, error=function(e){print(currcty)}) #just skip the error
}
detach(data)
summary(data$DG02lag[year==1996])


#Analysis
polity_diff <- data$DG01 - data$DG01lag
#polity_diffrate <- 100*(data$DG01 - data$DG01lag)/data$DG01lag
FrHo_diff <- data$DG02 - data$DG02lag

r8 <- polity_diff ~ AID120 + PRF01 + Inc_Consensus
r9 <- polity_diff ~ AID100 + factor(DG09)*AID100 + PRF01
robust_polity <- lm(r8, data=data)
summary(robust_polity)
robust_polityr <- lm(r9, data=data)
summary(robust_polityr)

r10 <- FrHo_diff ~ AID100 + PRF01 + Inc_Consensus
r11 <- FrHo_diff ~ AID100 + factor(DG09)*AID100 + PRF01
robust_FrHo <- lm(r10, data=data)
summary(robust_FrHo)
robust_FrHo <- lm(r11, data=data)
summary(robust_FrHo)


rp1 <- polity_diff ~ DG01lag + AID110lag + AID130lag + PRF01 + SOC09 + SOC10  + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
rp2 <- polity_diff ~ DG01lag + AID120lag + PRF01 + SOC09 + SOC10  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
rp3 <- polity_diff ~ DG01lag + AID140lag + PRF01 + SOC09 + SOC10  + Inc_Consensus # Control of Corruption = Governance(AID140)
#rp4 <- polity_diff ~ WGI_GElag + AID140lag + PRF01 + Inc_Consensus # Government Effectiveness = Governance(AID140)
#rp5 <- polity_diff ~ WGI_RQlag + AID140lag + PRF01 + Inc_Consensus # Regulatory Quality = Governance(AID140)
rp6 <- polity_diff ~ DG01lag + AID121lag + PRF01 + SOC09 + SOC10  + Inc_Consensus # Human Rights = CIRI (AID121)

rp1 <- polity_diff ~ AID110 + AID130 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
rp2 <- polity_diff ~ AID120 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
rp3 <- polity_diff ~ AID140 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Control of Corruption = Governance(AID140)
#rp4 <- polity_diff ~ AID140 + PRF01 + Inc_Consensus # Government Effectiveness = Governance(AID140)
#rp5 <- polity_diff ~ AID140 + PRF01 + Inc_Consensus # Regulatory Quality = Governance(AID140)
rp6 <- polity_diff ~ AID121 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Human Rights = CIRI (AID121)


fitsur <- systemfit(list(VA = rp1, RL = rp2, CC = rp3, HR = rp6), data=data)
summary(fitsur)

r1 <- WGI_VAdiff ~ AID110 + PRF01 # Voice and Accountability = Elections and Political Processes
r2 <- WGI_RLdiff ~ AID120 + PRF01 # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 # Control of Corruption = Governance(AID140)


# Histogam Revisited
hist(FrHo_diff,10, main="Changes in Freedom House Score", xlab="Changes")
hist(polity_diff,10, main="Changes in Polity Score", xlab="Changes")
hist(polity_diffrate,60, main="Change rates in Polity Score", xlab="Changes",xlim=range(-100:100))
hist(CIRI_HRdiff, 10,main="Changes in Human Rights", xlab="Changes")
hist(WGI_CCdiff, 20,main="Changes in Control of Corruption", xlab="Changes")
hist(WGI_RLdiff, 20,main="Changes in Rule of Law", xlab="Changes")
hist(WGI_VAdiff, 20,main="Changes in Voice and Accountability", xlab="Changes")

