require(XLConnect)
library(XLConnect)  
library(tidyr)
library(dplyr)

GetLoansAppliedbyPurpose <-function() {
  wk = loadWorkbook("1.10.xls") 
  df = readWorksheet(wk, sheet="1.10" )
  
  ##extract needed columns
  df <- df[,c("X1.10","Col2","Sistem.Perbankan..Pinjaman.yang.Dipohon.Mengikut.Tujuan","Col7")] 
  
  ##rename columns
  names(df) <- c("Years","Months","DescriptionT","AppliedPropertyLoan") 
  
  ##remove unused rows
  df <-df[-c(1, 2, 3, 4, 5 , 6 ,7 ), ] 
  df <- subset(df,df$DescriptionT == "Jumlah / Total " )
  df <-  subset(df,df$Years !=  2014 )
  
  ##fillin missing values
  df[is.na(df)] <-0 
  
  ##convert data type
  df <-  
    df %>% 
    mutate_each(funs(as.character(.)), AppliedPropertyLoan) %>%
    mutate_each(funs(gsub(",", "", .)), AppliedPropertyLoan) %>%
    mutate_each(funs(as.numeric(.)), AppliedPropertyLoan)
  df[,c(1)]<- sapply(df[, c(1)], as.integer)
      
  #Total Banking System: Loans Applied by Purpose
  
  df2 <- aggregate(x = list(Total = df$AppliedPropertyLoan), by=list(Years= df$Years), FUN=sum)
  
  return (df2)
}

GetLoansApprovedbyPurpose <-function() {
  wk = loadWorkbook("1.12.xls") 
  df = readWorksheet(wk, sheet="1.12" )
  
  ##extract needed columns
  df <- df[,c("X1.12","Col2","Sistem.Perbankan..Pinjaman.yang.Diluluskan.Mengikut.Tujuan","Col7")] 
  
  ##rename columns
  names(df) <- c("Years","Months","DescriptionT","ApprovedPropertyLoan") 
  
  ##remove unused rows
  df <-df[-c(1, 2, 3, 4, 5 , 6 ,7 ), ] 
  df <- subset(df,df$DescriptionT == "Jumlah / Total " )
  df
  
  ##fillin missing values
  df[is.na(df)] <-0 
  
  ##convert data type
  df <-  
    df %>% 
    mutate_each(funs(as.character(.)), ApprovedPropertyLoan) %>%
    mutate_each(funs(gsub(",", "", .)), ApprovedPropertyLoan) %>%
    mutate_each(funs(as.numeric(.)), ApprovedPropertyLoan)
  df[,c(1)]<- sapply(df[, c(1)], as.integer)
  
  #Total Banking System: Loans Approved by Purpose
  
  df2 <- aggregate(x = list(Total = df$ApprovedPropertyLoan), by=list(Years= df$Years), FUN=sum)
  
  return (df2)
}


GetNPICData <-function() {
  wk = loadWorkbook("BilangandanNilaiPindahMilikHartaTanahdanPerubahanTahunan1990-2014.xls") 
  df = readWorksheet(wk, sheet="Sheet1" , header = FALSE)
  df
  ##df <- transform(df, Col1 = colsplit(Col1, split = " ", names = c('Years', 'VolumnOfTrans','ChangeInVol','Value','ChangeInVal')))
  df <- strsplit(as.character(df$Col1),' ') 
  df<- do.call(rbind, df)
  df <- data.frame(df, stringsAsFactors = FALSE)
  names(df) <- c('Years', 'VolumnOfTrans','ChangeInVol','Value','ChangeInVal')
  df
  df[1,c(3)] <-0 
  df[1,c(5)] <-0 
  df[,c(1)]<- sapply(df[, c(1)], as.integer)
  df[,c(2)]<- sapply(df[, c(2)], as.numeric)
  df[,c(3)]<- sapply(df[, c(3)], as.numeric)
  df[,c(4)]<- sapply(df[, c(4)], as.numeric)
  df[,c(5)]<- sapply(df[, c(5)], as.numeric)  
 
  #df[1:25,c(4)]  <- df[1:25,c(4)]*1000
  return(df)
}


