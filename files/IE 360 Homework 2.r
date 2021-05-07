require(openxlsx) 
suppressWarnings(library(openxlsx)) #To eliminate warning messages
require(ggplot2)
suppressWarnings(library(ggplot2))
require(data.table)
suppressWarnings(library(data.table))
require(ggcorrplot)
suppressWarnings(library(ggcorrplot))
require(lubridate)
suppressWarnings(library(lubridate))
require(zoo)
suppressWarnings(library(zoo))
require(forecast)
suppressWarnings(library(forecast))
library(dplyr)
suppressWarnings(library(dplyr))
library(tidyr)
suppressWarnings(library(tidyr))

data_path = "C:/Users/Alkim/Desktop/8. Dönem/IE 360/Homework 2/alcohol_tobacco_cpi_1.xlsx"
alc_tob_cpi=read.xlsx(data_path,sheet='EVDS')

head(alc_tob_cpi,5)
tail(alc_tob_cpi,5)
str(alc_tob_cpi)

alc_tob_cpi$Tarih <- as.Date(paste0(alc_tob_cpi$Tarih,"-01",sep=""))

colnames(alc_tob_cpi) <- c("Date", "CPI")

ggplot(alc_tob_cpi ,aes(x=Date,y=CPI)) + geom_point() + geom_smooth(se=FALSE) + 
ggtitle("Consumer Price Index of Alcoholic Beverages and Tobacco") + ylab("CPI (TL)")

alc_tob_cpi = data.table(alc_tob_cpi)
alc_tob_cpi[,trend:=1:.N]

model_1 = lm(CPI~trend, data = alc_tob_cpi)
summary(model_1)
plot(model_1)
checkresiduals(model_1)

alc_tob_cpi[,log_CPI:=log(CPI)]
ggplot(alc_tob_cpi ,aes(x=Date,y=log_CPI)) + geom_point() + geom_smooth(se=FALSE)+
ggtitle("Logarithm of Consumer Price Index of Alcoholic Beverages and Tobacco") + ylab("log(CPI (TL))")

model_2 = lm(log_CPI~trend, data = alc_tob_cpi)
summary(model_2)
plot(model_2)
checkresiduals(model_2)

data_path = "C:/Users/Alkim/Desktop/8. Dönem/IE 360/Homework 2/dolar_1.xlsx"
dollar=read.xlsx(data_path,sheet='EVDS')

head(dollar,5)
tail(dollar,5)
str(dollar)

colnames(dollar) <- c("Date", "Dollar")
dollar$Date <- as.Date(paste0(dollar$Date,"-01",sep=""))

alc_tob_cpi[,Dollar:=dollar$Dollar]
alc_tob_cpi$Dollar_lagged <- as.numeric(sapply(1:nrow(alc_tob_cpi), function(x) alc_tob_cpi$Dollar[x-1]))
#Since the data are shifted by 1, January-2012 lag-1 variable is NA
                                #It is filled with the next lag-1 variable, February 2012
alc_tob_cpi <- alc_tob_cpi %>% fill(Dollar_lagged, .direction = "up")

model_3 = lm(log_CPI~trend+Dollar_lagged, data = alc_tob_cpi)
summary(model_3)
plot(model_3)

alc_tob_cpi[,predictions:=exp(predict(model_3,alc_tob_cpi))]
ggplot(alc_tob_cpi ,aes(x=Date)) +
        geom_line(aes(y=CPI,color='Actual Observations'), size = 0.8) + 
        geom_line(aes(y=predictions,color='Predictions'), size = 0.8) +
        theme(legend.title = element_blank())

checkresiduals(model_3)

data_path = "C:/Users/Alkim/Desktop/8. Dönem/IE 360/Homework 2/confidence_index.xlsx"
trust_index=read.xlsx(data_path,sheet='EVDS')
colnames(trust_index) <- c("Date", "Trust_index")
trust_index$Date <- as.Date(paste0(trust_index$Date,"-01",sep=""))

ggplot(trust_index ,aes(x=Date,y=Trust_index)) + geom_point() + geom_smooth(se=FALSE) + 
ggtitle("Consumer Confidence Index") + ylab("CCI")

alc_tob_cpi[,Trust_index:=trust_index$Trust_index]
alc_tob_cpi$Trust_index_lagged <- as.numeric(sapply(1:nrow(alc_tob_cpi), function(x) alc_tob_cpi$Trust_index[x-1]))
#Since the data are shifted by 1, January-2012 lag-1 variable is NA
                                #It is filled with the next lag-1 variable, February 2012
alc_tob_cpi <- alc_tob_cpi %>% fill(Trust_index_lagged, .direction = "up")

model_4 = lm(log_CPI~trend+Dollar_lagged+Trust_index_lagged, data = alc_tob_cpi)
summary(model_4)
plot(model_4)

checkresiduals(model_4)

data_path = "C:/Users/Alkim/Desktop/8. Dönem/IE 360/Homework 2/sigaraya_zam.csv"
zam = read.csv(data_path)
colnames(zam) <- c("Date", "zam")
zam$Date <- as.Date(paste0(zam$Date,"-01",sep=""))
str(zam)

zam$zam <- as.character(zam$zam)
zam$zam <- ifelse(zam$zam == "<1",0,zam$zam)
zam$zam <- as.numeric(zam$zam)

ggplot(zam ,aes(x=Date,y=zam)) + geom_line(color = "red", size = 0.8) + 
ggtitle("The Search Volume of Sigaraya Zam") + ylab("The Number of Search")

alc_tob_cpi[,zam:=zam$zam]
alc_tob_cpi$zam_lagged <- as.numeric(sapply(1:nrow(alc_tob_cpi), function(x) alc_tob_cpi$zam[x-1]))
#Since the data are shifted by 1, January-2012 lag-1 variable is NA
                                #It is filled with the next lag-1 variable, February 2012
alc_tob_cpi <- alc_tob_cpi %>% fill(zam_lagged, .direction = "up")
                              

model_5 = lm(log_CPI~trend+Dollar_lagged+zam_lagged, data = alc_tob_cpi)
summary(model_5)
plot(model_5)   

alc_tob_cpi[,predictions:=exp(predict(model_5,alc_tob_cpi))]
ggplot(alc_tob_cpi ,aes(x=Date)) +
        geom_line(aes(y=CPI,color='Actual Observations'), size = 0.8) + 
        geom_line(aes(y=predictions,color='Predictions'), size = 0.8) +
        theme(legend.title = element_blank())

checkresiduals(model_5)

alc_tob_cpi$log_lag_1 <- as.numeric(sapply(1:nrow(alc_tob_cpi), function(x) alc_tob_cpi$log_CPI[x-1]))
#Since the data are shifted by 1, January-2012 lag-1 variable is NA
                                #It is filled with the next lag-1 variable, February 2012
alc_tob_cpi <- alc_tob_cpi %>% fill(log_lag_1, .direction = "up")

model_6 = lm(log_CPI~trend+Dollar_lagged+zam_lagged+log_lag_1, data = alc_tob_cpi)
summary(model_6)
plot(model_6)

checkresiduals(model_6)

corr <- cor(alc_tob_cpi[,list(CPI,Dollar_lagged,zam_lagged)])
ggcorrplot(corr,
  hc.order = TRUE, type = "lower",
  lab = TRUE,
  ggtheme = ggplot2::theme_dark(),
)

alc_tob_cpi[,predictions:=exp(predict(model_6,alc_tob_cpi))]
ggplot(alc_tob_cpi ,aes(x=Date)) +
        geom_line(aes(y=CPI,color='Actual Observations'), size = 0.7) + 
        geom_line(aes(y=predictions,color='Predictions'), size = 0.7) +
        theme(legend.title = element_blank())


april_2021_pred <- data.frame(
  trend = alc_tob_cpi[.N,trend+1],
    Dollar_lagged=alc_tob_cpi[.N,Dollar],
    zam_lagged = alc_tob_cpi[.N,zam],
    log_lag_1 = alc_tob_cpi[.N,log_CPI]
)


paste0("The predicted consumer price index of alcohols and tobacco in April is ",round(exp(predict(model_6,april_2021_pred)),2)," TL, according to the model.")
data_path = "C:/Users/Alkim/Desktop/8. Dönem/IE 360/Homework 2/CPI_with_April..xlsx"
April=read.xlsx(data_path,sheet='EVDS')
April <- tail(April,1)
colnames(April) <- c("Date", "CPI")
paste0("The real consumer price index of alcohols and tobacco in April ",April$CPI," TL.")
paste0("The difference is ", round(exp(predict(model_6,april_2021_pred)),2) - April$CPI , " TL")
