url <- "https://raw.githubusercontent.com/kmyafi/Rupiah-Exchange-Rate-ARDL-Model/main/Money%20Supply-Exchange%20Rate.csv"
Table <- read.csv(url, sep=";")
Table$Waktu <- seq(1,28,1)

Yt <- Table$`Nilai.Tukar`
Xt <- Table$`Uang.Beredar`
data <- data.frame(Yt,Xt)

#Identifikasi data
str(data)
summary(data)

#Mengubah format data menjadi time series
#data time series
data.ts <- ts(data)
Yt.ts <- data$Yt
Xt.ts <- data$Xt

#### Uji Stasioner ####
library(tseries)
# ADF
adf.test(data.ts[,1]) #Yt
adf.test(data.ts[,2]) #Xt

#### Differencing Model ####
# Differencing
diff_ts <- diff(data.ts,
                differences = 2)
diff_ts

adf.test(diff_ts[,1])
adf.test(diff_ts[,2])

library(ggplot2)
options(scipen = 999)
library(scales)

g1 <- ggplot(Table, aes(x=Waktu, y=`Nilai.Tukar`)) +
  labs(x="\nWaktu",y = "Nilai tukar rupiah\n", title="ER\n") +
  geom_line(color="blue") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,16000,2000), limit = c(2000,16000))  +
  scale_x_continuous(breaks = seq(0,30,2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1))

g2 <- ggplot(Table, aes(x=Waktu, y=`Uang.Beredar`)) +
  labs(x="\nWaktu",y = "\nJumlah uang beredar (Miliar)\n", title="M2\n") +
  geom_line(color="blue") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,10000000,1000000), limit = c(100000,9000000)) +
  scale_x_continuous(breaks = seq(0,30,2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1))

gridExtra::grid.arrange(g1,g2,ncol=2)

#Model Koyck
library(dLagM)
model.koyck <- koyckDlm(x = data$Xt, y = data$Yt)
summary(model.koyck)

#Diagnosis model
library("lmtest")
library("olsrr")
jarque.bera.test(residuals(model.koyck)) # Normalitas
bptest(model.koyck$model) # Heteroskedastisitas
dwtest(model.koyck$model) # Autokorelasi

AIC(model.koyck)
BIC(model.koyck)

#Akurasi data
GoF(model.koyck)["MAPE"]
GoF(model.koyck)["MASE"]

#Model Almon
model.poly <- polyDlm(x = data$Xt, y = data$Yt, q = 2, k = 2)
summary(model.poly)

#Diagnosis model
library("lmtest")
library("olsrr")
jarque.bera.test(residuals(model.poly))
bptest(model.poly$model)
dwtest(model.poly$model)

AIC(model.poly)
BIC(model.poly)

#Akurasi data
GoF(model.poly)["MAPE"]
GoF(model.poly)["MASE"]

#Summary
GoF(model.koyck, model.poly)
