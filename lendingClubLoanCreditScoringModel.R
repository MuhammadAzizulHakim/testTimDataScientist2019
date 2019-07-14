#Load dataset
loan <- readRDS("lending_club_loan_data.rds")

#Read data as nice table
library(DT)
#Read first 5 observations
DT::datatable(loan[1:5,])

#Findout about missing values
is.na(loan)
#List unique values in loan status
unique(loan$loan_status)

#Transformasikan status pinjaman menjadi 2 kategori saja:
#0 = Bad statuses = "Late (16-30 days)", "Late (31-120 days)", "Charged Off", "In Grace Period"
#1 = Good Statuses = "Fully Paid", "Current"
library(dplyr)
#Bad statuses:
bad_indicators <- c("Charged Off",
                    "In Grace Period",
                    "Late (16-30 days)",
                    "Late (31-120 days)")
#Assign statuses di atas ke dalam kelompok "bad" (0):
loan$is_bad <- ifelse(loan$loan_status %in% bad_indicators, 0,
                      ifelse(loan$loan_status=="", NA, 1)
                      )
#Buat barplot (visualisasi 0 vs 1):
barplot(table(loan$is_bad), col = 'lightblue')

#Cek perilaku variabel numerik untuk good vs bad loans:
numeric_cols <- sapply(loan, is.numeric)

#Mulai dari sini, split data dahulu menjadi jumlah observasi yang lebih sedikit, dikarenakan keterbatasan hardware
#Teknik splitting data menggunakan stratified random samplin, untuk memaintain rasio jumlah data agar tetap representatif dengan jumlah data yang sebenarnya
library(caret)
library(knitr)
#Create function to calculate percent distribution for factors
pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}

#Lakukan stratified random sampling, gunakan 10% data yang ada
set.seed(42)
remaining_data <- createDataPartition(y=loan$is_bad, p = .9, list = FALSE)
str(remaining_data)
#Split data menjadi stratum yang akan digunakan untuk analisis vs stratum yang tidak digunakan
remaining_loan_data <- loan[remaining_data,]
loan2 <- loan[-remaining_data,] #Ke depannya, kita hanya akan menggunakan data loan2
#Test, apakah stratum loan2 sudah representatif dengan data loan
pct(loan$is_bad)
pct(loan2$is_bad)

# turn the data into long format
library(reshape2)
#Beri nama kolom id observasi
#melt(loan, id.vars="")
loan_long <- melt(loan2[,numeric_cols], id="is_bad")

#Plot the distribution for 'bad' and 'good' for each numeric variable
library(ggplot2)
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
            data = loan_long)
#Create the plot to check if there are any good variables that can be used in predictive models
#Catatan: metode ini crash ketika menggunakan stratum 20% dari total dataset, apalagi ketika menggunakan keseluruhan dataset
p + geom_density() +
  facet_wrap(~variable, scales="free")
#Dari metode di atas, diperoleh variabel-variabel yang mempengaruhi status pinjaman, sebagai berikut:




#Show the data setelah disimpulkan bahwa annual_inc dan int_rate sebagai dua predictor paling berpengaruh:
loan %>% 
  filter(is_bad == '0') %>% 
  select(annual_inc, int_rate, loan_status) %>% 
  datatable(., options = list(pageLength = 10))