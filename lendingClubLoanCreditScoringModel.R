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
# Create the new dataset by filtering 0's and 1's in the is_bad column and remove loan_status column for the modelling
loan2 = loan %>%
  select(-loan_status) %>%
  filter(is_bad %in% c(0 , 1))

#Convert 1 to "Good" and 0 to "Bad"
loan2$is_bad <- as.factor(ifelse(loan2$is_bad == 1, "Good", "Bad"))
pct(loan2$is_bad)

#Buat barplot (visualisasi 0 vs 1):
barplot(table(loan2$is_bad), col = 'lightblue')

#Cek perilaku variabel numerik untuk good vs bad loans:
numeric_cols <- sapply(loan2, is.numeric)

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
remaining_data <- createDataPartition(y=loan2$is_bad, p = .9, list = FALSE)
str(remaining_data)
#Split data menjadi stratum yang akan digunakan untuk analisis vs stratum yang tidak digunakan
remaining_loan_data <- loan2[remaining_data,]
loan3 <- loan2[-remaining_data,] #Ke depannya, kita hanya akan menggunakan data loan3
#Test, apakah stratum loan2 sudah representatif dengan data loan
pct(loan2$is_bad)
pct(loan3$is_bad)

#Catatan: data is_bad harus numeric
# turn the data into long format
library(reshape2)
#Beri nama kolom id observasi
#melt(loan, id.vars="")
loan_long <- melt(loan3[,numeric_cols], id="is_bad")

#Plot the distribution for 'bad' and 'good' for each numeric variable
library(ggplot2)
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
            data = loan_long)
#Create the plot to check if there are any good variables that can be used in predictive models
#Catatan: metode ini crash ketika menggunakan stratum 20% dari total dataset, apalagi ketika menggunakan keseluruhan dataset
p + geom_density() +
  facet_wrap(~variable, scales="free")
#Dari metode di atas, diperoleh variabel-variabel yang mempengaruhi status pinjaman, sebagai berikut:
#all_util
#annual_inc_joint
#dti_joint
#hardship_dpd
#hardship_last_payment_amount
#il_util
#int_rate
#last_pymnt_amnt
#revol_util
#settlement_amount
#settlement_percentage
#settlement_term
#total_pymnt
#total_pymnt_inv
#total_rec_prncp


#Split training dan test data, untuk diterapkan ke model credit scoring
#Lakukan stratified random sampling, split data menjadi 80% training data dan 20% test data
set.seed(42)
train_data <- createDataPartition(y=loan3$is_bad, p = .8, list = FALSE)
#Split data menjadi stratum yang akan digunakan untuk analisis vs stratum yang tidak digunakan
train_loan <- loan3[train_data,]
test_loan <- loan3[-train_data,] 

pct(loan2$is_bad)
pct(loan3$is_bad)
pct(train_loan$is_bad)
pct(test_loan$is_bad)

#Pilih Metode Random Forest, untuk sekalian menguji variabel mana yang paling berpengaruh untuk mempresiksi status peminjam
library(randomForest)

model_RF <- randomForest(is_bad ~ ., data = train_loan)
model_RF_fitForest <- predict(model_RF, newdata = test_loan, type="prob")[,2]
model_RF_pred <- prediction( model_RF_fitForest, test_loan$is_bad)
model_RF_perf <- performance(model_RF_pred, "tpr", "fpr")

#plot variable importance
varImpPlot(model_RF, main="Random Forest: Variable Importance")



colSums(is.na(train_loan))


#Show the data setelah disimpulkan bahwa annual_inc dan int_rate sebagai dua predictor paling berpengaruh:
loan %>% 
  filter(is_bad == '0') %>% 
  select(annual_inc, int_rate, loan_status) %>% 
  datatable(., options = list(pageLength = 10))