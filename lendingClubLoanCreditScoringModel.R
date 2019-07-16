#Load dataset
loan <- readRDS("loan2.rds")

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

#Hapus observasi dengan NA yang masih tersisa
loan2_na_omitted <- na.omit(loan2)

#Buat barplot (visualisasi 0 vs 1):
barplot(table(loan2_na_omitted$is_bad), col = 'lightblue')

#Cek perilaku variabel numerik untuk good vs bad loans:
numeric_cols <- sapply(loan2_na_omitted, is.numeric)


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
remaining_data <- createDataPartition(y=loan2_na_omitted$is_bad, p = .9, list = FALSE)
str(remaining_data)
#Split data menjadi stratum yang akan digunakan untuk analisis vs stratum yang tidak digunakan
remaining_loan_data <- loan2_na_omitted[remaining_data,]
loan3 <- loan2_na_omitted[-remaining_data,] #Ke depannya, kita hanya akan menggunakan data loan3

#Test, apakah stratum loan2 sudah representatif dengan data loan
pct(loan2_na_omitted$is_bad)
pct(loan3$is_bad)

#Catatan: data is_bad harus numeric
# turn the data into long format
#library(reshape2)
#Beri nama kolom id observasi
#melt(loan, id.vars="")
#loan_long <- melt(loan3[,numeric_cols], id="is_bad")

#Plot the distribution for 'bad' and 'good' for each numeric variable
#library(ggplot2)
#p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
#            data = loan_long)
#Create the plot to check if there are any good variables that can be used in predictive models
#Catatan: metode ini crash ketika menggunakan stratum 20% dari total dataset, apalagi ketika menggunakan keseluruhan dataset
#p + geom_density() +
#  facet_wrap(~variable, scales="free")
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

#Drop semua variabel character, karena menyebabkan error pada Model Random
train_loan <- select (train_loan,-c(title,emp_title))
test_loan <- select (test_loan, -c(title,emp_title))

train_loan2 <- train_loan[, !sapply(train_loan, is.character)]
test_loan2 <- test_loan[, !sapply(test_loan, is.character)]

#Pilih Metode Random Forest, untuk sekalian menguji variabel mana yang paling berpengaruh untuk mempresiksi status peminjam
library(randomForest)
library(ROCR)

model_RF <- randomForest(is_bad ~ ., data = train_loan2)

model_RF_fitForest <- predict(model_RF, newdata = test_loan, type="prob")[,2]
model_RF_pred <- prediction(model_RF_fitForest, test_loan$is_bad)
model_RF_perf <- performance(model_RF_pred, "tpr", "fpr")

#Print important variable
varImp(model_RF)
#Plot variable importance
varImpPlot(model_RF, main="Random Forest: Variable Importance")

#Model Performance plot
plot(model_RF_perf, colorize=TRUE, lwd=2, main = "ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

#Plot precision/recall curve
model_RF_perf_precision <- performance(model_RF_pred, measure = "prec", x.measure = "rec")
plot(model_RF_perf_precision, main="Random Forests: Precision/Recall Curve")

#Plot accuracy as function of threshold
model_RF_perf_accuracy <- performance(model_RF_pred, measure = "acc")
plot(model_RF_perf_accuracy, main="Random Forests:Accuracy as function of threshold")

#KS & AUC model Random Forest
model_RF_AUROC <- round(performance(model_RF_pred, measure = "auc")@y.values[[1]]*100, 2)
model_RF_KS <- round(max(attr(model_RF_perf,'y.values')[[1]] - attr(model_RF_perf,'x.values')[[1]])*100, 2)
model_RF_Gini <- (2*model_RF_AUROC - 100)
cat("AUROC: ",model_RF_AUROC,"\tKS: ", model_RF_KS, "\tGini:", model_RF_Gini, "\n")

#Function untuk test dataset baru: 
model_RF_pred <- prediction(model_RF_fitForest, test_loan$is_bad)