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
bad_indicators <- c("Charged Off ",
                    "In Grace Period",
                    "Late (16-30 days)",
                    "Late (31-120 days)")

#Assign statuses di atas ke dalam kelompok "bad" (0):
loan$is_bad <- ifelse(loan$loan_status %in% bad_indicators, 0,
                      ifelse(loan$loan_status=="Fully Paid", 1, "Current")
                      )
#Buat barplot:
barplot(table(loan$is_bad) , col = 'lightblue')
#Distribusi:
library(DescTools)

Desc(loan$is_bad, main="Loan Label Distribution", plotit = TRUE)