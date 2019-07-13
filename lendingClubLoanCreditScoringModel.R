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
# turn the data into long format
library(reshape2)
#Beri nama kolom id observasi
#melt(loan, id.vars="")
loan_long <- melt(loan[,numeric_cols], id="is_bad")

# plot the distribution for 'bad' and 'good' for each numeric variable
library(ggplot2)
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), 
            data = loan_long)

# create the plot to check if there are any good variables that can be used in predictive models
p + geom_density() +
  facet_wrap(~variable, scales="free")


#Show the data setelah disimpulkan bahwa annual_inc dan int_rate sebagai dua predictor paling berpengaruh:
loan %>% 
  filter(is_bad == '0') %>% 
  select(annual_inc, int_rate, loan_status) %>% 
  datatable(., options = list(pageLength = 10))