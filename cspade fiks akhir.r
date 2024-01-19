# arulesSequences using cSPADE algorithm, four inputs: sequenceID, eventID, items, SIZE
library(readxl)
library(dplyr)
library(stringr)
library(arulesSequences)

# Load the df data, download manually if the file becomes corrupted during download
df <- read.csv(file = "C://Users//...//MO2022.csv", sep = ";")
View(df)

# Add items together by type pukulam
# tambahkan items sebagai type pukulan (diganti)
df <- df %>% 
  group_by(rally, Banyak.pukulan) %>% 
  summarise(items = paste(type, collapse = " "))

# Hitung jumlah item pada setiap Banyak pukulan
# akan bertambah kolom size
df$SIZE <- str_count(df$items, " ") + 1
df <- as.data.frame(df)
View(df)

# transformasi data
# rally = sequenceID, Banyak.pukulan = eventID
colnames(df) <- c("sequenceID", "eventID", "items",  "size")
# cSPADE membutuhkan data untuk diurutkan berdasarkan sequenceID dan eventID
df <- df[order(df$sequenceID, df$eventID), ]

# ubah variabel eventID dan sequenceID ke numerik
df$eventID <- as.numeric(df$eventID)
df$sequenceID <- as.numeric(df$sequenceID)

# Atur ulang kolom dan baris, hapus data yang terlalu besar untuk cSPADE (berdasarkan size)
df <- df %>% 
  select(sequenceID, eventID, size, items) %>%
  arrange(sequenceID, eventID) %>%
  filter(size < 1000)

# Mining transactions
# di ubah menjadi data transaksi
write.table(df, "basket.txt", quote = F, row.names = F, col.names = F)
basket <- read_baskets("basket.txt", info = c("sequenceID", "eventID", "SIZE"))

# sequence berdasarkan metode cspade
baru <- cspade(basket,parameter=list(support=0.2,maxwin=60),
               control=list( tidLists=TRUE))
summary(baru)
as(baru,"data.frame")

#tambahkan confidence dan lift rasio
baru <- ruleInduction(baru, confidence = 1, 
                      control = list(verbose = TRUE))
baru <- sort(baru, decreasing = TRUE, na.last = NA, by = "lift")
baru <-as(baru,"data.frame")
View(baru)

# simpan dalam csv
write.csv(x=baru, file="C://Users//...//hasilMO2022.csv", row.names=FALSE)

