# Read Data
heart<-read.csv("D:/heart.csv", header = TRUE, sep = ",")
heart
str(heart) # cek tipe data

# Digunakan untuk memberi label pada variabel diskrit
library(dplyr)

heart <- heart %>% 
  mutate_if(is.integer, as.factor) %>% 
  mutate(sex = factor(sex, levels = c(0,1), labels = c("Female","Male")),
         fbs =factor(fbs, levels = c(0,1), labels = c("False","True")),
         exang = factor(exang, levels = c(0,1), labels = c("No","Yes")),
         target = factor(target, levels = c(0,1), 
                         labels = c("Health","Not Health")))
glimpse(heart)

# cek missing value
colSums(is.na(heart))

# Cek varians apakah mendekati nol (untuk feature sselection)
library(caret)
nearZeroVar(heart)

# membagi data menjadi data training dan data testing
set.seed(250)
intrain <- sample(nrow(heart),nrow(heart)*0.8)
heart_train <- heart[intrain, ]
heart_test <- heart[-intrain, ]

# cek proporsi masing masing split data
prop.table(table(heart$target))
prop.table(table(heart_train$target))
prop.table(table(heart_test$target))

# membuat model
library(partykit)
heart_tree <- ctree(target~., data = heart_train)
heart_tree

# membuat gambar grafiknya
plot(heart_tree, type = "simple")

# evaluasi model data training
pred_heart_train <- predict(heart_tree, heart_train)
confusionMatrix(pred_heart_train, heart_train$target, positive = "Not Health")

# evaluasi model data testing
pred_heart_test <- predict(heart_tree, heart_test)
confusionMatrix(pred_heart_test, heart_test$target, positive = "Not Health")  

# dilakukan resampling 
set.seed(250)
intrain <- sample(nrow(heart), nrow(heart)*0.8)
re_train <- heart[intrain,]
re_train <- upSample(re_train[,-14], re_train[,14], yname = "target" )
re_test <- heart[-intrain,]

# cek proporsi data training dan data testing
prop.table(table(re_train$target))
prop.table(table(re_test$target))

# dilakukan pemodelan menggunakan data yang sudah diresampling
heart_tree_new <- ctree(target~., re_train)
heart_tree_new

# menggambar grafiknya
plot(heart_tree_new, type = "simple")

# evaluasi model data training
pred_train <- predict(heart_tree_new, re_train)
confusionMatrix(pred_train, re_train$target)

# evaluasi model data testing
pred_test <- predict(heart_tree_new, re_test)
confusionMatrix(pred_test, re_test$target)
