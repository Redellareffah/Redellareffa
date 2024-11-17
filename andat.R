#nomor 2b
setwd("C:/Users/USER/Documents")
getwd()

UTSAndat2b <- read.csv("AnalisisDataB4..CSV", sep = ",", header = TRUE)
View(UTSAndat2b)

#nomor 2c
#mengecek data
nrow(UTSAndat2b)
ncol(UTSAndat2b)
str(UTSAndat2b)
#Ringkasan data
summary(UTSAndat2b)

#nomor2d
#look at the top and the bottom of your data
head(UTSAndat2b)
tail(UTSAndat2b)

#nomor 2e
#check your "n"s
dim(UTSAndat2b)
#mengecek nilai yang hilang
colSums(is.na(UTSAndat2b))

#nomor 2f
#Validate with at least one external data source 
summary(UTSAndat2b$NumberOfVideosWatched)
quantile(UTSAndat2b$NumberOfVideosWatched, seq(0, 1, 0.1))

#nomor2g
library(ggplot2)
# Membuat plot
ggplot(data = UTSAndat2b, aes(x = factor(NumberOfVideosWatched), y = CompletionRate)) +
  geom_boxplot(color = "red") +
  labs(
    title = "Hubungan Jumlah Video yang Ditonton dan Completion Rate",
    x = "Jumlah Video yang Ditonton",
    y = "Completion Rate"
  ) +
  theme_minimal()

#nomor2h
#Try the easy solution first 
mean(UTSAndat2b$NumberOfVideosWatched, na.rm = TRUE)
mean(UTSAndat2b$CompletionRate, na.rm = TRUE)

#nomor2i
# Distribusi data
hist(UTSAndat2b$NumberOfVideosWatched, 
     main = "Distribusi Jumlah Video yang Ditonton", 
     xlab = "Jumlah Video yang Ditonton", 
     col = "lightblue", 
     breaks = 10)

hist(UTSAndat2b$CompletionRate, 
     main = "Distribusi Completion Rate", 
     xlab = "Completion Rate (%)", 
     col = "pink", 
     breaks = 10)

# Hubungan antara jumlah video yang ditonton dan completion rate
plot(UTSAndat2b$NumberOfVideosWatched, UTSAndat2b$CompletionRate, 
     main = "Hubungan Jumlah Video Ditonton dan Completion Rate",
     xlab = "Jumlah Video yang Ditonton", 
     ylab = "Completion Rate (%)", 
     col = "brown", 
     pch = 19)
abline(lm(UTSAndat2b$CompletionRate ~ UTSAndat2b$NumberOfVideosWatched), col = "yellow")

# Menghitung korelasi
korelasi <- cor(UTSAndat2b$NumberOfVideosWatched, UTSAndat2b$CompletionRate, use = "complete.obs")
print(paste("Korelasi antara jumlah video yang ditonton dan completion rate: ", korelasi))

#nomor3a
#membuat model regresi sederhana
model=lm(CompletionRate ~ NumberOfVideosWatched, data=UTSAndat2b)
model
summary(model)

#nomor3b
#Comparing Model Expectations to Reality: buat histogram variabel dependen 
#dan bandingkan dengan histogram data yang berdistribusi normal! 
# Asumsikan data kamu disimpan di UTSAndat2b

# Membuat histogram dan menambahkan distribusi normal
ggplot(data = UTSAndat2b, aes(x = NumberOfVideosWatched)) +
  # Histogram, menggunakan skala density agar lebih mudah dibandingkan dengan distribusi normal
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "purple", 
                 color = "yellow", alpha = 0.7) +
  # Menambahkan kurva distribusi normal dengan mean dan SD dari data
  stat_function(fun = dnorm, 
                args = list(mean = mean(UTSAndat2b$NumberOfVideosWatched, na.rm = TRUE), 
                            sd = sd(UTSAndat2b$NumberOfVideosWatched, na.rm = TRUE)), 
                color = "red", size = 1) +
  # Menambahkan judul dan label
  labs(title = "Histogram dan Distribusi Normal dari NumberOfVideosWatched",
       x = "NumberOfVideosWatched",
       y = "Density") +
  theme_minimal()



