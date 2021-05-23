#Project - Sem 8 
dt <- read.csv("C:/Users/Adwait/Desktop/Adwait/Sem 8/Labs/BDA Lab/BDA Project/R_studio_House_price_predition-main/R_studio_House_price_predition-main/House_Price.csv",header = TRUE)
df <-dt
View(df)
summary(df)

#Visualization library 
install.packages("devtools")
library(devtools)
install.packages("ggplot2")
install_github("easyGgplot2", "kassambara")
install.packages("ggpubr")
library(ggpubr)

pairs(~price+crime_rate+rainfall+n_hot_rooms,data=df, col="yellow", density="65")

barplot(density=50,col="darkblue", table(df$airport))
barplot(col="darkred",horiz=TRUE, table(df$waterbody))


#histograms
hist(df$crime_rate, col='yellow', density = 60)
hist(df$resid_area ,col='yellow', density = 70)


#removing_variables
df <- df[-18]

summary(df$n_hot_rooms)
summary(df$rainfall)
#outliers
quantile(df$n_hot_rooms,0.99)
uv=3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)

lv=0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv] <-lv
summary(df$rainfall)

#Missing var
mean(df$n_hos_beds,na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)

#making_linear_relation
pairs(~price+crime_rate+rainfall,data=df, col="green", density="99")
plot(df$price,df$crime_rate)
df$crime_rate=log(1+df$crime_rate)
plot(df$price,df$crime_rate, col="darkred")


#creating_new_vaiable
df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
#removing var
df2 <-df[,-7:-10]



#creating_dummy_variable
install.packages("dummies")
df2 <-dummy.data.frame(df2)
df2 <-df2[,-9]
df2 <-df2[,-14]

#correlation_matrix
#cor(as.numeric(df2))
#round(cor(df2),2)
df2 <-df2[,-16]
pairs(~price+crime_rate+rainfall+n_hot_rooms,data=df2,col="green", density="99")

#linear_regression
simple_modal <- lm(price~room_num,data=df2)
plot(df2$room_num,df2$price,col = "darkred")
abline(simple_modal, col="darkblue")
legend("topleft",
       c("Non Deviating","Deviating"),
       fill = c("red","darkred")
)

simple_modal <- lm(price~age,data=df2)
plot(df2$age,df2$price,pch = 19, col = rgb(0, 0, 0, 0.15))
abline(simple_modal, col="blue")
legend("topleft",
       c("Normal","Saturating"),
       fill = c("lightgrey","grey")
)


#Multi_linear_regrssion
multi_modal <- lm(price~.,data = df2)

#Train_test_data
install.packages("caTools")
set.seed(0)

#spliting_ratio
#split = sample.split(df2,SplitRatio=0.8)
#training_set = subset(df2,split==TRUE)
#test_set = subset(df2,split==FALSE)
samp <- sample(1:nrow(df2), size=round(0.8*nrow(df2)), replace=FALSE)
training_set <- df2[samp,]  #Only takes rows that are in samp
test_set <- df2[-samp,]


#training the the training_set
lm_a = lm(price~.,data = training_set)
View(train_a)

#predicting_the_trained_set_and_
train_a =predict(lm_a,training_set)
View(train_a)
#prdicting_the_test_set_using_test_set
test_a = predict(lm_a,test_set)
View(test_a)

mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)
plot(test_set,training_set)
View(test_set)
View(training_set)
