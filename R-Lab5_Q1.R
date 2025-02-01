#lab5 31st January 2025 by Arushi

#1 
data=read.csv("/home/ibab/Downloads/BrainCancer.csv",header=TRUE)
print(data)

#2
# i)
print(dim(data))
# ii)
print(colnames(data))
# iii)
print(rownames(data))
# iv)
print(head(data,n=30))
# v)
lapply(data,table)
# vi)
data$sex<- factor(data$sex,levels=c("Male","Female"))
is.factor(data$sex)
data$diagnosis<- factor(data$sex,levels=c("Meningioma","Hg glioma","LG glioma"))
is.factor(data$diagnosis)
data$loc<- factor(data$sex,levels=c("Infratentorial","Supratentorial"))
is.factor(data$loc)
data$stereo<- factor(data$sex,levels=c("SRS","SRT"))
is.factor(data$stereo)

# vii)
print(levels(data$loc))
print(levels(data$sex))
print(levels(data$diagnosis))

# viii)
print(nlevels(data$loc))
print(nlevels(data$sex))
print(nlevels(data$diagnosis))

#3
# i)
print(mean(data$gtv))
# ii)
print(mean(data$time))
# iii)
print(median(data$gtv))
# iv)
print(which.max(table(data$gtv)))
# v)
print(sd(data$gtv))
# vi)
print(summary(data$gtv))
# vii)
hist(data$gtv)
# viii)
library(moments)
print(skewness(data$gtv))
# ix)
print(kurtosis(data$gtv))
# x)
boxplot(data$gtv)
boxplot(data$gtv, range=0.1, horizontal=FALSE, border="lavender", col= "pink")
boxplot(data$gtv, range=0.2, horizontal=FALSE, border="purple", col= "pink")
boxplot(data$gtv, range=0.5, horizontal=FALSE, border="purple", col= "lavender")
# xi)
par(mfrow=c(1, 3))
boxplot(data$gtv)
boxplot(data$ki)
boxplot(data$time)

#4
# i)
gtv_subset <- data[data$gtv > 20, ]
dim(gtv_subset)
print(gtv_subset)

# ii)
filter=data[c( 1,3,8,9,13,14,18,21),]
print(filter)

# iii)
female_indices <- which(data$sex == "Female")
print(female_indices)
female_subset <- subset(data, sex == "Female")
print(female_subset)

# iv)
new_column <- data$gtv * data$ki / 234
new_dataframe <- data.frame(GTV = data$gtv, KI = data$ki, NewColumn = new_column)
print(new_dataframe)

# v)
female_subset <- subset(data, sex == "Female")
write.csv(female_subset, "/home/ibab/Downloads/lab4_female_BrainCancer.csv", row.names = FALSE)
data_=read.csv("/home/ibab/Downloads/lab4_female_BrainCancer.csv",header=TRUE)
print(data_)
