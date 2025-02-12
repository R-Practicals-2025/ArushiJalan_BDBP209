#lab6 7th February 2025 by Arushi
data=read.csv("/home/ibab/BrainCancer.csv", header=TRUE)
print(data)
#5
# i)
print(class(data$sex))
data$sex <- factor(data$sex, levels=c("Male", "Female"))
#print(class(data$sex))
print(is.factor(data$sex))
# ii)
print(nlevels(data$sex))
# iii)
print(levels(data$diagnosis))

#6
# i)
num_rows <- nrow(data)
temperature_factor <- gl(n = 3, k = ceiling(num_rows / 3), length = num_rows, labels = c("Hot", "Cold", "Lukewarm"))
print(head(temperature_factor))
# ii)
brain_cancer_new <- data  
brain_cancer_new$Temperature <- temperature_factor  
print(brain_cancer_new)  

#7
# i)
tapply(data$gtv, data$ki, mean)
#ii)
tapply(data$gtv, data$ki, mean, trim=0.1)

#8
print(pmin(data$gtv,data$ki,data$time))
print(pmax(data$gtv,data$ki,data$time))

#9
# i)
ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(data$gtv, ranks, sorted, ordered)
print(view)
print(data$gtv[68])
print(data$time[ordered])
# ii)
print(data$diagnosis[ordered])
ordered_data <- data.frame(gtv = data$gtv[ordered], diagnosis = data$diagnosis[ordered])
write.csv(ordered_data, "lab4_ordered_data.csv", row.names = FALSE)
print(head(ordered_data))

#10
# i)
filter1 = data[1:6,3:8]
# ii)
filter1mat=as.matrix(filter1)
print(filter1mat)
print(class(filter1mat))
print(mode(filter1mat))
# iii)
newcol=data$ki+data$gtv+data$time
# iv)
newcoladded=data.frame(data,newcol)
print(newcoladded)
# v)
newcol <- data$ki + data$gtv + data$time
newcoladded2 <- cbind(data, newcol)
print(newcoladded2)
# iv)
new_rows <- data[c(26:35),] 
newdata <- rbind(data, new_rows)
print(newdata)


#11
X <- matrix(c(1,0,2,5,3, 1,1,3,1,3, 3,1,0,2,2, 1,0,2,1,0), nrow=4, byrow=TRUE)
print(rownames(X)) 
print(colnames(X))
rownames(X) <- paste("Trial", 1:4, sep=" ")
colnames(X) <- c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")
print(X)
dimnames(X) <- list(paste("Trial", 1:4, sep=" "), paste("drug", 1:5, sep=" "))
print(X)

#12
# i)
mean(X[,5])
# ii)
var(X[4,])
# iii)
rowSums(X) 
apply(X,1,sum)
# iv)
colSums(X)
apply(X,2,sum)
# v)
rowMeans(X)
apply(X,1,mean)
# vi)
colMeans(X)
apply(X,2,mean)
# vii)
group <- c("A", "B", "B", "A")
rowsum_result <- rowsum(X, group)
print(rowsum_result)
print(row(X))  
print(col(X))
tapply_result <- tapply(X, list(group[row(X)], col(X)), sum)
print(tapply_result)
aggregate_result <- aggregate(X, list(group), sum)
print(aggregate_result)
# viii)
apply(X,2,sample)
# ix)
X <- rbind(X, apply(X,2,mean))
X <- cbind(X,apply(X,1,var))
headings <- c(paste("drug.",1:5,sep=""),"var") 
dimnames(X)<- list(NULL,headings) 
headings <- c(paste("Trial-",1:4,sep=''),"Mean") 
rownames(X) <- headings
print(X)


#13
data=read.csv("/home/ibab/BrainCancer.csv", header=TRUE)
eg_sweep = data.frame(data$ki,data$gtv,data$time)
cols <- apply(eg_sweep,2,mean)
print(cols)
cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])), nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)
eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print("Method 2")
print(eg_sweep_alt2)
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))


#14
data <- read.table("/home/ibab/Downloads/pgfull.txt", header = TRUE)
species <- data[, 1:54]
max_indices <- max.col(species)
print(max_indices)
species_names <- names(species)[max_indices]
print(species_names)
species_freq <- table(species_names)
print(species_freq)
min_indices <- max.col(-species)
print(min_indices)
#alt method:
min_indices <- apply(species, 1, which.min)
print(min_indices)

#15
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
print(items[[3]])
print(items[[3]][3])
items[3]
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))