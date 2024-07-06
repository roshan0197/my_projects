###### Importing Required Libraries ######
library(readr)
library(greybox)
library(cluster)
library(plotrix)
install.packages("MASS")
library(MASS)
install.packages("scatterplot3d")
library(scatterplot3d)
installed.packages(scatterplot3d)
library(scatterplot3d)
installed.packages("plotly")
library(plotly)
##### Reading Original Excel Dataset #####

#1. Reading data
airlinesData90 <- read_csv("airlinesData90.csv")
#rm(airlinesData90) #remove if necessary
mydata <- airlinesData90
View(mydata)
col_names <- names(mydata)
print(col_names)
mydata <- na.omit(mydata)
summary(mydata)
attach(mydata)


#2.Changing non-numerical columns into factors
mydata$Gender <- factor(mydata$Gender)
mydata$`Customer Type` <- factor(mydata$`Customer Type`)
mydata$`Type of Travel` <- factor(mydata$`Type of Travel`)
mydata$Class <- factor(mydata$Class)
mydata$satisfaction <- factor(mydata$satisfaction)
y = mydata$satisfaction=="satisfied"
mydata["class"] = y*1
str(mydata)
assoc(mydata)

###### Exploratory Data Analysis #######
#1. Basic Analysis 

sum(satisfaction=="satisfied")
sum(satisfaction=="satisfied")/nrow(mydata) # Percentage of proportion
sum(satisfaction=="neutral or dissatisfied")
sum(satisfaction=="neutral or dissatisfied")/nrow(mydata) # Percentage of proportion

mean(satisfaction=="satisfied" & `Customer Type`=="Loyal Customer")*100

table(satisfaction,`Customer Type`)
#proportion of Loyal customers that are neutral or dissatisfied
sum(satisfaction=="neutral or dissatisfied" & `Customer Type`=="Loyal Customer")/sum(`Customer Type`=="Loyal Customer")*100
#proportion of Disloyal customers that are satisfied
sum(satisfaction=="satisfied" & `Customer Type`=="disloyal Customer")/sum(`Customer Type`=="disloyal Customer")*100

#2. Plots for different variables 

barp(mydata,"satisfaction")
cc_barplot(mydata,"satisfaction","Type of Travel", freq="relfreq")
cc_barplot(mydata,"satisfaction","Customer Type", freq="relfreq")
cc_barplot(mydata,"Age", "Flight Distance")



hist(mydata$`Flight Distance`,xlab = "Flight Distance",main = "Plot of Flight Distance")
hist(mydata$Age,xlab = "Passanger Age",main = "Passanger Age")
plot(mydata$Age,mydata$`Flight Distance`,xlab = "Passanger Age",ylab = "Flight Distance",main = "Age vs Flight Distance")

hist(mydata$Age,xlab = "Age of Passenger",main = "Passanger Age")

densityplot(~ mydata$Class, data = mydata)

ggplot(mydata,aes(x=satisfaction)) +
  geom_bar(aes(fill=satisfaction),position="dodge")+
  labs(title="Satisfaction Levels of Passengers",x="Satisfaction",y="Count")+
  scale_fill_manual(values=c("#A020F0","#56B4E9","#009E73"))+
  theme_minimal()
#Gender vs satisfaction bar graph
ggplot(mydata, aes(x=Gender, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#AE4371", "#ff7f0e")) +
  labs(title="Gender vs. Satisfaction", x="Gender", y="Count") +
  theme_bw()

ggplot(data=mydata,aes(x=`Online boarding`,y=satisfaction))+
  geom_point(aes(shape=satisfaction,color=satisfaction),size=4,alpha=0.6)+
  labs(x="Online boarding",y="Satisfaction")+
  scale_color_manual(values=c("red","blue","green"),name="Satisfaction Level")+
  scale_shape_manual(values=c(16,17,18),name="Satisfaction Level")+
  theme_minimal()

ggplot(data=mydata,aes(x=`Departure Delay in Minutes`,fill=satisfaction))+
  geom_histogram(binwidth=100,position="identity",alpha=0.5,color="black")+
  labs(title = "Histogram of DepartureDelay in Minutes by Satisfaction Level",
       x = "Departure Delay in Minutes", y="Count")+
  scale_fill_manual(values=c("#0072B2","#E69F00"))

#Subset data for satisfied customers only
satisfied_data<-mydata[mydata$satisfaction == "satisfied",c("Inflight wifi service","Inflight entertainment","Inflight service")]

# Create stacked histogram
ggplot(melt(satisfied_data),aes(x=value,fill=variable))+
  geom_histogram(binwidth=1,position="stack",alpha=0.7)+
  labs(title="Stacked Histogram for Inflight Services for Satisfied Passangers",
       x="Service Rating",y="Count")+
  scale_fill_manual(values=c("#66c2a5","#E69F00","#8da0cb"))+
  theme_bw()
# Only for satisfied customers
data_satsified <- subset(mydata,satisfaction=="satisfied")
Plot_Col <- colnames(data_satsified[,c(1,2,4,5)])
for (col in Plot_Col) {
  # Create the barplot
  freq_table <- table(data_satsified[, col])
  barplot(freq_table,
          main = paste("Count of", col),
          xlab = col,
          ylab = "Count",
          col = "#E69F00")
  # Add data labels
  text(seq_along(freq_table), freq_table, labels = freq_table, pos = 1)
}

# only for Dissatisfied customers 
data_dissatsified <- subset(mydata,satisfaction!="satisfied")
Plot_Col <- colnames(data_dissatsified[,c(1,2,4,5)])
for (col in Plot_Col) {
  # Create the barplot
  freq_table <- table(data_dissatsified[, col])
  barplot(freq_table,
          main = paste("Count of", col),
          xlab = col,
          ylab = "Count",
          col = "#8da0cb")
  # Add data labels
  text(seq_along(freq_table), freq_table, labels = freq_table, pos = 1)
}


#3. correlation matrix
numeric_data <- mydata[, sapply(mydata, is.numeric)]
corr_matrix <- cor(numeric_data)
print(corr_matrix)
#corrplot(corr_matrix)
corrplot(association(mydata)$value)

correlation_matrix <- cor(mydata[, sapply(mydata, is.numeric)])
correlation_matrix["Online boarding","class" ]
print(correlation_matrix)
assoc(mydata)
corrplot(association(mydata)$value)

#4. IV and Weights of Evidence 
require(Information)
IV <- create_infotables(data = mydata[,],y = "class",bins = 5)
IV

###### Multidimentional Scaling ######

mydata_new <- mydata[, 1:(ncol(mydata) - 1)]
mydataDissimMatrix_1 <- daisy(mydata_new, metric="gower")
mydata_MDS <- cmdscale(mydataDissimMatrix_1, k=2)
plot(mydata_MDS)
thigmophobe.labels(mydata_MDS[,1], mydata_MDS[,2], rownames(mydata))

###### Stress Plot ######
nDimensions <- ncol(mydata)
mydataStress <- vector("numeric",nDimensions)
for(i in 1:nDimensions){
  # Do MDS
  mydataMDSTest <- cmdscale(mydataDissimMatrix_1,k=i)
  # Produce dissimilarities matrix for the new dimensions
  mydataMDSDist <- daisy(mydataMDSTest,"gower")
  # Calculate stress metrics
  mydataStress[i] <- sqrt(sum((mydataDissimMatrix_1 -
                                 mydataMDSDist)^2)/sum(mydataDissimMatrix_1^2))
}
plot(mydataStress,main = "Stress plot for Airlines Data")

##### Multi-Dimensional Scaling after Stress plot #####
mydata_new <- mydata[, 1:(ncol(mydata) - 1)]
mydataDissimMatrix_1 <- daisy(mydata_new, metric="gower")
mydata_MDS <- cmdscale(mydataDissimMatrix_1, k=3)
scatterplot3d(mydata_MDS, xlab="Dimension 1", ylab="Dimension 2", zlab="Dimension 3",
              color=rainbow(nrow(mydata)))

###### Naming new variables ######
mydataMDSNew <- cbind(as.data.frame(mydata_MDS),mydata)
assoc(mydataMDSNew)
#Spread() will not work for more than 20 variables so we will explain using Assoc()







##### Final ######

mydataMDSNew <- cbind(as.data.frame(mydata_MDS),mydata)#not in use


mydataDissimMatrix_1 <- daisy(mydata_new, metric="gower")
mydata_MDS <- cmdscale(mydataDissimMatrix_1, k=3)
colnames(mydata_MDS) <- c("D1","D2","D3")
scatterplot3d(mydata_MDS, xlab="Dimension 1", ylab="Dimension 2", zlab="Dimension 3", color=rainbow(nrow(mydata)))

mydata_new <- as.data.frame(lapply(mydata_new, as.numeric))
mds_cor <- cor(mydata_new,mydata_MDS)
mydata_new <- mydata[, 1:(ncol(mydata) - 1)]

dim1_vars <- as.data.frame(names(sort(abs(mds_cor[,1]), decreasing = TRUE)))
dim1_vars
dim2_vars <- as.data.frame(names(sort(abs(mds_cor[,2]), decreasing = TRUE)))
dim2_vars
dim3_vars <- as.data.frame(names(sort(abs(mds_cor[,3]), decreasing = TRUE)))
dim3_vars
dfMDSplotdummy <- data.frame(mydata_MDS,D4=mydata_new[,23])
fig <- plot_ly(data = dfMDSplotdummy, x = ~D1, y = ~D2, z = ~D3, size = 1,
               color = factor(dfMDSplotdummy[,4]), colors = c("#0000FF", "#C2ADDA"),
               type = "scatter3d", mode = "markers")
fig
assoc(mydata_MDS)
