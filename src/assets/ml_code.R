#install.packages("tidyverse")
library("tidyverse")
getwd()

# You may need to change/include the path of your working directory
dat <- read.csv("C:/Users/Lavish/Documents/HealthCareData_2024.csv", stringsAsFactors = TRUE)
view(dat)
# Separate samples of normal and malicious events
dat.class0 <- dat %>% filter(Classification == "Normal") # normal
dat.class1 <- dat %>% filter(Classification == "Malicious") # malicious

# Randomly select 400 samples from each class, then combine them to form a working dataset
set.seed(10565489)
rand.class0 <- dat.class0[sample(1:nrow(dat.class0), size = 400, replace = FALSE),]
rand.class1 <- dat.class1[sample(1:nrow(dat.class1), size = 400, replace = FALSE),]
# Your sub-sample of 800 observations
mydata <- rbind(rand.class0, rand.class1)
dim(mydata) # Check the dimension of your sub-sample


view(mydata)
str(mydata)

#CategoricalFeatures

#NetworkEventType
network_event_type.number <- table(mydata$NetworkEventType)
network_event_type.prop <- prop.table(network_event_type.number)
network_event_type.percentage <- network_event_type.prop*100
NET_missing_perc <- (sum(is.na(mydata$NetworkEventType))/800)*100

#AlertCategory
alert_category.number <- table(mydata$AlertCategory)
alert_category.prop <- prop.table(alert_category.number)
alert_category.percentage <- alert_category.prop*100
AC_missing_perc <- (sum(is.na(mydata$AlertCategory))/800)*100

#NetworkInteractionType
network_interaction_type.number <- table(mydata$NetworkInteractionType)
network_interaction_type.prop <- prop.table(network_interaction_type.number)
network_interaction_type.percentage <- network_interaction_type.prop*100
NIT_missing_perc <- (sum(is.na(mydata$NetworkInteractionType))/800)*100

#Classification
classification.number <- table(mydata$Classification)
classification.prop <- prop.table(classification.number)
classification.percentage <- classification.prop*100
sum(is.na(mydata$Classification))
CN_missing_perc <- (sum(is.na(mydata$Classification))/800)*100


#SessionIntegrityCheck
session_integrity_check.number <- table(mydata$SessionIntegrityCheck)
session_integrity_check.prop <- prop.table(session_integrity_check.number)
session_integrity_check.percentage <- session_integrity_check.prop*100
SIC_missing_perc <- (sum(is.na(mydata$SessionIntegrityCheck))/800)*100

#ResourceUtilizationFlag
resource_utilization_flag.number <- table(mydata$ResourceUtilizationFlag)
resource_utilization_flag.prop <- prop.table(resource_utilization_flag.number)
resource_utilization_flag.percentage <- resource_utilization_flag.prop*100
RUF_missing_perc <- (sum(is.na(mydata$ResourceUtilizationFlag))/800)*100

#NumericFeatures

#install.packages("moments")
library(moments)

#DataTransferVolume_IN
data_transfer_volume_in.max <- max(mydata$DataTransferVolume_IN)
data_transfer_volume_in.min <- min(mydata$DataTransferVolume_IN)
data_transfer_volume_in.mean <- mean(mydata$DataTransferVolume_IN)
data_transfer_volume_in.median <- median(mydata$DataTransferVolume_IN)
data_transfer_volume_in.skewness <- skewness(mydata$DataTransferVolume_IN)
DTVI_missing_perc <- (sum(is.na(mydata$DataTransferVolume_IN))/800)*100

#DataTransferVolume_OUT
data_transfer_volume_out.max <- max(mydata$DataTransferVolume_OUT,na.rm = TRUE)
data_transfer_volume_out.min <- min(mydata$DataTransferVolume_OUT,na.rm = TRUE)
data_transfer_volume_out.mean <- mean(mydata$DataTransferVolume_OUT,na.rm = TRUE)
data_transfer_volume_out.median <- median(mydata$DataTransferVolume_OUT,na.rm = TRUE)
data_transfer_volume_out.skewness <- skewness(mydata$DataTransferVolume_OUT,na.rm = TRUE)
DTVO_missing_perc <- (sum(is.na(mydata$DataTransferVolume_OUT))/800)*100

#TransactionsPerSession
transaction_per_session.max <- max(mydata$TransactionsPerSession,na.rm = TRUE)
transaction_per_session.min <- min(mydata$TransactionsPerSession,na.rm = TRUE)
transaction_per_session.mean <- mean(mydata$TransactionsPerSession,na.rm = TRUE)
transaction_per_session.median <- median(mydata$TransactionsPerSession,na.rm = TRUE)
transaction_per_session.skewness <- skewness(mydata$TransactionsPerSession,na.rm = TRUE)
TpS_missing_perc <- (sum(is.na(mydata$TransactionsPerSession))/800)*100

#NetworkAccessFrequency 
network_access_frequency.max <- max(mydata$NetworkAccessFrequency,na.rm = TRUE)
network_access_frequency.min <- min(mydata$NetworkAccessFrequency,na.rm = TRUE)
network_access_frequency.mean <- mean(mydata$NetworkAccessFrequency,na.rm = TRUE)
network_access_frequency.median <- median(mydata$NetworkAccessFrequency,na.rm = TRUE)
network_access_frequency.skewness <- skewness(mydata$NetworkAccessFrequency,na.rm = TRUE)
NAF_missing_perc <- (sum(is.na(mydata$NetworkAccessFrequency))/800)*100

#UserActivityLevel 
user_activity_level.max <- max(mydata$UserActivityLevel,na.rm = TRUE)
user_activity_level.min <- min(mydata$UserActivityLevel,na.rm = TRUE)
user_activity_level.mean <- mean(mydata$UserActivityLevel,na.rm = TRUE)
user_activity_level.median <- median(mydata$UserActivityLevel,na.rm = TRUE)
user_activity_level.skewness <- skewness(mydata$UserActivityLevel,na.rm = TRUE)
UAL_missing_perc <- (sum(is.na(mydata$UserActivityLevel))/800)*100

#SystemAccessRate 
system_access_rate.max <- max(mydata$SystemAccessRate,na.rm = TRUE)
system_access_rate.min <- min(mydata$SystemAccessRate,na.rm = TRUE)
system_access_rate.mean <- mean(mydata$SystemAccessRate,na.rm = TRUE)
system_access_rate.median <- median(mydata$SystemAccessRate,na.rm = TRUE)
system_access_rate.skewness <- skewness(mydata$SystemAccessRate,na.rm = TRUE)
SAR_missing_perc <- (sum(is.na(mydata$SystemAccessRate))/800)*100
missing_number <- 800-(sum(table(mydata$SystemAccessRate)))

#SecurityRiskLevel 
security_risk_level.max <- max(mydata$SecurityRiskLevel,na.rm = TRUE)
security_risk_level.min <- min(mydata$SecurityRiskLevel,na.rm = TRUE)
security_risk_level.mean <- mean(mydata$SecurityRiskLevel,na.rm = TRUE)
security_risk_level.median <- median(mydata$SecurityRiskLevel,na.rm = TRUE)
security_risk_level.skewness <- skewness(mydata$SecurityRiskLevel,na.rm = TRUE)
SRL_missing_perc <- (sum(is.na(mydata$SecurityRiskLevel))/800)*100

#ResponseTime 
response_time.max <- max(mydata$ResponseTime,na.rm = TRUE)
response_time.min <- min(mydata$ResponseTime,na.rm = TRUE)
response_time.mean <- mean(mydata$ResponseTime,na.rm = TRUE)
response_time.median <- median(mydata$ResponseTime,na.rm = TRUE)
response_time.skewness <- skewness(mydata$ResponseTime,na.rm = TRUE)
RT_missing_perc <- (sum(is.na(mydata$ResponseTime))/800)*100


#OUTLIERS
length(which(mydata$ResponseTime=="99999"))
outlier1.number_perc <- ((length(which(mydata$ResponseTime=="99999")))/800)*100

length(which(mydata$NetworkAccessFrequency=="-1"))
outlier2.number_perc <- ((length(which(mydata$NetworkAccessFrequency=="-1")))/800)*100



#Yes, there are outliers as can be observed through the table and the percentages above.
#Network Access Frequency
#outlier is -1, since it is very odd from other values. 
#ResponseTime
#99999.0 is an outlier since it is comparatively a very high number than the rest of the values. 

#Fixing the outliers
mydata$NetworkAccessFrequency <- replace(mydata$NetworkAccessFrequency, mydata$NetworkAccessFrequency==-1, NA)

mydata$ResponseTime <- replace(mydata$ResponseTime, mydata$ResponseTime==99999.00000, NA)

view(mydata)

write.csv(mydata,"mydata.csv")


df <- data.frame(mydata)

df_main <- data.frame(mydata$DataTransferVolume_IN, mydata$DataTransferVolume_OUT, mydata$TransactionsPerSession, mydata$NetworkAccessFrequency, mydata$UserActivityLevel, mydata$SystemAccessRate, mydata$SecurityRiskLevel, mydata$ResponseTime, mydata$Classification)

df <- na.omit(df_main)
view(df)

#install.packages(c("vegan","tidyverse","timeDate","scales","scatterplot3d","mlbench","mvabund","ggpubr","factoextra"))

library(vegan) #PCoA and distance/similarity matrices
library(scatterplot3d) #3-D plot
library(mlbench) #Cancer dataset
library(mvabund) #Spider dataset
library(scales) #For more graphic scales options
library(tidyverse)
library(ggpubr)
library(factoextra)


pca.df <- prcomp(df[,1:8], scale=TRUE)
summary(pca.df)

pca.df$rotation

#scree plot
plot(pca.df, type="lines", main="Scree plot - mydata")

varexp.df <- summary(pca.df)$importance; varexp.df
df_screeplot <- data.frame(Variance=varexp.df[1,]^2, PC=1:length(pca.df$sdev));

#another way
ggplot(df_screeplot,aes(PC,Variance))+
  geom_line(colour="steelblue",size=1.5,linetype=2)+
  geom_point(size=5)+
  theme_minimal(base_size=14)+
  xlab("Principal Component")+
  ylab("Variance")+
  scale_x_discrete(limits=paste("PC",1:length(pca.df$sdev),sep=""))+
  annotate("text",x=c(1:8)+0.15,y=varexp.df[1,]^2+0.3,
           label=paste(round(varexp.df[2,]*100,1),"%",sep=""))  


#biplot
fviz_pca_biplot(pca.df,
                axes = c(1,2), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=df$mydata.Classification , #Outline colour of the shape
                fill.ind=df$mydata.Classification, #fill colour of the shape
                alpha=0.5, #transparency of the fill colour
                pointsize=4, #Size of the shape
                pointshape=21, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="Classification",fill="Classification",alpha="Classification"))

#Dimension 
#Projection of Points

plot_PC1 <- data.frame(PC1 = pca.df$x[, 1], PC2 = rep(0, length(pca.df$x[, 2])), Classification = df$mydata.Classification)

# Plot using ggplot
ggplot(plot_PC1, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour=Classification),alpha=0.8, size=5) +
  theme_minimal()+
  xlab("PC1")+
  ylab("PC2")

plot_PC2 <- data.frame(PC1 =rep(0, times=length(pca.df$x[,1])), PC2 = pca.df$x[,2], Classification = df$mydata.Classification)

# Plot using ggplot
ggplot(plot_PC2, aes(x = PC2, y = PC1)) +
  geom_point(aes(colour=Classification),alpha=0.8, size=5) +
  theme_minimal()+
  xlab("PC2")+
  ylab("PC1")
  






