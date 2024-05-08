# Library
# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("RColorBrewer")
# install.packages("DescTools")
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(DescTools)

#Data processing
#Reads the data and assigns the empty string and "N/A" values to NA
Intel_CPUs = read.csv("C:/Users/DELL/OneDrive/Máy tính/XSTK_KHOA/BTL/Intel_CPUs.csv",na.strings = c("", "N/A")) 

#Select the variables to use
CPUs_data = Intel_CPUs[,c("Vertical_Segment","Status","Lithography"
                          ,"Recommended_Customer_Price","nb_of_Cores","nb_of_Threads"
                          ,"Processor_Base_Frequency","TDP","Max_Memory_Size")]

#Check defective data
print(apply(is.na(CPUs_data),2,sum))

#Lithography processing
CPUs_data$Lithography <-as.double( gsub(" nm$", "", CPUs_data$Lithography))
median_Lithography <- median(CPUs_data$Lithography, na.rm = TRUE)
CPUs_data$Lithography[is.na(CPUs_data$Lithography)] <- median_Lithography

#Recommended_Customer_Price processing
recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}
CPUs_data$Recommended_Customer_Price <- gsub("\\$", "", CPUs_data$Recommended_Customer_Price)
CPUs_data$Recommended_Customer_Price <- sapply(CPUs_data$Recommended_Customer_Price, recommend_price) 
CPUs_data$Recommended_Customer_Price <- as.double(CPUs_data$Recommended_Customer_Price)
median_Recommended_Customer_Price <- median(CPUs_data$Recommended_Customer_Price, na.rm = TRUE)
CPUs_data$Recommended_Customer_Price[is.na(CPUs_data$Recommended_Customer_Price)] <- median_Recommended_Customer_Price

#nb_of_Threads processing
median_nb_of_Threads <- median(CPUs_data$nb_of_Threads, na.rm = TRUE)
CPUs_data$nb_of_Threads[is.na(CPUs_data$nb_of_Threads)] <- median_nb_of_Threads

#Processer_Base_Frequency processing
base_frequency <-function(f){
  if (grepl(' GHz',f)) {
    return (as.double(gsub(" GHz","",f))*1000)
  }
  return (as.double(gsub(" MHz","",f)))
}
CPUs_data$Processor_Base_Frequency <-as.integer( sapply(CPUs_data$Processor_Base_Frequency,base_frequency))
mean_Processor_Base_Frequency <- mean(CPUs_data$Processor_Base_Frequency, na.rm = TRUE)
CPUs_data$Processor_Base_Frequency[is.na(CPUs_data$Processor_Base_Frequency)] <- mean_Processor_Base_Frequency

#TDP processing
CPUs_data$TDP <-as.double( gsub(" W", "", CPUs_data$TDP))
median_TDP <- median(CPUs_data$TDP, na.rm = TRUE)
CPUs_data$TDP[is.na(CPUs_data$TDP)] <- median_TDP

#Max_memory_size processing
max_mem_size_clean <- function(size){  
  if(grepl('G',size)){
    return ( as.double(gsub(" GB","",size)) )
  }
  return ( as.double(gsub(" TB","",size)) * 1024 )
}
CPUs_data$Max_Memory_Size <- sapply(CPUs_data$Max_Memory_Size,max_mem_size_clean)     
median_Max_Memory_Size <- median(CPUs_data$Max_Memory_Size, na.rm = TRUE)
CPUs_data$Max_Memory_Size[is.na(CPUs_data$Max_Memory_Size)] <- median_Max_Memory_Size

# Check again
print(apply(is.na(CPUs_data),2,sum))
print(str(CPUs_data))

# Descriptive Statistic
# Separate data into numerical variables and categorical variables
# Numerical variables
numerical_cols = c("Lithography","Recommended_Customer_Price"
                   ,"nb_of_Cores","nb_of_Threads","TDP", 
                   "Processor_Base_Frequency", "Max_Memory_Size")

# Descriptive statistical table
summary_numeric_table <- data.frame(
  Staticstic=c("Mean", "Sd", "Median", 
               "First Quantile", "Third Quantile","Min", "Max")
)
for (i in numerical_cols){
  mean<- mean(CPUs_data[[i]])
  sd <- sd(CPUs_data[[i]])
  median <- median(CPUs_data[[i]])
  first_quantile <- quantile(CPUs_data[[i]], probs = 0.25)
  third_quantile <- quantile(CPUs_data[[i]], probs = 0.75)
  min <- min(CPUs_data[[i]])
  max <- max(CPUs_data[[i]])
  summary_numeric_table[[i]] <- c(mean, sd, median, 
                                  first_quantile, third_quantile, min, max)
}
colnames(summary_numeric_table)[-1] <- numerical_cols

# Categorical variables
# Check the number of categorical variables
categorical_cols = c("Vertical_Segment", "Status")
summary_categorical_table <- data.frame(
  Staticstic = c("Count","Unique","Mode","Freq")
)
for (i in categorical_cols) {
  count <- length(CPUs_data[[i]])
  unique <- length(unique(CPUs_data[[i]]))
  mode <- Mode(CPUs_data[[i]])
  freq <- attr(mode,"freq")
  summary_categorical_table <- 
    cbind(summary_categorical_table,new_col = c(count, unique, mode, freq))
}
colnames(summary_categorical_table) <- c("", categorical_cols)

#Check the classification of qualitative variables
table(CPUs_data$Vertical_Segment)
table(CPUs_data$Status)

# Plot the distribution of the numerical variables
# Density Histogram Lithography
ggplot(CPUs_data, aes(x = Lithography)) +
  geom_histogram(aes(y = ..density..), binwidth = 13, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 7) +
  scale_x_continuous(breaks = seq(0, 250, by = 50)) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.01)) +
  labs(x = "Lithography", y = "Density", title = "Density Histogram of Lithography") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram Recommended_Customer_Price
ggplot(CPUs_data, aes(x = Recommended_Customer_Price)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 250) +
  scale_x_continuous(breaks = seq(0, 8000, by = 2000)) +
  labs(x = "Recommended_Customer_Price", y = "Density", 
       title = "Density Histogram of Recommended Customer Price") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram nb_of_Cores
ggplot(CPUs_data, aes(x = nb_of_Cores)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 3) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(x = "Number of Cores", y = "Frequency",
       title = "Density Histogram of Number of Cores") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram nb_of_Threads
ggplot(CPUs_data, aes(x = nb_of_Threads)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 2) +
  scale_x_continuous(breaks = seq(0, 140, by = 20)) +
  labs(x = "Number of Threads", y = "Density",
       title = "Density Histogram of Number of Threads") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram Processor_Base_Frequency
ggplot(CPUs_data, aes(x = Processor_Base_Frequency)) +
  geom_histogram(aes(y = ..density..), binwidth = 400, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 200) +
  scale_x_continuous(breaks = seq(0, 4300, by = 1000)) +
  labs(x = "Processor Base Frequency", y = "Density",
       title = "Density Histogram of Processor Base Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram TDP
ggplot(CPUs_data, aes(x = TDP)) +
  geom_histogram(aes(y = ..density..), binwidth = 20, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 10) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  labs(x = "TDP", y = "Density", title = "Density Histogram of TDP") +
  theme(plot.title = element_text(hjust = 0.5))

# Density Histogram Max_Memory_Size
ggplot(CPUs_data, aes(x = Max_Memory_Size)) +
  geom_histogram(aes(y = ..density..), binwidth = 400, color = "black", fill = "white") +
  geom_density(colour = "black",fill = 4, alpha = 0.2, bw = 200) +
  scale_x_continuous(breaks = seq(0, 4200, by = 1000)) +
  labs(x = "Max Memory Size", y = "Density", 
       title = "Density Histogram of Max Memory Size") +
  theme(plot.title = element_text(hjust = 0.5))

# Bar plot Vertical_Segment
table(CPUs_data$Vertical_Segment)
Vertical_Segment_table <- data.frame(table(CPUs_data$Vertical_Segment))
colnames(Vertical_Segment_table) <- c("Vertical_Segment", "Frequency")

ggplot(Vertical_Segment_table, 
       aes(x = Vertical_Segment, y = Frequency, fill = Vertical_Segment)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Vertical Segment", y = "Count", 
       title = "Bar Chart of Vertical Segment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Bar plot Status
table(CPUs_data$Status)
Status_table <- data.frame(table(CPUs_data$Status))
colnames(Status_table) <- c("Status", "Frequency")

ggplot(Status_table, 
       aes(x = Status, y = Frequency, fill = Status)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Status", y = "Count", 
       title = "Bar Chart of Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Box plot between Recommended_Customer_Price and Vertical_Segment
ggplot(data = CPUs_data, aes(x = Vertical_Segment, y = Recommended_Customer_Price, fill = Vertical_Segment)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  labs(x = "Vertical Segment", y = "Recommended Customer Price", 
       title = "Boxplot of Recommended Customer Price and Vertical Segment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Box plot between Recommended_Customer_Price and Status
ggplot(data = CPUs_data, aes(x = Status, y = Recommended_Customer_Price, fill = Status)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  labs(x = "Status", y = "Recommended Customer Price", 
       title = "Boxplot of Recommended Customer Price and Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Box plot between Recommended_Customer_Price and Status
ggplot(data = CPUs_data, aes(x = Status, y = Recommended_Customer_Price, fill = Status)) +
  geom_boxplot() +
  labs(x = "Status", y = "Recommended Customer Price", 
       title = "Boxplot of Recommended Customer Price and Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

# Scatter plot between Recommended_Customer_Price and Lithography
ggplot(CPUs_data, aes(x = Lithography, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Lithography", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price vs Lithography") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

# Scatter plot between Recommended_Customer_Price and nb_of_Cores
ggplot(CPUs_data, aes(x = nb_of_Cores, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Number of Cores", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price 
       vs Number of Cores") +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot between Recommended_Customer_Price and nb_of_Threads
ggplot(CPUs_data, aes(x = nb_of_Threads, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Number of Threads", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price 
       vs Number of Threads") +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot between Recommended_Customer_Price and Processor_Base_Frequency
ggplot(CPUs_data, aes(x = Processor_Base_Frequency, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Processor Base Frequency", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price 
       vs Processor Base Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

# Scatter plot between Recommended_Customer_Price and TDP
ggplot(CPUs_data, aes(x = TDP, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "TDP", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price vs TDP") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

# Scatter plot between Recommended_Customer_Price and Max_Memory_Size
ggplot(CPUs_data, aes(x = Max_Memory_Size, y = Recommended_Customer_Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Max Memory Size", y = "Recommended Customer Price", 
       title = "Scatterplot of Recommended Customer Price 
       vs Max Memory Size") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))

# Correlation matrix 
correlation_matrix <- round(cor(CPUs_data[, c("Lithography",
                                              "Recommended_Customer_Price"
                                              ,"nb_of_Cores","nb_of_Threads",
                                              "Processor_Base_Frequency", "TDP"
                                              ,"Max_Memory_Size")]), 2)
corrplot(correlation_matrix, method = "number", type = "upper")

#MLR
#Use linear model function to build the Models
lm(Recommended_Customer_Price~Lithography+nb_of_Cores+nb_of_Threads+Processor_Base_Frequency+TDP+Max_Memory_Size+Vertical_Segment+Status,data=CPUs_data)

#Build and summary the Model 1
Model_1<-lm(Recommended_Customer_Price~Lithography+nb_of_Cores+nb_of_Threads+Processor_Base_Frequency+TDP+Max_Memory_Size+Vertical_Segment+Status,data=CPUs_data)
summary(Model_1)

#Build and summary the Model 2
Model_2<-lm(Recommended_Customer_Price~nb_of_Cores+nb_of_Threads+Processor_Base_Frequency+TDP+Max_Memory_Size+Vertical_Segment+Status,data=CPUs_data) 
summary(Model_2)

#Build and summary the Model 3
Model_3<-lm(Recommended_Customer_Price~nb_of_Cores+nb_of_Threads +TDP+Max_Memory_Size+Vertical_Segment+Status,data=CPUs_data) 
summary(Model_3)

#Build and summary the Model 4
Model_4<-lm(Recommended_Customer_Price~nb_of_Cores+nb_of_Threads +TDP+Max_Memory_Size+Status,data=CPUs_data) 
summary(Model_4)

#Compare model 1 and model 2
anova(Model_1,Model_2)

#Compare model 2 and model 3
anova(Model_2,Model_3)

#Compare model 3 and model 2
anova(Model_3,Model_4)

#Draw residuals plots
plot(Model_3)

##Calculate the predicted value of Recommend Customer Price
new_x = data.frame(CPUs_data[,c(1,2,3,5,6,7,8,9)])
new_x$pred_Price = predict(Model_3, new_x)
head(new_x$pred_Price, 10)

#Two-way anova model
#Check normal distribution
#Check normal distribution for vertical segment variable
#Check the normal distribution for desktop classification
Desktop_data <- subset(CPUs_data,CPUs_data$Vertical_Segment=="Desktop")
qqnorm(Desktop_data$Recommended_Customer_Price)
qqline(Desktop_data$Recommended_Customer_Price)

shapiro.test(Desktop_data$Recommended_Customer_Price)

#Check the normal distribution for mobile classification
Mobile_data <- subset(CPUs_data,CPUs_data$Vertical_Segment=="Mobile")
qqnorm(Mobile_data$Recommended_Customer_Price)
qqline(Mobile_data$Recommended_Customer_Price)

shapiro.test(Mobile_data$Recommended_Customer_Price)

#Check the normal distribution for server classification
Server_data <- subset(CPUs_data,CPUs_data$Vertical_Segment=="Server")
qqnorm(Server_data$Recommended_Customer_Price)
qqline(Server_data$Recommended_Customer_Price)

shapiro.test(Server_data$Recommended_Customer_Price)

#Check the normal distribution for embedded classification
Embedded_data <- subset(CPUs_data,CPUs_data$Vertical_Segment=="Embedded")
qqnorm(Embedded_data$Recommended_Customer_Price)
qqline(Embedded_data$Recommended_Customer_Price)

shapiro.test(Embedded_data$Recommended_Customer_Price)

#Check normal distribution for status variable
#Check the normal distribution for Announced classification
Announced_data <- subset(CPUs_data,CPUs_data$Status=="Announced")

#Check the normal distribution for End_of_Interactive_Support classification
End_of_Interactive_Support_data <- subset(CPUs_data,CPUs_data$Status=="End of Interactive Support")
qqnorm(End_of_Interactive_Support_data$Recommended_Customer_Price)
qqline(End_of_Interactive_Support_data$Recommended_Customer_Price)

shapiro.test(End_of_Interactive_Support_data$Recommended_Customer_Price)

#Check the normal distribution for End of life classification
End_of_Life_data <- subset(CPUs_data,CPUs_data$Status=="End of Life")
qqnorm(End_of_Life_data$Recommended_Customer_Price)
qqline(End_of_Life_data$Recommended_Customer_Price)

shapiro.test(End_of_Life_data$Recommended_Customer_Price)

#Check the normal distribution for Launched classification
Launched_data <- subset(CPUs_data,CPUs_data$Status=="Launched")
qqnorm(Launched_data$Recommended_Customer_Price)
qqline(Launched_data$Recommended_Customer_Price)

shapiro.test(Launched_data$Recommended_Customer_Price)

#Check variance
#Check variance for vertical segment variable
LeveneTest(Recommended_Customer_Price~as.factor(Vertical_Segment),CPUs_data)

#Check variance for status variable
filtered_data <- subset(CPUs_data,CPUs_data$Status %in% c("End of Interactive Support","End of Life","Launched"))
LeveneTest(Recommended_Customer_Price~as.factor(Status),filtered_data)

#Execute anova 2 way model
anova_2_way_model <- aov(Recommended_Customer_Price~Vertical_Segment+Status,data=filtered_data)
summary(anova_2_way_model)

#Multiple comparison
TukeyHSD(anova_2_way_model)
plot(TukeyHSD(anova_2_way_model))