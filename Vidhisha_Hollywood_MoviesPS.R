#1) import data- I opened the Rstudio app. From the Environment tab I selected Import Dataset
#From Import Dataset select from Text(base).. select file from the location of the folder it's saved in
#selecet the file HollywoodMostProfitableStories , since the file name is too long I shortened the name to movdf
# I checked the values and selected import

#Load library:

install.packages("tidyverse")

# 2) Import library

library(tidyverse)

# 3) Check data types:

str(movdf)

# dim displays the values of the rows and columns
dim(movdf) 

#4) Check for missing values: It shows there are few missing values  

colSums(is.na(movdf))

# 5) Remove missing values in rows
movdf <- na.omit(movdf)

# 6) Check for duplicates and 

# Remove duplicated rows based on a given column
movdf <-movdf %>% distinct(Film, .keep_all = TRUE)

# dim displays the values of the rows and columns
dim(movdf)

#7) round off values to 2 places ( I see two columns where the decimals can be rounded up to 2decimal places)
# they are the columns 1) Profitability and 2)Worldwide.Gross
# 1) Profitability column= the $ sign is added as a note from which column we need the result from

movdf$Profitability <- round(movdf$Profitability ,digit=2)

#This function helps us see the result if the decimals have been rounded upto 2 decimal places
head(movdf)

# 7)round off values to 2 places:
# 2) Worldwide.Gross column= the $ sign is added as a note from which column we need the result from

movdf$Worldwide.Gross <- round(movdf$Worldwide.Gross ,digit=2)

#This function helps us see the result if the decimals have been rounded upto 2 decimal places
head(movdf)

#Check for outliers using a boxplot
library(ggplot2)

#Create a boxplot that highlights the outliers
ggplot(movdf,aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))

#Remove outliers in 'Profitability'
Q1 <- quantile(movdf$Profitability, .25)
Q3 <- quantile(movdf$Profitability, .75)
IQR <- IQR(movdf$Profitability)

no_outliers <- subset(movdf, movdf$Profitability> (Q1 - 1.5*IQR) & movdf$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

movdf <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))


# dim displays the values of the rows and columns

dim(movdf)

#Do a Summary Statistics/Univariate Analysis using appropriate code.
summary(no_outliers)

summary(movdf)

#bivariate analysis

#scatterplot
ggplot(movdf, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart
ggplot(movdf, aes(x=Year)) + geom_bar()

ggplot(movdf, aes(x=Audience..score..)) + geom_dotplot()

# Box Plot
data(movdf)
boxplot(Profitability ~ Worldwide.Gross, data=movdf, main="Box Plot", xlab="Worldwide.Gross", ylab="Profitability")

#Write a line of code to export the clean data
write.csv(no_outliers, "clean_movdf.csv")




