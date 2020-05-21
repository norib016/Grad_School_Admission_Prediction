library(ggplot2)

mydata <- read.csv(file="Admission_Predict_Ver1.1.csv", header=TRUE, sep=",")

colnames(mydata)[colnames(mydata)=="GRE.Score"] <- "GRE"
colnames(mydata)[colnames(mydata)=="TOEFL.Score"] <- "TOEFL"
colnames(mydata)[colnames(mydata)=="University.Rating"] <- "UniRating"
colnames(mydata)[colnames(mydata)=="Chance.of.Admit"] <- "AdmitChance"

head(mydata)
hist(mydata$AdmitChance)

hist1 <- ggplot(mydata, aes(x=AdmitChance)) + geom_histogram(color = "white", fill = "steel blue",
                                                           binwidth = 0.05) + labs(title = "Histogram of Admit Chance")
hist1

hist2 <- ggplot(mydata, aes(x=GRE)) + geom_histogram(fill = "steel blue",
                                                           binwidth = 0.5) + labs(title = "Histogram of GRE")
hist2

hist3 <- ggplot(mydata, aes(x = TOEFL)) + geom_histogram(fill = "steel blue",
                                                         binwidth = 0.5) + labs(title = "Histogram of TOEFL")
hist3

hist4 <- ggplot(mydata, aes(x = CGPA)) + geom_histogram(color = "white", fill = "steel blue",
                                                         binwidth = 0.5) + labs(title = "Histogram of CGPA")
hist4

library(gridExtra)
grid.arrange(hist1, hist2, hist3, hist4, nrow = 2)


scatplt1 <- ggplot(mydata, aes(x = CGPA, y = AdmitChance)) + geom_point() + labs(title = "Admit Chance & CGPA Plot")
scatplt1

scatplt2 <- ggplot(mydata, aes(x = GRE, y = AdmitChance)) + geom_point() + labs(title = "Admit Chance & GRE Plot")
scatplt2

scatplt3 <- ggplot(mydata, aes(x = TOEFL, y = AdmitChance)) + geom_point() + labs(title = "Admit Chance & TOEFL Plot")
scatplt3

scatplt4 <- ggplot(mydata, aes(x = LOR, y = AdmitChance)) + geom_point() + labs(title = "Admit Chance & LOR Plot")
scatplt4

grid.arrange(scatplt1, scatplt2, scatplt3, scatplt4)

