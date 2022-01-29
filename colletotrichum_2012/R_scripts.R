## ---------------------CONIDIA-----------------------------------
conidia <- read.csv("conidia.csv")
conidia$conlength <- (conidia$rawconlength/conidia$mag)
conidia$conwidth <- (conidia$rawconwidth/conidia$mag)
conidia$volume <- (conidia$conlength*conidia$conwidth)
conidia$ratio <- (conidia$conlength/conidia$conwidth)
length <-conidia$conlength

head(conidia)
summary(conidia, maxsum=40)

plot(conidia$isolate, conidia$conlength)
plot(conidia$species, conidia$conlength)
plot(conidia$species, conidia$conwidth)
plot(conidia$species, conidia$volume)
plot(conidia$species, conidia$ratio)
abline(h=86.31,lty=2,col="red")
plot(conidia$conwidth, conidia$conlength)

plot(conidia$species, conidia$conlength, main="Conidial length variation", xlab="Species", ylab="Length")


pdf("conidia-length.pdf")
par(mar = c(12, 4, 4, 2) + 0.1)
par(las = 2)
plot(conidia$species, conidia$conlength, main="Conidial length variation", xlab="Species", ylab="Length")
abline(h=16.68,lty=2,col="red")
dev.off()

plot(conidia$species, conidia$conwidth)
abline(h=5.2,lty=2,col="red")

#ggplot version
attach(conidia) #this means we dont need the $ sign
require(ggplot2)
p <- ggplot(conidia, aes(factor(species), conlength)) + opts(title = "Conidial length variation") + labs(x = "taxon", y =  expression(paste(plain(length)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=16.77, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='condia-length2.pdf', width=10, height=7)


##=========individual stats========
conlengthmeans <- tapply(conidia$conlength, conidia$species, mean)
round(conlengthmeans, 1)
conwidthmeans <-tapply(conidia$conwidth, conidia$species, mean)
round(conwidthmeans, 1)
tapply(conidia$conlength, conidia$species, range)
tapply(conidia$conlength, conidia$species, quantile)
tapply(conidia$conwidth, conidia$species, range)
tapply(conidia$conwidth, conidia$species, quantile)

## ---------------------CULTURE-----------------------------------

culture <- read.csv("culture.csv")
head(culture)
summary(culture, maxsum=25)
plot(culture$isolate, culture$diameter)
par(las = 2)
plot(culture$species, culture$diameter, main="Culture size at 10 days", ylab="diameter (mm)")
abline(h=64.99,lty=2,col="blue")

## Increase bottom margin to make room for rotated labels
par(mar = c(7, 4, 4, 2) + 0.1)

#print the culture size graph
pdf("culture-size.pdf")
par(mar = c(7, 4, 4, 2) + 0.1)
par(las = 2)
plot(culture$species, culture$diameter, main="Culture size at 10 days", ylab="diameter (mm)")
abline(h=65.13,lty=2,col="blue")
dev.off()

#ggplot version
attach(culture) #this means we dont need the $ sign
require(ggplot2)
p <- ggplot(culture, aes(factor(species), diameter)) + opts(title = "Culture size at 10 days") + labs(x = "species", y =  "diameter (mm)" , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=61.56, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='culture-size.pdf', width=10, height=7)

## ---------------------Appressoria-----------------------------------

appressoria <- read.csv("appressoria.csv")
appressoria$applength <- (appressoria$rawapplength/appressoria$mag)
appressoria$appwidth <- (appressoria$rawappwidth/appressoria$mag)
appressoria$appratio <- (appressoria$applength/appressoria$appwidth)
appressoria$appproduct <- (appressoria$applength*appressoria$appwidth)

head(appressoria)
summary(appressoria)

plot(appressoria$species, appressoria$appratio)
plot(appressoria$species, appressoria$appproduct)
plot(appressoria$appwidth, appressoria$applength)

## ---------------------PATHOLOGY-----------------------------------
pathology <- read.csv("PathologyR.csv")
head(pathology)
summary(pathology)

#print the culture size graph
pdf("lesion-size.pdf", width=10, height=5)
par(mar = c(7, 4, 4, 2) + 0.1) #increases bottom margin
par(las = 2) #rotates labels
par(mfrow=c(1,2)) #two plots side by side
plot(pathology$isolate, pathology$size, main="lesion size at 10 days, on tea", ylab="diameter (mm)", xlab="isolate")
abline(h=7.984,lty=2,col="red")
plot(pathology$host, pathology$size, main="lesion size at 10 days, on tea", ylab="diameter (mm)", xlab="host")
abline(h=7.984,lty=2,col="red")
dev.off()

##--------PATHOLOGY 2 GGPLOT------------------------------
#need to split by the factor "plant"
pathology <- read.csv("PathologyR3.csv")
pathology$mean <- (pathology$length+pathology$width)/2

head(pathology)
summary(pathology)
#th below are quick tests
plot(pathology$isolate, pathology$length)
plot(pathology$isolate, pathology$mean) #looks like the best
plot(pathology$ID, pathology$mean)
plot(pathology$plant, pathology$mean)

#ggplot
attach(pathology) #this means we dont need the $ sign
require(ggplot2)
p <- ggplot(pathology, aes(factor(isolate), mean)) 
p + geom_boxplot()
p + geom_boxplot(aes(fill = factor(plant)))
q <- ggplot(pathology, aes(factor(ID), mean))
q + geom_boxplot(aes(fill = factor(plant)))

 

#final code for printing nice graphs
attach(pathology) #this means we dont need the $ sign
require(ggplot2)
p <- ggplot(pathology, aes(factor(isolate), mean)) + opts(title = "Lesion size at 15 days") + labs(x = "Isolate", y = "mean lesion diameter (mm)", fill = "")  
p + geom_boxplot(aes(fill = factor(plant)))

q <- ggplot(pathology, aes(factor(ID), mean)) + opts(title = "Lesion size at 15 days") + labs(x = "ID of isolate", y = "mean lesion diameter (mm)", fill = "")  
q + geom_boxplot(aes(fill = factor(plant)))


byisolate <- p + geom_boxplot(aes(fill = factor(plant))) 
ggsave(byisolate, file='byisolate.pdf', width=10, height=7) 

byID <- q + geom_boxplot(aes(fill = factor(plant))) 
ggsave(byID, file='byIDe.pdf', width=10, height=10)


#----testing-------


