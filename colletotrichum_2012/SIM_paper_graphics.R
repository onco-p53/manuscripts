## ---------------------CONIDIA-----------------------------------
conidia <- read.csv("conidia.csv")
conidia$conlength <- (conidia$rawconlength/conidia$mag)
conidia$conwidth <- (conidia$rawconwidth/conidia$mag)


head(conidia)
summary(conidia, maxsum=40)


#ggplot graphics length
attach(conidia) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(conidia, aes(factor(species), conlength)) + opts(title = "Conidial length variation") + labs(x = "species", y =  expression(paste(plain(length)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=16.74, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='conidia-length.eps', width=7, height=7)
ggsave(print_length, file='conidia-length.pdf', width=7, height=7)
ggsave(print_length, file='conidia-length.png', dpi=600, width=6.967, height=4.72)

#ggplot graphics width
attach(conidia) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(conidia, aes(factor(species), conwidth)) + opts(title = "Conidial width variation") + labs(x = "species", y =  expression(paste(plain(width)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=5.104, linetype=2)
p + geom_boxplot()

print_width <- p + geom_boxplot() 
ggsave(print_width, file='conidia-width.eps', width=7, height=7)
ggsave(print_width, file='conidia-width.pdf', width=7, height=7)
ggsave(print_width, file='conidia-width.png', dpi=600, width=6.967, height=4.72)


#printed statistics
conlengthmeans <- tapply(conidia$conlength, conidia$species, mean)
round(conlengthmeans, 1)
conwidthmeans <-tapply(conidia$conwidth, conidia$species, mean)
round(conwidthmeans, 1)
tapply(conidia$conlength, conidia$species, quantile)
tapply(conidia$conwidth, conidia$species, quantile)







## ---------------------CULTURE-----------------------------------

culture <- read.csv("culture.csv")
head(culture)
summary(culture, maxsum=25)


#ggplot graphics
attach(culture) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(culture, aes(factor(species), diameter)) + opts(title = "Culture size at 10 days") + labs(x = "species", y =  "diameter (mm)" , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=61.56, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='culture-size.eps', width=10, height=7)
ggsave(print_length, file='culture-size.eps', width=10, height=7)


## ---------------------ASCOSPORES-----------------------------------


ascospores <- read.csv("ascospores.csv")
ascospores$ascolength <- (ascospores$rawascolength/ascospores$mag)
ascospores$ascowidth <- (ascospores$rawascowidth/ascospores$mag)

head(ascospores)
summary(ascospores, maxsum=40)


#ggplot graphics length
attach(ascospores) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ascospores, aes(factor(species), ascolength)) + opts(title = "Ascospore length variation") + labs(x = "species", y =  expression(paste(plain(length)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=17.46, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot()
ggsave(print_length, file='acsospore-length.eps', width=7, height=7)
ggsave(print_length, file='ascospores-length.pdf', width=10, height=10)

#ggplot graphics width
attach(ascospores) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ascospores, aes(factor(species), ascowidth)) + opts(title = "Ascospore width variation") + labs(x = "species", y =  expression(paste(plain(width)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=4.804, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot()
ggsave(print_width, file='acsospore-width.eps', width=7, height=7)
ggsave(print_width, file='ascospores-width.pdf', width=5, height=5)


#printed statistics
conlengthmeans <- tapply(ascospores$ascolength, ascospores$species, mean)
round(conlengthmeans, 1)
conwidthmeans <-tapply(ascospores$ascowidth, ascospores$species, mean)
round(conwidthmeans, 1)
tapply(ascospores$ascolength, ascospores$species, quantile)
tapply(ascospores$ascowidth, ascospores$species, quantile)



## ---------------------ASCI-----------------------------------

asci <- read.csv("asci.csv")
asci$ascilength <- (asci$rawascilength/asci$mag)
asci$asciwidth <- (asci$rawasciwidth/asci$mag)


head(asci)
summary(asci, maxsum=40)


#ggplot graphics length
attach(asci) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(asci, aes(factor(species), ascilength)) + opts(title = "Asci length variation") + labs(x = "species", y =  expression(paste(plain(length)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=71.7, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='asci-length.pdf', width=10, height=10)

#ggplot graphics width
attach(asci) #this means we dont need the $ sign
require(ggplot2)
p <- ggplot(asci, aes(factor(species), asciwidth)) + opts(title = "Asci width variation") + labs(x = "species", y =  expression(paste(plain(width)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=11.28, linetype=2)
p + geom_boxplot()

print_width <- p + geom_boxplot() 
ggsave(print_width, file='asci-width.pdf', width=10, height=10)


#printed statistics

summary(asci, maxsum=40)
conlengthmeans <- tapply(asci$ascilength, asci$species, mean)
round(conlengthmeans, 1)
conwidthmeans <-tapply(asci$asciwidth, asci$species, mean)
round(conwidthmeans, 1)
tapply(asci$ascilength, asci$species, quantile)
tapply(asci$asciwidth, asci$species, quantile)


## ---------------------APPRESORIA-----------------------------------

appresoria <- read.csv("appresoria.csv")
appresoria$applength <- (appresoria$rawapplength/appresoria$mag)
appresoria$appwidth <- (appresoria$rawappwidth/appresoria$mag)


head(appresoria)
summary(appresoria, maxsum=40)


#ggplot graphics length
attach(appresoria) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(appresoria, aes(factor(species), applength)) + opts(title = "appresoria length variation") + labs(x = "species", y =  expression(paste(plain(length)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=10.58, linetype=2)
p + geom_boxplot()

print_length <- p + geom_boxplot() 
ggsave(print_length, file='appresoria-length.pdf', width=10, height=10)

#ggplot graphics width
attach(appresoria) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(appresoria, aes(factor(species), appwidth)) + opts(title = "appresoria width variation") + labs(x = "species", y =  expression(paste(plain(width)~~(mu * "m"))) , fill = "") 
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
p <- p + geom_hline(yintercept=6.937, linetype=2)
p + geom_boxplot()

print_width <- p + geom_boxplot() 
ggsave(print_width, file='appresoria-width.pdf', width=10, height=10)


#printed statistics

summary(appresoria, maxsum=40)
conlengthmeans <- tapply(appresoria$applength, appresoria$species, mean)
round(conlengthmeans, 1)
conwidthmeans <-tapply(appresoria$appwidth, appresoria$species, mean)
round(conwidthmeans, 1)
tapply(appresoria$applength, appresoria$species, quantile)
tapply(appresoria$appwidth, appresoria$species, quantile)
