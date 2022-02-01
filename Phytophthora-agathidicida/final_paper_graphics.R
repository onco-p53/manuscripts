#==  Graphics for radial growth rate == Fig. X

culture  <- read.csv("culture.csv")
head(culture)
summary(culture, maxsum=40)

cbPalette4 <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73") # 4 best colours
# NB: this is 213,94,0  230,159,0   86,180,233  0,158,155

#box plot
attach(culture)
require(ggplot2)
rg <- ggplot(culture, aes(factor(species), radius)) + labs(x = "Species", y =  "Culture radius at 7 days (mm)" , fill = "")
rg <- rg + theme(axis.text.x=element_text(angle=-90, hjust=0)) 
rg <- rg + theme(legend.position="none")
rg <- rg + geom_boxplot(aes(fill = factor(species)))
rg <- rg + scale_fill_manual(values=cbPalette4)
# rg <- rg + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
rg + facet_grid(. ~ media)
facet.media <- rg + facet_grid(. ~ media) 
ggsave(facet.media, file='radial-growth.png', width=15.92, height=10.61, units="cm") # good size for A4
ggsave(facet.media, file='radial-growth.pdf', width=15.92, height=10.61, units="cm") # good size for A4




#box plot updated code # not working yet, actually the above code is better for testing because you can see which line errors
attach(culture)
require(ggplot2)
rg2 <- ggplot(culture, aes(factor(species), radius))
rg2 + geom_boxplot(aes(fill = factor(species))) +
	labs(title = "Radial growth rate at 7 days") + labs(x = "Species", y =  "Radius of culture (mm)" , fill = "") +
	opts(axis.text.x=theme_text(angle=-90, hjust=0)) +
	scale_fill_manual(values=cbPalette4) +
	stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
rg2 + facet_grid(. ~ media)
ggsave("radial-growth2.pdf", width=10, height=7, units="cm")




#statistics

cma <- subset(culture, media=="CMA") #subset by media
radiusmeans.cma <- tapply(cma$radius, cma$species, mean)
round(radiusmeans.cma, 1)
tapply(cma$radius, cma$species, quantile)

mea <- subset(culture, media=="MEA") #subset by media
radiusmeans.mea <- tapply(mea$radius, mea$species, mean)
round(radiusmeans.mea, 1)
tapply(mea$radius, mea$species, quantile)

pda <- subset(culture, media=="PDA") #subset by media
radiusmeans.pda <- tapply(pda$radius, pda$species, mean)
round(radiusmeans.pda, 1)
tapply(pda$radius, pda$species, quantile)

v8a <- subset(culture, media=="V8A") #subset by media
radiusmeans.v8a <- tapply(v8a$radius, v8a$species, mean)
round(radiusmeans.v8a, 1)
tapply(v8a$radius, v8a$species, quantile)


#=========  Graphics for Cardinal growth == Fig. X

temp <- read.csv("simple_temp2.csv")
head(temp)
summary(temp, maxsum=40)

cbPalette4 <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73") # 4 best colours
# NB: this is 213,94,0  230,159,0   86,180,233  0,158,155

#quick check of points
require(ggplot2)
p <- ggplot(temp, aes(temperature, value)) 
p + geom_point(aes(colour = species))

#cardinal temperature graph with lines
p3 <- ggplot() +
  geom_point(data = temp, mapping = aes(x = temperature, y = value7, colour = species), size = 1) +
  geom_line(data = temp, mapping = aes(x = temperature, y  = value7, colour = species), method = loess, size=1) +
  scale_x_continuous(breaks=seq(0,35,2)) + #edit this for axis ticks
  scale_colour_manual(values=cbPalette4)+
  ylim(0,6.5) +
  labs(x = "Temperature (dC)", y = "Radial growth (mm/day)") +
  theme_bw()
p3
ggsave(p3, file='cardinal-temp.pdf', width=15.92, height=10.61, units="cm")
ggsave(p3, file='cardinal-temp.png', width=15.92, height=10.61, units="cm")

# a faceted version
p4 <- p3 + 
   facet_wrap(~species)
p4


plot(temp$temperture, temp$value7, type="0")


p4 <- ggplot() +
  geom_point(data = temp, mapping = aes(x = temperature, y = value7, colour = species), size = 1) +
  geom_line(data = temp, mapping = aes(x = temperature, y  = value7, colour = species), size=1) +
  scale_x_continuous(breaks=seq(0,35,2)) + #edit this for axis ticks
  scale_colour_manual(values=cbPalette4)+
  ylim(0,6.5) +
  labs(x = "Temperature (dC)", y = "Radial growth (mm/day)") +
  theme_bw()
p4

#========  Graphics for oogonium == Fig. X


oogonium <- read.csv("oogonium.csv")
head(oogonium)
summary(oogonium, maxsum=40)

# normal box plot graphics
attach(oogonium)
require(ggplot2)
osw <- ggplot(oogonium, aes(factor(species), oospore.width)) + labs(title = "Oospore width") + labs(x = "Species", y =  "diameter (um)" , fill = "") 
osw <- osw + geom_hline(yintercept=mean(oogonium$oospore.width, na.rm=T), linetype=2)
osw + geom_boxplot()
p.oospore.width <- osw + geom_boxplot() 
ggsave(p.oospore.width, file='oospore.width.pdf', width=10, height=7)

attach(oogonium)
require(ggplot2)
ogw <- ggplot(oogonium, aes(factor(species), oogonium.width)) + labs(title = "Oogonium width") + labs(x = "Species", y =  "diameter (um)" , fill = "") 
ogw <- ogw + geom_hline(yintercept=mean(oogonium$oogonium.width, na.rm=T), linetype=2)
ogw + geom_boxplot()
p.oogonium.width <- ogw + geom_boxplot() 
ggsave(p.oogonium.width, file='oogonium.width.pdf', width=10, height=7)

#calculate the means
library(plyr)
osw.sp.mean <- ddply(oogonium, .(species), summarise, dia.mean=mean(oospore.width, na.rm=T))
ogw.sp.mean <- ddply(oogonium, .(species), summarise, dia.mean=mean(oogonium.width, na.rm=T))
#print them
osw.sp.mean
ogw.sp.mean

#Final faceting oogonium.width
attach(oogonium)
require(ggplot2)
cz <- ggplot(oogonium, aes(x=oogonium.width)) + labs(title = "Oogonium width") + labs(x = "diameter (um)", y =  "density" , fill = "") 
cz <- cz + geom_vline(data=ogw.sp.mean, aes(xintercept=dia.mean), linetype=2, colour="red")
cz + geom_density(alpha=.3)+ facet_grid(species ~ .)
cz.print <- cz + geom_density(alpha=.3)+ facet_grid(species ~ .)
ggsave(cz.print, file='oogonium.width.density.plot.pdf', width=15.92, height=10.61, units="cm")
ggsave(cz.print, file='oogonium.width.density.plot.png', width=15.92, height=10.61, units="cm")


#Final faceting oospore.width
attach(oogonium)
require(ggplot2)
cosz <- ggplot(oogonium, aes(x=oospore.width)) + labs(title = "Oospore width") + labs(x = "diameter (um)", y =  "density" , fill = "") 
cosz <- cosz + geom_vline(data=osw.sp.mean, aes(xintercept=dia.mean), linetype=2, colour="red")
cosz + geom_density(alpha=.3)+ facet_grid(species ~ .)
cosz.print <- cosz + geom_density(alpha=.3)+ facet_grid(species ~ .)
ggsave(cosz.print, file='oospore.width.density.plot.pdf', width=15.92, height=15.92, units="cm")
ggsave(cosz.print, file='oospore.width.density.plot.png', width=15.92, height=15.92, units="cm")

#Violin plot!
vi <- ggplot(oogonium, aes(factor(species), oospore.width))
vi <- vi + scale_fill_manual(values=cbPalette4)
vi <- vi + geom_boxplot(width=.1, fill="black", outlier.colour=NA)
vi <- vi + stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
vi + geom_violin(aes(fill = factor(species), colour = factor(species)))
vi.print <- vi + geom_violin(aes(fill = factor(species), colour = factor(species)))
ggsave(vi.print, file='oospore.violin.plot.pdf', width=7, height=7.5)


#Violin plot! - this version with tidier code and centre bars
vii <- ggplot(oogonium, aes(factor(species), oospore.width))
vii + geom_violin(aes(fill = factor(species), colour = factor(species)),adjust = 1.4) + 
	labs(title = "Oospore width variation") + 
	labs(x = "species", y = "oospore width (um)") +
      theme(legend.position = 'none') +
      scale_fill_manual(values=cbPalette4) +
	scale_colour_manual(values=cbPalette4) +
      geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
      stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)+
	geom_hline(yintercept=23.89, linetype=2)
ggsave("oospore.violin.plot3.pdf", width=15, height=15, units="cm")
ggsave("oospore.violin.plot3.png", width=15, height=15, units="cm")


#====   T-test staticstics (oogonium)   ==============================================

# independent 2-group t-test. Format is " t.test(y~x) " where y is numeric and x is a binary factor 
# so subset the data to have just P. katsurae 2 and P. katsurae ss
head(oogonium) #reminder of what the dataframe looks like

head(pta.v.heveae) # this checks that the subsetting works correctly
#then:
pta.v.heveae <- subset(oogonium, species=="P. agathidicida" | species=="P. heveae")
t.test(pta.v.heveae$oospore.width~pta.v.heveae$species)

pta.v.katsurae <- subset(oogonium, species=="P. agathidicida" | species=="P. castaneae")
t.test(pta.v.katsurae$oospore.width~pta.v.katsurae$species)

pta.v.katsurae.cocos <- subset(oogonium, species=="P. agathidicida" | species=="P. cocois")
t.test(pta.v.katsurae.cocos$oospore.width~pta.v.katsurae.cocos$species)

#these two are really close should be much greater
heveae.v.katsurae.cocos <- subset(oogonium, species=="P. heveae" | species=="P. cocois")
t.test(heveae.v.katsurae.cocos$oospore.width~heveae.v.katsurae.cocos$species)
#yep, not significant. p-value = 0.01047




heveae.v.cocos <- subset(oogonium, species=="P. heveae" | species=="P. cocois")
t.test(heveae.v.cocos$oospore.width~heveae.v.cocos$species)
#yep, not significant. p-value = 0.01047



#============== Oogonia and oospore statistics data ============


oogonium <- read.csv("oogonium.csv")
head(oogonium)
summary(oogonium, maxsum=40)

#printed statistics
oosporelengthmeans <- tapply(oogonium$oospore.width, oogonium$species, mean)
round(oosporelengthmeans, 1)
oogoniumlengthmeans <- tapply(oogonium$oogonium.width, oogonium$species, mean)
round(oogoniumlengthmeans, 1)
tapply(oogonium$oospore.width, oogonium$species, quantile)
tapply(oogonium$oogonium.width, oogonium$species, quantile)


#============== antheridium statistics data ============


antheridium <- read.csv("antheridium.csv")
head(antheridium)
summary(antheridium , maxsum=40)

#printed statistics
antheridiumlengthmeans <- tapply(antheridium$length, antheridium$species, mean)
round(antheridiumlengthmeans, 1)
antheridiumwidthmeans <- tapply(antheridium$width, antheridium$species, mean)
round(antheridiumwidthmeans, 1)

tapply(antheridium$length, antheridium$species, quantile)
tapply(antheridium$width, antheridium$species, quantile)


#============== sporangia statistics data ============


sporangia <- read.csv("sporangia.csv")
head(sporangia)
summary(sporangia, maxsum=40)

#printed statistics

sporangialengthmeans <- tapply(sporangia$length, sporangia$species, mean)
round(sporangialengthmeans, 1)

sporangiawidthmeans <- tapply(sporangia$width, sporangia$species, mean)
round(sporangiawidthmeans, 1)

sporangiaporemeans <- tapply(sporangia$pore, sporangia$species, mean)
round(sporangiaporemeans, 1)

tapply(sporangia$length, sporangia$species, quantile)
tapply(sporangia$width, sporangia$species, quantile)
tapply(sporangia$pore, sporangia$species, quantile)


# normal box plot graphics
attach(sporangia)
require(ggplot2)
sw <- ggplot(sporangia, aes(factor(species), width)) + labs(title = "sporangia width") + labs(x = "Species", y =  "diameter (um)" , fill = "") 
sw <- sw + geom_hline(yintercept=mean(sporangia$width, na.rm=T), linetype=2)
sw + geom_boxplot()
p.sporangia.width <- sw + geom_boxplot() 
ggsave(p.sporangia.width, file='oospore.width.pdf', width=10, height=7)

attach(sporangia)
require(ggplot2)
sl <- ggplot(sporangia, aes(factor(species), length)) + labs(title = "sporangia length") + labs(x = "Species", y =  "diameter (um)" , fill = "") 
sl <- sl + geom_hline(yintercept=mean(sporangia$length, na.rm=T), linetype=2)
sl + geom_boxplot()
p.sporangia.length<- sl + geom_boxplot() 
ggsave(p.sporangia.length, file='oospore.length.pdf', width=10, height=7)

attach(sporangia)
require(ggplot2)
sp <- ggplot(sporangia, aes(factor(species), pore)) + labs(title = "sporangia pore") + labs(x = "Species", y =  "diameter (um)" , fill = "") 
sp <- osw + geom_hline(yintercept=mean(sporangia$pore, na.rm=T), linetype=2)
sp + geom_boxplot()
p.sporangia.pore <- sp + geom_boxplot() 
ggsave(p.sporangia.pore, file='oospore.pore.pdf', width=10, height=7)



