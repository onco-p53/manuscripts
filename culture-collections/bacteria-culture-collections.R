## R Script to process bacterial collection data
# Author: B.S. Weir (2022)

#============Load all the packages ================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(svglite)
library(lubridate)
library(readr)

#============Load data================

#---- ICMP ----#
ICMP.initial.df <- read.csv("icmp.csv", header=TRUE, sep=",")
head(ICMP.initial.df)
tail(ICMP.initial.df)
summary(ICMP.initial.df, maxsum=10)

ICMP.df <- subset(ICMP.initial.df,(SpecimenType == "Bacterial Culture"))
summary(ICMP.df, maxsum=40)

ICMP.df %>% 
  glimpse

#----NRRL ----#
NRRL.df <- read_csv("nrrl.csv", col_types = cols(date_accessioned = col_date(format = "%d/%m/%Y")))

NRRL.df %>% 
  glimpse


#NRRL all
NRRL.all.df <- read_csv("nrrl.all.csv", col_types = cols(date_accessioned = col_date(format = "%d/%m/%Y")))

NRRL.all.df %>% 
  glimpse



#---- DAR ----#

DAR.df <- read.csv("dar.csv", header=TRUE, sep=",")
DAR.df %>% 
  glimpse


#---- CFBP ----#

CFBP.df <- read.csv("cfbp.csv", header=TRUE, sep=",")
CFBP.df %>% 
  glimpse

#============Calculate dates and Normalise column names================

#ICMP.df$date.deposited <- ymd(ICMP.df$DepositedDateISO, truncated = 3)

ICMP.df <- ICMP.df %>%
  mutate(date.deposited = ymd(DepositedDateISO, truncated = 3) )

NRRL.df <- NRRL.df %>%
  mutate(date.deposited = ymd(date_accessioned, truncated = 3) ) %>%
  rename(CurrentName = current_name) %>%
  rename(Country = country)

NRRL.all.df <- NRRL.all.df %>%
  mutate(date.deposited = ymd(date_accessioned, truncated = 3) ) %>%
  rename(CurrentName = current_name) %>%
  rename(Country = country)

#DAR.df$date.deposited <- ymd(DAR.df$Date, truncated = 3)
DAR.df <- DAR.df %>%
  mutate(date.deposited = ymd(Date, truncated = 3) ) %>%
  rename(CurrentName = Species)

CFBP.df <- CFBP.df %>%
  mutate(date.deposited = ymd(Deposit_date, truncated = 3) ) %>%
  rename(CurrentName = Taxonomy.CFBP_Taxonomy) %>%
  rename(Country = Geographic_origin__Country)


#============Data checking================

#all cultures sorted by deposited date
arrange(ICMP.df, date.deposited) %>%
  select("AccessionNumber","CurrentName", "Country", "date.deposited") %>%
  slice_head(n=10)

#all cultures sorted by deposited date
arrange(NRRL.df, date.deposited) %>%
  select("nrrl_no","CurrentName", "Country", "date.deposited") %>%
  slice_head(n=10)

#all cultures sorted by deposited date
arrange(NRRL.all.df, date.deposited) %>%
  select("nrrl_Acc#","CurrentName", "Country", "date.deposited") %>%
  slice_head(n=10)

#all cultures sorted by deposited date
arrange(DAR.df, date.deposited) %>%
  select("Accesion.no.","CurrentName", "Country", "date.deposited") %>%
  slice_head(n=10)

#all cultures sorted by deposited date
arrange(CFBP.df, date.deposited) %>%
  select("Accession_number","CurrentName", "Country", "date.deposited") %>%
  slice_head(n=10)


#============Top species================

#top 30 species
sort(table(ICMP.df$CurrentName),decreasing=TRUE)[1:30] 
sort(table(NRRL.df$CurrentName),decreasing=TRUE)[1:30] 
sort(table(NRRL.all.df$CurrentName),decreasing=TRUE)[1:30] 
sort(table(DAR.df$CurrentName),decreasing=TRUE)[1:30] 
sort(table(CFBP.df$CurrentName),decreasing=TRUE)[1:30] 


#============compute if else================

#host country true false?


#============Combining the data================

#Add a ICMP / NRRL column
ICMP.df$Collection <- "ICMP"
NRRL.df$Collection <- "NRRL"
NRRL.all.df$Collection <- "NRRL"
DAR.df$Collection <- "DAR"

#stack them using bind_rows()

combined.df <- bind_rows(ICMP.df, NRRL.all.df, DAR.df, CFBP.df, .id = "id")
#combined.path.df <- bind_rows(ICMP.df, NRRL.df, DAR.df, .id = "id")

combined.df %>% 
  glimpse

head(combined.df)
tail(combined.df)


#============filtering================

#could filter or mutate into a pathgens column

#still need to verify that dates work

combined.pathogens.df <- combined.df %>% 
  filter(str_detect(CurrentName, "^Pseudomonas") |
           str_detect(CurrentName, "^Xanthomonas") |
           str_detect(CurrentName, "^Pectobacterium") |
           str_detect(CurrentName, "^Acidovorax a") |
           str_detect(CurrentName, "^Agrobacterium tumefaciens") |
           str_detect(CurrentName, "^Agrobacterium larrymoorei") |
           str_detect(CurrentName, "^Agrobacterium rhizogenes") |
           str_detect(CurrentName, "^Agrobacterium rubi") |
           str_detect(CurrentName, "^Agrobacterium vitis") |
           str_detect(CurrentName, "^Brenneria") |
           str_detect(CurrentName, "^Rhizobium tumefaciens") |
           str_detect(CurrentName, "^Rhizobium larrymoorei") |
           str_detect(CurrentName, "^Rhizobium rhizogenes") |
           str_detect(CurrentName, "^Rhizobium rubi") |
           str_detect(CurrentName, "^Rhizobium vitis") |
           str_detect(CurrentName, "^Burkholderia") |
           str_detect(CurrentName, "^Clavibacter") |
           str_detect(CurrentName, "^Curtobacterium") |
           str_detect(CurrentName, "^Corynebacterium") |
           str_detect(CurrentName, "^Dickeya") |
           str_detect(CurrentName, "^Enterobacter") |
           str_detect(CurrentName, "^Erwinia") |
           str_detect(CurrentName, "^Pantoea") |
           str_detect(CurrentName, "^Ralstonia") |
           str_detect(CurrentName, "^Rathayibacter") |
           str_detect(CurrentName, "^Streptomyces ipomoea") |
           str_detect(CurrentName, "^Streptomyces scabiei") |
           str_detect(CurrentName, "^Xylophilus ampelinus") |
           str_detect(CurrentName, "^Xylella")) %>%
  glimpse()





#============Graphics================

#Deposit dates faceted all data binned by year
ggplot(combined.df, aes(date.deposited, fill = Collection)) +
  labs(title = "Deposit dates of bacterial cultures") +
  labs(x = "Date of deposit", y =  "Number of cultures per year" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + 
  facet_grid(Collection ~ . , scales = "free")
ggsave(file='./combined-deposits4.png', width=8, height=6)

#Deposit dates faceted all data binned by 2 years
ggplot(combined.df, aes(date.deposited, fill = Collection)) +
  labs(title = "Deposit dates of bacterial cultures - binned two years") +
  labs(x = "Date of deposit", y =  "Number of cultures per two years" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=730.5, show.legend = FALSE) + 
  facet_grid(Collection ~ . , scales = "free")
ggsave(file='./combined-deposits3-binned2.png', width=8, height=6)

#Deposit dates faceted all data binned by year
ggplot(combined.path.df, aes(date.deposited, fill = Collection)) +
  labs(title = "Deposit dates of bacterial cultures - NRRL pathogens only") +
  labs(x = "Date of deposit", y =  "Number of cultures per year" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + 
  facet_grid(Collection ~ . , scales = "free")
ggsave(file='./combined-deposits3-pathogens.png', width=8, height=6)


#Deposit dates faceted all data binned by year
ggplot(combined.pathogens.df, aes(date.deposited, fill = Collection)) +
  labs(title = "Deposit dates of plant pathogenic bacterial cultures") +
  labs(x = "Date of deposit", y =  "Number of cultures per year" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", limits = c(as.Date("1935-01-01"), NA)) +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + 
  facet_grid(Collection ~ . , scales = "free")
ggsave(file='./combined4-filtered-pathogens.png', width=8, height=7)


#+ scale_x_date(limits = c(as.Date("2006-1-1"), end))
#, limits = c(as.Date("2006-1-1"), end)


#============ Pseudomonas syringae pv. actinidiae case study ================

#subset kiwifruit bacteria
ICMP.kiwifruit.df <- ICMP.df %>% 
  mutate(date.isolated = ymd(IsolationDateISO, truncated = 3) ) %>%
  filter(str_detect(CurrentName, "^Pseudomonas")) %>%
  filter(str_detect(TaxonName_C2, "^Actinidia")) %>%
  filter(Country == "New Zealand") %>%
  glimpse()
    

#graphics
ggplot(ICMP.kiwifruit.df, aes(date.deposited, fill = Country)) +
  labs(title = "Deposits of Pseudomonas ex kiwifruit in New Zealand into the ICMP culture collection") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  #theme(legend.position = c(0.1, 0.7)) +
  theme(legend.position = "none") +
  geom_histogram(binwidth=365.25) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set2")
ggsave(file='ICMP-deposit-dates-kiwifruit.png', width=8, height=5)

#sorting
sort(table(ICMP.kiwifruit.df$date.deposited),decreasing=TRUE)[1:200] #top 20 species


test
