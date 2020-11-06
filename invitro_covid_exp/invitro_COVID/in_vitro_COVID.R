library(tidyverse)

#load data files
fitc <- read_tsv("tables_flowjo/72hr_dioc.txt")
pe <- read_tsv("tables_flowjo/72hr_mito.txt")

#clean data
fitcCleaned <- fitc %>% 
  slice(-(10:11)) %>% 
  mutate(`Sample:` = str_replace(`Sample:`, 
                                 "DIOC 72 hr Vero Covid_9-11-2021_", 
                                 ""),
         `Sample:` = str_replace(`Sample:`, "_1,3a,", " "),
         `Sample:` = str_replace(`Sample:`, ".fcs", ""),
         `Sample:` = str_replace(`Sample:`, "_[0-9][0-9][0-9]", ""),
         `Sample:` = str_replace(`Sample:`, "_[0-9][0-9][0-9]", "")) %>% 
  rename(Sample = `Sample:`,
         FITC_pos_of_singlets = `Singlets/FITC-A+ | Freq. of Parent`)

peCleaned <- pe %>% 
  slice(-(10:11)) %>% 
  mutate(`Sample:` = str_replace(`Sample:`, 
                                 "MitoSoX 72 hr  Vero  9_11_2020_",
                                 ""),
         `Sample:` = str_replace(`Sample:`, "1,3a,", " "),
         `Sample:` = str_replace(`Sample:`, ".fcs", ""),
         `Sample:` = str_replace(`Sample:`, "_[0-9][0-9][0-9]", ""),
         `Sample:` = str_replace(`Sample:`, "_[0-9][0-9][0-9]", "")) %>% 
  rename(Sample = `Sample:`,
         PE_pos_of_singlets = `Singlets/PE_pos | Freq. of Parent`)

#graph data
fitcCleanedGraph <- fitcCleaned %>% 
  ggplot(aes(y = FITC_pos_of_singlets, x = Sample)) +
  geom_point(color = 'red') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

peCleanedGraph <- peCleaned %>% 
  ggplot(aes(y = PE_pos_of_singlets, x = Sample)) +
  geom_point(color = 'blue') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

gridExtra::grid.arrange(fitcCleanedGraph, peCleanedGraph)  

#statistics

# I don't think a t test across the whole group makes sense since we want to 
# know if each sample is significant???

# for a single t test mu = unstained freq of parent since our null is that an 
# experimental group should be different that the unstained which = ~0
fitcTTest <- t.test(fitcCleaned[2], mu = 0.15)
peTTest <- t.test(peCleaned[2], mu = 0.00544)

wilcox.test(fitcCleaned[2], mu = 0.15)
