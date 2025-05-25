# BrBl vs percentage (Dengler & Dembicz)

library (readxl)
library (vegetarian)
library (tidyverse)
setwd ('c:\\Users\\zeleny\\Dropbox\\editor\\vegsciblog.org\\behind the paper\\20250521_Dembicz-Dengler_VCS\\')

dA <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset A")
dAb <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset A BrBl")
dA <- dA %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
dAb <- dAb %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
plot (dAb ~ dA)

dB <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset B")
dBb <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset B BrBl")
dB <- dB %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
dBb <- dBb %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
plot (dBb ~ dB)

dC <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset C")
dCb <- read_excel(path = "vegetation_classification_and_survey-006-133-s001_DZ.xlsx", sheet = "Dataset C BrBl")
dC <- dC %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
dCb <- dCb %>% as.data.frame %>% select (-1) %>% mutate(across(everything(), ~replace_na(., 0))) %>% as.matrix
plot (dCb ~ dC)

jpeg ('Perc_brbl.jpeg', width = 18, height = 18, units = 'cm', res = 600)
par (mfrow = c(3,3))

# dataset A
ShA <- apply(dA, 2, H, q = 1)
ShAb <- apply (dAb, 2, H, q = 1)
boxplot (ShA, ShAb, notch = T, names = c("%", "BrBl"), main = 'Shannon index')
title ('A', adj = 0)
t.test (ShA, ShAb) # t = -0.51271, df = 61.932, p-value = 0.61
mtext("t = -0.51271, df = 61.932, p-value = 0.61", side = 3, line = 0.5, cex = 0.5)

SEA <- apply (dA, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
SEAb <- apply (dAb, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
boxplot (SEA, SEAb, notch = T, names = c("%", "BrBl"), main = 'Shannon evenness')
title ('A', adj = 0)
t.test (SEA, SEAb) # t = -0.92492, df = 61.335, p-value = 0.3586
mtext("t = -0.92492, df = 61.335, p-value = 0.3586", side = 3, line = 0.5, cex = 0.5)

SiA <- apply (dA, 2, H, q = 2)
SiAb <- apply (dAb, 2, H, q = 2)
boxplot (1-SiA, 1-SiAb, notch = TRUE, names = c("%", "BrBl"), main = 'Simpson index')
title ('A', adj = 0)
t.test (1-SiA, 1-SiAb) # t = -0.47827, df = 61.667, p-value = 0.6341
mtext("t = -0.47827, df = 61.667, p-value = 0.6341", side = 3, line = 0.5, cex = 0.5)

# dataset B
ShB <- apply(dB, 2, H, q = 1)
ShBb <- apply (dBb, 2, H, q = 1)
boxplot (ShB, ShBb, notch = T, names = c("%", "BrBl"), main = 'Shannon index')
title ('B', adj = 0)
t.test (ShB, ShBb) # t = -0.40566, df = 338.32, p-value = 0.6852
mtext("t = -0.40566, df = 338.32, p-value = 0.6852", side = 3, line = 0.5, cex = 0.5)

SEB <- apply (dB, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
SEBb <- apply (dBb, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
boxplot (SEB, SEBb, notch = T, names = c("%", "BrBl"), main = 'Shannon evenness')
title ('B', adj = 0)
t.test (SEB, SEBb) # t = -0.51877, df = 334.78, p-value = 0.6043
mtext("t = -0.51877, df = 334.78, p-value = 0.6043", side = 3, line = 0.5, cex = 0.5)

SiB <- apply (dB, 2, H, q = 2)
SiBb <- apply (dBb, 2, H, q = 2)
boxplot (1-SiB, 1-SiBb, notch = TRUE, names = c("%", "BrBl"), main = 'Simpson index')
title ('B', adj = 0)
t.test (1-SiB, 1-SiBb) # t = -0.79736, df = 337.75, p-value = 0.4258
mtext("t = -0.79736, df = 337.75, p-value = 0.4258", side = 3, line = 0.5, cex = 0.5)

# dataset C
ShC <- apply(dC, 2, H, q = 1)
ShCb <- apply (dCb, 2, H, q = 1)
boxplot (ShC, ShCb, notch = T, names = c("%", "BrBl"), main = 'Shannon index')
title ('C', adj = 0)
t.test (ShC, ShCb) # c
mtext("t = -0.39513, df = 117.76, p-value = 0.6935", side = 3, line = 0.5, cex = 0.5)

SEC <- apply (dC, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
SECb <- apply (dCb, 2, FUN = function (x) H(x, q = 1)/log (sum (x>0)))
boxplot (SEC, SECb, notch = T, names = c("%", "BrBl"), main = 'Shannon evenness')
title ('C', adj = 0)
t.test (SEC, SECb) # t = -0.62031, df = 113.36, p-value = 0.5363
mtext("t = -0.62031, df = 113.36, p-value = 0.5363", side = 3, line = 0.5, cex = 0.5)

SiC <- apply (dC, 2, H, q = 2)
SiCb <- apply (dCb, 2, H, q = 2)
boxplot (1-SiC, 1-SiCb, notch = TRUE, names = c("%", "BrBl"), main = 'Simpson index')
title ('C', adj = 0)
t.test (1-SiC, 1-SiCb) # t = -0.21082, df = 117.65, p-value = 0.8334
mtext("t = -0.21082, df = 117.65, p-value = 0.8334", side = 3, line = 0.5, cex = 0.5)

dev.off ()
