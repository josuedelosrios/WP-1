# library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(stringr)
library(scales)
library(janitor)
library(lubridate)
library(ggpubr)
library(ggpmisc)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(knitr)
library(nlme)
library(lme4)
library(broom)
library(xlsx)
require(mgcv)
library(multcomp)
 library(MuMIn)
library(broom.mixed)
require(mgcv)
library(extrafont)
library(rstatix)
library(patchwork)
list(.packages())


# set working directory ---------------------------------------------------


#setwd()
#list.files()


# data import -------------------------------------------------------------

Cstock <- read.table(file="C stock Lindhof2 613_with_2019.txt",header=TRUE, 
                     sep="\t", dec=".")

Cstock <- Cstock %>% clean_names() %>% as_tibble()

Cstock$datum <-excel_numeric_to_date(Cstock$datum) 


# data transformation: use whatever name you need ------------------------------------------------------

 Cstock <- Cstock %>% 
  mutate(across(c(block, plot, fert, ff_inga, fruchtff, forage_system, jahr), as.factor))

levels(Cstock$fert) <- c("N0", "N1")
levels(Cstock$fruchtff) <- c("ERB-HA-WW", "Grassland", "KG-HA-WW", "KG-M-WW", "Maize")

levels(Cstock$fruchtff) <- c("Cash-crop Rotation", "Permanent Grassland", "Mixed Rotation", "Forage Rotation", "Continuous Maize")
levels(Cstock$fruchtff) <- c("GR", "PG", "MR", "FR", "CM")

#This will reorder the levels of fruchtff to the desire plot order
Cstock$fruchtff <- factor(Cstock$fruchtff, levels = c("PG", "FR", "MR","GR", "CM"))


# Visual outlier detection -------------------------------------------------------

#dev.new()
#outliers <- ggplot(data= Cstock, mapping = aes(x=C_N))
#outliers + geom_histogram(binwidth = 0.1) + facet_grid(fruchtff~fert)
#outliers + geom_density() + facet_grid(fruchtff~fert)

# Exclude outlier in Grassland CN >20
 Cstock <- droplevels(subset( Cstock, c_n <= 20)) 


# Transformations---------------------------------------------------------


#Create new variable and reorder
Cstock<- Cstock %>% 
  mutate(datacomb = paste(Cstock$fruchtff, Cstock$fert, sep =" ", colapse=NULL))

Cstock$datacomb <- as.factor( Cstock$datacomb)

Cstock$jahr2 <- as.factor(Cstock$akk_year)


# Statistical model -------------------------------------------------------

mod0 <- lme(cstock_tha ~ datacomb*akk_year, 
             random=~1|jahr2/block/ff_inga,
             weights = varIdent(form = ~1|datacomb), data = Cstock)

mod1 <- update(mod0, . ~ 0 + datacomb + datacomb:akk_year)
anova(mod1, type="sequential")
MuMIn::r.squaredGLMM(mod1)

mod1b <- update(mod0, . ~ fruchtff*fert*akk_year)
anova(mod1b, type="sequential")
MuMIn::r.squaredGLMM(mod1b)

AIC(mod0,mod1,mod1b)


# Residuals analysis ----------------------------------------------------------


dev.new(); plot(mod1b) 
dev.new();qqnorm(mod1b)


# model estimates ---------------------------------------------------------------

system_estimates <- broom.mixed::tidy(mod1, conf.int=TRUE)
View(system_estimates)


# .fitted and residuals .fixed values
plot_estimates <- broom.mixed::augment(mod1) ;
View(plot_estimates)


# model estimates across N rates 

mod_fert <-  update(mod0, . ~ 0 + fert + fert:akk_year) 
anova(mod_fert, type="marginal")
MuMIn::r.squaredGLMM(mod_fert)

fert_estimates  <- broom.mixed::tidy(mod_fert, conf.int=TRUE)

View(fert_estimates)



# Statistical details-----------------------------------------------

coefficients(mod1)

summary(mod1)$tTable

mod1$contrasts

# Are the different slopes really different from each other 
#(caused by fertilizer, SPLIT for the FF levels)?

coma <- cbind(matrix(0,nrow=5,ncol=10), 
              rbind(
                "240 - 0:ERB-HA-WW"=c(-1,1, 0,0, 0,0, 0,0, 0,0),
                "240 - 0:Grassland"=c( 0,0,-1,1, 0,0, 0,0, 0,0),
                "240 - 0:KG-HA-WW" =c( 0,0, 0,0,-1,1, 0,0, 0,0),
                "240 - 0:KG-M-WW"  =c( 0,0, 0,0, 0,0,-1,1, 0,0),
                "240 - 0:Maize"    =c( 0,0, 0,0, 0,0, 0,0,-1,1)
              )
)
colnames(coma) <- names(coef(mod1))[-length(names(coef(mod1)))]

compFert <- glht(mod1, linfct=coma, df=nrow( Cstock)-2*length(levels( Cstock$datacomb)), 
                 alternative="greater")

summary(compFert)


# Are the different slopes really different from each other 
#(caused by fertilizer, POOLED over the FF levels)?

coma <- cbind(matrix(0,nrow=1,ncol=10), 
              rbind(
                "240 - 0"=c(-1/5,1/5,-1/5,1/5,-1/5,1/5,-1/5,1/5,-1/5,1/5)
              ))
colnames(coma) <- names(coef(mod1))[-length(names(coef(mod1)))]

compFert <- glht(mod1, linfct=coma, df=nrow( Cstock)-2*length(levels( Cstock$datacomb)), 
                 alternative="greater")

summary(compFert)



# Are the different slopes really different from each other (caused by FF)?

ffmat <- contrMat(n=table( Cstock$fruchtff), type="Tukey")
fertmat <- diag(length(levels( Cstock$fert))); colnames(fertmat) <- rownames(fertmat) <- levels( Cstock$fert)

cm <- kronecker(ffmat,fertmat,make.dimnames=TRUE)

coma <- cbind(matrix(0,nrow=nrow(cm),ncol=10), cm)
colnames(coma) <- names(coef(mod1))[-length(names(coef(mod1)))]

compFF <- glht(mod1, linfct=coma, df=nrow( Cstock)-2*length(levels( Cstock$datacomb)))
summary(compFF)


save(Cstock,mod0,mod1,mod1b,system_estimates,plot_estimates, fert_estimates, compFert,compFF,
     file= "LH613 stats output.Rdata")


