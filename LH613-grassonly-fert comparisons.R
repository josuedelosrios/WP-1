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
require(mPGv)
library(multcomp)
library(piecewiseSEM); library(MuMIn)
library(broom.mixed)
require(mgcv)
library(extrafont)
library(rstatix)
library(patchwork)
list(.packages())

################Important note###################

######Keep this file as it is, without changes###

######05/11/2013 should be removed (forget it, keep both measurements of 2013)


# set working directory ---------------------------------------------------


list.files("Z:\\Documents\\Long-term field data carbon\\Soil C Lindhof")
setwd("Z:\\Documents\\Long-term field data carbon\\Soil C Lindhof")
getwd()


# data import -------------------------------------------------------------



## #F:\Mario analysis\SOC stock_29.09.2020

Cstock_grassonly <- read.table(file="C stock Lindhof2 613_with_2019-with double fert.txt",header=TRUE, sep="\t", dec=".", stringsAsFactors=FALSE)


Cstock_grassonly <- Cstock_grassonly %>% clean_names() %>% as_tibble() ; Cstock_grassonly

Cstock_grassonly$datum<- excel_numeric_to_date(Cstock_grassonly$datum)

# data transformation ------------------------------------------------------

Cstock_grassonly$datum <- as_date(Cstock_grassonly$datum); Cstock_grassonly

Cstock_grassonly$month <- month(Cstock_grassonly$datum)

Cstock_grassonly$jahr_month <- paste(Cstock_grassonly$jahr, Cstock_grassonly$month, sep = "_")


#Cstock_grassonly <- Cstock_grassonly %>%  filter(jahr <= 2019) 


Cstock_grassonly <- Cstock_grassonly %>% 
  mutate(across(c(block, plot, fert, ff_inga, fruchtff, jahr, jahr_month, forage_system), as.factor)) %>% 
  rename(c_tm_mean = perc_c_tm)

levels(Cstock_grassonly $fert) <- c("N0", "N1", "N2")
levels(Cstock_grassonly $fruchtff) <- c("GR", "PG", "MR", "FR", "CM")


Cstock_grassonly <- Cstock_grassonly %>% filter(fruchtff == "PG") %>%  filter (c_n < 20)

PG_dataset <- Cstock_grassonly

PG_dataset %>% map(levels)



# figures: trendlines -----------------------------------------------------



#bp
PG_dataset_bp <-  ggplot(PG_dataset, aes(x=as.factor(jahr_month), y=c_tm_mean)) + #x=as.factor(jahr)
  geom_boxplot(outlier.colour = "dark green", outlier.shape = 3,outlier.fill = "green") +
  geom_text(aes(label=paste(plot, jahr, sep = "_")), size=2,check_overlap=TRUE) + 
  geom_point(aes(col=plot),  alpha=0.5, position = position_jitterdodge()) + 
  #geom_smooth(aes(x= jahr, y=c_tm_mean, group = plot)) + 
  facet_grid(.~fert) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, hjust=1)) +  
  labs(x= "year", y= "Corg in TM (%)", title = "PG dataset - bloxplot")



# dynamic
PG_dataset_dynamic <-  ggplot(PG_dataset, aes(x=datum, y=c_tm_mean, group=plot)) +
  geom_line() + 
  geom_text(aes(label=paste(plot, jahr, sep = "_")), size=2,check_overlap=TRUE) + 
  geom_point(aes(col=plot),  alpha=0.5, position = position_jitterdodge()) + 
  facet_grid(.~fert) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, hjust=1)) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x= "year", y= "Corg in TM (%)", title = "PG dataset - dynamic")

# smoothing
PG_dataset_smooth<-  ggplot(PG_dataset, aes(x=datum, y=c_tm_mean, group=plot)) +
  geom_smooth(se=FALSE) +
  geom_text(aes(label=paste(plot, jahr, sep = "_")), size=2,check_overlap=TRUE) + 
  geom_point(aes(col=plot),  alpha=0.5, position = position_jitterdodge()) + 
  facet_grid(.~fert) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, hjust=1)) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x= "year", y= "Corg in TM (%)", title = "PG dataset - smoothing")


# regressions
PG_dataset_reg<-  ggplot(PG_dataset, aes(x=datum, y=c_tm_mean, group=plot)) +
  geom_smooth(se=FALSE, method ="lm") +
  geom_text(aes(label=paste(plot, jahr, sep = "_")), size=2,check_overlap=TRUE) + 
  geom_point(aes(col=plot),  alpha=0.5, position = position_jitterdodge()) + 
  facet_grid(.~fert) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, hjust=1)) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x= "year", y= "Corg in TM (%)", title = "PG dataset - regression")



# regressions combined
PG_dataset_reg_combined<-  ggplot(PG_dataset, aes(x=datum, y=c_tm_mean)) +
  geom_smooth(se=FALSE, method ="lm") +
  geom_text(aes(label=paste(plot, jahr, sep = "_")), size=2,check_overlap=TRUE) + 
  geom_point(aes(col=plot),  alpha=0.5, position = position_jitterdodge()) + 
  facet_grid(.~fert) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, hjust=1)) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x= "year", y= "Corg in TM (%)", title = "PG dataset - regression combined")

dev.new(); PG_dataset_bp 
dev.new(); PG_dataset_dynamic
dev.new(); PG_dataset_reg  
dev.new(); PG_dataset_reg_combined
dev.new(); PG_dataset_smooth    

# save --------------------------------------------------------------------

My_plots <- list( 
                  PG_dataset_bp, PG_dataset_dynamic,  PG_dataset_smooth,  PG_dataset_reg,  PG_dataset_reg_combined)


pdf(file = "LH613 Grassland SOC figures report-N rates comparisons.pdf",
    width =30, height = 20, 
    bg = "white",
    paper= "a4r")
print(My_plots)
dev.off()





# Save and use the ready data ---------------------------------------------




save(PG_dataset, file= "LH613 SOC grassland-fert comparisons.rdata")




# Statistical model -------------------------------------------------------

load(file= "LH613 SOC grassland-fert comparisons.rdata")

PG_dataset_stats<- PG_dataset %>%
  mutate(datacomb = paste(PG_dataset$fruchtff, PG_dataset$fert, sep =" ", colapse=NULL))

PG_dataset_stats$datacomb <- as.factor(PG_dataset_stats$datacomb)

#check data
PG_dataset_stats

PG_dataset_stats$Jahr <- as.factor(PG_dataset_stats$akk_year) 


mod0 <- lme(cstock_tha ~ datacomb*akk_year, 
            random=~1|Jahr/block/ff_inga,
            weights = varIdent(form = ~1|datacomb), data =PG_dataset_stats)



mod1 <- update(mod0, . ~ 0 + fert + fert:akk_year)
anova(mod1, type="marginal")
MuMIn::r.squaredGLMM(mod1)

mod1b <- update(mod0, . ~ fert*akk_year)
anova(mod1b, type="marginal")
MuMIn::r.squaredGLMM(mod1b)

AIC(mod0,mod1,mod1b)


# #See residuals ----------------------------------------------------------

dev.new(); plot(mod1b) 
dev.new();qqnorm(mod1b)





# See estimates -----------------------------------------------------------

syst_estimates <- broom.mixed::tidy(mod1, conf.int=TRUE)     #Jump to figure 2 to work by system   

View(syst_estimates)


plot_estimates1 <- broom.mixed::augment(mod1) ; 
View(plot_estimates1)

plot_estimates2 <- broom.mixed::augment(mod1b)  #Use once only and store it, very consuming

View(plot_estimates2)



