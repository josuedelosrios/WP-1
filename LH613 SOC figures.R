
# Figure for the different systems and N rates ------------------------------------------------------------------

#If you need to see 
shell.exec("LH613 SOC statistics.R")

#Load output
load("LH613 stats output.Rdata")

#Reorder the levels of fruchtff to the desire order
Cstock$fruchtff <- factor(Cstock$fruchtff,  levels = c("CM", "GR", "MR","FR", "PG")) 


#fixed vs fitted
#choose fixed for regression estimates (stat_poly_eq: intercept and slopes) and fitted for points (geom_point)

plot_estimates <- plot_estimates %>%
  dplyr::select(-c(diff_days, diff_year, intercept_11_09_2020, intercept_30_11_2020)) %>%
  dplyr::select(datum, block, plot, datacomb, fruchtff, ff_inga, forage_system, fert, akk_year,
                everything())

Cstock <- Cstock %>% 
  mutate(across(c(block, plot, fert, ff_inga, fruchtff, forage_system, jahr), as.factor))

#check
plot_estimates %>% map(levels)

plot_estimates$jahr <- as.character((plot_estimates$jahr))
plot_estimates$jahr<- as.numeric((plot_estimates$jahr))
plot_estimates$jahr<- as.factor((plot_estimates$jahr))
plot_estimates$fert <- as.factor(plot_estimates$fert)
plot_estimates$jahr <- as.character((plot_estimates$jahr))
plot_estimates$jahr <- as.numeric((plot_estimates$jahr))
plot_estimates$plot <- as.factor((plot_estimates$plot))
plot_estimates$block <- as.factor((plot_estimates$block))


#reorder for figure
plot_estimates$fruchtff <- factor(plot_estimates$fruchtff, 
                                  levels = c("CM",   "GR",  "FR",   "MR",  "PG"))


plot_estimates_summary <- plot_estimates %>%
  group_by(datacomb, fruchtff, fert, jahr, akk_year, datum, akk_days) %>% 
  summarise(C_stock_mean = mean(cstock_tha, na.rm=TRUE),
            C_stock_mean_fitted = mean(.fitted, na.rm=TRUE),
            C_stock_mean_fixed  = mean(.fixed, na.rm=TRUE)) %>% ungroup()

View(plot_estimates_summary)



# 1st figure --------------------------------------------------------------


my.formula.linear <- y ~ x

y <- ggplot(plot_estimates_summary, aes(x = akk_year, linetype=fert, shape=fert)) #specify x and y in the aes()

system_estimates_plots <- y + 
 
  geom_point(aes(y=C_stock_mean_fitted), size=2) + 
  geom_smooth(aes(y=C_stock_mean_fixed), method= lm,  na.rm = TRUE, se=FALSE, color="black") +
  facet_grid(.~fruchtff) +
  scale_shape_manual(name= "N rate", values = c(1,2)) +
  theme_classic( base_family = "Times New Roman", base_size=16) + 
  scale_x_continuous(breaks=c(0,2, 4, 6, 8, 10)) +
  labs(linetype= "N rate", shape="N rate", x = "Time (year)", y= bquote('SOC stock ('*' Mg' ~ ha^-1*')'))

dev.new() ; system_estimates_plots 



# 2nd figure, by system -----------------------------------------------------

system_estimates2 <- system_estimates %>%  mutate( grepl(':akk_year', term))


syst_est_param <- system_estimates2 %>%  mutate(param_est = case_when(
  str_detect(term, ':akk_year') == TRUE ~ "slope", 
  str_detect(term, ':akk_year') == FALSE ~ "intercept")
) %>% mutate(system = case_when(
  str_detect(term, 'datacombCM') == TRUE ~ "CM", 
  str_detect(term, 'datacombGR ') == TRUE ~ "GR",
  str_detect(term, 'datacombFR ') == TRUE ~ "FR", 
  str_detect(term, 'datacombMR ') == TRUE ~ "MR",
  str_detect(term, 'datacombPG ') == TRUE ~ "PG")
) %>% mutate(Nrate = case_when(
  str_detect(term, 'N0') == TRUE ~ "N0", 
  str_detect(term, 'N1') == TRUE ~ "N1")) #%>%  filter(param_est =="slope")

syst_est_param$annotation <- paste(syst_est_param$system, syst_est_param$Nrate, sep=" ")

syst_est_param <- syst_est_param %>% 
  dplyr::select(effect, group,  term, param_est,   system, Nrate, annotation, everything()) 


syst_est_param <- syst_est_param %>% 
  mutate(across(c(param_est, system, Nrate, annotation), as.factor))


syst_est_param$system <- factor(syst_est_param$system, levels = c("CM", "GR", "FR", "MR", "PG")) 


syst_est_mod4_test <-syst_est_param %>% 
  mutate(p.score= case_when(
    p.value > 0.05 & p.value < 0.1 ~".",
    p.value < 0.05 & p.value > 0.01  ~"*",
    p.value < 0.01 & p.value > 0.001 ~ "**",
    p.value < 0.001 & p.value > 0.0001 ~ "***",
    p.value < 0.0001  ~ "****")) %>% 
  mutate(perc= case_when(
    annotation == "CM N0" ~  (-0.65945425/45.66569800)*100,
    annotation == "CM N1" ~ (-0.56910206/46.15409985)*100,
    annotation == "GR N0" ~  (-0.4564882/48.5118913)*100,
    annotation == "GR N1" ~  (-0.2277769/48.5442373)*100,
    annotation == "MR N0" ~  (-0.3404297/46.9918371)*100,
    annotation == "MR N1" ~   (0.0766146/45.7440727)*100,
    annotation == "FR N0" ~ (-0.2942976/49.17704517)*100,
    annotation == "FR N1" ~   (0.05822797/49.51891031)*100,
    annotation == "PG N0" ~   (0.228244/44.3793393)*100,
    annotation == "PG N1" ~   (1.07255/47.44024)*100
  )) 

syst_est_mod4_test$perc <- round(syst_est_mod4_test$perc, digits=1) 


#figure

system_slopes_plots <- ggplot(syst_est_mod4_test , aes(Nrate, estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_text(label= syst_est_mod4_test$p.score, 
            nudge_y = 1.5, check_overlap = TRUE, size=10) +
  facet_grid(.~ system) +
  scale_fill_colorblind() + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha=1) +
  theme_classic( base_family = "Times New Roman", base_size=16)+ 
  #theme_bw(base_size=20, base_family = "Times New Roman") + 
  ylim(-2,3) + 
  labs(y= bquote(Delta*SOC* ' ('*'Mg ' ~ ha^-1 ~ yr^-1*')'), 
       x= "N rate")

dev.new() ; system_slopes_plots 




# Figure 3, With percentages -------------------------------------------------------

syst_est_mod4_bar <- syst_est_mod4_test %>%  filter(param_est =="slope")

perc_bars<- ggplot(syst_est_mod4_bar , aes(Nrate, perc)) + 
  geom_bar(stat = "identity", color="black", fill="grey") +
  geom_hline(yintercept=0.4, linetype="dashed", color = "red", size=0.5, alpha=1) +
  facet_grid(.~ system) +
  theme_classic( base_family = "Times New Roman", base_size=16)+
  #theme_bw() +
  ylim(-2,3) + 
  labs(y = bquote(Delta*SOC*' ('*' %' ~ yr^-1*')'), 
       x= "N rate")

dev.new() ; perc_bars



#JOIN FIGURES ------------------------------------------------------------



#system_estimates_plots/(system_slopes_plots | perc_bars)
dev.new();  system_estimates_plots  /(system_slopes_plots + perc_bars) + 
  plot_annotation(tag_levels ="a", theme= theme_bw(base_family="Times New Roman", base_size=13)) +
  plot_layout(guides = "collect") #title = "Annual SOC changes (2010-2018)", 





# Figure with Average across N rates -------------------------------------------------

fert_est2 <- fert_estimates %>%  mutate( grepl(':akk_year', term))


fert_est_param <- fert_est2 %>%  mutate(param_est = case_when(
  str_detect(term, ':akk_year') == TRUE ~ "slope", 
  str_detect(term, ':akk_year') == FALSE ~ "intercept")
) %>% mutate(Nrate = case_when(
  str_detect(term, 'N0') == TRUE ~ "N0", 
  str_detect(term, 'N1') == TRUE ~ "N1")) %>%  filter(param_est =="slope"); str(fert_est_param)

# syst_est_param$annotation <- paste(syst_est_param$system, syst_est_param$Nrate, sep=" ")

fest_est_param <- syst_est_param  %>% 
  dplyr::select(effect, group,  term, param_est, Nrate,  everything()) 



#transform
fert_est_param$param_est <- as.factor(fert_est_param$param_est)
fert_est_param$Nrate <- as.factor(fert_est_param$Nrate);
fert_est_param$Nrate <- factor(fert_est_param$Nrate, levels = c("N0", "N1")) 

#check
str(fert_est_param)



fert_est_param_test <-fert_est_param %>% 
  mutate(p.score= case_when(
    p.value < 0.05 ~"*",
    p.value < 0.01 ~ "**",
    p.value < 0.01 ~ "***",
    p.value < 0.001 ~ "****")); 

View(fert_est_param_test)


#add p value between N rates
stat.test <- tibble::tribble(
  ~group1, ~group2,   ~ t,   ~p.adj,
  "N0",     "N1",       "3.095",   "<0.01",  
)


#figure
Nrate_plot <- ggplot(fert_est_param_test , aes(Nrate, estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  stat_pvalue_manual(stat.test, y.position = 0.5, label = "p.adj") +
  #geom_text(label= fert_est_param_test$p.score,  nudge_y = 0.4, check_overlap = TRUE, size=10) +theme_bw(base_size=20, base_family = "Times New Roman") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5, alpha=1) +
  theme_classic(base_family ="Times New Roman", base_size=16) +
  ylim(-1,1) + 
  labs(y= bquote(Delta*SOC* ' ('*'Mg C' ~ ha^-1 ~ yr^-1*')'), x= "N rate" )


dev.new() ; Nrate_plot