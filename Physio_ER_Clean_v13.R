#Created 9/20/18 for "Do you know how you regulate? Comparing self-report and psychophysiological measures of emotion regulation" 

###SETUP####
setwd("/Users/daisyburr/Dropbox/Dartmouth/Research/Kraemer/study3")
d  <- read.csv("./All_Data.csv", header=T)

#*factor####
d$ID <- as.factor (d$ID)
d$ER_CAT <- as.factor (d$ER_CAT)
d$STIM_CAT <- as.factor (d$STIM_CAT)

#*rename variables####
d$ER_METHOD <- d$ER_CAT
d$STIM <- d$STIM_CAT

levels(d$ER_METHOD)[levels(d$ER_METHOD)==1] <- "UNINSTRUCTED"
levels(d$ER_METHOD)[levels(d$ER_METHOD)==2] <- "SUPPRESS"
levels(d$ER_METHOD)[levels(d$ER_METHOD)==3] <- "REAPPRAISE"

levels(d$STIM)[levels(d$STIM)==1] <- "ANALOGY"
levels(d$STIM)[levels(d$STIM)==2] <- "MATH"
levels(d$STIM)[levels(d$STIM)==3] <- "NEGATIVE"
levels(d$STIM)[levels(d$STIM)==4] <- "NEUTRAL"

levels(d$MARS_GROUP)[levels(d$MARS_GROUP)==1] <- "Low"
levels(d$MARS_GROUP)[levels(d$MARS_GROUP)==2] <- "High"

#*create new variables####
#ERQ_diff
#supp - reapp, pos scores = they suppress more
d$ERQ_diff <- d$ERQ_ES - d$ERQ_ER

#STAI
d$STAI_3 <- cut(d$STAI, 3, include.lowest = TRUE, labels = c('Low','Middle','High'))
d$STAI_3 <- as.factor(d$STAI_3)



#*contrasts####
#sum
contrasts(d$STAI_3) <- contr.poly(3)

#planned
#ER_Method
#CONTROL REAPPRAISE SUPPRESS
Instructed_vs_unisntructed <- c(-2,1,1)
Suppress_vs_reappraise <- c(0,-1,1)
contrasts(d$ER_METHOD) <- cbind(Instructed_vs_unisntructed, Suppress_vs_reappraise)


#*scale within subjects####
library(mousetrap)
d <- scale_within(d, variables=c("STIM_GSRFR_BA", "STIM_LL_BA", "STIM_CS_BA"),within=c("ID"),prefix="z_")
d <- scale_within(d, variables=c("STIM_GSRFR", "STIM_LL", "STIM_CS"),within=c("ID"),prefix="z_")

#*subset df####
#only neg
d_neg <- subset(d, STIM == "NEGATIVE")

#save
write.csv(d_neg, file = "d_neg.csv")

#GENERAL PLOTS####
library(ggplot2)
library(ggpubr)

#single legend
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

#*individual diff####
#*erq STAI GROUP####

ggplot(d_neg, aes(x = ERQ_diff, y = STAI)) + 
  geom_point(aes()) +
  geom_smooth(method = "lm", colour="black") + 
  ggtitle("Self-reported regulation tendency and trait anxiety") + 
  xlim(-4,1) +
  ylab("Trait Anxiety") +
  xlab("Self-reported Regulation Difference Score\n Reappraising more than suppressing <-    -> Suppressing more than reappraising") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        plot.title = element_text(size=20, face="bold"))

cor(d_neg$ERQ_diff, d_neg$STAI)
#0.2796535 
#p-value =  p-value  0.044385


#*Cross correlation in correlation distance####
which(names(d_neg)=="z_STIM_GSRFR")
#55

which(names(d_neg)=="z_STIM_CS")
#57

which(names(d_neg)=="z_STIM_LL")
#56

#Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


#high
cormat_high <- round(1-cor(d_neg[d_neg$STAI_3=="High",][, c(55,57,56)]),2)
lower_tri_high <- get_lower_tri(cormat_high)

#melt
library(reshape2)
melted_cormat_high <- melt(lower_tri_high, na.rm = TRUE)
#heatmap
ggheatmap_high <- ggplot(melted_cormat_high, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "white", 
                       midpoint = 0, limit = c(0,2), space = "Lab", 
                       name="Correlation distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("+1 SD trait anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  scale_x_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                            "z_STIM_LL" = "LL")) +
  scale_y_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                            "z_STIM_LL" = "LL"))


#middle
cormat_middle <- round(1-cor(d_neg[d_neg$STAI_3=="Middle",][, c(55,57,56)]),2)

lower_tri_middle <- get_lower_tri(cormat_middle)
#melt
library(reshape2)
melted_cormat_middle <- melt(lower_tri_middle, na.rm = TRUE)
#heatmap
ggheatmap_middle <- ggplot(melted_cormat_middle, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "white", 
                       midpoint = 0, limit = c(0,2), space = "Lab", 
                       name="Correlation distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("Mean trait anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5)  +
  scale_x_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                            "z_STIM_LL" = "LL")) +
  scale_y_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                            "z_STIM_LL" = "LL"))


#low
cormat_low <- round(1-cor(d_neg[d_neg$STAI_3=="Low",][, c(55,57,56)]),2)
lower_tri_low <- get_lower_tri(cormat_low)
#melt
library(reshape2)
melted_cormat_low <- melt(lower_tri_low, na.rm = TRUE)
#heatmap
ggheatmap_low <- ggplot(melted_cormat_low, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "white", 
                       midpoint = 0, limit = c(0,2), space = "Lab", 
                       name="Correlation distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("- 1 SD trait anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  scale_x_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                                "z_STIM_LL" = "LL")) +
  scale_y_discrete(labels=c("z_STIM_GSRFR" = "EDA", "z_STIM_CS" = "CS",
                            "z_STIM_LL" = "LL"))



require(gridExtra)
library(grid)
require(lattice)

cross_corr <- grid_arrange_shared_legend(ggheatmap_low, ggheatmap_middle, ggheatmap_high, ncol=3, nrow=1)

#*physio descriptives####
library(Rmisc)

ddply(d, "STAI_3", summarise,
      mean = mean(STAI),
      n = length(unique(ID)))
#STAI_3     mean  n
#1    Low 1.630592 24
#2 Middle 2.222885 19
#3   High 2.859357  9

d_neg$ER_METHOD <- factor(d_neg$ER_METHOD, labels = c("Uninstructed", "Suppress", "Reappraise"))

cs <- summarySE(d_neg, measurevar="z_STIM_CS", groupvars=c("ER_METHOD","STAI_3"))
# ER_METHOD STAI_3   N   z_STIM_CS        sd         se         ci
# Spontaneous    Low 480  1.24147017 1.4499688 0.06618172 0.13004237
# Spontaneous Middle 380  0.73322346 1.4086926 0.07226441 0.14208938
# Spontaneous   High 180  0.57714886 1.4614080 0.10892692 0.21494608
#    Suppress    Low 480  0.10006991 0.7491392 0.03419337 0.06718754
#    Suppress Middle 380  0.12550144 1.0088193 0.05175134 0.10175570
#    Suppress   High 180 -0.28461849 0.6913924 0.05153335 0.10169103
#  Reappraise    Low 480  0.17086352 1.0846545 0.04950748 0.09727867
#  Reappraise Middle 380  0.42126872 0.8201094 0.04207073 0.08272127
#  Reappraise   High 180  0.02626882 0.8263527 0.06159269 0.12154119
cs_noAnx <- summarySE(d_neg, measurevar="z_STIM_CS", groupvars=c("ER_METHOD"))
#    ER_METHOD    N  z_STIM_CS        sd         se         ci
#Spontaneous 1040 0.94078595 1.4633838 0.04537761 0.08904220
#    Suppress 1040 0.04278151 0.8572681 0.02658275 0.05216200
#  Reappraise 1040 0.23733172 0.9630651 0.02986338 0.05859941
csme <- ggplot(cs, aes(x=STAI_3, y=z_STIM_CS, fill = STAI_3)) + 
  xlab("Trait anxiety") +
  ylab("Corrugator (z)") +
  stat_summary(fun.y="mean", geom="bar") + facet_grid(.~ER_METHOD) + geom_errorbar(aes(ymin=z_STIM_CS-se, ymax=z_STIM_CS+se), width=.2, position=position_dodge(.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_grey(start = 0.4, end = 0.8, aesthetics = "fill") + theme_apa()


ll <- summarySE(d_neg, measurevar="z_STIM_LL", groupvars=c("ER_METHOD","STAI_3"))
#  ER_METHOD STAI_3   N   z_STIM_LL        sd         se         ci
# Spontaneous    Low 480  0.48134656 1.2084833 0.05515946 0.10838442
# Spontaneous Middle 380  0.42713420 1.1829803 0.06068561 0.11932265
# Spontaneous   High 180  0.51533361 1.2042016 0.08975589 0.17711579
#    Suppress    Low 480 -0.20190844 0.6310302 0.02880246 0.05659478
#    Suppress Middle 380 -0.05774360 0.9807051 0.05030911 0.09891993
#    Suppress   High 180 -0.10402444 1.1444660 0.08530346 0.16832978
#  Reappraise    Low 480 -0.14397062 0.7748267 0.03536584 0.06949135
#  Reappraise Middle 380  0.03056806 1.0299012 0.05283282 0.10388216
#  Reappraise   High 180 -0.22173432 0.9570214 0.07133216 0.14076014
ll_noAnx <- summarySE(d_neg, measurevar="z_STIM_LL", groupvars=c("ER_METHOD"))
#    ER_METHOD    N   z_STIM_LL        sd         se         ci
# Spontaneous 1040  0.46742057 1.1977789 0.03714155 0.07288100
#  Suppress 1040 -0.13229136 0.8743426 0.02711221 0.05320094
#  Reappraise 1040 -0.09365597 0.9116669 0.02826959 0.05547200
llme <- ggplot(ll, aes(x=STAI_3, y=z_STIM_LL, fill = STAI_3)) + 
  xlab("Trait anxiety") +
  ylab("Levator (z)") +
  stat_summary(fun.y="mean", geom="bar") + facet_grid(.~ER_METHOD) + geom_errorbar(aes(ymin=z_STIM_LL-se, ymax=z_STIM_LL+se), width=.2, position=position_dodge(.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_grey(start = 0.4, end = 0.8, aesthetics = "fill") + theme_apa()

gsr <- summarySE(d_neg, measurevar="z_STIM_GSRFR", groupvars=c("ER_METHOD","STAI_3"))
#  ER_METHOD STAI_3   N z_STIM_GSRFR        sd         se         ci
# Spontaneous    Low 480  -0.01389333 0.8548666 0.03901914 0.07666984
# Spontaneous Middle 380   0.01021617 1.2003635 0.06157735 0.12107602
# Spontaneous   High 180   0.11243269 0.6129069 0.04568338 0.09014726
#    Suppress    Low 480  -0.05874604 0.7904212 0.03607763 0.07088997
#    Suppress Middle 380   0.05332000 0.8472708 0.04346407 0.08546093
#    Suppress   High 180   0.04187532 0.5996704 0.04469679 0.08820043
#  Reappraise    Low 480  -0.14984347 0.8814111 0.04023073 0.07905052
#  Reappraise Middle 380  -0.02690439 0.9966759 0.05112840 0.10053085
#  Reappraise   High 180  -0.08176878 1.2489932 0.09309445 0.18370379
gsr_noAnx <- summarySE(d_neg, measurevar="z_STIM_GSRFR", groupvars=c("ER_METHOD"))
#    ER_METHOD    N  z_STIM_GSRFR        sd         se         ci
# Spontaneous 1040  0.0167800282 0.9639805 0.02989177 0.05865511
#    Suppress 1040 -0.0003835997 0.7840790 0.02431326 0.04770868
#  Reappraise 1040 -0.0931412629 0.9961831 0.03089033 0.06061454
gsrme <- ggplot(gsr, aes(x=STAI_3, y=z_STIM_GSRFR, fill = STAI_3)) + 
  xlab("Trait anxiety") +
  ylab("Skin conductance (z)") +
  stat_summary(fun.y="mean", geom="bar") + facet_grid(.~ER_METHOD) + geom_errorbar(aes(ymin=z_STIM_GSRFR-se, ymax=z_STIM_GSRFR+se), width=.2, position=position_dodge(.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_grey(start = 0.4, end = 0.8, aesthetics = "fill") + theme_apa()

library(cowplot)
legend <- get_legend(
  llme + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right")
)


plots <- plot_grid(
  gsrme + theme(legend.position="none"),
  csme + theme(legend.position="none"),
  llme + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 3, 
  ncol = 1
)

plot_grid(plots, ncol = 1, rel_heights = c(1, .1))

ggsave("physio_descrip.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)


#*MODELS####

######GLMER ERCONDITION####
library(lmerTest)
library(lme4)
library(sjPlot)
library(sjmisc)
library(jtools)
library(car)
library(sjstats)
library(sjlabelled)
library(optimx) 
library(ggplot2)
library(interactions) #new jtools interactions plots
library(cowplot)
library(interactions)

#only instructed
glmer <- d_neg[d_neg$ER_METHOD!="UNINSTRUCTED",] 
glmer$ER_METHOD <- factor(glmer$ER_METHOD)

#check contrasts for logit interpretation
contrasts(glmer$ER_METHOD)
#SUPPRESS            0
#REAPPRAISE          1


glmer_ERQ_STAI <- glmer(ER_METHOD ~ z_STIM_GSRFR + z_STIM_CS + z_STIM_LL + z_STIM_LL:STAI + z_STIM_CS:STAI + z_STIM_GSRFR:STAI + (1|ID), data = glmer, family = binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) 
summ(glmer_ERQ_STAI, center = TRUE, confint = TRUE)
#                             Est.    2.5%   97.5%   z val.      p
#z_STIM_GSRFR              -0.12   -0.22   -0.02    -2.33   0.02
#z_STIM_CS                  0.24    0.15    0.34     4.86   0.00
#z_STIM_LL                  0.06   -0.05    0.16     1.05   0.29
#z_STIM_LL:STAI            -0.23   -0.43   -0.04    -2.40   0.02
#z_STIM_CS:STAI             0.36    0.15    0.58     3.33   0.00
#z_STIM_GSRFR:STAI         -0.01   -0.22    0.20    -0.13   0.89

glmer_GSR <- effect_plot(glmer_ERQ_STAI, pred = z_STIM_GSRFR, interval = TRUE, outcome.scale = "response", x.label = "Skin conductance (z)", y.label = "Predicted probability \n of reappraising", data = glmer) + theme_apa()
glmer_CS <- effect_plot(glmer_ERQ_STAI, pred = z_STIM_CS, interval = TRUE, outcome.scale = "response", x.label = "Corrugator (z)", y.label = "Predicted probability \n of reappraising", data = glmer)  + theme_apa()
glmer_LL <- effect_plot(glmer_ERQ_STAI, pred = z_STIM_LL, interval = TRUE, outcome.scale = "response", x.label = "Levator (z)", y.label = "Predicted probability \n of reappraising", data = glmer) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_apa()
glmer_LL_STAI <- interact_plot(glmer_ERQ_STAI, pred = z_STIM_LL, modx = STAI, interval = TRUE, outcome.scale = "response", x.label = "Levator (z)", y.label = "Predicted probability \n of reappraising", data = glmer, legend.main = "Trait anxiety") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + theme_apa()
glmer_CS_STAI <- interact_plot(glmer_ERQ_STAI, pred = z_STIM_CS, modx = STAI, interval = TRUE, outcome.scale = "response", x.label = "Corrugator (z)", y.label = "Predicted probability \n of reappraising", data = glmer, legend.main = "Trait anxiety")  + theme_apa()
glmer_GSR_STAI <- interact_plot(glmer_ERQ_STAI, pred = z_STIM_GSRFR, modx = STAI, interval = TRUE, outcome.scale = "response", x.label = "Skin conductance (z)", y.label = "Predicted probability \n of reappraising", data = glmer, legend.main = "Trait anxiety")  + theme_apa()

legend <- interact_plot(glmer_ERQ_STAI, pred = z_STIM_LL, modx = STAI, interval = TRUE, outcome.scale = "response", x.label = "Levator (z)", y.label = "Predicted probability \n of reappraising", data = glmer, legend.main = "Trait anxiety") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(cowplot)
plots <- plot_grid(
  glmer_GSR + theme(legend.position="none"),
  glmer_CS + theme(legend.position="none"),
  glmer_LL + theme(legend.position="none"),
  glmer_GSR_STAI + theme(legend.position="none"),
  glmer_CS_STAI + theme(legend.position="none"),
  glmer_LL_STAI + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 2, 
  ncol = 3
)

legend <- get_legend(
  legend + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))


ggsave("GLMER_ERCOND.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 9, height = 7, dpi = 300)

#*DISTANCE####

library(tidyr)
#predict distance to unistructed from instructed conditionm ERQ, and STAI

#reformat so dist is nested under condition
data_long <- gather(d_neg, ins_cond, dist_to_unins, REAPPRAISE_UNINSTRUCTED_cor:SUPPRESS_UNINSTRUCTED_cor, factor_key=TRUE)

#factor
data_long$ins_cond <- as.factor(data_long$ins_cond)

#relabel condition
levels(data_long$ins_cond)[levels(data_long$ins_cond)=="REAPPRAISE_UNINSTRUCTED_cor"] <- "Reappraise"
levels(data_long$ins_cond)[levels(data_long$ins_cond)=="SUPPRESS_UNINSTRUCTED_cor"] <- "Suppress"

#contrasts
Reapp_Supp <- c(1,-1)
contrasts(data_long$ins_cond) <- Reapp_Supp


#model
#ERQ_ER
dist_1 <- lmer(dist_to_unins ~ ERQ_ER*ins_cond + (1|ID), data = data_long, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(dist_1, center = TRUE, confint = TRUE)
#
#MODEL FIT:
#AIC = 9079.82, BIC = 9106.78
#Pseudo-R² (fixed effects) = 0.00
#Pseudo-R² (total) = 0.54 
#FIXED EFFECTS:
#                          Est.    2.5%   97.5%   t val.      d.f.      p
#ERQ_ER                   0.04   -0.12    0.21     0.51     50.68   0.61
#ins_cond                -0.04   -0.06   -0.02    -3.25   6186.00   0.00
#ERQ_ER:ins_cond         -0.02   -0.04    0.01    -1.19   6186.00   0.23

dist_1_effect <- effect_plot(dist_1, pred = ERQ_ER, interval = TRUE, outcome.scale = "response", x.label = "Self-reported reappraisal \n tendency", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_1_effect <- dist_1_effect + ylim(0,2) + theme_apa()

dist_1_effect2 <- cat_plot(dist_1, pred = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Instructed condition", label = "Dissimilarity to instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_1_effect2 <- dist_1_effect2 + ylim(0,2) + theme_apa()

dist_1_int <- interact_plot(dist_1, pred = ERQ_ER, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "ERQ-Reappraisal", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_1_int <- dist_1_int + ylim(0,2) + theme_apa(legend.pos = "bottom")

library(cowplot)
plots <- plot_grid(
  dist_1_effect,
  dist_1_effect2,
  dist_1_int + theme(legend.position = "none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)


#shared legend
legend <- get_legend(
  dist_1_int + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottomright")
)


plot_grid(plots, legend,  ncol = 1, rel_heights = c(1, .1))

ggsave("dist_reapp.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 7, dpi = 300)

#ERQ_ES
dist_2 <- lmer(dist_to_unins ~ ERQ_ES*ins_cond + (1|ID), data = data_long, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(dist_2, center = TRUE, confint = TRUE)
#MODEL FIT:
#AIC = 9081.40, BIC = 9108.35
#Pseudo-R² (fixed effects) = 0.00
#Pseudo-R² (total) = 0.54 
#FIXED EFFECTS:
#Est.    2.5%   97.5%   t val.      d.f.      p
#(Intercept)              0.93    0.78    1.07    12.45     50.96   0.00
#ERQ_ES                   0.12   -0.02    0.26     1.64     50.72   0.11
#ins_cond                -0.04   -0.06   -0.02    -3.25   6186.00   0.00
#ERQ_ES:ins_cond         -0.00   -0.03    0.02    -0.17   6186.00   0.86


dist_2_effect <- effect_plot(dist_2, pred = ERQ_ES, interval = TRUE, outcome.scale = "response", x.label = "Self-reported suppression \n tendency", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_2_effect <- dist_2_effect + ylim(0,2) + theme_apa()

dist_2_effect2 <- cat_plot(dist_2, pred = ins_cond, interval = TRUE, outcome.scale = "response", y.label = "Dissimilarity to instructed condition", x.label = "Instructed condition", label = "Dissimilarity to \n instructed", data = data_long) 
dist_2_effect2 <- dist_2_effect2 + ylim(0,2) + theme_apa()

dist_2_int <- interact_plot(dist_2, pred = ERQ_ES, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "ERQ-Suppression", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_2_int <- dist_2_int + ylim(0,2) + theme_apa()

library(cowplot)
plots <- plot_grid(
  dist_2_effect + theme(legend.position="none"),
  dist_2_effect2 + theme(legend.position="none"),
  dist_2_int + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)

plot_grid(plots, legend,  ncol = 1, rel_heights = c(1, .1))

ggsave("dist_supp.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)
#STAI
dist_3 <- lmer(dist_to_unins ~ STAI*ins_cond + (1|ID), data = data_long, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(dist_3, center = TRUE, confint = TRUE)
#MODEL FIT:
#AIC = 8751.67, BIC = 8778.63
#Pseudo-R² (fixed effects) = 0.04
#Pseudo-R² (total) = 0.59 
#FIXED EFFECTS:
#                        Est.    2.5%   97.5%   t val.      d.f.      p
#(Intercept)            0.93    0.78    1.08    12.19     50.97   0.00
#STAI                   0.22   -0.08    0.52     1.47     50.65   0.15
#ins_cond              -0.04   -0.06   -0.02    -3.34   6186.00   0.00
#STAI:ins_cond         -0.45   -0.50   -0.40   -18.44   6186.00   0.00

dist_3_effect <- effect_plot(dist_3, pred = STAI, interval = TRUE, outcome.scale = "response", x.label = "Trait anxiety", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_3_effect <- dist_3_effect + ylim(0,2) + theme_apa()

dist_3_effect2 <- cat_plot(dist_3, pred = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Instructed condition", label = "Dissimilarity to \n instructed",y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_3_effect2 <- dist_3_effect2 + ylim(0,2) + theme_apa()

dist_3_int <- interact_plot(dist_3, pred = STAI, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Trait anxiety", legend.main = "Instructed condition", y.label = "Dissimilarity to \n instructed", data = data_long) 
dist_3_int <- dist_3_int + ylim(0,2) + theme_apa()

library(cowplot)
plots <- plot_grid(
  dist_3_effect + theme(legend.position="none"),
  dist_3_effect2 + theme(legend.position="none"),
  dist_3_int + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)
plot_grid(plots, legend,  ncol = 1, rel_heights = c(1, .1))

ggsave("dist_stai.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)


#all models in 1 plot

#shared legend
legend_b <- get_legend(
  dist_1_int + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


library(cowplot)
plots <- plot_grid(
  dist_3_int + theme(legend.position="none"),
  dist_1_int + theme(legend.position="none"),
  dist_2_int + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  ncol = 3
)



plot_grid(plots, legend_b,  ncol = 1, rel_heights = c(1, .1))

ggsave("dist_effects.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)

#ALL
dist_4 <- lmer(dist_to_unins ~ STAI:ins_cond + ERQ_ES:ins_cond + ERQ_ER:ins_cond + (1|ID), data = data_long, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(dist_4, center = TRUE, confint = TRUE)
#MODEL FIT:
#AIC = 8723.63, BIC = 8764.06
#Pseudo-R² (fixed effects) = 0.05
#Pseudo-R² (total) = 0.59 
#FIXED EFFECTS:
#                         Est.    2.5%   97.5%   t val.      d.f.      p
#(Intercept)              0.93    0.78    1.08    12.20     50.94   0.00
#STAI:ins_cond           -0.50   -0.55   -0.45   -19.60   6233.09   0.00
#ins_cond:ERQ_ES          0.04    0.01    0.06     3.07   6233.09   0.00
#ins_cond:ERQ_ER         -0.09   -0.11   -0.06    -6.24   6233.09   0.00

dist_4_stai <- interact_plot(dist_4, pred = STAI, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Trait Anxiety", legend.main = "Instructed Condition", y.label = "Dissimilarity", plot.points = TRUE, data = data_long) 
dist_4_stai <- dist_4_stai + ylim(0,2) + theme_apa()

dist_4_erqes <- interact_plot(dist_4, pred = ERQ_ES, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Self-Reported Suppression Tendency", legend.main = "Instructed Condition", y.label = "Dissimilarity", plot.points = TRUE, data = data_long) 
dist_4_erqes <- dist_4_erqes + ylim(0,2) + theme_apa()

dist_4_erqer <- interact_plot(dist_4, pred = ERQ_ER, modx = ins_cond, interval = TRUE, outcome.scale = "response", x.label = "Self-Reported Reappraisal Tendency", legend.main = "Instructed Condition", y.label = "Dissimilarity", plot.points = TRUE, data = data_long) 
dist_4_erqer <- dist_4_erqer + ylim(0,2) + theme_apa()

library(cowplot)
legend <- get_legend(
  # create some space to the left of the legend
  dist_4_erqer + theme(legend.box.margin = margin(0, 0, 0, 12))
)
title <- ggdraw() + 
  draw_label(
    "Do self-reported regulation tendency and trait anxiety predict \n
    dissimilarity between unistructed and isntructed regulation?",
    fontface = 'bold',
    x = 0,
    hjust = 2
  )  +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plots <- plot_grid(
  dist_4_stai + theme(legend.position="none"),
  dist_4_erqes + theme(legend.position="none"),
  dist_4_erqer + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1
)
plot_grid(title, plots, legend, ncol = 1)

ggsave("physio_descrip.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)

dist_5 <- lmer(dist_to_unins ~ STAI:ins_cond + ERQ_ES:ins_cond + ERQ_ER:ins_cond + ERQ_ER:ERQ_ES:STAI:ins_cond + (1|ID), data = data_long, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(dist_5, center = TRUE, confint = TRUE)
#MODEL FIT:
#AIC = 8724.64, BIC = 8771.81
#Pseudo-R² (fixed effects) = 0.05
#Pseudo-R² (total) = 0.59 
#FIXED EFFECTS:
#(Intercept)                          0.93    0.78    1.08    12.22     50.95   0.00
#STAI:ins_cond                       -0.48   -0.53   -0.42   -17.78   6232.02   0.00
#ins_cond:ERQ_ES                      0.04    0.02    0.06     3.25   6232.05   0.00
#ins_cond:ERQ_ER                     -0.09   -0.12   -0.06    -6.50   6232.05   0.00
#STAI:ins_cond:ERQ_ES:ERQ_ER          0.06    0.01    0.11     2.56   6231.66   0.01


interact_plot(dist_5, pred = ERQ_ER, modx = ins_cond, mod2 = STAI,interval = TRUE, outcome.scale = "response", x.label = "Self-Reported Reappraisal Tendency", legend.main = "Instructed Condition", y.label = "Dissimilarity", plot.points = TRUE, data = data_long) 


#*REAPP_UNINSTRUCTED
#COR
reapp_cor <- lmer(REAPPRAISE_UNINSTRUCTED_cor ~ ERQ_ER * STAI + (1|ID), data = d_neg, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(reapp_cor, center = TRUE, confint = TRUE)
#                    Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)          0.58    0.55    0.62    35.79   0.32   0.22
#ERQ_ER              -0.06   -0.10   -0.03    -3.48   1.05   0.17
#STAI                -0.39   -0.46   -0.32   -10.69   2.27   0.01
#ERQ_ER:STAI         -0.38   -0.44   -0.32   -12.26   0.59   0.14

library("ResourceSelection")
hoslem.test(d_neg$REAPPRAISE_UNINSTRUCTED_cor, fitted(reapp_cor))
#data:  d_neg$REAPPRAISE_UNINSTRUCTED_cor, fitted(reapp_cor)
#X-squared = 3.3395e-24, df = 8, p-value = 1
#no evidence that model is poor fit!


reapp_ERQ <- effect_plot(reapp_cor, pred = ERQ_ER, interval = TRUE, outcome.scale = "response", x.label = "Self-reported Reappraisal \n (non-significant)", y.label = "Distance Between Reappraisal and Uninstructed Conditions", data = d_neg) 
reapp_ERQ <- reapp_ERQ + ylim(0,2) + theme_apa()

reapp_STAI <- effect_plot(reapp_cor, pred = STAI, interval = TRUE, outcome.scale = "response", x.label = "Trait Anxiety", y.label = "Distance Between Reappraisal and Uninstructed Conditions", data = d_neg) 
reapp_STAI <- reapp_STAI + ylim(0,2) + theme_apa()


plot_grid(reapp_ERQ,
          reapp_STAI,
          ncol = 2)

ggsave("Reapp_dist_ME.eps", plot = last_plot(), device = "eps",
       scale = 1, width = 7, height = 6, dpi = 300)


reapp_int <- interact_plot(reapp_cor, pred = ERQ_ER, modx = STAI, x.label = "Self-reported Reappraisal \n (non-significant interaction)",  y.label = "Distance Between Reappraisal and Uninstructed Conditions", interval = TRUE, legend.main = "Trait Anxiety") 
reapp_int <- reapp_int + ylim(0,2) + theme_apa()

ggsave("Reapp_dist_int.eps", plot = last_plot(), device = "eps",
       scale = 1, width = 7, height = 6, dpi = 300)


#*SUPP_UNINSTRUCTED
supp_cor <- lmer(SUPPRESS_UNINSTRUCTED_cor ~ ERQ_ES * STAI + (1|ID), data = d_neg, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(supp_cor, center = TRUE, confint = TRUE)
#                     Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)          0.57    0.54    0.60    40.01   0.28   0.25
#ERQ_ES               0.07    0.05    0.10     5.27   1.40   0.07
#STAI                -0.29   -0.35   -0.24   -10.14   0.43   0.24
#ERQ_ES:STAI          0.07    0.01    0.13     2.38   0.19   0.64

library("ResourceSelection")
hoslem.test(d_neg$SUPPRESS_UNINSTRUCTED_cor, fitted(supp_cor))
#data:  d_neg$SUPPRESS_UNINSTRUCTED_cor, fitted(supp_cor)
#X-squared = 1.9101e-23, df = 8, p-value = 1
#random subject variabilty strengthens effect, loosing effect in correlation 


supp_ERQ <- effect_plot(supp_cor, pred = ERQ_ES, interval = TRUE, outcome.scale = "response", x.label = "Self-reported Suppression\n (marginally significant)", y.label = "Distance Between Suppression and Uninstructed Conditions", data = d_neg) 
supp_ERQ <- supp_ERQ + ylim(0,2) + theme_apa()


supp_STAI <- effect_plot(supp_cor, pred = STAI, interval = TRUE, outcome.scale = "response", x.label = "Trait Anxiety \n (non-significant)", y.label = "Distance Between Suppression and Uninstructed Conditions", data = d_neg) 
supp_STAI <- supp_STAI + ylim(0,2) + theme_apa()


plot_grid(supp_ERQ,
          supp_STAI,
          ncol = 2)

ggsave("Supp_dist_ME.eps", plot = last_plot(), device = "eps",
       scale = 1, width = 7, height = 6, dpi = 300)



supp_int <- interact_plot(supp_cor, pred = ERQ_ES, modx = STAI, x.label = "Self-reported Suppression \n (non-significant interaction)",  y.label = "Distance Between Suppression and Uninstructed Conditions", interval = TRUE, legend.main = "Trait Anxiety") 
supp_int <- supp_int + ylim(0,2)+ theme_apa()


ggsave("Supp_dist_int.eps", plot = last_plot(), device = "eps",
       scale = 1, width = 7, height = 6, dpi = 300)



#*distance heatmaps####
#Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#high
mean_high_supp_uns <- mean(subset(d_neg, STAI_3 == "High")$SUPPRESS_UNINSTRUCTED_cor)#0.8972837
mean_high_reapp_uns <- mean(subset(d_neg, STAI_3 == "High")$REAPPRAISE_UNINSTRUCTED_cor)# 1.163805
mean_high_supp_reapp <- mean(subset(d_neg, STAI_3 == "High")$REAPPRAISE_SUPPRESS_cor)#1.186526

one_high <- c(0,  1.19, 0.90) 
two_high <- c(01.17, 0, 1.19)
three_high <- c(0.90, 1.19, 0)

matrix_high <- rbind(one_high, two_high, three_high)

colnames(matrix_high) <- c('Spontaneous', 'Reappraise','Suppress')
rownames(matrix_high) <- c('Spontaneous', 'Reappraise','Suppress')

lower_tri_high_dist <- get_lower_tri(matrix_high)

library(reshape2)
melted_high <- melt(lower_tri_high_dist,na.rm = TRUE)

#heatmap
ggheatmap_high_dist <- ggplot(melted_high, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "white", 
                       limit = c(0,2), space = "Lab", 
                       name="Correlation Distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("+1 SD trait anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  scale_x_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise")) +
  scale_y_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise"))

#middle
mean_middle_supp_uns <- mean(subset(d_neg, STAI_3 == "Middle")$SUPPRESS_UNINSTRUCTED_cor) #0.7902481
mean_middle_reapp_uns <- mean(subset(d_neg, STAI_3 == "Middle")$REAPPRAISE_UNINSTRUCTED_cor) #0.7857481
mean_middle_supp_reapp <- mean(subset(d_neg, STAI_3 == "Middle")$REAPPRAISE_SUPPRESS_cor) #0.8746705

one_middle <- c(0,  0.79, 0.79) 
two_middle <- c(0.79, 0, 0.87)
three_middle <- c(0.79,0.87, 0)

matrix_middle <- rbind(one_middle, two_middle, three_middle)

colnames(matrix_middle) <- c('Spontaneous', 'Reappraise','Suppress')
rownames(matrix_middle) <- c('Spontaneous', 'Reappraise','Suppress')

lower_tri_middle_dist <- get_lower_tri(matrix_middle)

library(reshape2)
melted_middle <- melt(lower_tri_middle_dist,na.rm = TRUE)

#heatmap
ggheatmap_middle_dist <- ggplot(melted_middle, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "white", 
                       limit = c(0,2), space = "Lab", 
                       name="Correlation Distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("Mean trait anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  scale_x_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise")) +
  scale_y_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise"))

#low
mean_low_supp_uns <- mean(subset(d_neg, STAI_3 == "Low")$SUPPRESS_UNINSTRUCTED_cor)#1.002448
mean_low_reapp_uns <- mean(subset(d_neg, STAI_3 == "Low")$REAPPRAISE_UNINSTRUCTED_cor)# 0.9934857
mean_low_supp_reapp <- mean(subset(d_neg, STAI_3 == "Low")$REAPPRAISE_SUPPRESS_cor) #0.8524738

one_low <- c(0,  0.99, 1) 
two_low <- c(0.99, 0, 0.85)
three_low <- c(1, 0.85, 0)

matrix_low <- rbind(one_low, two_low, three_low)

colnames(matrix_low) <- c('Spontaneous', 'Reappraise','Suppress')
rownames(matrix_low) <- c('Spontaneous', 'Reappraise','Suppress')

lower_tri_low_dist <- get_lower_tri(matrix_low)

library(reshape2)
melted_low <- melt(lower_tri_low_dist,na.rm = TRUE)

#heatmap
ggheatmap_low_dist <- ggplot(melted_low, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "white", 
                       limit = c(0,2), space = "Lab", 
                       name="Correlation Distance") +
  xlab("") +
  ylab("") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  theme(axis.text.y = element_text(vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + ggtitle("-1 SD Trait Anxiety") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  scale_x_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise")) +
  scale_y_discrete(labels=c("supp_spont" = "Suppress-Spontaneous", "reapp_spont" = "Reappraise-Spontaneous",
                            "supp_reapp" = "Suppress-Reappraise"))


require(gridExtra)
library(grid)
require(lattice)

cross_corr <- grid_arrange_shared_legend(ggheatmap_low_dist, ggheatmap_middle_dist, ggheatmap_high_dist, ncol=3, nrow=1)



