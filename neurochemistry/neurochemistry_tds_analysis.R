#Analysis scripts for data on extracellular glutamate and tissue nitrite levels after CAS exposure in zebrafish.
#Project: "Nitrergic signalling in an animal model for post-traumatic stress disorder"
#Project coordinator: Monica Lima Maximino (ORCID: https://orcid.org/0000-0002-9816-3443)
#Project grant: CNPq/Brazil (Process no. 423735/2016-0)

#Install and load libraries
if(!require(readr)){
  install.packages("readr")
  library(readr)
}

if(!require(RCurl)){
  install.packages("RCurl")
  library(RCurl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(dabestr)){
  install.packages("dabestr")
  library(dabestr)
}

#Load dataset on glutamate
glu_timecourse <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/neurochemistry/glu-timecourse.csv", col_types = cols(Time = col_factor(levels = c("0 min", "15 min", "30 min", "90 min", "24 h")), Treatment = col_factor(levels = c("CTRL", "CAS"))))
View(glu_timecourse)

#Run two-way ANOVA on dataset, followed by Tukey's HSD post-hoc test
glu_anova <- aov(formula = Glu ~ Time*Treatment, data = glu_timecourse)
summary(glu_anova)
TukeyHSD(glu_anova)

#Reorganize data for plotting
glu_timecourse_forplot <- glu_timecourse %>%
    unite(groups, c("Time", "Treatment"))

#Create object for plotting
multi_2group_glu <- load(glu_timecourse_forplot,
  x = groups, y = Glu,
  idx = list(
    c("0 min_CTRL", "0 min_CAS"),
    c("15 min_CTRL", "15 min_CAS"),
    c("30 min_CTRL", "30 min_CAS"),
    c("90 min_CTRL", "90 min_CAS"),
    c("24 h_CTRL", "24 h_CAS")
  )
)

#Plot
multi_2group_glu %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset on timecourse of nitrite levels in the forebrain
#Load dataset on glutamate
NOX_timecourse <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/neurochemistry/NOX-timecourse.csv", col_types = cols(Time = col_factor(levels = c("0 min", "15 min", "30 min", "90 min", "24 h")),Treatment = col_factor(levels = c("CTRL", "CAS"))))
View(NOX_timecourse)

#Run two-way ANOVA on dataset, followed by Tukey's HSD post-hoc test
nox_anova <- aov(formula = Nitrite ~ Time*Treatment, data = NOX_timecourse)
summary(nox_anova)
TukeyHSD(nox_anova)

#Reorganize data for plotting
NOX_timecourse_forplot <- NOX_timecourse %>%
  unite(groups, c("Time", "Treatment"))

#Create object for plotting
multi_2group_NOX <- load(NOX_timecourse_forplot,
  x = groups, y = Nitrite,
  idx = list(
    c("0 min_CTRL", "0 min_CAS"),
    c("15 min_CTRL", "15 min_CAS"),
    c("30 min_CTRL", "30 min_CAS"),
    c("90 min_CTRL", "90 min_CAS"),
    c("24 h_CTRL", "24 h_CAS")
  )
)

#Plot
multi_2group_NOX %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset on role of NOS-2 on CAS-elicited nitrite elevations in the forebrain
NOX_forebrain <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/neurochemistry/NOX_forebrain.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "AG"))))
View(NOX_forebrain)

#Run two-way ANOVA on data for nitrite levels 30 min after CAS, followed by Tukey's HSD post-hoc test
nox_fb_30m_anova <- aov(formula = t30min ~ Treatment*Drug, data = NOX_forebrain)
summary(nox_fb_30m_anova)
TukeyHSD(nox_fb_30m_anova)

#Reorganize data for plotting
NOX_forebrain_forplot <- NOX_forebrain %>%
  unite(groups, c("Treatment", "Drug"))

#Create object for plotting
multi_2group_NOX_fb_30 <- load(NOX_forebrain_forplot,
                         x = groups, y = t30min,
                         idx = list(
                           c("CTRL_VEH", "CAS_VEH"),
                           c("CTRL_AG", "CAS_AG")
                         )
)

#Plot
multi_2group_NOX_fb_30 %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on data for nitrite levels 90 min after CAS, followed by Tukey's HSD post-hoc test
nox_fb_90m_anova <- aov(formula = t90min ~ Treatment*Drug, data = NOX_forebrain)
summary(nox_fb_90m_anova)
TukeyHSD(nox_fb_90m_anova)

#Create object for plotting
multi_2group_NOX_fb_90 <- load(NOX_forebrain_forplot,
                            x = groups, y = t90min,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_NOX_fb_90 %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset on role of NOS-2 on CAS-elicited nitrite elevations in the head kidney
NOX_headkidney <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/neurochemistry/NOX_headkidney.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "AG"))))
View(NOX_headkidney)

#Run two-way ANOVA on data for nitrite levels 30 min after CAS, followed by Tukey's HSD post-hoc test
nox_hk_30m_anova <- aov(formula = t30min ~ Treatment*Drug, data = NOX_headkidney)
summary(nox_hk_30m_anova)
TukeyHSD(nox_hk_30m_anova)

#Reorganize data for plotting
NOX_headkidney_forplot <- NOX_headkidney %>%
  unite(groups, c("Treatment", "Drug"))

#Create object for plotting
multi_2group_NOX_hk_30 <- load(NOX_headkidney_forplot,
                               x = groups, y = t30min,
                               idx = list(
                                 c("CTRL_VEH", "CAS_VEH"),
                                 c("CTRL_AG", "CAS_AG")
                               )
)

#Plot
multi_2group_NOX_hk_30 %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on data for nitrite levels 90 min after CAS, followed by Tukey's HSD post-hoc test
nox_hk_90m_anova <- aov(formula = t90min ~ Treatment*Drug, data = NOX_headkidney)
summary(nox_hk_90m_anova)
TukeyHSD(nox_hk_90m_anova)

#Create object for plotting
multi_2group_NOX_hk_90 <- load(NOX_headkidney_forplot,
                               x = groups, y = t90min,
                               idx = list(
                                 c("CTRL_VEH", "CAS_VEH"),
                                 c("CTRL_AG", "CAS_AG")
                               )
)

#Plot
multi_2group_NOX_hk_90 %>%
  mean_diff() %>%
  dabest_plot()

