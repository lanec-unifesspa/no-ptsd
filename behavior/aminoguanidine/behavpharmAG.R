#Analysis scripts for data on the effects of aminoguanidine on time-dependent sensitization in zebrafish
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
  devtools::install_github(repo = "ACCLAB/dabestr", ref = "dev")
  library(dabestr)
}

#Load dataset for animals treated 30 min after CAS expsure
AG30 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/aminoguanidine/AG30.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "AG"))))
View(AG30)

#Reorganize data for plotting
AG30_forplot <- AG30 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_AG30_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = AG30)
summary(anova_AG30_tw)
TukeyHSD(anova_AG30_tw)

#Create object for plotting
multi_2group_AG30_tw <- load(AG30_forplot,
                         x = groups, y = Time_on_white,
                         idx = list(
                           c("CTRL_VEH", "CAS_VEH"),
                           c("CTRL_AG", "CAS_AG")
                         )
)

#Plot
multi_2group_AG30_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_AG30_en <- aov(formula = Entries ~ Treatment*Drug, data = AG30)
summary(anova_AG30_en)
TukeyHSD(anova_AG30_en)

#Create object for plotting
multi_2group_AG30_en <- load(AG30_forplot,
                            x = groups, y = Entries,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_AG30_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_AG30_dur <- aov(formula = Duration ~ Treatment*Drug, data = AG30)
summary(anova_AG30_dur)
TukeyHSD(anova_AG30_dur)

#Create object for plotting
multi_2group_AG30_dur <- load(AG30_forplot,
                            x = groups, y = Duration,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_AG30_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_AG30_ra <- aov(formula = Risk_assessment_N ~ Treatment*Drug, data = AG30)
summary(anova_AG30_ra)
TukeyHSD(anova_AG30_ra)

#Create object for plotting
multi_2group_AG30_ra <- load(AG30_forplot,
                            x = groups, y = Risk_assessment_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_AG30_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_AG30_es <- aov(formula = Erratic_swimming_N ~ Treatment*Drug, data = AG30)
summary(anova_AG30_es)
TukeyHSD(anova_AG30_es)

#Create object for plotting
multi_2group_AG30_es <- load(AG30_forplot,
                            x = groups, y = Erratic_swimming_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_AG30_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_AG30_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = AG30)
summary(anova_AG30_th)
TukeyHSD(anova_AG30_th)

#Create object for plotting
multi_2group_AG30_th <- load(AG30_forplot,
                            x = groups, y = Thigmotaxis_Percent,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_AG", "CAS_AG")
                            )
)

#Plot
multi_2group_AG30_th %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset for animals treated 90 min after CAS expsure
AG90 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/aminoguanidine/AG90.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "AG"))))
View(AG90)

#Reorganize data for plotting
AG90_forplot <- AG90 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_AG90_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = AG90)
summary(anova_AG90_tw)
TukeyHSD(anova_AG90_tw)

#Create object for plotting
multi_2group_AG90_tw <- load(AG90_forplot,
                             x = groups, y = Time_on_white,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_AG", "CAS_AG")
                             )
)

#Plot
multi_2group_AG90_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_AG90_en <- aov(formula = Entries ~ Treatment*Drug, data = AG90)
summary(anova_AG90_en)
TukeyHSD(anova_AG90_en)

#Create object for plotting
multi_2group_AG90_en <- load(AG90_forplot,
                             x = groups, y = Entries,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_AG", "CAS_AG")
                             )
)

#Plot
multi_2group_AG90_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_AG90_dur <- aov(formula = Duration ~ Treatment*Drug, data = AG90)
summary(anova_AG90_dur)
TukeyHSD(anova_AG90_dur)

#Create object for plotting
multi_2group_AG90_dur <- load(AG90_forplot,
                              x = groups, y = Duration,
                              idx = list(
                                c("CTRL_VEH", "CAS_VEH"),
                                c("CTRL_AG", "CAS_AG")
                              )
)

#Plot
multi_2group_AG90_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_AG90_ra <- aov(formula = Risk_assessment_N ~ Treatment*Drug, data = AG90)
summary(anova_AG90_ra)
TukeyHSD(anova_AG90_ra)

#Create object for plotting
multi_2group_AG90_ra <- load(AG90_forplot,
                             x = groups, y = Risk_assessment_N,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_AG", "CAS_AG")
                             )
)

#Plot
multi_2group_AG90_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_AG90_es <- aov(formula = Erratic_swimming_N ~ Treatment*Drug, data = AG90)
summary(anova_AG90_es)
TukeyHSD(anova_AG90_es)

#Create object for plotting
multi_2group_AG90_es <- load(AG90_forplot,
                             x = groups, y = Erratic_swimming_N,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_AG", "CAS_AG")
                             )
)

#Plot
multi_2group_AG90_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_AG90_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = AG90)
summary(anova_AG90_th)
TukeyHSD(anova_AG90_th)

#Create object for plotting
multi_2group_AG90_th <- load(AG90_forplot,
                             x = groups, y = Thigmotaxis_Percent,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_AG", "CAS_AG")
                             )
)

#Plot
multi_2group_AG90_th %>%
  mean_diff() %>%
  dabest_plot()