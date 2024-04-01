#Analysis scripts for data on the effects of ODQ on time-dependent sensitization in zebrafish
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
ODQ30 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/ODQ/ODQ30.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "ODQ"))))
View(ODQ30)

#Reorganize data for plotting
ODQ30_forplot <- ODQ30 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_ODQ30_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_tw)
TukeyHSD(anova_ODQ30_tw)

#Create object for plotting
multi_2group_ODQ30_tw <- load(ODQ30_forplot,
                         x = groups, y = Time_on_white,
                         idx = list(
                           c("CTRL_VEH", "CAS_VEH"),
                           c("CTRL_ODQ", "CAS_ODQ")
                         )
)

#Plot
multi_2group_ODQ30_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_ODQ30_en <- aov(formula = Entries ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_en)
TukeyHSD(anova_ODQ30_en)

#Create object for plotting
multi_2group_ODQ30_en <- load(ODQ30_forplot,
                            x = groups, y = Entries,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_ODQ", "CAS_ODQ")
                            )
)

#Plot
multi_2group_ODQ30_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_ODQ30_dur <- aov(formula = Duration ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_dur)
TukeyHSD(anova_ODQ30_dur)

#Create object for plotting
multi_2group_ODQ30_dur <- load(ODQ30_forplot,
                            x = groups, y = Duration,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_ODQ", "CAS_ODQ")
                            )
)

#Plot
multi_2group_ODQ30_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_ODQ30_ra <- aov(formula = Risk_Assessment ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_ra)
TukeyHSD(anova_ODQ30_ra)

#Create object for plotting
multi_2group_ODQ30_ra <- load(ODQ30_forplot,
                            x = groups, y = Risk_Assessment,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_ODQ", "CAS_ODQ")
                            )
)

#Plot
multi_2group_ODQ30_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_ODQ30_es <- aov(formula = Erratic_Swimming_N ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_es)
TukeyHSD(anova_ODQ30_es)

#Create object for plotting
multi_2group_ODQ30_es <- load(ODQ30_forplot,
                            x = groups, y = Erratic_Swimming_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_ODQ", "CAS_ODQ")
                            )
)

#Plot
multi_2group_ODQ30_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_ODQ30_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = ODQ30)
summary(anova_ODQ30_th)
TukeyHSD(anova_ODQ30_th)

#Create object for plotting
multi_2group_ODQ30_th <- load(ODQ30_forplot,
                            x = groups, y = Thigmotaxis_Percent,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_ODQ", "CAS_ODQ")
                            )
)

#Plot
multi_2group_ODQ30_th %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset for animals treated 90 min after CAS expsure
ODQ90 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/ODQ/ODQ90.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "ODQ"))))
View(ODQ90)

#Reorganize data for plotting
ODQ90_forplot <- ODQ90 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_ODQ90_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_tw)
TukeyHSD(anova_ODQ90_tw)

#Create object for plotting
multi_2group_ODQ90_tw <- load(ODQ90_forplot,
                             x = groups, y = Time_on_white,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_ODQ", "CAS_ODQ")
                             )
)

#Plot
multi_2group_ODQ90_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_ODQ90_en <- aov(formula = Entries ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_en)
TukeyHSD(anova_ODQ90_en)

#Create object for plotting
multi_2group_ODQ90_en <- load(ODQ90_forplot,
                             x = groups, y = Entries,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_ODQ", "CAS_ODQ")
                             )
)

#Plot
multi_2group_ODQ90_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_ODQ90_dur <- aov(formula = Duration ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_dur)
TukeyHSD(anova_ODQ90_dur)

#Create object for plotting
multi_2group_ODQ90_dur <- load(ODQ90_forplot,
                              x = groups, y = Duration,
                              idx = list(
                                c("CTRL_VEH", "CAS_VEH"),
                                c("CTRL_ODQ", "CAS_ODQ")
                              )
)

#Plot
multi_2group_ODQ90_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_ODQ90_ra <- aov(formula = Risk_assessment ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_ra)
TukeyHSD(anova_ODQ90_ra)

#Create object for plotting
multi_2group_ODQ90_ra <- load(ODQ90_forplot,
                             x = groups, y = Risk_assessment,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_ODQ", "CAS_ODQ")
                             )
)

#Plot
multi_2group_ODQ90_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_ODQ90_es <- aov(formula = Erratic_Swimming ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_es)
TukeyHSD(anova_ODQ90_es)

#Create object for plotting
multi_2group_ODQ90_es <- load(ODQ90_forplot,
                             x = groups, y = Erratic_Swimming,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_ODQ", "CAS_ODQ")
                             )
)

#Plot
multi_2group_ODQ90_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_ODQ90_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = ODQ90)
summary(anova_ODQ90_th)
TukeyHSD(anova_ODQ90_th)

#Create object for plotting
multi_2group_ODQ90_th <- load(ODQ90_forplot,
                             x = groups, y = Thigmotaxis_Percent,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_ODQ", "CAS_ODQ")
                             )
)

#Plot
multi_2group_ODQ90_th %>%
  mean_diff() %>%
  dabest_plot()
