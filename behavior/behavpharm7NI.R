#Analysis scripts for data on the effects of 7-NI on time-dependent sensitization in zebrafish
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

#Load dataset
X7NI <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/7NI.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "7NI"))))
View(X7NI)

#Reorganize data for plotting
X7NI_forplot <- X7NI %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_7NI_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_tw)
TukeyHSD(anova_7NI_tw)

#Create object for plotting
multi_2group_7NI_tw <- load(X7NI_forplot,
                         x = groups, y = Time_on_white,
                         idx = list(
                           c("CTRL_VEH", "CAS_VEH"),
                           c("CTRL_7NI", "CAS_7NI")
                         )
)

#Plot
multi_2group_7NI_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_7NI_en <- aov(formula = Entries ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_en)
TukeyHSD(anova_7NI_en)

#Create object for plotting
multi_2group_7NI_en <- load(X7NI_forplot,
                            x = groups, y = Entries,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_7NI", "CAS_7NI")
                            )
)

#Plot
multi_2group_7NI_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_7NI_dur <- aov(formula = Duration ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_dur)
TukeyHSD(anova_7NI_dur)

#Create object for plotting
multi_2group_7NI_dur <- load(X7NI_forplot,
                            x = groups, y = Duration,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_7NI", "CAS_7NI")
                            )
)

#Plot
multi_2group_7NI_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_7NI_ra <- aov(formula = Risk_assessment_N ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_ra)
TukeyHSD(anova_7NI_ra)

#Create object for plotting
multi_2group_7NI_ra <- load(X7NI_forplot,
                            x = groups, y = Risk_assessment_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_7NI", "CAS_7NI")
                            )
)

#Plot
multi_2group_7NI_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_7NI_es <- aov(formula = Erratic_swimming_N ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_es)
TukeyHSD(anova_7NI_es)

#Create object for plotting
multi_2group_7NI_es <- load(X7NI_forplot,
                            x = groups, y = Erratic_swimming_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_7NI", "CAS_7NI")
                            )
)

#Plot
multi_2group_7NI_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_7NI_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = X7NI)
summary(anova_7NI_th)
TukeyHSD(anova_7NI_th)

#Create object for plotting
multi_2group_7NI_th <- load(X7NI_forplot,
                            x = groups, y = Thigmotaxis_Percent,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_7NI", "CAS_7NI")
                            )
)

#Plot
multi_2group_7NI_th %>%
  mean_diff() %>%
  dabest_plot()
