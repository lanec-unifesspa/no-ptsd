#Analysis scripts for data on the effects of ivabradine on time-dependent sensitization in zebrafish
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
IVA30 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/ivabradine/IVA30.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "IVA"))))
View(IVA30)

#Reorganize data for plotting
IVA30_forplot <- IVA30 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_IVA30_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_tw)
TukeyHSD(anova_IVA30_tw)

#Create object for plotting
multi_2group_IVA30_tw <- load(IVA30_forplot,
                         x = groups, y = Time_on_white,
                         idx = list(
                           c("CTRL_VEH", "CAS_VEH"),
                           c("CTRL_IVA", "CAS_IVA")
                         )
)

#Plot
multi_2group_IVA30_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_IVA30_en <- aov(formula = Entries ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_en)
TukeyHSD(anova_IVA30_en)

#Create object for plotting
multi_2group_IVA30_en <- load(IVA30_forplot,
                            x = groups, y = Entries,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_IVA", "CAS_IVA")
                            )
)

#Plot
multi_2group_IVA30_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_IVA30_dur <- aov(formula = Duration ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_dur)
TukeyHSD(anova_IVA30_dur)

#Create object for plotting
multi_2group_IVA30_dur <- load(IVA30_forplot,
                            x = groups, y = Duration,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_IVA", "CAS_IVA")
                            )
)

#Plot
multi_2group_IVA30_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_IVA30_ra <- aov(formula = Risk_assessment_N ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_ra)
TukeyHSD(anova_IVA30_ra)

#Create object for plotting
multi_2group_IVA30_ra <- load(IVA30_forplot,
                            x = groups, y = Risk_assessment_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_IVA", "CAS_IVA")
                            )
)

#Plot
multi_2group_IVA30_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_IVA30_es <- aov(formula = Erratic_swimming_N ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_es)
TukeyHSD(anova_IVA30_es)

#Create object for plotting
multi_2group_IVA30_es <- load(IVA30_forplot,
                            x = groups, y = Erratic_swimming_N,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_IVA", "CAS_IVA")
                            )
)

#Plot
multi_2group_IVA30_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_IVA30_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = IVA30)
summary(anova_IVA30_th)
TukeyHSD(anova_IVA30_th)

#Create object for plotting
multi_2group_IVA30_th <- load(IVA30_forplot,
                            x = groups, y = Thigmotaxis_Percent,
                            idx = list(
                              c("CTRL_VEH", "CAS_VEH"),
                              c("CTRL_IVA", "CAS_IVA")
                            )
)

#Plot
multi_2group_IVA30_th %>%
  mean_diff() %>%
  dabest_plot()

#Load dataset for animals treated 90 min after CAS expsure
IVA90 <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/no-ptsd/main/behavior/ivabradine/IVA90.csv", col_types = cols(Treatment = col_factor(levels = c("CTRL", "CAS")), Drug = col_factor(levels = c("VEH", "IVA"))))
View(IVA90)

#Reorganize data for plotting
IVA90_forplot <- IVA90 %>%
  unite(groups, c("Treatment", "Drug"))

#Run two-way ANOVA on "Time on white", followed by Tukey's HSD post-hoc test
anova_IVA90_tw <- aov(formula = Time_on_white ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_tw)
TukeyHSD(anova_IVA90_tw)

#Create object for plotting
multi_2group_IVA90_tw <- load(IVA90_forplot,
                             x = groups, y = Time_on_white,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_IVA", "CAS_IVA")
                             )
)

#Plot
multi_2group_IVA90_tw %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Entries on white", followed by Tukey's HSD post-hoc test
anova_IVA90_en <- aov(formula = Entries ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_en)
TukeyHSD(anova_IVA90_en)

#Create object for plotting
multi_2group_IVA90_en <- load(IVA90_forplot,
                             x = groups, y = Entries,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_IVA", "CAS_IVA")
                             )
)

#Plot
multi_2group_IVA90_en %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Average entry duration", followed by Tukey's HSD post-hoc test
anova_IVA90_dur <- aov(formula = Duration ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_dur)
TukeyHSD(anova_IVA90_dur)

#Create object for plotting
multi_2group_IVA90_dur <- load(IVA90_forplot,
                              x = groups, y = Duration,
                              idx = list(
                                c("CTRL_VEH", "CAS_VEH"),
                                c("CTRL_IVA", "CAS_IVA")
                              )
)

#Plot
multi_2group_IVA90_dur %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Risk assessment", followed by Tukey's HSD post-hoc test
anova_IVA90_ra <- aov(formula = Risk_assessment_N ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_ra)
TukeyHSD(anova_IVA90_ra)

#Create object for plotting
multi_2group_IVA90_ra <- load(IVA90_forplot,
                             x = groups, y = Risk_assessment_N,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_IVA", "CAS_IVA")
                             )
)

#Plot
multi_2group_IVA90_ra %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Erratic swimming", followed by Tukey's HSD post-hoc test
anova_IVA90_es <- aov(formula = Erratic_swimming_N ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_es)
TukeyHSD(anova_IVA90_es)

#Create object for plotting
multi_2group_IVA90_es <- load(IVA90_forplot,
                             x = groups, y = Erratic_swimming_N,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_IVA", "CAS_IVA")
                             )
)

#Plot
multi_2group_IVA90_es %>%
  mean_diff() %>%
  dabest_plot()

#Run two-way ANOVA on "Thigmotaxis", followed by Tukey's HSD post-hoc test
anova_IVA90_th <- aov(formula = Thigmotaxis_Percent ~ Treatment*Drug, data = IVA90)
summary(anova_IVA90_th)
TukeyHSD(anova_IVA90_th)

#Create object for plotting
multi_2group_IVA90_th <- load(IVA90_forplot,
                             x = groups, y = Thigmotaxis_Percent,
                             idx = list(
                               c("CTRL_VEH", "CAS_VEH"),
                               c("CTRL_IVA", "CAS_IVA")
                             )
)

#Plot
multi_2group_IVA90_th %>%
  mean_diff() %>%
  dabest_plot()
