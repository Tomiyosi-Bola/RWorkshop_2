circa = read.csv(file.choose())

View(circa)

library(dplyr)



#dplyr::sample_n(circa, 6)

levels(circa$treatment)

circa$treatment = ordered(circa$treatment, levels= c("control", "knee", "eyes"))

levels(circa$treatment)


group_by(circa, treatment) %>% 
  summarize(
    count = n(),
    mean= mean(shift, na.rm = T),
    sd = sd(shift, na.rm = T)
  )


#if(!require(ggpubr)) {install.packages("ggpubr"); library(ggpubr)}

# Install the ggpubr package to have a publication friendly plots

install.packages("ggpubr")

library(ggpubr)


ggboxplot(circa, x = "treatment", y = "shift", color = "treatment", palette = c ("#56B4E9", "#F0E442", "#CC79A7"),
order = c ("control", "eyes", "knee"),
ylab = "Shift", xlab = "Treatment")



#Anova test

circa_ano = lm (shift ~ treatment, data = circa)

circa_ano



anova(circa_ano)


#Test for ANOVA assumption

# Test for homogeneity 

plot(circa_ano, 1)

library(car)

leveneTest(shift~factor(treatment), data = circa)



#Test for normality

plot(circa_ano, which = 2)


circa_residual <- residuals(object = circa_ano)
shapiro.test (circa_residual)


# Since ANOVA is significant, we should conduct a posthoc test to see the difference in means 


#circa_aov = aov(shift~treatment, data = circa)  #2nd way to computer anova instead of using lm

TukeyHSD(circa_aov)


toad = read.csv(file.choose(), header = T)

toad_ano = lm(Call ~ Predator*Survey, data = toad)

anova(toad_ano)

# Things to do (hint- look up for all the code)
# Test for homgeneity
# Test for normality

install.packages("psych")
library(psych)

toad_sum = toad %>%
  group_by(Survey, Predator) %>% 
  summarise(y_mean = mean(Call), y_se = psych::describe(Call)$se)

toad_sum


toad_sum %>% 
  ggplot(aes(x= Survey, y = y_mean, color = Predator)) + 
  geom_line(aes(group = Predator)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = y_mean-1.5*y_se,
                    ymax = y_mean+1.5*y_se),
                width = 0.5) + 
  labs(x = "Survey", color = "Predator", y = "toad survey") + 
  theme_classic()

