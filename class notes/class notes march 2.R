#2023-02-03


#generating new data
#but this won't work on this computer bc i don't have the variables on here from last class
#save this and send to self and then get one from other class and add this all to it


newdata= penguins_lm_3 #this actually is not going to work either bc walked in late and didn't get whole bunch of code, something about hw tho
  tidyr::expand(bill_length_mm, species)
  
  
  
#interaction term
lm_4= lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm:species, 
        data=penguins_lm_3)
#another way to write the exact same function
lm_4= lm(bill_depth_mm ~ bill_length_mm * species, 
        data=penguins_lm_3)

lm_4= lm(bill_depth_mm ~ bill_length_mm * species, 
        data=penguins_lm_3)
summary(lm_4)

AIC(lm_3, lm_4)
best_model = step(lm_4)
summary(best_model)

#plot with the new interaction
lm_4_predict = lm_4 %>%
  broom::augment(interval="confidence")
head(lm_4_predict)

ggplot(data=lm_4_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_line(aes(y=.fitted, x=bill_length_mm, color=species))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x+bill_length_mm, fill=species), alpha=0.5)


#depth ~ bill_length + flipper_length
library(car) #vif()

gentoo=penguins %>%
  filter(species=="Gentoo")

lm_gentoo_1=lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2= lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3= lm(bill_depth_mm ~bill_length_mm +flipper_length_mm +body_mass_g, data=gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_3)
vif(lm_gentoo_3)

head(penguins_lm_3)

#below we are looking at bill length and bill depth while holding flipper length and body mass constant

newdata= gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE), 
         body_mass_g=median(gentoo$body_mass_g, na.rm=TRUE))
  
head(newdata)
lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")
head(lm_gentoo_3_predict)

ggplot(data=lm_gentoo_3_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=bill_length_mm))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm), alpha=0.5)+
  annotate("text", x=57, y=13.5, 
           label=paste0("flipper length = ",
                        median(gentoo$flipper_length_mm, na.rm=TRUE), 
                        "mm"))+
  annotate("text", x=57, y=13.9, 
           label=paste0("body mass = ",
                        median(gentoo$body_mass_g, na.rm=TRUE), 
                        "g"))


#Class exercise 5.3
#looking for variation in bill depth vs. FLIPPER LENGTH, while bill length and body mass are constant at the medians
newdata= gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE), 
         body_mass_g=median(gentoo$body_mass_g, na.rm=TRUE))
head(newdata)

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")
head(lm_gentoo_3_predict)

ggplot(data=lm_gentoo_3_predict) +
  geom_point(aes(x=flipper_length_mm, y=bill_depth_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=flipper_length_mm))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=flipper_length_mm), alpha=0.5)+
  annotate("text", x=225, y=13.5, 
           label=paste0("bill length = ",
                        median(gentoo$bill_length_mm, na.rm=TRUE), 
                        "mm"))+
  annotate("text", x=225, y=13.9, 
           label=paste0("body mass = ",
                        median(gentoo$body_mass_g, na.rm=TRUE), 
                        "g"))


# ANOVA
penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
anova(penguins_lm)

penguin_anova= aov(body_mass_g~species + sex, data=penguins)
summary(penguin_anova)
TukeyHSD(penguin_anova)











