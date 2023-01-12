#Final
#pokemon
library(tidyverse)
library(rpart)
library(rpart.plot)
library(ggplot2)

#PREDICTION-1: Are Pokemon getting stronger over time?

pk<-read_csv("/Users/rishpednekar/Desktop/pokemon.csv")
View(pk)
pokemon<-as.data.frame(pk)
#Data_Viz
#Team_Builder

#Check_Number_of_Pokemon_per_Generation
ggplot(data = pokemon,
       mapping = aes(generation,frequency(generation),fill=as.factor(generation)))+geom_col()+ylab("Number of Pokemon")+labs(title="Number of Pokemon Across Generations")+xlab("Generation")

#Comparing_on_regions_for_Legendary_Pokemon
ggplot(data = pk,
       mapping = aes(x=generation,y=is_legendary,fill=as.generation))+geom_col()+labs(title = "Legendaries")+xlab("Generation")+ylab("Amount of Legendaries")

#strength_of_attack_and_defense_and_sp_and_defense
#replace 0 and 1 with Legendary as True or False
pokemon$is_legendary<-as.logical(pokemon$is_legendary)
ggplot(data=pokemon,
       mapping = aes(attack,y=defense,colour=is_legendary))+geom_point(size=0.3)+labs(title = "Attack & Defense")+facet_wrap(~generation) +xlab("Attack") + ylab("Defense")
ggplot(data=pokemon,
       mapping = aes(sp_attack,y=sp_defense,colour=is_legendary))+geom_point(size=0.2)+labs(title = "Special Attack & Special Defense")+facet_wrap(~generation)+xlab("Special Attack") + ylab("Special Defense")

#------------------------------------
# Hypothesis Testing - Comparing 2 Special Attacking Types
# Null: Psychic have a higher base stat total than Fairies
# Alternate: Fairies have a higher base stat total than Psychic
# This can be useful when adding an addition to your team.
# NOTE: Legendaries will not be included.
fairies<-subset(pokemon,(pokemon$type1=="fairy"|pokemon$type2=="fairy")&pokemon$is_legendary==FALSE)
view(fairies)
psychic<-subset(pokemon,(pokemon$type1=="psychic"|pokemon$type2=="psychic")&pokemon$is_legendary==FALSE)
view(psychic)

#getting average base stat total for both
avg_base_stat_total_fairies<-mean(fairies$base_total)
avg_base_stat_total_fairies#393.825
avg_base_stat_total_psychic<-mean(psychic$base_total)
avg_base_stat_total_psychic#416.7377

sd_base_stat_total_fairies<-sd(fairies$base_total)
sd_base_stat_total_fairies#116.112
sd_base_stat_total_psychic<-sd(psychic$base_total)
sd_base_stat_total_psychic#108.3057

n_fairies<-nrow(fairies)
n_fairies#40
n_p<-nrow(psychic)
n_p#61

z_score = (avg_base_stat_total_psychic-avg_base_stat_total_fairies)/sqrt((sd_base_stat_total_fairies^2/n_fairies)+(sd_base_stat_total_psychic^2/n_p))
z_score
p <- 1 - pnorm(z_score)
p
#The p-value is more than the significance level, thus we fail to reject the null hypothesis.
# Psychic pokemon, on average, are stronger than fairies

# Prediction For Class: What are the 3 strongest types and will the strongest types be the happiest - guess for class?
# 18 types
unique(pokemon$type1)
grass<-subset(pokemon,(pokemon$type1=="grass"|pokemon$type2=="grass")&pokemon$is_legendary==FALSE)
fire<-subset(pokemon,(pokemon$type1=="fire"|pokemon$type2=="fire")&pokemon$is_legendary==FALSE)
water<-subset(pokemon,(pokemon$type1=="water"|pokemon$type2=="water")&pokemon$is_legendary==FALSE)
bug<-subset(pokemon,(pokemon$type1=="bug"|pokemon$type2=="bug")&pokemon$is_legendary==FALSE)
normal<-subset(pokemon,(pokemon$type1=="normal"|pokemon$type2=="normal")&pokemon$is_legendary==FALSE)
poison<-subset(pokemon,(pokemon$type1=="poison"|pokemon$type2=="poison")&pokemon$is_legendary==FALSE)
electric<-subset(pokemon,(pokemon$type1=="electric"|pokemon$type2=="electric")&pokemon$is_legendary==FALSE)
ground<-subset(pokemon,(pokemon$type1=="ground"|pokemon$type2=="ground")&pokemon$is_legendary==FALSE)
fighting<-subset(pokemon,(pokemon$type1=="fighting"|pokemon$type2=="fighting")&pokemon$is_legendary==FALSE)
rock<-subset(pokemon,(pokemon$type1=="rock"|pokemon$type2=="rock")&pokemon$is_legendary==FALSE)
ghost<-subset(pokemon,(pokemon$type1=="ghost"|pokemon$type2=="ghost")&pokemon$is_legendary==FALSE)
ice<-subset(pokemon,(pokemon$type1=="ice"|pokemon$type2=="ice")&pokemon$is_legendary==FALSE)
dragon<-subset(pokemon,(pokemon$type1=="dragon"|pokemon$type2=="dragon")&pokemon$is_legendary==FALSE)
dark<-subset(pokemon,(pokemon$type1=="dark"|pokemon$type2=="dark")&pokemon$is_legendary==FALSE)
steel<-subset(pokemon,(pokemon$type1=="steel"|pokemon$type2=="steel")&pokemon$is_legendary==FALSE)
flying<-subset(pokemon,(pokemon$type1=="flying"|pokemon$type2=="flying")&pokemon$is_legendary==FALSE)

#scale_color_manual_here
colors()
colors <- c("darkgreen","black","darkblue","yello2","pink","orange",
            "red","grey","purple", "green","tan3","lightblue","tan","purple3",
            "hotpink1","brown2","grey4","lightcyan")

#What are the most widespread types of pokemon in both primary and secondary types?
#seeing primary types
ggplot(data = pk,
       mapping = aes(base_total,base_happiness,color=type1))+geom_point(size=0.5)+facet_wrap(~type1)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))

#seeing if secondary type had any affect on any type
ggplot(data = pk,
       mapping = aes(base_total,base_happiness,color=type2))+geom_point(size=0.5)+facet_wrap(~type2)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))

#comparing_histograms
hist(grass$base_total,main = "Histogram of Grass Base Total",col = "green3",xlab = "Base Total",ylab = "Frequency")
hist(fire$base_total,main = "Histogram of Fire Base Total",col = "orange",xlab = "Base Total",ylab = "Frequency")
hist(water$base_total,main = "Histogram of Water Base Total",col = "slateblue1",xlab = "Base Total",ylab = "Frequency")
hist(bug$base_total,main = "Histogram of Bug Base Total",col = "darkgreen",xlab = "Base Total",ylab = "Frequency")
hist(normal$base_total,main = "Histogram of Normal Base Total",col = "tan",xlab = "Base Total",ylab = "Frequency")
hist(poison$base_total,main = "Histogram of Poison Base Total",col = "purple",xlab = "Base Total",ylab = "Frequency")
hist(electric$base_total,main = "Histogram of Electric Base Total",col = "yellow1",xlab = "Base Total",ylab = "Frequency")
hist(ground$base_total,main = "Histogram of Ground Base Total",col = "brown",xlab = "Base Total",ylab = "Frequency")
hist(fairies$base_total,main = "Histogram of Fairies Base Total",col = "pink",xlab = "Base Total",ylab = "Frequency")
hist(fighting$base_total,main = "Histogram of Fighting Base Total",col = "orange",xlab = "Base Total",ylab = "Frequency")
hist(psychic$base_total,main = "Histogram of Psychic Base Total",col = "hotpink1",xlab = "Base Total",ylab = "Frequency")
hist(rock$base_total,main = "Histogram of Rock Base Total",col = "tan3",xlab = "Base Total",ylab = "Frequency")
hist(ghost$base_total,main = "Histogram of Ghost Base Total",col = "purple",xlab = "Base Total",ylab = "Frequency")
hist(ice$base_total,main = "Histogram of Ice Base Total",col = "lightblue",xlab = "Base Total",ylab = "Frequency")
hist(dragon$base_total,main = "Histogram of Dragon Base Total",col = "darkblue",xlab = "Base Total",ylab = "Frequency")
hist(dark$base_total,main = "Histogram of Dark Base Total",col = "black",xlab = "Base Total",ylab = "Frequency")
hist(steel$base_total,main = "Histogram of Steel Base Total",col = "grey",xlab = "Base Total",ylab = "Frequency")
hist(flying$base_total,main = "Histogram of Flying Base Total",col = "skyblue1",xlab = "Base Total",ylab = "Frequency")
#--------------------------------------------------------------
#Bayesian Inference
# What is the probability that a Pokemon will be a legendary given it is a Dragon type rather
# just being a legendary?
# P(Legendary|Dragon) compared to all Legendary P(Legendary)

nrow(pokemon)#801
legends<-subset(pokemon,pokemon$is_legendary==TRUE)
nrow(legends)
#70
prob_legendary<-70/801

nrow(dragon)#33
prob_dragon<-33/801

#prior->70/801

#P(Dragon|Legendary)
sub<-subset(pokemon,pokemon$is_legendary==TRUE&(pokemon$type1=="dragon"|pokemon$type2=="dragon"))
view(sub)
nrow(sub)#11
p1<-11/nrow(legends)

#posterior
posterior_1<-(p1*prob_legendary)/prob_dragon
posterior_1

#----------------------------------------------------------------------------
#Data Visualization
#plot of how height and weight affects base stats
ggplot(data = pokemon,
       mapping = aes(base_total,weight_kg,color=type1))+geom_point(size=0.4)+facet_wrap(~type1)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))+xlab("Base Total")+ylab("Weight(kg)")

ggplot(data = pokemon,
       mapping = aes(base_total,height_m,color=type1))+geom_point(size=0.4)+facet_wrap(~type1)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))+xlab("Base Total")+ylab("Height(m)")

#-------secondary------------
ggplot(data = pokemon,
       mapping = aes(base_total,weight_kg,color=type2))+geom_point(size=0.4)+facet_wrap(~type2)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))+xlab("Base Total")+ylab("Weight")

ggplot(data = pokemon,
       mapping = aes(base_total,height_m,color=type2))+geom_point(size=0.4)+facet_wrap(~type2)+
  scale_color_manual(values = c("darkgreen","black","darkblue","yellow2","pink","orange",
                                "red","grey","purple", "green","tan3","lightblue","tan","purple4",
                                "hotpink1","brown2","grey4","slateblue1"))+xlab("Base Total")+ylab("Height")

