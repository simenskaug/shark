# Created by Simen Skaug-Hønsi
# 25 sep. 2025

#preliminiaries
rm(list=ls())
gc()


#libraries ####
library(performance)
library(tidyverse)
library(boot)
library(GGally)
library(patchwork)
library(ggimage)

theme_bw()

# read the data
df_shark <- read.table("Shark.txt", sep="\t", 
                       header=T, fileEncoding="UTF-8") %>% 
  rename(Total_length = Total.Length..cm., 
         Dorsal_height = Dorsal.fin.height..cm.,
         Common_name = Common.name)

# making Common name a factor
df_shark <- df_shark %>%
  mutate(Common_name = factor(Common_name),
         Sex = factor(Sex))

# create dataframe fro each shark

df_GW <- df_shark %>% 
  filter(Common_name == "Great White")

df_LM <- df_shark %>% 
  filter(Total_length < 431 & Common_name == "Longfin Mako")

df_PB <- df_shark %>% 
  filter(Total_length < 366 & Common_name == "Porbeagle")

df_SA <- df_shark %>% 
  filter(Total_length < 301 & Common_name == "Salmon")

df_SM <- df_shark %>% 
  filter(Total_length < 401 & Common_name == "Shortfin Mako")


# build a model for each
lm_GW <- lm(Dorsal_height ~ Total_length,
             data = df_GW)

lm_LM <- lm(Dorsal_height ~ Total_length,
            data = df_LM)

lm_PB <- lm(Dorsal_height ~ Total_length,
            data = df_PB)

lm_SA <- lm(Dorsal_height ~ Total_length,
            data = df_SA)

lm_SM <- lm(Dorsal_height ~ Total_length,
            data = df_SM)


summary(lm_GW)

summary(df_SM$Total_length)

df_newGW <- tibble(Total_length = seq(120, 700, length= 100))
df_newLM <- tibble(Total_length = seq(118, 227, length= 100))
df_newPB <- tibble(Total_length = seq(81, 196, length= 100))
df_newSA <- tibble(Total_length = seq(33.5, 250, length= 100))
df_newSM <- tibble(Total_length = seq(74.24, 200, length= 100))


preds_matrixGW <- predict(lm_GW, newdata = df_newGW, int = "c")
preds_matrixLM <- predict(lm_LM, newdata = df_newLM, int = "c")
preds_matrixPB <- predict(lm_PB, newdata = df_newPB, int = "c")
preds_matrixSA <- predict(lm_SA, newdata = df_newSA, int = "c")
preds_matrixSM <- predict(lm_SM, newdata = df_newSM, int = "c")

df_predsGW <- bind_cols(df_newGW, preds_matrixGW)
df_predsLM <- bind_cols(df_newLM, preds_matrixLM)
df_predsPB <- bind_cols(df_newPB, preds_matrixPB)
df_predsSA <- bind_cols(df_newSA, preds_matrixSA)
df_predsSM <- bind_cols(df_newSM, preds_matrixSM)


# make plots ####

PGW <- df_predsGW %>% 
  ggplot(aes(x= Total_length, y= fit)) + 
  geom_line(color = "blue4") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha= 0.2) +
  geom_point(color = "dodgerblue3", data= df_GW, alpha= 0.5, aes(y= Dorsal_height)) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)",
       title = "Great White")

PLM <- df_predsLM %>% 
  ggplot(aes(x= Total_length, y= fit), xlim = 700) + 
  geom_line(color = "seagreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha= 0.2) +
  geom_point(color = "seagreen3", data= df_LM, alpha= 0.5, aes(y= Dorsal_height)) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)", 
      title = "Longfin Mako")

PPB <- df_predsPB %>% 
  ggplot(aes(x= Total_length, y= fit)) + 
  geom_line(color = "seagreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha= 0.2) +
  geom_point(color = "seagreen3", data= df_PB, alpha= 0.5, aes(y= Dorsal_height)) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)",
       title = "Porbeagle")

PSA <- df_predsSA %>% 
  ggplot(aes(x= Total_length, y= fit)) + 
  geom_line(color = "seagreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha= 0.2) +
  geom_point(color = "seagreen3", data= df_SA, alpha= 0.5, aes(y= Dorsal_height)) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)",
       title = "Salmon Shark")

PSM <- df_predsSM %>% 
  ggplot(aes(x= Total_length, y= fit)) + 
  geom_line(color = "seagreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha= 0.2) +
  geom_point(color = "seagreen3", data= df_SM, alpha= 0.5, aes(y= Dorsal_height)) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)",
       title = "Shortfin Mako")

plots <- PGW + PLM + PPB + PSA + PSM



