library(ggplot2)
library(here)
library(tidyverse)

airyards <- read_csv(here("content", "data", "New wpDataTable.csv"))


det <- airyards %>% 
        filter(Team == "DET")


ggplot(det) +
  geom_col(aes(y = Name, x = Targets))


ggplot(det) +
  geom_col(aes(x = "", y = Targets, fill = Name)) +
  coord_polar("y", start=0) +
  #theme_void() +
  geom_text(aes(label = paste0(Targets, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)

?geom_text

  scale_fill_manual(rep(c("0076B6", "B0B7BC"), length.out = 11))

#0076B6
#B0B7BC

rep(c("0076B6", "B0B7BC"), length.out = 11)        

c("0076B6", "B0B7BC")

det <- mutate(det, targ_share = round((Targets / sum(Targets)) * 100, digits = 1), targ_label = paste(targ_share, '%', sep = ""))
det <- mutate(det, Name = factor(Name, levels = as.character(Name)))

?as.factor

ggplot(det,aes(x = "", y = Targets)) +
  geom_col(aes(fill= Name)) +
  #coord_polar("y", start=0) +
  geom_text(aes(label = rev(targ_label)))#, position = position_stack(vjust=0.9)) +
  theme_void()

rev(det$targ_label)

ggplot(det,aes(x = "", y = Targets)) +
  geom_col(aes(fill= Name)) +
  geom_text(aes(label = rev(targ_label)), position = position_stack(vjust= 0.5)) +
  coord_polar("y", start=0)



tidykids
tidykids <- read_csv(here("content", "data", "tidykids.csv"))
