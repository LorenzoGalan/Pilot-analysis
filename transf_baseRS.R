rm(list=ls(all=TRUE))

library(readxl)
library(tidyverse)
library(psych)
library(ggplot2)

## RUNNING SPAN

# base running span
dbwm<-read_excel(choose.files())
db<-dbwm

# elim consigna/estimulo
db <- db %>% filter(if_any(response, ~ !(.x %in% c('null'))))

# agregar columna item
db$item<-rep(1:15,36)

# elim segunda respuesta de ayelenleroux@gmail.com (sujeto 25)
db <- db %>% filter(if_any(Sujeto, ~ !(.x %in% 25)))

# check que haya 15 respuestas por participante
summary(as.factor(db$participant_id))

# sacar "Q0" de la respuesta
db$response<-gsub("Q0","",as.character(db$response))

# wide
db_wide1 <- db %>% select (participant_id, item, response)
db_wide1 <- pivot_wider(db_wide1, names_from= c(item), values_from=response)
colnames(db_wide1) <- paste("wm",colnames(db_wide1),sep="_") 

# wide rt
db_wide2 <- db %>% select (participant_id, item, rt)
db_wide2 <- pivot_wider(db_wide2, names_from= c(item), values_from=rt)
colnames(db_wide2) <- paste("wmrt",colnames(db_wide2),sep="_") 

# de respuesta a acierto (0/1)
db_wide1$wm_1<-ifelse(db_wide1$wm_1=="KL"|db_wide1$wm_1=="kl",1,0)
db_wide1$wm_2<-ifelse(db_wide1$wm_2=="TG"|db_wide1$wm_1=="tg",1,0)
db_wide1$wm_3<-ifelse(db_wide1$wm_3=="JS"|db_wide1$wm_1=="js",1,0)
db_wide1$wm_4<-ifelse(db_wide1$wm_4=="QSH"|db_wide1$wm_1=="qsh",1,0)
db_wide1$wm_5<-ifelse(db_wide1$wm_5=="QNL"|db_wide1$wm_1=="qnl",1,0)
db_wide1$wm_6<-ifelse(db_wide1$wm_6=="NLQ"|db_wide1$wm_1=="nlq",1,0)
db_wide1$wm_7<-ifelse(db_wide1$wm_7=="KNTJ"|db_wide1$wm_1=="kntj",1,0)
db_wide1$wm_8<-ifelse(db_wide1$wm_8=="TFNP"|db_wide1$wm_1=="tfnp",1,0)
db_wide1$wm_9<-ifelse(db_wide1$wm_9=="GSTR"|db_wide1$wm_1=="gstr",1,0)
db_wide1$wm_10<-ifelse(db_wide1$wm_10=="QFRJH"|db_wide1$wm_1=="qfrjh",1,0)
db_wide1$wm_11<-ifelse(db_wide1$wm_11=="FHKLS"|db_wide1$wm_1=="fhkls",1,0)
db_wide1$wm_12<-ifelse(db_wide1$wm_12=="PFGRN"|db_wide1$wm_1=="pfgrn",1,0)
db_wide1$wm_13<-ifelse(db_wide1$wm_13=="FJNKLQ"|db_wide1$wm_1=="fjnklq",1,0)
db_wide1$wm_14<-ifelse(db_wide1$wm_14=="QSFNHG"|db_wide1$wm_1=="qsfnhg",1,0)
db_wide1$wm_15<-ifelse(db_wide1$wm_15=="HNGFTJ"|db_wide1$wm_1=="hngftj",1,0)

# alpha (sin regla de interrupcion)
it<-db_wide1[,2:16]
alpha(it)

# grafico aciertos por set size
ss<-mutate(it, ss2 = rowMeans(select(it, 1:3), na.rm = TRUE)) %>%
  mutate(it, ss3 = rowMeans(select(it, 4:6), na.rm = TRUE)) %>%
  mutate(it, ss4 = rowMeans(select(it, 7:9), na.rm = TRUE)) %>%
  mutate(it, ss5 = rowMeans(select(it, 10:12), na.rm = TRUE)) %>%
  mutate(it, ss6 = rowMeans(select(it, 13:15), na.rm = TRUE))

sslong <- gather(data = ss[,16:20], key = "setsize", value = "score")

longsummary <- sslong %>%
  group_by(setsize) %>%
  summarise( 
    mean = mean(score, na.rm = TRUE),
    sem = sd(score, na.rm = TRUE) / sqrt(n()))

plotss<-longsummary %>%
  ggplot(aes(
    x = setsize,
    y = mean,
    ymin = mean - sem,
    ymax = mean + sem)) +
  geom_col() + 
  geom_errorbar(width = .3)+
  theme_minimal() +
  labs(y = "Proporcion de aciertos", x = "serie") +
  theme(text = element_text(size = 15))

plotss