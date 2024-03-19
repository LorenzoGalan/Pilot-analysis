rm(list=ls(all=TRUE))

library(readxl)
library(dplyr)
library(openxlsx)
library(tidyverse)

#MW

db<-read_excel(choose.files())

db_wide <- db %>% select (Sujeto, item, response)
db_wide1 <- pivot_wider(db_wide, names_from= c(item), values_from=response) #descargar items x respuesta
db_wide2 <- db %>% select (Sujeto, item, rt)
Hb_wide2 <- pivot_wider(db_wide2, names_from= c(item), values_from=rt) #descargar rt x item


#Tengo que didivir MW por reporte o attention check, reporte es 1,3,5,7,9 y attention_check 2,4,6,8,10
# rtas attention: 3,0,2,1,1

db_reporte<- db_wide1[,c("1","3","5", "7", "9")]
colnames(db_reporte) <- paste("mw",colnames(db_reporte),sep="_")

db_attention <- db_wide1[,c("2","4","6","8","10")]
colnames(db_attention) <- paste("p",colnames(db_attention),sep="_")

#Esto anda pero los nulls son 0, para tener en cuenta

db_attention$p_2<-ifelse(db_attention$p_2=="3",1,0)
db_attention$p_4<-ifelse(db_attention$p_4=="0",1,0)
db_attention$p_6<-ifelse(db_attention$p_6=="2",1,0)
db_attention$p_8<-ifelse(db_attention$p_8=="1",1,0)
db_attention$p_10<-ifelse(db_attention$p_10=="1",1,0)

#on task = 0 y 1 == 1 ; of task = 2,3,4 == 0
#mismo caso de nulls

db_reporte$mw_1<-ifelse(db_reporte$mw_1=="0"|db_reporte$mw_1=="1" ,1,0)
db_reporte$mw_3<-ifelse(db_reporte$mw_3=="0"|db_reporte$mw_3=="1" ,1,0)
db_reporte$mw_5<-ifelse(db_reporte$mw_5=="0"|db_reporte$mw_5=="1" ,1,0)
db_reporte$mw_7<-ifelse(db_reporte$mw_7=="0"|db_reporte$mw_7=="1" ,1,0)
db_reporte$mw_9<-ifelse(db_reporte$mw_9=="0"|db_reporte$mw_9=="1" ,1,0)

#union mw
union_mw <- stack(db_reporte)

#grafico mw
#hago un recuento de la cantidad de respuestas

reporte_counts <- union_mw %>%
  mutate(value = factor(values)) %>%  
  group_by(value) %>%
  summarise(count = n())  

#Grafico

ggplot(reporte_counts, aes(x = value, y = count)) +
  geom_bar(stat = "identity") +  
  labs(title = "On task or Of task", x = "Value", y = "Count") +
  theme_classic()





#Explorando en R | aprendiendo
#summary(db_wide1)
#names(db_wide1)
#str(db_wide1)
#db_wide1$"1"
#db_wide1%>%
 # tally()
#db_wide1 %>% 
 # count("1", name = "1")

#db_wide1 %>% 
 # group_by(1) %>% 
  #tally(sort = TRUE)

# Cantidad de veces que hay un valor por columna
for (col in names(db_wide1)) {
  print(paste("Counts for", col, sep = " : "))
  print(table(db_wide1[[col]])) 
}







