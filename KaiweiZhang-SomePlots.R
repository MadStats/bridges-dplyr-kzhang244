library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library("ggplot2")

load("allStates.RData")
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" ,"MAINTENANCE_021", "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C" ,"FUTURE_ADT_114"  )

x= select(M,one_of(keep))

# Question: how is the future ADT and current ADT related taking rating into account

wi = filter(x,STATE_CODE_001 ==55)
wi = wi %>% mutate(rating = pmin(DECK_COND_058 , SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062,na.rm = T))
wi[-which(wi$rating==0),]%>%ggplot(mapping = aes(x =ADT_029,y = FUTURE_ADT_114 ))+geom_point()+geom_smooth()+facet_wrap(~ rating, nrow = 3)+ggtitle("Future ADT vs. ADT of Different Rating in WI")

ca = filter(x,STATE_CODE_001 ==6 )
ca = ca %>% mutate(rating = pmin(DECK_COND_058 , SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062,na.rm = T))
ca[-which(ca$rating==0),]%>%ggplot(mapping = aes(x =ADT_029,y = FUTURE_ADT_114 ))+geom_point()+geom_smooth()+facet_wrap(~ rating, nrow = 3)+ggtitle("Future ADT vs. ADT of Different Rating in CA")

# Year built and rating

wi%>%ggplot(mapping = aes(x =YEAR_BUILT_027,fill= rating ))+geom_bar(position = "identity")+ggtitle("Bar Plot of Bridges Built in Different Year with Rating in WI")
ca%>%ggplot(mapping = aes(x =YEAR_BUILT_027,fill= rating ))+geom_bar(position = "identity")+ggtitle("Bar Plot of Bridges Built in Different Year with Rating in CA")
