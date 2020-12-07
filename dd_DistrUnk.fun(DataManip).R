#------------------------
# Distribute Unknowns

# ---------
# Function
# ---------
DistrUnk.fun <- function(data){
  require(tidyverse)
  
  data1 <- data %>% 
    filter(AgeLabel!="Total" , AgeLabel!="Unknown") %>% 
    filter(!is.na(DataValue),!is.na(SexID)) %>%  
    group_by(id,
             SexID 
    ) %>% 
    mutate(pch=DataValue/sum(DataValue)) %>%  
    ungroup() 
  
  data2 <- data %>%
    filter(AgeLabel=="Unknown" , !is.na(SexID)) %>% 
    mutate(DataValue=case_when(is.na(DataValue)==T~0,
                               TRUE ~ DataValue)) 
  
  data.out <- bind_rows(data1,data2) %>%
    group_by(id,
             SexID ) %>% 
    mutate(Unknown = round(pch*DataValue[which(AgeLabel =="Unknown")],0),
           DataValueNew=round(DataValue+Unknown,0)) %>% 
    filter(AgeLabel!='Unknown') %>% 
    group_by(id,
             SexID 
    ) %>% 
    select(!c(Unknown,pch,DataValue)) %>% 
    pivot_wider(names_from = AgeLabel, values_from = DataValueNew) %>% 
    pivot_longer(cols=!c("AgeStart",  "AgeEnd", "AgeSpan",   "AgeSort",   "SexID",    
                         "note",  "abridged",  "complete", "series", "id",  "id_series",
                         "DataSourceYear" ,"LocName" , "LocID" ,"ReferencePeriod","TimeMid", "DataSourceName",
                         "StatisticalConceptName", "DataTypeName","DataSeriesID","DataReliabilityName",
                         "DataReliabilitySort"),
                 names_to = "AgeLabel", 
                 values_to = "DataValueNew") %>% 
    filter(is.na(DataValueNew)==F)   %>% 
    rename(DataValue=DataValueNew)
  
  return(data.out)
  
}
