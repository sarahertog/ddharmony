#----------------------------------------------------------------------
# Check if Population counts by sex is equal to 
# population counts for both sexes and adjust in case of any difference

# Criteria: If Female Population + Male Population is different from the population for Both sexes,
# Both sexes = Female + Male

# ---------
# Function
# ---------
DiffSex.fun <- function(data){
  data1 <- data %>% 
    filter(SexID==1 |SexID==2 ) %>% 
    group_by(id,
             AgeLabel) %>% 
    summarise(NewValue=sum(DataValue)) %>% 
    ungroup()
  
  data2 <- data %>% 
    filter(SexID==3) 
  
  data3 <- suppressMessages(left_join(x=data1,data2) %>% 
    mutate(Difference=NewValue-DataValue,
           DataValue=case_when(Difference!=0 ~ NewValue,
                               Difference==0 ~ DataValue),
           Difference2=NewValue-DataValue) )
  
  data3 <- data3 %>% 
    select(!c(Difference,Difference2, NewValue)) 
  
  data.out=bind_rows(data %>% filter(SexID!=3),data3)
  return(data.out)
}

