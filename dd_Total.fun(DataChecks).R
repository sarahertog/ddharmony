#--------------------------------------------------
# check if Totals after having standardized age 
# series are equal or different from previous values

# ---------
# Function
# ---------
NewTotal.fun <- function(data){
  require(dplyr)
  
  locid = unique(na.omit(data$LocID))
  abr = unique(data$abridged)
  cpl = unique(data$complete)
  
  data1 <- data %>%  
    filter(AgeSpan==max(AgeSpan) | (AgeSpan==-1 & AgeStart==max(AgeStart))) %>% 
    group_by(id,
             SexID) %>% 
    summarise(DataValue=sum(DataValue)) %>% 
    ungroup() %>% 
    mutate(AgeLabel='Total',
           AgeStart=0, AgeEnd=-1, AgeSpan=-1,
           LocID=locid,
           abridged=abr,
           complete=cpl)
  
  data.out <- suppressMessages(full_join(data%>%
                                             filter(AgeLabel != 'Total'),
                                         data1)) 
  return(data.out)
}


  