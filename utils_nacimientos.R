library(dplyr)

# Esta función no es nada eficiente, su propósito es únicamente didáctico.
get_nacimientos_para_pais<-function(pais){
  
  #Leemos el dataframe del la ONU de nacimientos
  df_births <- read.csv('datos/UNdata_birthdays.csv', stringsAsFactors = F)
  
  #Renombramos ciertos paises para hacerlos compatibles con el dataframe´ usando R base
  df_births$Country.or.Area[grepl('United Kingdom',df_births$Country.or.Area)]<-'England'
  df_births$Country.or.Area[grepl('United States',df_births$Country.or.Area)] <-'United States'
  
  #Renombramos los nombres de los meses usando la librería dplyr
  df_births <- df_births %>% mutate(
    Month=case_when(
      Month=='January' ~ 'ene',
      Month=='February' ~ 'feb',
      Month=='March' ~ 'mar',
      Month=='April' ~ 'abr',
      Month=='May' ~ 'may',
      Month=='June' ~ 'jun',
      Month=='July' ~ 'jul',
      Month=='August' ~ 'ago',
      Month=='September' ~ 'sep',
      Month=='October' ~ 'oct',
      Month=='November' ~ 'nov',
      Month=='December' ~ 'dic',
    )
  ) %>% tidyr::drop_na(Month)
  
  df_births$Month<-factor(df_births$Month, levels = c('ene','feb','mar','abr','may','jun','jul','ago','sep','oct','nov','dic'))
  
  
  df_births_grouped_by_month <- df_births %>% group_by(Country.or.Area,Month) %>% summarise(num=sum(Value), .groups='drop')
  out <- df_births_grouped_by_month %>% filter(Country.or.Area==pais)
  nacimientos <- out$num
  names(nacimientos)<-out$Month
  tbl_out <- as.table(nacimientos)

  return(tbl_out)
}

