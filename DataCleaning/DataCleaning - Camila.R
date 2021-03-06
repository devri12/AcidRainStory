---
  title: "Cleaning Data For Exploratory Water Viz"
output: html_notebook
---
  
  ```{r message=FALSE}
#Libraries
#detach(package:plyr) ### Run once.. Important to get the weighted average to work. 
library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
library(magrittr)
library(stats)
library(rio)
library(ggplot2)
library(plotly)
library(data.table)
library(ggthemes)
```

#Functions
```{r}

#load in the data from the download
precip_stream_data <- read_rds("D:/Duke/Work(Environ)/Programming/AcidRainStory/DataCleaning/precip_stream_data.rds")

#Function imports various dataframes into the global environment given a list with the file names, 
#the file extension, the path to the files, and the naming convention for the imported data frames. 

importf <- function(alist, extension, apath, dfname){
  dfList <- lapply(alist, function(x){
    return(read_csv(paste(apath, x, extension, sep = "")))
  })
  for (i in seq(dfList))
    assign(paste("w",i, dfname, sep = ""), dfList[[i]], envir=.GlobalEnv)
}


#Applying lapply and then saving the output dfs back to the global environment Applies a function 
#to each data frame in a list of data frames, and saves them to the global environment. 

lapply_and_save <- function(a_df_list, a_function, optional_argument1, optional_argument2, optional_argument3, a_name) {
  if(missing(optional_argument1) & missing(optional_argument2) & missing(optional_argument3)) {
    temp_df <- lapply(a_df_list, a_function)
    for (i in seq(temp_df))
      assign(paste("w",i, a_name, sep = ""), temp_df[[i]], envir=.GlobalEnv)
  }
  else if(missing(optional_argument2) & missing(optional_argument3)){
    temp_df <- lapply(a_df_list, a_function, optional_argument1)
    for (i in seq(temp_df))
      assign(paste("w",i, a_name, sep = ""), temp_df[[i]], envir=.GlobalEnv)
  }
  
  else if(missing(optional_argument3)){
    temp_df <- lapply(a_df_list, a_function, optional_argument1, optional_argument2)
    for (i in seq(temp_df))
      assign(paste("w",i, a_name, sep = ""), temp_df[[i]], envir=.GlobalEnv)
  }
  else {
    temp_df <- lapply(a_df_list, a_function, optional_argument1,
                      optional_argument2, optional_argument3) 
    for (i in seq(temp_df))
      assign(paste("w",i, a_name, sep = ""), temp_df[[i]], envir=.GlobalEnv)
  }}


#Replaces a value (by default -3) throughout a dataframe with another value (by default NA). 

recodef <- function(df, replace_value = -3, replace_with = NA) {
  x <- df
  x[x == replace_value] <- replace_with
  #return(x)
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)}

#Given year and month in two separate columns, creates a field called date that is in the format 
#year, month, day with the day being the first day of the month (01)

create_date_field <- function(df){
  x <- df
  x <- unite(x, "date", c(`year`, `mo`), sep = "-")
  x["date"] <- apply(x[,c("date")],2, function(each){paste0(each, '-01')})
  x$date <- as.Date(x$date)
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)
}

#Find Water year from date field, given water year starting month. 

get_water_date <- function(df, water_year_start = 06){
  x <- df
  x %<>%
    mutate(water_date = date %m-% months(water_year_start - 1))
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)}

#Create Year Date
create_water_year_date <- function(df, water_year_start = 06){
  x <- df
  x %<>%
    mutate(water_year_date = water_date) %>%
    separate(water_year_date, c("water_year"), sep = "-")
  x["water_year"] <- apply(x[,c("water_year")],2, function(x){paste0(x,'-', as.character(water_year_start), '-01')})
  x$water_year <- as.Date(x$water_year)
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)
}

#Get H concentration from pH

get_Hydrogen <- function(df){
  x <- df
  x %<>% 
    mutate(H = 10^(-pH)*1000)
  #return(x)
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)}


#Gather function that creates solute and concentration_mg columns. It understands all columns that 
#have a capital letter as elements and puts them under the solute column. It takes their 
#corresponding values and puts them under the concentration_mg column. 
gather_concentration <- function(df){
  x <- df
  x <- gather(x, solute, concentration_mg, matches("[A-Z]", ignore.case = FALSE))
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)
}


#Gather function that creates water source and water_mm_pm (water in mm per month) columns. It 
#understands columns that have a either precip or flow as showing water sources and puts them under 
#the source column. It takes their corresponding values and puts them under the water_mm_pm column. 

gather_source <- function(df){
  x <- df
  x <- gather(x, source, water_mm_pm, matches("precip|flow", ignore.case = FALSE))
  assign(deparse(substitute(df)), x, envir=.GlobalEnv)
}

```


#Import All Data

```{r message=FALSE}

#Import Data

precipList <- list("w9_precip_chem", "w8_precip_chem","w7_precip_chem", "w6_precip_chem", 
                   "w5_precip_chem", "w4_precip_chem", "w3_precip_chem", "w2_precip_chem", 
                   "w1_precip_chem")

streamList <- list("w9_stream_chem", "w8_stream_chem","w7_stream_chem", "w6_stream_chem", 
                   "w5_stream_chem", "w4_stream_chem", "w3_stream_chem", "w2_stream_chem", 
                   "w1_stream_chem")


importf(precipList,".txt", "~/dataplus/PlayingAround/Data/", "_precip_chem")
importf(streamList, ".txt", "~/dataplus/PlayingAround/Data/", "_stream_chem")


load("~/dataplus/PlayingAround/public_data.Rdata", envir = parent.frame(), verbose = FALSE)

#change charge for Al from NA to 3
MW.data[4, "z"] <- 3

#change charge for NH4 from NA to 1
MW.data[5, "z"] <- 1



```


#Clean Data

```{r}

#recode NA values from -3 to NA

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, 
                       w5_precip_chem, w4_precip_chem, w3_precip_chem, w2_precip_chem, 
                       w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, 
                       w5_stream_chem, w4_stream_chem, w3_stream_chem, w2_stream_chem, 
                       w1_stream_chem)


lapply_and_save(precip_df_list, recodef, -3, NA, a_name = "_precip_chem")
lapply_and_save(stream_df_list, recodef, -3, NA, a_name = "_stream_chem")

```

```{r message = FALSE}
#Create date field

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, 
                       w5_precip_chem, w4_precip_chem, w3_precip_chem, w2_precip_chem, 
                       w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, 
                       w5_stream_chem, w4_stream_chem, w3_stream_chem, w2_stream_chem, 
                       w1_stream_chem)

lapply_and_save(precip_df_list, create_date_field, a_name = "_precip_chem")
lapply_and_save(stream_df_list, create_date_field, a_name = "_stream_chem")


precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, 
                       w5_precip_chem, w4_precip_chem, w3_precip_chem, w2_precip_chem, 
                       w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, 
                       w5_stream_chem, w4_stream_chem, w3_stream_chem, w2_stream_chem, 
                       w1_stream_chem)

lapply_and_save(precip_df_list, get_water_date, 06, a_name = "_precip_chem")
lapply_and_save(stream_df_list, get_water_date, 06, a_name = "_stream_chem")

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, 
                       w5_precip_chem, w4_precip_chem, w3_precip_chem, w2_precip_chem, 
                       w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, w5_stream_chem,
                       w4_stream_chem, w3_stream_chem, w2_stream_chem, w1_stream_chem)

lapply_and_save(precip_df_list, create_water_year_date, 06, a_name = "_precip_chem")
lapply_and_save(stream_df_list, create_water_year_date, 06, a_name = "_stream_chem")
```


```{r}
#Get H from pH

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, w5_precip_chem,
                       w4_precip_chem, w3_precip_chem, w2_precip_chem, w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, w5_stream_chem,
                       w4_stream_chem, w3_stream_chem, w2_stream_chem, w1_stream_chem)

lapply_and_save(precip_df_list, get_Hydrogen, a_name = "_precip_chem")
lapply_and_save(stream_df_list, get_Hydrogen, a_name = "_stream_chem")


```

```{r}
#Gather Solutes and Concentration_mg

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, w5_precip_chem,
                       w4_precip_chem, w3_precip_chem, w2_precip_chem, w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, w5_stream_chem,
                       w4_stream_chem, w3_stream_chem, w2_stream_chem, w1_stream_chem)

lapply_and_save(precip_df_list, gather_concentration, a_name = "_precip_chem")

lapply_and_save(stream_df_list, gather_concentration, a_name = "_stream_chem")

#Gather Water source so that there's a column for water source (either precip or flow) and one for 

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, w5_precip_chem,
                       w4_precip_chem, w3_precip_chem, w2_precip_chem, w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, w5_stream_chem,
                       w4_stream_chem, w3_stream_chem, w2_stream_chem, w1_stream_chem)

lapply_and_save(precip_df_list, gather_source, a_name = "_precip_chem")
lapply_and_save(stream_df_list, gather_source, a_name = "_stream_chem")

precip_df_list <- list(w9_precip_chem, w8_precip_chem,w7_precip_chem, w6_precip_chem, w5_precip_chem,
                       w4_precip_chem, w3_precip_chem, w2_precip_chem, w1_precip_chem)

stream_df_list <- list(w9_stream_chem, w8_stream_chem,w7_stream_chem, w6_stream_chem, w5_stream_chem,
                       w4_stream_chem, w3_stream_chem, w2_stream_chem, w1_stream_chem)
```



```{r}

#Concatenate/ stack all 9 ws data frames  

precip_stream_df_list <- append(precip_df_list, stream_df_list) 
precip_stream_data <- do.call("rbind", precip_stream_df_list)


#Join new complete df with molecular weight data (MW.data) to calculate mol and eq values. 

precip_stream_data<- left_join(precip_stream_data, MW.data, by = "solute")

# Calculate ueq, umol, and flux. 
precip_stream_data %<>% 
  mutate(concentration_ueq =
           ifelse(is.na(precip_stream_data$MW), NA,(((concentration_mg/1000)/MW)*(abs(z))*10^6))) %>% 
  mutate(concentration_umol = 
           ifelse(is.na(precip_stream_data$MW), NA,(((concentration_mg/1000)/MW))*10^6)) %>% 
  mutate(flux = 
           ifelse(is.na(precip_stream_data$MW), NA,(((concentration_mg/1000)/MW)*(abs(z))*water_mm_pm*10000)))


#Calculate weighted averages. 

precip_stream_data %<>% group_by(ws, water_year, source, solute) %>%
  mutate(mg_weighted_average = weighted.mean(concentration_mg, water_mm_pm)) %>%
  mutate(umol_weighted_average = weighted.mean(concentration_umol, water_mm_pm)) %>%
  mutate(ueq_weighted_average = weighted.mean(concentration_ueq,
                                              water_mm_pm))

precip_stream_data$ws <- as.factor(precip_stream_data$ws)



```

#Export data to precip_stream_data.rds

```{r}

#Export File

precip_stream_data %>% 
  saveRDS(file = "precip_stream_data.rds")
```
