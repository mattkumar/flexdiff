#this script uses the dev version of flextable
#devtools::install_github("davidgohel/flextable")
library(tidyverse)
library(flextable)
library(diffdf)
library(readr)
library(officer)

#load data sets provided
df_1 <- 
  read_csv(here::here("prototype", "df1.csv")) 

df_2 <- 
  read_csv(here::here("prototype", "df2.csv"))

#get vector of rows that need indentation in the eventual flextable
#do this now because we'll drop these  
indent_df_1 <- which(df_1[,1] == 1)
indent_df_2 <- which(df_2[,1] == 1)
bold_df_1   <- which(df_1[,1] == 0)
bold_df_2   <- which(df_2[,1] == 0)

#drop indent variable prior to comparisons
df_1 <- df_1 %>% select(-indent)
df_2 <- df_2 %>% select(-indent)

#compare!
deltas <- diffdf::diffdf(df_1,df_2)

#process differences
var_list <- colnames(df_1)

#comparison place holder
deltalist <- list()

#iterate over variables to find if cell differences was detected - get "coordinates"
for(i in var_list) {
  dat <- data.frame(eval(parse(text=paste0("deltas$VarDiff_",i)))) %>% 
    janitor::clean_names()
  deltalist[[i]] <- dat
}

#across multiple variables, rbind the results
delta_data = do.call(rbind, deltalist)

#look for extra columns
extra_cols <- data.frame(data.frame(deltas$ExtColsComp)) %>%
  janitor::clean_names()

#look for extra rows
extra_rows <- data.frame(data.frame(deltas$ExtRowsComp)) %>%
  janitor::clean_names()

#flextables
df_1 %>%
  flextable() %>%
  autofit() %>%
  flextable::align(align="left", part="body") %>%
  flextable::padding(j=1, i=indent_df_1, padding.left = 20, part="body") %>%
  flextable::bold(j=1, i=bold_df_1, part="body")



df_2 %>%
  #Cell Differences
  flextable() %>%
  (function(x){
    for (i in 1:nrow(delta_data)) {
      x <- highlight(x, 
                     i=delta_data$rownumber[[i]], 
                     j=delta_data$variable[[i]], 
                     color="skyblue", 
                     part="body") %>% 
           bold(i=delta_data$rownumber[[i]], 
                j=delta_data$variable[[i]])
    }
    x
  })() %>%
  
  #Extra Rows
  (function(x){
    for (i in 1:nrow(extra_rows)) {
      x <- color(x, 
                 i=extra_rows$rownumber[[i]], 
                 color="red", 
                 part="body") 
    }
    x
  })() %>%
  
  #Extra Columns
  (function(x){
    for (i in 1:nrow(extra_cols)) {
      x <-  border(x, 
               i=NULL,  
               j=extra_cols$columns[[i]],
               border = fp_border(color = "green", style="dashed", width=1.5), 
               part = "all")   
      
    }
    x
  })() %>%
  autofit() %>%
  align(align="left", part="body") %>%
  padding(j=1, i=indent_df_2, padding.left = 20, part="body") %>%
  bold(j=1, i=bold_df_2, part="body") %>%
  highlight(j="P", i = ~ P == "-9999", color = "yellow", part="body")



