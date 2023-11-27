###------------------------------------------###
#---               excel        -----
###------------------------------------------###

#source("C:/Users/eleonora.zarrilli/Documents/CORSI/Dati R/esercizi/Normalizzazione_git_R/normalization/Main_Normalization.R")

###------------------------------------------###
#---            first sheet       -----
###------------------------------------------###

# here we want to create a column with all the unique type.of.credit 
#and put it in the first sheet of a new excel file


unique_types_of_credit <- unique(Loans$type)
unique_df <- data.frame(type.of.credit = unique_types_of_credit )


prov <- Loans %>% select(type)
prov <- prov %>% mutate(subtype =  gsub("n\\..*", "", type))
types <- prov %>% select(subtype) %>% distinct()
types <- data.frame(type.of.credit = types)
###------------------------------------------###
#---            second sheet       -----
###------------------------------------------###


# we want to sum,max,min, mean of the gbv columns and 
# 'make a report' in excel

summary_df <- data.frame(
  Metric = c("Sum", "Min", "Max"),
  gbv.original = c(sum(Loans$gbv.original), min(Loans$gbv.original), max(Loans$gbv.original)),
  principal = c(sum(Loans$principal), min(Loans$principal), max(Loans$principal)),
  interest = c(sum(Loans$interest), min(Loans$interest), max(Loans$interest))
)

summ <- t(summary_df)
colnames(summ) <- summ[1, ]
summ <- summ[-1, ]
summ <- cbind(RowName = rownames(summ),summ)
df <- as.data.frame(summ)


###------------------------------------------###
#---       create the excel file        -----
###------------------------------------------###
excel_file <- "File/Unique_Types_of_Credit.xlsx"
write_xlsx(list("FirstSheet" = unique_df,"SecondSheet" = df), excel_file)


excel_file <- "File/Types_of_Credit.xlsx"
write_xlsx(list("FirstSheet" = types,"SecondSheet" = df), excel_file)




# categorize type of Loans:                       ------ scritto a mano
categorize_type <- function(name) {
  name <- tolower(name)  
  
  if (grepl("conto corrente", name)) {
    return("Bank Accounts")
  } else if (grepl("mutuo ipotecario", name)) {
    return("Mortgages")
  } else if (grepl("credito di firma", name)) {
    return("Other")
  } else if (grepl("mutuo chiro|mutuo chirografario", name)) {
    return("Personal Loans")
  } else if (grepl("fondiario", name)) {
    return("Mortgages(Fondiario)")
  }else {
    return("NA")
  }
}




# categorize type of Loans:                       ------ da tabella

types_table <-  read_excel("File/Types_of_Credit.xlsx", sheet = "FirstSheet")
types_table <- types_table %>% as.data.frame() %>% rename(type = subtype)
#### aggiungere colonna Type
Type <- c('Bank Accounts','Mortgages','Other','Personal Loans','Personal Loans')
types_table <- cbind(types_table$type,Type)
types_table <- types_table  %>% as.data.frame() %>% rename(type = V1)

map_types_Loans <- function(table, table_types){
  table_types$type <- str_trim(tolower(table_types$type))
  
  table <- table %>% mutate(type = gsub("n\\..*", "", type))
  table$type <- str_trim(tolower(table$type))
  
  table_mapped <- table %>%
    left_join(table_types, by = "type")
  table_mapped <- table_mapped %>% select(-type)
  table_mapped <- table_mapped %>% relocate(Type, .before = status) %>% rename(type = Type)
  return(table_mapped)
}



