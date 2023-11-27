
make.title("Reports")
systime.0 <- Sys.time()

###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###

list.files(here(.profile$path.data))

read_ <- function(x, y) feather::read_feather(here(.profile$path.data, paste("Test", x, y, sep = "_") %>% paste0(".feather")))

df <- list()

df0 = list(
  main = read_("Report", "Borrowers"),
  loan = read_("Report", "Loans"),
  
  counterparty = read_("Report", "Counterparties"),
  entity = read_("Report", "Entities"),
  
  procedure = read_("Report", "Procedures"),
  
  collection = read_("Report", "Collections"),
  
  guarantee = read_("Report", "Guarantees")
)


link0 <- list(
  counterparty.entity = read_("Link", "Counterparty_Entity"),

  proc.loan = read_("Link", "Procedure_Loan"),
  proc.collateral = read_("Link", "Procedure_Collateral"),
  proc.entity = read_("Link", "Procedure_Entity"),
  
  guarantee.cadastral = read_("Link", "Guarantee_Cadastral"),
  guarantee.loan = read_("Link", "Guarantee_Loan"),
  guarantee.entity = read_("Link", "Guarantee_Entity")
)


###... Check
# df0$main %>% inspect
# df0$loan %>% inspect
# df0$counterparties %>% View



###-----------------------------------------------------------------------###
#-----          - Borrower Aggregation                            -----         
###-----------------------------------------------------------------------###

temp <- list()


check.duplicates <- function(df, x) {
  df %>%
    
    filter(duplicated({{x}}) | 
             duplicated({{x}}, fromLast = T))
  
  # filter(duplicated(!!enquo(x)) | 
           # duplicated(!!enquo(x), fromLast = T))

}


#########......... 0) aggregate relevant chars/factors by id_bor
temp$highest <- df0$loan %>% 
  
  arrange(desc(gbv.residual)) %>% 
  group_by(id.bor) %>% slice(1) %>% ungroup %>% 
  
  select(id.bor, originator:status, date.status)


#. Check
# temp$highest %>% View
# df0$loan %>%
#   group_by(id.bor) %>%
#   summarize_at(vars(originator:status, date.status), 
#                n_distinct) %>%
#   View


#########......... 0) aggregate ind/corp (only for role = "borrower")
temp$type.bor <- df0$entity %>% 
  
  left_join(link0$counterparty.entity, by = "id.entity", multiple = "all") %>% 
  # check.duplicates(id.entity) %>% View
  # check.duplicates(paste(id.entity, id.counterparty)) %>% View
  
  left_join(df0$counterparty, by = "id.counterparty", multiple = "all") %>% 
  # check.duplicates(paste(id.counterparty, id.bor)) %>% select(id.entity, id.counterparty, name.y, id.bor) %>%View
  # check.duplicates(paste(id.entity, id.counterparty, id.bor)) %>% select(id.entity, id.counterparty, name.y, id.bor) %>%View

  filter(role == "borrower") %>% 
  
  arrange(desc(type.subject)) %>% 
  group_by(id.bor) %>% slice(1) %>% ungroup %>% 
  
  # rename(type.bor = type.subject) %>% 
  
  mutate(type.bor = type.subject %>% 
           factor(c("corporate", "individual"))) %>% 
  
  select(id.bor, type.bor)


###... Check
# View(df0$entity)
# df0$entity %>% filter(grepl("[a-z]", tolower(cf.piva))) %>% nrow
# df0$entity %>% filter(type.subject == "individual") %>% nrow
# df0$entity %>% filter(!grepl("[a-z]", tolower(cf.piva)) & type.subject == "individual") %>% View
# df0$entity %>% filter(grepl("[a-z]", cf.piva))

# temp$type.bor %>% View
# temp$type.bor %>% group_by(type.bor) %>% tally
# temp$type.bor %>% freq(type.bor)



#########......... 2) 
breaks.gbv <- c(0, 15, 30, 50, 100, 250, 500, Inf)
labels.gbv <- c(c("0-15", "15-30", "30-50", "50-100", "100-250", "250-500") %>% paste0("k"),
                "500k+")
                

df$borrowers <- df0$loan %>% 
  
  mutate(gbv.original = if_else(!is.na(gbv.original),
                                gbv.original,
                                gbv.residual),
         
         n.loans = 1L) %>% 
  
  group_by(id.bor) %>% 
  summarize_at(vars(n.loans, gbv.original:expenses), sum) %>% 
  ungroup %>% 
  
  left_join(temp$highest, by = "id.bor") %>% 
  rename(type.highest.loan = type) %>% 
  
  left_join(temp$type.bor, by = "id.bor") %>% 
  
  mutate(
    range.gbv.original = cut(gbv.original/10^3, breaks.gbv, labels.gbv, include.lowest = T),
    range.gbv.residual = cut(gbv.residual/10^3, breaks.gbv, labels.gbv, include.lowest = T)) %>% 
  
  arrange(desc(gbv.residual)) %>% 
  
  select(id.bor, 
         originator, ptf, cluster.ptf,
         status, date.status,
         type.bor, type.highest.loan,
         matches("^range"),
         n.loans,
         gbv.original:expenses)
  

###... Check (0)
df$borrowers %>% View
df0$loan %>% distinct(id.bor) %>% nrow

###... Check
# df$borrowers %>% freq(ptf, gbv.original, scale = "m")
# df$borrowers %>% freq(ptf, gbv.residual, scale = "m")
# df$borrowers %>% freq(n.loans, gbv.residual, scale = "m")
# df$borrowers %>% freq(cluster.ptf, gbv.residual, scale = "m", format = F) %>% export
# df$borrowers %>% freq(type.bor, gbv.residual, scale = "m")
# df$borrowers %>% filter(ptf == "bb") %>% freq(cluster.ptf, gbv.residual, scale = "m")
# df$borrowers %>% freq(type.bor, gbv.residual, scale = "m")
# df$borrowers %>%
#   group_by(type.bor, ptf) %>% 
#   summarize(n = n(), gbv = sum(gbv.residual)/10^6, .groups = "drop") %>% 
#   mutate(avg.gbv = (gbv/n*10^3) %>% round(0),
#          gbv = gbv %>% round(2),
#          p.gbv = ((gbv/sum(gbv))*100) %>% round(2)) %>% 
#   arrange(desc(gbv)) %>% 
#   setNames(c("type", "bor", "#","gbv.M€", "avg.GBV.€", "%")) %>% 
#   View
# 
# df$borrowers %>%
#   group_by(ptf, type.bor) %>% 
#   summarize(n = n(), gbv = sum(gbv.residual)/10^6, .groups = "drop") %>% 
#   mutate(avg.gbv = (gbv/n*10^3) %>% round(0),
#          gbv = gbv %>% round(2),
#          p.gbv = ((gbv/sum(gbv))*100) %>% round(2)) %>% 
#   
#   group_by(ptf) %>% mutate(subp.gbv = ((gbv/sum(gbv))*100) %>% round(2)) %>% ungroup %>% 
#   
#   arrange(ptf, type.bor) %>% 
#   setNames(c("type", "bor", "#","gbv.M€", "avg.GBV.€", "%", "sub%")) %>% 
#   select(-`%`) %>% 
#   View

###-----------------------------------------------------------------------###
#-----          - Functions                                           -----         
###-----------------------------------------------------------------------###

#########....... 1) Function to make tables
make_table <- function(df, x, y, title,  ...) {
  
  df %>% 
    
    mutate(n = 1L) %>% 
    group_by(!!enquo(x), !!enquo(y)) %>% 
    summarize_at(vars(n, !!!enquos(...)), sum) %>% 
    ungroup %>% 
    
    pivot_longer(c(n, !!!enquos(...)), names_to = "item", values_to = "amount") %>% 
    
    rename(cluster := !!enquo(x),
           subcluster := !!enquo(y)) %>% 
    
    mutate(title = title) %>% 
    
    select(title, cluster, subcluster, item, amount)
  
  
}



###-----------------------------------------------------------------------###
#-----          - Tables                                           -----         
###-----------------------------------------------------------------------###

tables <- list()

#########......... 0) Laziness = Awesomeness
list.vars <- quos(n.loans, gbv.original, gbv.residual, principal)

# quo(gbv.original)
# expr(gbv.original >= 1000)

#########......... 1) Table_1
# df$borrowers %>% 
#   
#   mutate(n = 1L) %>% 
#   group_by(type.bor, range.gbv.residual) %>% 
#   summarize_at(vars(n, gbv.original, gbv.residual, principal), sum) %>% 
#   ungroup %>% 
#   
#   pivot_longer(c(n:principal), names_to = "item", values_to = "amount") %>% 
#   
#   rename(cluster = type.bor,
#          subcluster = range.gbv.residual) %>% 
#   
#   mutate(title = "Table_TypeBor_GBV") %>% 
#   
#   select(title, cluster, subcluster, item, amount)

#########......... 1) Table_1
tables$typebor.gbv <- df$borrowers %>% make_table(type.bor, range.gbv.residual, "Table_TypeBor_GBV",  !!!list.vars)


#########......... 2) Table_2
tables$tables.ptf <- df$borrowers %>% make_table(ptf, cluster.ptf, "Table_PTF",  !!!list.vars)


tables$status.gbv <- df$borrowers %>% make_table(status, type.bor, "Status_GBV",  !!!list.vars)



#########......... 1) Table_1
#bind_rows(tables) %>% write_csv(here(.profile$path.output, "tables_v1.csv"))

bind_rows(tables) %>% write.csv(here("tables_v1.csv"))



###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###

df0$loan %>% 
  
  mutate(range.gbv = cut(gbv.residual/10^3, 
                         c(0, 15, 30, 50, 100, 250, 500, Inf),
                         include.lowest = T)) %>% 
  
  # freq(range.gbv, gbv.residual, scale = "m")
  # freq(range.gbv, gbv.residual, type = "mean", n.dec = 0)
  
  
  arrange(desc(gbv.residual)) %>% 
  # select(id.loan, id.bor, range.gbv, gbv.original, gbv.residual) %>% 
  # inspect(n.dec = 0)
  
  group_by(type, range.gbv) %>% 
  # summarize(gbv = sum(gbv.residual))
  summarize_at(vars(gbv.original, gbv.residual), 
               function(x) sum(x)/10^6) %>% 
  ungroup %>% 
  mutate(p.res = gbv.residual/sum(gbv.residual)) %>% 
  
  # View
  export

###-----------------------------------------------------------------------###
#-----          - Intro                                           -----         
###-----------------------------------------------------------------------###
