

df0$entity %>%
  
  filter(grepl("^pens|^empl", solvency.pf)) %>%
  
  mutate(range = cut(income.pf/1000,
                     c(0, 500, 800, 1200, 1500, 2000, Inf)/1000,
                     include.lowest = T)) %>%
  
  # freq(range, income.pf, type = "median", scale = "base")
  
  mutate(n = 1) %>% 
  group_by(range) %>% 
  summarize(n = sum(n),
            income = median(income.pf),
            .groups = "drop") %>% 
  mutate(p.n = n/sum(n)) %>%
  # mutate(p.n = ((n/sum(n))) %>% printf.perc(dec = 2)) %>% 
  export




df0$loan %>% arrange(desc(gbv.residual)) %>% inspect

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

