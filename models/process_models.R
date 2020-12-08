# process CMMID model output
# now reading shiny output files directly 

library(dplyr)
library(caTools)
library(data.table)
library(purrr)

# tbl_fread.A <- 
#   list.files(path="~/dat/Covid model outputs CMMID/A/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.B <- 
#   list.files(path="~/dat/Covid model outputs CMMID/B/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.C <- 
#   list.files(path="~/dat/Covid model outputs CMMID/C/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.D <- 
#   list.files(path="~/dat/Covid model outputs CMMID/D/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.E <- 
#   list.files(path="~/dat/Covid model outputs CMMID/E/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.F <- 
#   list.files(path="~/dat/Covid model outputs CMMID/F/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.G <- 
#   list.files(path="~/dat/Covid model outputs CMMID/G/",full.names = T) %>% 
#   map_df(~fread(.))
# 
# tbl_fread.H <- 
#   list.files(path="~/dat/Covid model outputs CMMID/H/",full.names = T) %>% 
#   map_df(~fread(.))

tbl_fread.I <- 
  list.files(path="~/dat/Covid model outputs CMMID/I 2.7/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.J <- 
  list.files(path="~/dat/Covid model outputs CMMID/J 1.6/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.K <- 
  list.files(path="~/dat/Covid model outputs CMMID/K 3.9/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.L <- 
  list.files(path="~/dat/Covid model outputs CMMID/L 2.7/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.M <- 
  list.files(path="~/dat/Covid model outputs CMMID/M 1.6/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.N <- 
  list.files(path="~/dat/Covid model outputs CMMID/N 3.9/",full.names = T) %>% 
  map_df(~fread(.))



# all_sim <- bind_rows(list(A=tbl_fread.A,B=tbl_fread.B,C=tbl_fread.C,D=tbl_fread.D,E=tbl_fread.E,F=tbl_fread.F,G=tbl_fread.G,H=tbl_fread.H),.id="Sim") %>% filter(scenario=="Unmitigated")
# 
# all_sim$date <- as.Date(all_sim$epi_date)
# 
# save(all_sim,file="~/git/covid-litsearch/all_sim.RData")


##

load("~/git/covid-litsearch/all_sim.RData")

all_sim_new <- bind_rows(list(I=tbl_fread.I,J=tbl_fread.J,K=tbl_fread.K, L=tbl_fread.L,M=tbl_fread.M,N=tbl_fread.N),.id="Sim") %>% filter(scenario=="Unmitigated")

all_sim <- bind_rows(all_sim,all_sim_new)
all_sim$date <- as.Date(all_sim$epi_date)

## date of first death

a1 <- all_sim %>% filter(date <= "2020-07-20") %>% group_by(Sim,population,readable_name,date,t,run) %>% summarise(allages=sum(value))
a2 <- all_sim %>% filter(date <= "2020-07-20") %>% group_by(Sim,population,readable_name,date,t,run,group_combined) %>% summarise(byages=sum(value))

a1 %>% filter(readable_name=="Deaths" & allages >= 0.5) %>% group_by(Sim,population) %>% summarise(date=min(date)) %>% View()# consider first death when number of deaths rounds up to 1? Can change this assumption. There is a lot of stochasticity involved in when the first death will be.
a1 %>% filter(readable_name=="Deaths" & allages >= 10) %>% group_by(Sim,population) %>% summarise(date=min(date)) %>% View() # 10th death

# check method - are cases new cases per day or total cases (d(t-1) + dx/dt), compare to estimate plotted on website, should be about 23,000,000
checks <- all_sim %>% filter(population=="United Kingdom" & Sim=="B") %>% group_by(Sim,population,readable_name,date,t,run) %>% summarise(allages=sum(value)) %>% group_by(readable_name) %>% summarise(area=trapz(t,allages),cumsum=max(cumsum(allages)),first= first(allages),last=last(allages),change=last-first)

# yes, cases compartment is separate from SEIR compartments and presents daily cases. Same for deaths. 
a1 %>% ungroup() %>% group_by(Sim,readable_name,population) %>% mutate(cumsum=cumsum(allages)) %>% filter(date >= "2020-07-15")


# cumulative cases and deaths
C <- a1  %>% ungroup() %>% group_by(Sim,readable_name,population) %>% summarise(total_trapz=trapz(t,allages),total_cumsum=max(cumsum(allages))) # which of these is correct? check with Sam. I think this is not the right way to calculate it as now we have death-days (area under curve) - no, cases and deaths look like daily.
C %>% filter(readable_name=="Cases")
C %>% filter(readable_name=="Deaths")
C %>% filter(readable_name=="ICU beds occupied")
C %>% filter(readable_name=="Non-ICU beds occupied")

C %>% filter(population=="United Kingdom" & readable_name=="Cases")

Cw <- C %>% dplyr::select(-total_trapz) %>% tidyr::spread(readable_name,total_cumsum)

CA <- a2 %>% filter(readable_name=="Deaths") %>% group_by(Sim,readable_name,population,group_combined) %>% summarise(total_cumsum=max(cumsum(byages)))
CAw <- CA %>% tidyr::spread(group_combined,total_cumsum) 

tofill <- expand.grid(population=c("Germany","Ireland","Spain","Sweden","United Kingdom"),Sim=LETTERS[1:14]) # reordered output

Cout <- left_join(tofill,Cw,by=c("population","Sim")) %>% left_join(.,CAw,by=c("population","Sim"))

write.csv(Cout,"~/git/covid-litsearch/models/cumulative.csv")

