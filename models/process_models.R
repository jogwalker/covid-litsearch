# process CMMID model output
# now reading shiny output files directly 

library(dplyr)
library(caTools)
library(data.table)


tbl_fread.A <- 
  list.files(path="~/dat/Covid model outputs CMMID/A/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.B <- 
  list.files(path="~/dat/Covid model outputs CMMID/B/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.C <- 
  list.files(path="~/dat/Covid model outputs CMMID/C/",full.names = T) %>% 
  map_df(~fread(.))

tbl_fread.D <- 
  list.files(path="~/dat/Covid model outputs CMMID/D/",full.names = T) %>% 
  map_df(~fread(.))

all_sim <- bind_rows(list(A=tbl_fread.A,B=tbl_fread.B,C=tbl_fread.C,D=tbl_fread.D),.id="Sim") %>% filter(scenario=="Unmitigated")

all_sim$date <- as.Date(all_sim$epi_date)

save(all_sim,file="~/git/covid-litsearch/all_sim.RData")


##

load("~/git/covid-litsearch/all_sim.RData")


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

Cw <- C %>% select(-total_trapz) %>% spread(readable_name,total_cumsum)

CA <- a2 %>% filter(readable_name=="Deaths") %>% group_by(Sim,readable_name,population,group_combined) %>% summarise(total_cumsum=max(cumsum(byages)))
CAw <- CA %>% spread(group_combined,total_cumsum) 

tofill <- expand.grid(population=c("United Kingdom","Sweden","Germany","Spain","Ireland","Republic of Korea"),Sim=c("A","B","C","D"))

Cout <- left_join(tofill,Cw,by=c("population","Sim")) %>% left_join(.,CAw,by=c("population","Sim"))

write.csv(Cout,"~/git/covid-litsearch/models/cumulative.csv")

