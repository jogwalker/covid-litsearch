# process CMMID model output
# inputting results into spreadsheet - this is just easier to parse the data

library(dplyr)
library(caTools)



UK1 <- read.csv("~/Downloads/Cases_pop_United_Kingdom_cm_United_Kingdom_(Mossong)_int_Social_Distancing (1).csv")
SW1 <- read.csv("~/Downloads/Cases_pop_Sweden_cm_Sweden_int_Social_Distancing.csv")
SP1 <- read.csv("~/Downloads/Cases_pop_Spain_cm_Spain_int_Social_Distancing.csv")
GE1 <- read.csv("~/Downloads/Cases_pop_Germany_cm_Germany_int_Social_Distancing.csv")
IR1 <- read.csv("~/Downloads/Cases_pop_Ireland_cm_Ireland_int_Social_Distancing.csv")
KO1 <- read.csv("~/Downloads/Cases_pop_Republic_of_Korea_cm_Republic_of_Korea_int_Social_Distancing.csv")

## eventually read in data direct from Excel sheet with results - this is quicker for now

all <- bind_rows(list(UK=UK1,Sweden=SW1,Spain=SP1,Germany=GE1,Ireland=IR1,Korea=KO1),.id="country")

all$date <- as.Date(all$epi_date)

all1.a <- all %>% filter(scenario=="Unmitigated" & date <= "2020-07-20") %>% group_by(country,readable_name,date,t,run) %>% summarise(allages=sum(value))

all1.a %>% filter(readable_name=="Deaths" & allages >= 0.5) %>% group_by(country) %>% summarise(date=min(date)) # consider first death when number of deaths rounds up to 1? Can change this assumption. There is a lot of stochasticity involved in when the first death will be.

# cumulative cases and deaths
C <- all1.a  %>% ungroup() %>% group_by(readable_name,country) %>% summarise(total1=trapz(t,allages),total2=max(cumsum(allages))) # which of these is correct? check with Sam. I think this is not the right way to calculate it as now we have death-days (area under curve)
C %>% filter(readable_name=="Cases")
C %>% filter(readable_name=="Deaths")
C %>% filter(readable_name=="ICU beds occupied")
C %>% filter(readable_name=="Non-ICU beds occupied")

C2 <- all  %>% filter(scenario=="Unmitigated" & date <= "2020-07-20")  %>% group_by(country,readable_name,group_combined) %>% summarise(total1=trapz(t,value),total2=max(cumsum(value)))

C2 %>% filter(readable_name=="Deaths")


