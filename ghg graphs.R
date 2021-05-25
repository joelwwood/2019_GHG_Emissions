
library(tidyverse)
library(cowplot)
library(cansim)

ghg_data<-read_csv("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv")

prov_order<-ghg_data %>% filter(Index==0) %>%
  filter(!Region %in% c("Canada","Northwest Territories", "Northwest Territories and Nunavut", "Nunavut","Yukon")) %>%
  filter(Year==1990) %>%
  select(Region) %>%
  mutate(order=c(2,1,4,7,10,9,5,8,6,3),
         name=c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK"))

provs<-ghg_data %>% filter(Index==0) %>%
  filter(!Region %in% c("Canada","Northwest Territories", "Northwest Territories and Nunavut", "Nunavut","Yukon")) %>%
  left_join(prov_order) %>%
  mutate(name=reorder(name,order)) %>% 
  ggplot(aes(Year,CO2eq)) +
  geom_area(color="black",fill="blue",alpha=0.3)+
  facet_wrap(~name, nrow=1)+  #scales="free_y",
  theme_cowplot(12)+
  labs(y=NULL,
       x=NULL)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


can<-ghg_data %>% filter(Index==0) %>%
  filter(Region=="Canada") %>%
  ggplot(aes(Year,CO2eq)) +
  geom_area(color="black",fill="blue",alpha=0.3)+
  geom_point()+
  theme_cowplot(12)+
  labs(y="Mt CO2eq",
       title="Canadian Greenhouse Gas Emissions: 1990-2019",
       caption="Data: Environment and Climate Change Canada, 2021\nNational Inventory Report")

plot_grid(can,provs,nrow=2,ncol=1)



#BC emissions by source

ghg_data %>%
  filter(Region=="British Columbia",
         Index!=0,
         is.na(Category)) %>%
  ggplot()+
  geom_area(aes(Year,CO2eq,fill=Source))+
  theme_cowplot(12)+
  theme(legend.position="bottom")+
  labs(y="Mt CO2eq",
       title="#BigTrucks4Lyfe\nBritish Columbia's Greenhouse Gas Emissions: 1990-2019",
       caption="Data: Environment and Climate Change Canada, 2021\nNational Inventory Report")


ghg_data %>%
  filter(Region=="British Columbia",
         Source=="Transport",
         !is.na(Category),
         is.na(`Sub-category`) ) %>%
  ggplot()+
  geom_line(aes(Year,CO2eq,color=Category),size=2)+
  theme_minimal_hgrid(12)+
  theme(legend.position="bottom")+
  labs(y="Mt CO2eq",
       title="#BigTrucks4Lyfe or Freight?\n Transport GHGs in BC: 1990-2019",
       caption="Data: Environment and Climate Change Canada, 2021\nNational Inventory Report")

  

#Get population data

pop_data<-get_cansim("17-10-0005-01") %>%
  normalize_cansim_values %>%
  rename(Year=REF_DATE,
         Region=GEO,
         Population=VALUE) %>%
  filter(Sex=="Both sexes",
         `Age group`=="All ages",
         Year>1989) %>%
  select(Year,Region,Population) %>%
  mutate(Year=as.numeric(Year),
         Region=ifelse(Region %in% c("Northwest Territories", "Northwest Territories including Nunavut", "Nunavut","Yukon"),"Territories",Region)) %>%
  group_by(Year,Region) %>%
  summarize(Population=sum(Population,na.rm=TRUE)) %>%
  ungroup()



data<-ghg_data %>%
  filter(Index==0) %>%
  mutate(Region=ifelse(Region %in% c("Northwest Territories", "Northwest Territories and Nunavut", "Nunavut","Yukon"),"Territories",Region)) %>%
  group_by(Year,Region) %>%
  summarize(CO2eq=sum(CO2eq,na.rm=TRUE))%>%
  ungroup() %>%
  left_join(pop_data)


#GDP Data

gdp_data<- get_cansim("36-10-0222-01") %>%
  normalize_cansim_values %>%
  rename(Year=REF_DATE,
         Region=GEO,
         GDP=VALUE) %>%
  filter(Year>1989,
         Estimates=="Gross domestic product at market prices",
         Prices=="Chained (2012) dollars") %>%
  mutate(Year=as.numeric(Year),
         Region=ifelse(Region %in% c("Northwest Territories", "Northwest Territories including Nunavut", "Nunavut","Yukon"),"Territories",Region)) %>%
  group_by(Year,Region) %>%
  summarize(GDP=sum(GDP,na.rm=TRUE)) %>%
  ungroup()


data_combined<-data %>%
  left_join(gdp_data) %>%
  select(Year,Region,CO2eq,Population,GDP) %>%
  mutate(Region=ifelse(Region %in% c("Newfoundland and Labrador", "New Brunswick", "Nova Scotia","Prince Edward Island"),"Atlantic provinces",Region)) %>%
  group_by(Year,Region) %>%
  summarize(CO2eq=sum(CO2eq),
            Population=sum(Population,na.rm=TRUE),
            GDP=sum(GDP,na.rm=TRUE)) %>%
    mutate(GDPpc=GDP/Population,
         Intensity=CO2eq*1000000/GDP) %>%
  ungroup()

base_values<- data_combined %>%
  filter(Year==1990) %>%
    filter(Year==1990) %>%
  rename(CO2_base=CO2eq,
         Pop_base=Population,
         GDPpc_base=GDPpc,
         Int_base=Intensity) %>%
  select(Region,CO2_base,Pop_base,GDPpc_base,Int_base) 

kaya_data<-data_combined %>%
  left_join(base_values) %>%
  group_by(Year, Region) %>%
  mutate("GHG emissions"=100*CO2eq/CO2_base,
         Population=100*Population/Pop_base,
    "GDP per capita"=100*GDPpc/GDPpc_base,
         "Emissions intensity"=100*Intensity/Int_base) %>%
  select(Year,Region,"GHG emissions",Population,"GDP per capita","Emissions intensity") %>%
  pivot_longer("GHG emissions":"Emissions intensity") %>%
  ungroup()



can_kaya<-  kaya_data %>%
  filter(Region=="Canada") %>%
  ggplot(aes(Year,value))+
  geom_line(aes(color=name),show.legend=FALSE,size=1.4)+
  theme_minimal_hgrid(12)+
  labs(title="Canadian Greenhouse Gas Emissions: 1990-2019",
       caption="Data: Environment and Climate Change Canada, 2021\n      Statistics Canada Tables 17-10-0005-01 & 36-10-0222-01",
       y="Index value (1990=100)")

can_labels<-kaya_data %>%
  ungroup() %>%
  filter(Region=="Canada",
         Year==2019) %>%
  mutate(y=value,
         y=ifelse(name=="Population",132,y),
         name=ifelse(name=="GHG emissions","GHG emissions\n  +21%",name),
         name=ifelse(name=="GDP per capita","GDP per capita\n  +42%",name),
         name=ifelse(name=="Population","Population\n   +36%",name),
         name=ifelse(name=="Emissions intensity","Emissions/GDP\n   -36.9%",name)
  ) %>%
  select(name,y)



can_label_axis<-axis_canvas(can_kaya,axis="y")+
  geom_text(
    data=can_labels,
    aes(y=y, label=name, color=name),
    x=0.05,
    size=4.5,
    hjust=0
  )

p_can_labels<-insert_yaxis_grob(can_kaya, can_label_axis)

ggdraw(p_can_labels)


#Kaya facet for provinces
province_names<-  kaya_data %>%
    select(Region) %>%
  distinct() %>%
  filter( Region != "Canada") %>%
  mutate(order=c(2,7,1,4,5,6,3,8))
  
kaya_data %>%
  filter(Region != "Canada") %>%
  left_join(province_names) %>%
  mutate(Region=reorder(Region,order)) %>%
  ggplot(aes(Year,value))+
  geom_line(aes(color=name),size=1.4)+
  geom_hline(yintercept=100)+
  theme_minimal()+
  ylim(c(40,180))+
  background_grid(major = "y")+
  facet_wrap(~Region,nrow=2)+
  labs(title="Provincial Greenhouse Gas Emissions: 1990-2019",
           caption="Data: Environment and Climate Change Canada, 2021\n      Statistics Canada Tables 17-10-0005-01 & 36-10-0222-01",
           y="Index value (1990=100)",
           x=NULL)+
  theme(legend.position = "bottom",
            legend.title=element_blank())
  
