library (readxl)
library (dplyr)
library (tidyr)

#wrote code to read singles directly
#download the responses from HERE:
#https://docs.google.com/spreadsheets/d/1o8CtBehHLrxnbVX6FUPiAHdIuoUihQhp8YFWE3WuZV4/edit#gid=1881943992
itex_locs=read.csv('ITEX research site survey 2019 (Responses) - Form Responses.csv')[,c(1:201)]
nm=read_excel('rename.xlsx', col_names=FALSE)

#nm=read.csv('clipboard', header=FALSE)

names(itex_locs)=nm$`...2`

single=itex_locs%>%
  filter(mult_subsites%in%c(
    'No I have only one site',
    'No I have only one site ... Yes multiple sites; too many to enter details for all'))%>%
  select(-comments)%>%
  filter(!is.na(site_name))%>%
  mutate(OTCs = grepl('OTCs', site_expt))%>%
  select(`Principal investigator's name`,  `Email Address`, site_name, 
         site_lat,
         site_long,
         site_elev, OTCs, site_expt, site_data_plot,
         site_data, site_genera
  )%>%
  rename(PI=`Principal investigator's name`, email=`Email Address`, 
         latitude=site_lat,
         longitude=site_long,
         elevation=site_elev)%>%
  mutate(OTCs=case_when(
    OTCs=='TRUE' ~ 'Experimental warming',
    OTCs=='FALSE' ~ 'Long-term monitoring only',
    TRUE ~ 'NA'
  ))

#subsites start at col 40
#which (names(multiple)=='Subsite.name')

#multiples, too much name confusion, so renamed in this mapping file

nm=read_excel('rename.xlsx', col_names=FALSE)

#nm=read.csv('clipboard', header=FALSE)

names(itex_locs)=nm$`...2`

#Make multiple
multiple=itex_locs%>%mutate_all(as.character)%>%
  filter(mult_subsites=='Yes I have multiple sites/subsites')%>%
  select(-comments)%>%
  pivot_longer(-c(1:39), 
               names_to = "what", values_to = "value")%>%
  mutate(subnumber=stringr::str_sub(what, -1, -1))%>%
  mutate(value=ifelse(value==''|is.na(value),
                      NA, value))%>%
  
  #View (multiple%>%filter(`Email Address`=='hollistr@gvsu.edu'))
  #mutate(subnumber_fixed=ifelse(subnumber%in%c('1','2','3','4','5','6'),
  #       subnumber, '0'))%>%
  rowwise()%>%
  mutate(what_modified=gsub(paste0('\\.', subnumber, '$'), '', what))%>%
  select(-what)#

#remove extra subsites with no data
all_na=multiple%>%
  group_by(`Email Address`, site_name, subnumber)%>%
  summarise(all_na=all(is.na(value)))%>%
  filter(all_na==FALSE)

multiple=multiple%>%
  inner_join(., all_na)%>%
  select(-all_na)

#%>%#, -subnumber)%>%#, #-Any.other.comments.or.details.about.your.sites.)%>%
  #widen
multiple=multiple%>%pivot_wider(names_from='what_modified', values_from='value', names_repair = "unique")%>%
  #View (multiple%>%filter(`Email Address`=='hollistr@gvsu.edu'))
  #if no subsite location, use site
  mutate(elevation = ifelse(!is.na(sub_elev),
                            sub_elev,
                            site_elev),
         latitude=ifelse(!is.na(sub_lat),
                         sub_lat,
           site_lat),
         longitude=ifelse(!is.na(sub_long),
                          sub_long,
                          site_long
         )
  )%>%
  mutate(OTCs = grepl('OTCs', sub_expt))%>%
  #two dots = subsite - I think? for the duplicated name
  #mutate(mutate(OTCs = grepl('OTCs', What.experimental.manipulation.is.found.at.this.site..)))%>%
  mutate(OTCs=case_when(
    OTCs=='TRUE' ~ 'Experimental warming',
    OTCs=='FALSE' ~ 'Long-term monitoring only',
    TRUE ~ 'NA'
  ))%>%select(`Principal investigator's name`,  `Email Address`, site_name, 
              elevation, latitude, longitude, sub_name,
           OTCs, sub_data_plot,
           sub_data, sub_genera, sub_expt
  )%>%
  mutate(site_name=paste(site_name,sub_name))%>%
  rename(PI=`Principal investigator's name`, email=`Email Address`,
         site_data_plot=sub_data_plot,
         site_data=sub_data, site_genera=sub_genera, site_expt=sub_expt)%>%
  select(PI, email, site_name, latitude, longitude, elevation, OTCs,
         site_data, site_data_plot, site_genera, site_expt)%>%
  mutate(latitude=as.numeric(as.character(latitude)),
         longitude=as.numeric(as.character(longitude)),
         elevation=as.numeric(as.character(elevation)))
  
  
all_locs=bind_rows(single, multiple)%>%
                     rename(`elevation (m.a.s.l)`=elevation,
                            `data collected in plots` = site_data_plot,
                            `data collected in site` = site_data,
                            `experimental manipulations` = site_expt,
                            `common genera` = site_genera)

write.csv(all_locs%>%filter(OTCs=='Experimental warming'), 'experimental_warming.csv', row.names=FALSE, na='')

write.csv(all_locs%>%filter(OTCs!='Experimental warming'), 'monitoring_only.csv', row.names=FALSE, na='')

# embed on map
# open arcgis online
# new map (or edit ITEX)
# add -> upload each csv
# symbols - current size is 12, otc mapped to red, ctl only to blue.
