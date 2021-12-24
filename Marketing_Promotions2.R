library(readr)
m <- read_delim("/Users/Home/Desktop/IESE/Stefan/MP_5_11.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

m <- select(m, Name, Event, Event_Details= `Event Details`, Year)
m <- tbl_df(m)

m$Event <- as.factor(m$Event)
m$Event_Details <- as.factor(m$Event_Details)
m$Name <- as.factor(m$Name)


  
m$IDt <- seq.int(nrow(m))

x <- m%>%
  group_by(Name)%>%
  #arrange(IDt)%>%
  mutate(Base_Year=min(Year, na.rm=T), Since_Base_Year=Year-Base_Year)%>%
  ungroup()

x$ID <- seq.int(nrow(x))

test1 <- x%>%
  spread(Event,Since_Base_Year)%>%
  select(Name,Event_Details, ID, Year, Base_Year, Joined_EBS='Joined European Business School', Promotion, Published)%>%
  group_by(Name)%>%
  arrange(Year, ID)%>%
  mutate(Since_Base_Year=Year-Base_Year)%>%
  mutate(Promo=if_else(Event_Details=='1. Graduated',1,
                       if_else(Event_Details == "2. Assistant Prof",2,
                               if_else(Event_Details == "1.5 Visting Assistant Prof.", 1,
                                       if_else(Event_Details=="1.5 Lecturer"|Event_Details=="1.5 Senior Lecturer", 1,
                                               if_else(Event_Details == "3. Associate Prof", 3,
                                                       if_else(Event_Details== "4. Prof", 4, 
                                                               if_else(Event_Details == "5. Chaired Professor"|Event_Details=="5. Emeritus Prof."| Event_Details =="5. Dean",4,0))))))))%>%
  #arrange(Name, Year, Promo)%>%
  mutate(Level=replace(Promo,Promo==0,NA))%>%
  fill(Level)%>%
  replace_na(list(Level=0))%>%
  mutate(Published1 = if_else(Published=="NA",0,1))%>%
  replace_na(list(Published1=0))%>%
  mutate(Cumalitive_Pubs=cumsum(Published1))%>%
  mutate(Teaching_European_BizSchool = if_else(Joined_EBS=="NA",0,1))%>%
  mutate(Teaching_European_BizSchool=replace(Teaching_European_BizSchool,Teaching_European_BizSchool==0,NA))%>%
  fill(Teaching_European_BizSchool)%>%
  replace_na(list(Teaching_European_BizSchool=0))%>%
  mutate(School=case_when(Joined_EBS!="NA" ~ Event_Details))%>%
  fill(School, .direction="up")%>%
  fill(School)%>%
  replace_na(list(School="ESADE"))%>%
  ungroup()%>%
  mutate(Job_Level = if_else(Level==1, "Just Graduated", 
                             if_else(Level == 2, "Assistant Prof.",
                                     if_else(Level==3, "Associate Prof.",
                                             if_else(Level==4, "Professor",
                                                     if_else(Level==5, "Chaired Prof./Dean/Emeritus",
                                                             if_else(Level==0, "PhD. Student", "NA")))))))%>%
  ungroup()%>%
  group_by(Name)%>%
  mutate(Max_Level=if_else(max(Level)==4, "Prof",
                           if_else(max(Level)==3, "Associate",
                                   if_else(max(Level)==2, "Assistant", "NA"))))%>%
  ungroup()

test1$Job_Level <- factor(test1$Job_Level, levels= c("PhD. Student",
                                                       "Just Graduated",
                                                       "Assistant Prof.",
                                                       "Associate Prof.",
                                                       "Professor",
                                                       "Chaired Prof./Dean/Emeritus"))

  
#View(test1)
a<- test1%>%
  select(Name, Event_Details, Year, Job_Level, Published1, School, Max_Level)
write.csv(a, file='PvP6.csv')
glimpse(test1)
test1%>%
  group_by(Name)%>%
  summarise(sum=sum(Published1), years = max(Since_Base_Year))%>%
  arrange(desc(sum))
  

test1%>%
  filter(School=="IESE")%>%
  #filter(Published1==1)%>%
  #filter(Name!="Stefan Stremerch")%>%
  select(Since_Base_Year, Level,Cumalitive_Pubs, School)%>%
  na.omit()%>%
  group_by(School, Since_Base_Year)%>%
  summarise(avg_cumilative_number_of_publications=mean(Cumalitive_Pubs))%>%
  ggplot(aes(Since_Base_Year,avg_cumilative_number_of_publications, col=School))+
  geom_point()+
  geom_smooth(se=F)+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Cumilative Publications per school without Prof.SS")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC6666", "#0072B2", "#D55E00", "#CC79A7")

test1%>%
  select(School, Since_Base_Year, Level,Cumalitive_Pubs)%>%
  na.omit()%>%
  #filter(School=="IESE")%>%
  group_by(School, Level)%>%
  summarise(avg_promotion_time=mean(Since_Base_Year))%>%
  ggplot(aes(avg_promotion_time,Level, col=School))+
  geom_point()+
  geom_smooth(se=F)+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Promotion since base year per school")

test1%>%
  select(School, Since_Base_Year, Cumalitive_Pubs)%>%
  na.omit()%>%
  group_by(School,Since_Base_Year)%>%
  summarise(avg_cumalitive_pubs=mean(Cumalitive_Pubs))%>%
  ggplot(aes(Since_Base_Year, avg_cumalitive_pubs, col=School))+
  geom_point()+
  geom_smooth(se=F)

glimpse(test1)
View(a)
a <- test1%>%
  select(School, Level, Cumalitive_Pubs, Name,Published1)%>%
  filter(Published1==1)%>%
  filter(School=="IESE")%>%
  #filter(Name!="Stefan Stremerch")%>%
  na.omit()%>%
  group_by(School,Level)%>%
  summarise(avg_cumalitive_pubs=sum(Cumalitive_Pubs))%>%
  ggplot(aes(Level, avg_cumalitive_pubs,col=School))+
  geom_point()+
  geom_smooth(se=F)+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Number of publications per level per school without Prof.SS")

test1%>%
  select(School, Name, Level, Cumalitive_Pubs, Published1)%>%
  #filter(School=="INSEAD")%>%
  #filter(Name!="Stefan Stremerch")%>%
  #filter(Level!=2)%>%
  filter(Published1==1)%>%
  na.omit()%>%
  group_by(School,Level)%>%
  summarise(avg_cumalitive_pubs=mean(Cumalitive_Pubs))%>%
  ggplot(aes(x=Level, y=avg_cumalitive_pubs,color=School))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Number of publications per level per school")+
  scale_x_continuous(name= "Job Level", breaks = c(0,1, 2,3,4), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of Cumalitive Publications per Job Level",
       title = "Average Number of Cumilitve Publications")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)




# Without SS
ggplotly(test1%>%
           select(School, Level, Cumalitive_Pubs, Published1, Name)%>%
           filter(Name!="Stefan Stremerch")%>%
           filter(Published1==1)%>%
           na.omit()%>%
           group_by(School,Level)%>%
           summarise(avg_cumalitive_pubs=mean(Cumalitive_Pubs))%>%
           ggplot(aes(x=Level, y=avg_cumalitive_pubs,color=School))+
           geom_point()+
           geom_line()+
           scale_colour_manual(values=cbPalette)+
           labs(title = "Avg. Number of publications per level per school"))

test%>%
  select(Name, Since_BaseYear, Level,Cumalitive_Pubs)%>%
  na.omit()%>%
  ggplot(aes(Since_BaseYear,Level, col=Name))+
  geom_jitter(width = 0.1,height = 0.1)+
  geom_step(se=F)

test1%>%
  select(School, Level,Cumalitive_Pubs)%>%
  na.omit()%>%
  ggplot(aes(Cumalitive_Pubs,Level, col=School))+
  geom_jitter(width = 0.1,height = 0.1)+
  geom_smooth(se=F)

View(test1)

test1%>%
  select(Name, Level,Cumalitive_Pubs, Published1, School)%>%
  group_by(School)%>%
  mutate(Num_of_Profs=n_distinct(Name))%>%
  ungroup()%>%
  #filter(Name!="Stefan Stremerch")%>%
  filter(Published1=="1")%>%
  na.omit()%>%
  group_by(Name, Level, School)%>%
  summarise(one1=sum(Published1), Num_of_Profs=mean(Num_of_Profs))%>%
  ungroup()%>%
  group_by(Level, School)%>%
  summarise(sum2 = mean(one1))%>%
  #filter(School=="IESE")%>%
  ggplot(aes(Level, sum2, col=School))+
  geom_smooth(method="lm", se=F)+
  scale_x_continuous(name= "Job Level", breaks = c(0,1,2,3,4,5), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor","Chaired Prof./Dean/Emeritus"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of Publications per Job Level",
       title = "Average Number of Publications within a Job Level")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)

## Histrogram
test1%>%
  select(Name, Level,Cumalitive_Pubs, Published1, School)%>%
  filter(Name!="Stefan Stremerch")%>% 
  filter(Published1=="1")%>%
  na.omit()%>%
  group_by(Name, Level, School)%>%
  summarise(one1=sum(Published1))%>%
  ungroup()%>%
  group_by(Level, School)%>%
  summarise(sum2 = mean(one1))%>%
  complete(sum2, Level )%>%
  #filter(School=="IESE")%>%
  ggplot(aes(Level, sum2))+
  geom_col(position=position_dodge())+
  scale_x_continuous(name= "Job Level", breaks = c(0,1,2,3,4,5), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor","Chaired/Dean/Emeritus"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor=element_blank())+
  labs(y = "Average number of Publications",
       title = "Average Number of Publications within a Job Level Without Prof.Stefan Stremerch" )+
  scale_y_continuous(breaks =seq(0,24,by=2))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)+
  facet_wrap(~School)

a <- test1%>%
  group_by(School)%>%
  mutate(Total_Profs= n_distinct(Name))%>%
  filter(Published1==1, Name!="Stefan Stremerch")%>%
  summarise(Total_Profs = as.integer(mean(Total_Profs)), Num_prof_who_published = n_distinct(Name),Percent_profs_who_publish = n_distinct(Name)/as.integer(mean(Total_Profs))*100, number_of_Publications = n(),Avg._Pubs_per_prof_wo_Prof.Stefan = n()/n_distinct(Name) )


write.csv(x = a, file="aaaa.csv")

test1%>%
  group_by(School)%>%
  filter(Published1==1)%>%
  summarise()

test1%>%
  group_by(School)%>%
  filter(Published1==1)%>%
  summarise(Avg._Pubs_per_prof = n()/n_distinct(Name))

test1%>%
  group_by(School)%>%
  filter(Published1==1, Name!="Stefan Stremerch")%>%
  summarise()

test1%>%
  filter(School=="IESE")%>%
  select(Level,Published1, School, Name)%>%
  group_by(School, Level)%>%
  mutate(Num_of_Profs=n_distinct(Name))%>%
  ungroup()%>%
  #filter(Name!="Stefan Stremerch")%>%
  filter(Published1=="1")%>%
  na.omit()%>%
  group_by(Level, School)%>%
  summarise(one1=sum(Published1), Num_of_Profs=Num_of_Profs)%>%
  summarise(total_pubs_per_Level = one1)

a<- test1%>%
  filter(School=="IESE")%>%
  select(Name, Level,Cumalitive_Pubs, Published1, School)%>%
  group_by(Level, Name, School)%>%
  summarise(Published_in_level=sum(Published1))%>%
  ungroup()%>%
  group_by(Level, School)%>%
  summarise(n_distinct(Name))
  filter(Published_in_level!=0)%>%
  group_by(School, Level)%>%
  summarise(Pubs_per_level=mean(Published_in_level))%>%
  ungroup()

View(test1)

a%>%
  #filter(School=="INSEAD")%>%
  na.omit()%>%
  ggplot(aes(Level, Pubs_per_level, col=School))+
  geom_point()+
  geom_smooth(method='auto', se=F)+
  scale_x_continuous(name= "Job Level", breaks = c(0,1,2,3,4,5), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor","Chaired Prof./Dean/Emeritus"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of Publications per Job Level",
       title = "Average Number of Publications within a Job Level")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)
  
suppressMessages(print(b)) 


View(a)
a<-test1%>%
  #filter(School=="INSEAD")%>%
  group_by(Job_Level, School)%>%
  mutate(total_num_of_profs=n_distinct(Name))%>%
  filter(Published1!=0)%>%
  summarise(Num_pubs_per_level=n(), total_num_of_profs_per_level=mean(total_num_of_profs))%>%
  group_by(Job_Level, School)%>%
  summarise( average_num_of_pubs_per_prof_per_level=Num_pubs_per_level/total_num_of_profs_per_level)%>%
  ggplot(aes(Job_Level, average_num_of_pubs_per_prof_per_level))+
  geom_col()+
  facet_wrap(~School)

View(a)
test1%>%
  filter(School=="INSEAD")%>%
  group_by(Level, School)%>%
  mutate(total_num_of_profs=n_distinct(Name))%>%
  filter(Published1=="1")%>%
  mutate(total_profs_who_pub_in_level=n_distinct(Name))%>%
  summarise(Num_pubs_per_level=n(), total_num_of_profs_per_level=mean(total_num_of_profs), a = mean(total_profs_who_pub_in_level))%>%
  group_by(Level, School)%>%
  summarise(Num_pubs_per_level, b= Num_pubs_per_level/a)%>%
  ungroup()%>%
  ggplot(aes(x=Level, y=b,col=School))+
  geom_line()+
  #scale_colour_manual(values=cbPalette)+
  geom_smooth( se=F, na.rm=T)+
  scale_colour_manual(values=cbPalette)
  
  test1%>%
    #filter(School=="INSEAD")%>%
    group_by(Level, School)%>%
    mutate(total_num_of_profs=n_distinct(Name))%>%
    #filter(Published1!=0)%>%
    summarise(Num_pubs_per_level=n(), total_num_of_profs_per_level=mean(total_num_of_profs))%>%
    group_by(Level, School)%>%
    summarise( average_num_of_pubs_per_prof_per_level=Num_pubs_per_level/total_num_of_profs_per_level)%>%
    ungroup()%>%
    ggplot(aes(x=Level, y=average_num_of_pubs_per_prof_per_level,col=School))+
    geom_line()+
    scale_colour_manual(values=cbPalette)
  geom_smooth( se=F, na.rm=T)



test1%>%
  select(Name, School, Published1, Job_Level)%>%
  filter(Published1==1)%>%
  group_by(Name, Job_Level, School)%>%
  summarise(n=n())%>%
  ungroup()%>%
  ggplot(aes(Job_Level, n))+
  geom_point()+
  geom_smooth()

test1%>%
  #filter(School=="INSEAD")%>%
  group_by(School, Level)%>%
  mutate(total_num_of_profs=n_distinct(Name))%>%
  #filter(Published1=="1")%>%
  mutate(a=n_distinct(Name))%>%
  summarise(Num_pubs_per_level=sum(Published1), total_num_of_profs_per_level=mean(total_num_of_profs), b = mean(a))%>%
  ungroup()%>%
  group_by(Level, School)%>%
  summarise(Num_pubs_per_level, b, c= Num_pubs_per_level/b)%>%
  ungroup()%>%
  ggplot(aes(x=Level, y=c,col=School))+
  geom_line()+
  scale_x_continuous(name= "Job Level", breaks = c(0,1,2,3,4,5), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor","Chaired Prof./Dean/Emeritus"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of Publications per Job Level",
       title = "Average Number of Publications within a Job Level")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)

glimpse(test1)

# Cumiliative Prublications by published Profs
test1%>%
  #filter(School=="Cambridge")%>%
  filter(Name!="Stefan Stremerch")%>%
  #filter(Published1==1)%>%
  select(Name, Published1, School, Level)%>%
  group_by(School, Level)%>%
  summarise(pubs=sum(Published1), num_p=n_distinct(Name),avg_cum_sum=pubs/num_p)%>%
  mutate(cum_pubs=cumsum(avg_cum_sum))%>%
  ggplot(aes(Level, cum_pubs, col=School))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Number of cumulative publications per level per school")+
  scale_x_continuous(name= "Job Level", breaks = c(0,1, 2,3,4), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of cumulative Publications per Job Level",
       title = "Cumulative Average Number of Publications without Prof.Stefan Stremerch")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)

test1%>%
  #filter(School=="IESE")%>%
  filter(Name!="Stefan Stremerch")%>%
  #filter(Published1==1)%>%
  select(Name, Published1, School, Level)%>%
  group_by(School, Level)%>%
  summarise(pubs=sum(Published1), num_p=n_distinct(Name),avg_pubs=pubs/num_p)%>%
  ggplot(aes(Level, avg_pubs, col=School))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=cbPalette)+
  labs(title = "Avg. Number of cumalitive publications per level per school")+
  scale_x_continuous(name= "Job Level", breaks = c(0,1, 2,3,4), labels=c("Phd Student", "Just Graduated", "Assistant Prof.", "Associate Prof.", "Professor"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor=element_blank())+
  labs(y = "Average number of Publications per Job Level",
       title = "Average Number of Publications without Prof.Stefan Stremerch")+
  scale_y_continuous(breaks =seq(0,20,by=1))+
  scale_colour_manual(values=cbPalette)+
  expand_limits(y=0)

n <- test1%>%
  #filter(School=="IESE")%>%
  #filter(Name!="Stefan Stremerch")%>%
  #filter(Published1==1)%>%
  filter(Max_Level!="Assistant", Max_Level!="Assistant")%>%
  select(Name, Published1, School, Level, Job_Level)%>%
  group_by(School,Job_Level)%>%
  summarise(pubs=sum(Published1), num_p=n_distinct(Name),avg_pubs=pubs/num_p)%>%
  select(School,Job_Level,avg_pubs)
  b <- spread(n, Job_Level, avg_pubs)
View(b)
write.csv(x=b,file = "b.csv")
c <- test1%>%
  #filter(School=="IESE")%>%
  #filter(Name!="Stefan Stremerch")%>%
  filter(Published1==1)%>%
  select(Name, Published1, School, Level, Job_Level)%>%
  group_by(School,Job_Level)%>%
  summarise(pubs=sum(Published1), num_p=n_distinct(Name),avg_pubs=pubs/num_p)%>%
  mutate(cum_pubs=cumsum(avg_pubs))%>%
  select(School,Job_Level,cum_pubs)
d <- spread(c, Job_Level, cum_pubs)
View(d)