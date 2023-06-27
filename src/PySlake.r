library(tidyverse)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(GGally)
library(cowplot)

v1.list <- list.dirs('/project/nsaru/work/PySlake/V1')
v2.list <- list.dirs('/project/nsaru/work/PySlake/V2')
diff_mod<-function(x){
  
  y<-rep(0,nrow(x))
  yy<-rep(0,nrow(x)*ncol(x))
  yy<-matrix(yy,ncol=ncol(x))
  for (j in 1:ncol(x)){
    y[2:nrow(x)]<-diff(x[,j])#/x[2:nrow(x),j]
    yy[,j]<-y
  }
  
  return(yy)
}
i=1

for (d in v1.list){
  ls.files <-list.files(d)
  for (f in ls.files){
    if (grepl("csv", f, fixed=TRUE)){
      df<-read.csv(paste0(d,'/',f))
      #filter out "bad" images - weird Area values
      mat_area<-df%>%dplyr::select(-Column,-Row,-Well,-Sample,-Rep)
      mat_area<-mat_area%>%t()%>%as.data.frame()%>%dplyr::filter(if_all(starts_with("V"), ~ . < 200))#%>%dplyr::filter(if_all(starts_with("V"), ~ . != 0))
      diff_mat<-diff_mod(mat_area)
      ind<-which(abs(diff_mat)>40,arr.ind=T)
      if(nrow(ind)>0){
        mat_area<-mat_area[-ind[,1],]
        }
      colnames(mat_area)<-df$Sample
      times<-as.numeric(names(as.data.frame(mat_area))%>%substr(2,4))
      
      mat_area<-t(mat_area)
      times<-as.numeric(names(as.data.frame(mat_area))%>%substr(2,4))
      times<-times-times[2]#use second image as wetting start
      df<-cbind(df%>%dplyr::select(Column,Row,Well,Sample,Rep),mat_area)
      i600<-which.min(abs(times-600))+5
      i0<-which.min(times)+5
      df0<-df
      df_ts<-pivot_longer(df0,cols=starts_with("X"),names_to = "Sec",values_to="Area")
      df_ts$Sec<-as.numeric(substr(df_ts$Sec,2,4))
      df_ts<-df_ts%>%mutate(Sec=Sec- df_ts[2, ]$Sec)
      df_ts<-df_ts%>%dplyr::filter(Area>15)
      df_ts$Area_Change<-NA
      samples<-unique(df_ts$Sample)
      for (sample in samples){
        reps<-unique(df_ts$Rep)
        for (rep in reps){
          mask1<-which(df_ts$Sample==sample & df_ts$Rep==rep & df_ts$Sec>=0)
          mask2<-which(df_ts$Sample==sample & df_ts$Rep==rep & df_ts$Sec<0)
          df_ts[mask1,'Area_Change']=df_ts[mask1,'Area']$Area/df_ts[mask2,'Area']$Area
        }
      }
      p<-ggplot(df_ts,aes(x=Sec,y=Area_Change,color=as.factor(Rep)))+
        geom_point()+
        facet_wrap(~Sample)+
        theme_classic()+scale_x_continuous(name="Sec", limits=c(0,605),breaks=seq(0,600,200))
      ggsave(paste0('../../../work/PySlake/PySlakeTime',substr(f,1,(str_length(f)-4)),'.png'),p)
      
      df$SI600=(df[,i600]-df[,i0])/df[,i0]
      df$Initial_Area=df[,i0]
      df<-df%>%dplyr::filter(Initial_Area>15)
      df<-df%>%dplyr::select(Column,Row,Well,Sample,Rep,Initial_Area,SI600)
      if (i==1){
        df_v1=df
      }else{
        df_v1<-rbind(df_v1,df)
      }
      i<-i+1
      }
    }
  }

i=1

for (d in v2.list){
  ls.files <-list.files(d)
  for (f in ls.files){
    if (grepl("csv", f, fixed=TRUE)){
      df<-read.csv(paste0(d,'/',f))
      #filter out "bad" images - weird Area values
      mat_area<-df%>%dplyr::select(-Column,-Row,-Well,-Sample,-Rep)
      mat_area<-mat_area%>%t()%>%as.data.frame()%>%dplyr::filter(if_all(starts_with("V"), ~ . < 300))#%>%dplyr::filter(if_all(starts_with("V"), ~ . != 0))
      diff_mat<-diff_mod(mat_area)
      ind<-which(abs(diff_mat)>40,arr.ind=T)
      if(nrow(ind)>0){
        mat_area<-mat_area[-ind[,1],]
      }
      mat_area<-t(mat_area)
      times<-as.numeric(names(as.data.frame(mat_area))%>%substr(2,4))
      times<-times-times[2]#use second image as wetting start
      
      df<-cbind(df%>%dplyr::select(Column,Row,Well,Sample,Rep),mat_area)
      i600<-which.min(abs(times-600))+5
      i0<-which.min(times)+5
      df0<-df
      df_ts<-pivot_longer(df0,cols=starts_with("X"),names_to = "Sec",values_to="Area")
      df_ts$Sec<-as.numeric(substr(df_ts$Sec,2,4))
      df_ts<-df_ts%>%mutate(Sec=Sec- df_ts[2, ]$Sec)
      df_ts<-df_ts%>%dplyr::filter(Area>15)
      df_ts$Area_Change<-NA
      samples<-unique(df_ts$Sample)
      for (sample in samples){
        reps<-unique(df_ts$Rep)
        for (rep in reps){
          mask1<-which(df_ts$Sample==sample & df_ts$Rep==rep & df_ts$Sec>=0)
          mask2<-which(df_ts$Sample==sample & df_ts$Rep==rep & df_ts$Sec<0)
          df_ts[mask1,'Area_Change']=df_ts[mask1,'Area']$Area/df_ts[mask2,'Area']$Area
        }
      }
      p<-ggplot(df_ts,aes(x=Sec,y=Area_Change,color=as.factor(Rep)))+
        geom_point()+
        facet_wrap(~Sample)+
        theme_classic()+scale_x_continuous(name="Sec", limits=c(0,605),breaks=seq(0,600,200))
      ggsave(paste0('../../../work/PySlake/PySlakeTime',substr(f,1,(str_length(f)-4)),'.png'),p)
      df$SI600=(df[,i600]-df[,i0])/df[,i0]
      df$Initial_Area=df[,i0]
      df<-df%>%dplyr::filter(Initial_Area>15)
      df<-df%>%dplyr::select(Column,Row,Well,Sample,Rep,Initial_Area,SI600)
      if (i==1){
        df_v2<-df
        #df_v2_ts<-df_ts
      }else{
        df_v2<-rbind(df_v2,df)
        #df_v2_ts<-cbind(df_v2_ts,df_ts)
      }
      i<-i+1
    }
  }
}
df_v1$Version<-'V1'
df_v2$Version<-'V2'
df_all<-rbind(df_v1,df_v2)
df_all<-df_all[order(df_all$Initial_Area),]%>%filter(Initial_Area>10)
p<-ggplot(df_all)+geom_boxplot(aes(x=SI600,colour=Sample))+facet_wrap(~Version)
p
ggsave('../../../work/PySlake/PySlakeBoxplotSI600.png',p)

p<-ggplot(df_all)+geom_boxplot(aes(x=Initial_Area,colour=Sample))+facet_wrap(~Version)
p
ggsave('../../../work/PySlake/PySlakeBoxplotA0.png',p)
write.csv(file = '../../../work/PySlake/PySlakeOutput.csv',df_all)
