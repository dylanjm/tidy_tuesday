#####################################################################################
## Step 1: Load Libraries ##
#####################################################################################
library(tidyverse)
library(tidyquant)
library(geofacet)
library(viridis)
library(scales)
library(tweenr)
library(animation)
library(dplyr)

#####################################################################################
## Step 2: go get data ##
## FHFA's ALL-Transactions House Price Index for US and states (NSA) **
#####################################################################################
df <- tq_get(c("USSTHPI",paste0(us_state_grid3$code,      # get state abbreviations
                                "STHPI")),                    # append STHPI
             get="economic.data",             # use FRED
             from="2000-01-01")               # go from 1990 forward

df %>% mutate(state=substr(symbol,1,2)        # create a state variable
) -> df


df %>% group_by(state) %>% 
  mutate(hpi=100*price/price[date=="2000-01-01"]) %>% # rebenchmark index to 100 in Q1 2000
  ungroup() %>% 
  map_if(is.character,as.factor) %>%   # tweenr will try to interpolate characters, but will ignore factors
  as.tibble() -> df
knitr::kable(head(df))

dplyr::filter(df,state=="CA") %>% 
  ggplot(aes(x=date, y=hpi))+
  geom_line(data=df, aes(group=state),color="lightgray",alpha=0.5)+
  geom_line(size=1.1,color="royalblue") + 
  geom_line(data=dplyr::filter(df,state=="US"),color="black",linetype=2, alpha=0.85)+
  geom_text(data=dplyr::filter(df,state=="US" & date==max(df$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="black",label="US")+
  geom_text(data=df %>% filter(state=="CA" & date==max(df$date)), aes(label=state), nudge_y=0.01,fontface="bold",color="royalblue")+
  # set axis labels
  scale_y_log10(breaks=c(100,150,200,250,300),limits=c(85,300),sec.axis=dup_axis())+
  labs(x="",y="House Price Index (2000 Q1=100, log scale NSA)",
       title="House price trends by state",
       subtitle=paste("Each gray line a state, highlighted state CA"),
       caption="@lenkiefer Source: U.S. Federal Housing Finance Agency, All-Transactions House Price Index for the United States [USSTHPI],\nretrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/USSTHPI, March 18, 2018.")+
  theme(plot.subtitle=element_text(face="italic",size=14),
        plot.title=element_text(color="royalblue",face="bold",size=18),
        plot.caption=element_text(hjust=0))

df.state2 <- df %>% mutate(state=as.factor(state), date=as.factor(date)) %>%   select(date,state,hpi) %>% group_by(state) %>% 
  mutate(hpilag12=c(lag(hpi,4))) %>% ungroup()

myf<-function(y=2012) {
  filter(df.state2,year(date)==y & month(date)==10) %>% data.frame()
}

mylist<-lapply(c(2000:2017,2000),myf)
tween.df<-tween_states(mylist,tweenlength=3,statelength=2, ease=rep('cubic-in-out',11), nframes=250)


plotf3<- function(i=1){
  g<-
    tween.df %>% filter(.frame==i) %>%
    ggplot(aes(x=hpi, y=state, label=state))+
    geom_text(nudge_x = 0.025,color="royalblue")  +
    geom_point(color="royalblue",size=3)+
    scale_x_log10(limits=c(70,300), breaks=c(70,100,150,200,250,300))+
    geom_segment(aes(xend=hpilag12,x=hpi,y=state,yend=state),alpha=0.7)+
    theme_minimal()  +
    labs(y="State", x="House price index (log scale, 2000 Q1 =100, NSA)",
         title="State house price dynamics",
         subtitle=paste0("Q4 of ",as.character(as.Date( tail(tween.df  %>% filter(.frame==i),1)$date), format="%Y")," line 4-quarter lag\n"),
         caption="@lenkiefer Source: U.S. Federal Housing Finance Agency, All-Transactions House Price Index,\nretrieved from FRED, Federal Reserve Bank of St. Louis;\nhttps://fred.stlouisfed.org/series/XXSTHPI, March 18, 2018. [XX= state code or US]")+
    theme(plot.title=element_text(size=18,color="royalblue",face="bold"),
          plot.subtitle=element_text(size=14,face="italic"),
          plot.caption=element_text(hjust=0,vjust=1),
          axis.text.x=element_text(size=12),
          legend.key.width=unit(1,"cm"),
          legend.position="top",
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major.y =element_blank())
}

saveGIF({for (i in 1:max(tween.df$.frame)){
  #  saveGIF({for (i in 1:20){
  g<-plotf3(i)
  print(g)
  print(paste(i,"out of",max(tween.df$.frame)))
  ani.pause()
}
},movie.name="hpi4.gif", ani.width=650, ani.height=800)
