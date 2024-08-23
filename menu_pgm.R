#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("plotly")
#install.packages("gapminder")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gapminder))


#extended graphical features
library(ggplot2)

#data transformation
suppressPackageStartupMessages(library(dplyr))

#x and y scaling
suppressPackageStartupMessages(library(scales))

#reading csv files
suppressPackageStartupMessages(library(readr))

#reading main csv file
all <- read_csv("Batsmen vs bowler IPL 2008 to 2020.csv",show_col_types = FALSE)

check_valid=function(name)
{
  for(i in unique(all$batsman))
  {
    if(toupper(i)==toupper(name))
      return(TRUE)
  }
  for(i in unique(all$bowler))
  {
    if(toupper(i)==toupper(name))
      return(TRUE)
  }
  cat("Player not found\n")
  return(FALSE)
}


while(TRUE)
{
  cat("\n\nMENU\n1->Stats of batsman\n2->Graph of batsman\n3->Comparision between two batsman\n4->Bowler performance\n5->Comparision between two bowlers\n6->Exit\n\nPlease enter your choice")
  choice=suppressWarnings(as.numeric(readline())) 
  if(choice==1)
  {
    bat=readline("Enter the batsmen name -> ")
    
    if(check_valid(bat))
    {
      a=subset(all,toupper(batsman)==toupper(bat))
      
      cat("\n",toupper(bat))
      cat("\nTotal runs -> ",sum(a$runs_scored)," (",sum(a$balls_faced),")",sep = "")
      cat("\nStrike rate -> ",round(sum(a$runs_scored)*100/sum(a$balls_faced),2))
      cat("\nHighest runs against a bowler ->",max(a$runs_scored),"(",subset(a$bowler,a$runs_scored==max(a$runs_scored)),")")
      
      d="None"
      d=subset(a$bowler,a$runs_scored==max(a$runs_scored))
      cat("\nBest against ->",paste(d,sep = ",",collapse = " , "))
      
      c="None"
      c=subset(a$bowler,a$runs_scored==min(a$runs_scored) & ((a$runs_scored)*100/(a$balls_faced))<100)
      cat("\nWorst against ->",paste(c,sep = ",",collapse = " , "))
    }
  }
  
  else if(choice==2)
  {
    bat=readline("Enter the batsmen name -> ")
    
    if(check_valid(bat))
    {
      x=all %>% 
        filter(toupper(batsman) == toupper(bat) & runs_scored>=50)%>%
        ggplot(mapping=aes(x = bowler , y = runs_scored , label = bowler , fill=bat))+
        geom_bar(stat="identity")+
        scale_y_continuous(breaks = seq(0,200,10))+
        theme_bw()+
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 90))+
        labs(title = toupper(bat),
             x = "Bowlers",
             y = "Runs Scored")
      
      print(ggplotly(x))
    }
    
  }
  
  else if(choice==3)
  {
    bat1=readline("Enter the first batsmen name -> ")
    bat2=readline("Enter the second batsmen name -> ")
    
    if(check_valid(bat1)&(check_valid(bat2)))
    {
      bb=all %>%
        filter((toupper(batsman) == toupper(bat1) & runs_scored>=50) | (toupper(batsman) == toupper(bat2) & runs_scored>=50))%>%
        mutate(Run=case_when(toupper(batsman)==toupper(bat1)~runs_scored,
                             TRUE~-runs_scored),
               signal=case_when(toupper(batsman)==toupper(bat1)~1,TRUE~-1)) %>%
        ggplot()+
        geom_bar(aes(x = bowler , y = Run , fill = batsman ), stat="identity")+
        coord_flip()+
        scale_fill_manual(name='',values = c("indianred2","blueviolet"))+
        scale_y_continuous(breaks = seq(-200,200,10))+
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(title = paste("Comparision between",toupper(bat2),"and",toupper(bat1)),
             x = "Bowlers",
             y = "Runs Scored")
      
      print(ggplotly(bb))
    }
  }
  
  else if(choice==4)
  {
    bow=readline("Enter the bowler name -> ")
    
    if(check_valid(bow))
    {
      bow_bar=all%>%
              filter(toupper(bowler)==toupper(bow) & (runs_scored*6/balls_faced)<4 ) %>%
              ggplot(aes(x = batsman , y = round(runs_scored*6/balls_faced,2) ,fill=bow))+
              geom_bar(stat="identity")+
              scale_fill_discrete(name = NULL)+
              theme_bw()+
              theme(legend.position = "none",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.x = element_text(angle = 90))+
              labs(title = toupper(bow),
                   x = "Batsman",
                   y = "Economy")
      
      print(ggplotly(bow_bar))
    }
  }
  
  else if(choice==5)
  {
    bow1=readline("Enter the first bowler name -> ") 
    bow2=readline("Enter the second bowler name -> ")
    
    if(check_valid(bow1)&(check_valid(bow2)))
    {
      bow_bar=all%>%
              filter((toupper(bowler)==toupper(bow1) & (runs_scored*6/balls_faced)<4) | 
                       (toupper(bowler)==toupper(bow2) & (runs_scored*6/balls_faced)<4)) %>%
              mutate(Economy=case_when(toupper(bowler)==toupper(bow1)~round(runs_scored*6/balls_faced,2),
                                       TRUE~-round(runs_scored*6/balls_faced,2)),
                     signal=case_when(toupper(bowler)==toupper(bow1)~1,TRUE~-1)) %>%
              ggplot(aes(x = batsman , y = Economy ,fill=bowler) )+
              geom_bar(stat="identity")+
              coord_flip()+
              scale_fill_discrete(name = NULL)+
              theme_bw()+
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())+
              labs(title = paste("Comparision between",toupper(bow2),"and",toupper(bow1)),
                   x = "Batsman",
                   y = "Economy")
      
      print(ggplotly(bow_bar))
    }
  }
  
  else if(choice==6)
  {
    cat("\nThank you\n")
    break
  }
  
  else
    cat("\nEnter valid choice")
}