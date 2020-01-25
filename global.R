#Preamble
require('tidyverse') 
require('gridExtra')
require('reshape2')
require('shiny')
require('shinydashboard')
require('extrafont')
require('plotly')

#UDF Space 
BackTesting <- function(freq)     #UDF 1
{
  
  InputData <- read.csv("Dataset for R.csv",header=TRUE)    #Load raw file
  
  Title <-                              #Define the title string
    case_when(
      freq == "Daily" ~ "Daily",
      freq == "Weekly" ~ "Weekly",
      freq == "Monthly" ~ "Monthly",
      freq == "Quarterly" ~ "Quarterly",
      freq == "Yearly" ~ "Yearly"
    )
  
  freq <- subset(InputData,ï..Frequency==freq)            #Prepare the analysis table
  freq$ï..Frequency <- NULL
  freq$DatePseudo <- as.Date(freq$Date,format='%d/%m/%Y',origin="1970-01-01")
  freq$Date <- as.character(freq$DatePseudo)
  freq$ETF.Return <- as.numeric(c("-",diff(log(freq$ETF.Index))))
  freq$Index.Return <- as.numeric(c("-",diff(log(freq$Underlying.Index))))
  freq$Multiple <- freq$ETF.Return/freq$Index.Return
  freq$Non.Regular.Movement <- ifelse(freq$Multiple<0,1,0)
  freq$Non.Regular.Positive.Movements <- ifelse(freq$Non.Regular.Movement==1 & freq$ETF.Return>0,1,0)
  freq$Absolute.Deviation.From.Ideal.In.Basis.Points <- round(abs(freq$ETF.Return-(2*freq$Index.Return))*10000,digits=0)
  
  
  
  
  
  RegressionPlot <-                                                                      #Plot 1
    ggplot(data=freq, aes(x=Index.Return, y=ETF.Return)) +           
    geom_point(shape=16,alpha=1/2,colour="magenta") +    
    geom_smooth(method=lm,colour="cyan")+   
    labs(x="Index Return",y="ETF Return")+
    ggtitle(paste(Title,'Return Regression Plot'))
  # theme(text=element_text(family="courier new", size = 10))
  
  temp1 <- ggplot(data=freq,aes(x=DatePseudo))+
    geom_line(aes(y=Underlying.Index),color='cyan')+
    labs(x="Date", y="Level")+
    ggtitle("Index Levels")
  # theme(text=element_text(family="courier new", size = 10))
  temp2 <- ggplot(data=freq,aes(x=DatePseudo))+
    geom_line(aes(y=ETF.Index), color='orange')+
    labs(x="Date", y="Level")+
    ggtitle("ETF Levels")
  # theme(text=element_text(family="courier new", size = 10))  
  TimeEvolutionPlots <-                                                                  #Plot 2
    grid.arrange(temp1,temp2,nrow=1)
  
  ReturnAnalysisPlot <-                                                                  #Plot 3
    ggplot(data=freq,aes(x=DatePseudo))+                                   
    geom_line(aes(y=Index.Return,color='Index.Return'))+
    labs(x="Date", y="Index Return")+
    scale_color_discrete(name='Legend',labels=c('ETF Return','Index Return'))+
    geom_line(aes(y=ETF.Return,color='ETF.Return',alpha=0.1),show.legend = FALSE)+
    scale_y_continuous(sec.axis = sec_axis(~.,name="ETF Return"))+
    ggtitle(paste(Title,'Return Series Plot'))
  # theme(text=element_text(family="courier new", size = 10))
  
  freq$DatePseudo <- NULL
  
  myFinalPlot <- grid.arrange(TimeEvolutionPlots,RegressionPlot,ReturnAnalysisPlot,nrow=3,top=paste(Title,'Visualizations'))
  
  return(list(freq,myFinalPlot))
  
}

MCS <- function(Frequency,NumberofSimulation,StartingPoint) #UDF 2
{
  Title <- case_when(
    Frequency =="Daily" ~ "Daily",
    Frequency =="Weekly" ~ "Weekly",
    Frequency =="Monthly" ~ "Monthly",
    Frequency =="Quarterly" ~ "Quarterly",
    Frequency =="Yearly" ~ "Yearly"
  )
  SimTemp <- as.data.frame(BackTesting(Frequency)[1])
  Simulation <- as.data.frame(SimTemp[,4])          #Calling AnalysisofReturn to fetch results
  Simulation <- na.omit(Simulation)                                     #Cleaning and formatting data
  Simulation <- as.data.frame(Simulation)
  Simulation <- tibble::rowid_to_column(Simulation,"TimeStamp")
  names(Simulation)[2] <- "Returns"
  NumberofDays <- nrow(Simulation)                                      #Get number of data points
  MeanReturn <- mean(Simulation$Returns,na.rm=TRUE)
  ReturnVariance <- var(Simulation$Return,na.rm = TRUE)
  Drift <- MeanReturn-(ReturnVariance/2)
  ReturnSD <- sd(Simulation$Return,na.rm=TRUE)
  
  
  
  for (i in 1:NumberofSimulation)                                       #Loop through simulations
    
  {
    
    
    Simulation[1,i+2] <- StartingPoint
    
    
    for(j in 2:NumberofDays)                                        #Loop through days
      
    {
      
      RandomValue <- Drift + (ReturnSD*qnorm(runif(1)))
      Simulation[j,i+2] <- Simulation[j-1,i+2]*exp(RandomValue)
      
      
      
    }
    
  }
  
  Simulation$Returns <- NULL
  
  
  SimulationPlot <- melt(Simulation,id.vars="TimeStamp")
  SimulationPlot <- SimulationPlot[,c(2,1,3)]
  names(SimulationPlot)[1] <- "Simulation"
  
  plot4 <- ggplot(SimulationPlot,aes(x=TimeStamp,y=value,color=Simulation))+
    geom_line(alpha=0.1)+  #,se=FALSE)
    theme(legend.position="none")+
    ggtitle(paste(Title,'Returns Monte Carlo Simulation'))
  return(list(Simulation,plot4))
}