.libPaths("//SPVIPH07/testvip0002/03.Individual/Ramachandran/Shiny Practice/Packages")
# install.packages("tidyverse",dependencies=TRUE)
# install.packages("gridExtra",dependencies=TRUE)
# install.packages("reshape2",dependencies=TRUE)
# install.packages("shiny",dependencies=TRUE)
# install.packages("shinydashboard",dependencies=TRUE)
# install.packages("extrafont",dependencies = TRUE)
# install.packages("plotly",dependencies=TRUE)
require('tidyverse')
require('gridExtra')
require('reshape2')
require('shiny')
require('shinydashboard')
require('extrafont')
require('plotly')
font_import()



InputData <- read.csv("Input/Dataset for R.csv",header=TRUE)                     #Load raw file


AnalysisofReturn <- function(Frequency)                                          #Function to calculate all the relevant, time point-to-time point data
{
  
  
  Frequency <- subset(InputData,ï..Frequency==Frequency)
  Frequency$ï..Frequency <- NULL
  Frequency$Date <- as.Date(Frequency$Date,format='%d/%m/%Y',origin="1970-01-01")
  Frequency$ETF.Return <- as.numeric(c("-",diff(log(Frequency$ETF.Index))))
  Frequency$Index.Return <- as.numeric(c("-",diff(log(Frequency$Underlying.Index))))
  Frequency$Multiple <- Frequency$ETF.Return/Frequency$Index.Return
  Frequency$Non.Regular.Movement <- ifelse(Frequency$Multiple<0,1,0)
  Frequency$Non.Regular.Positive.Movements <- ifelse(Frequency$Non.Regular.Movement==1 & Frequency$ETF.Return>0,1,0)
  Frequency$Absolute.Deviation.From.Ideal.In.Basis.Points <- round(abs(Frequency$ETF.Return-(2*Frequency$Index.Return))*10000,digits=0)
  
  return(Frequency)
}

Visualizations <- function(AOR)                                            #Function to visualize the results
{
  
  Title <- case_when(
    nrow(AOR) ==2162 ~ "Daily",
    nrow(AOR) ==448 ~ "Weekly",
    nrow(AOR) ==103 ~ "Monthly",
    nrow(AOR) ==35 ~ "Quarterly",
    nrow(AOR) ==9 ~ "Yearly"
  )
  
  
  
  Plot1 <- ggplot(data=AOR, aes(x=Index.Return, y=ETF.Return)) +           #Regression Plot
    geom_point(shape=16,alpha=1/2,colour="magenta") +    
    geom_smooth(method=lm,colour="cyan")+   
    labs(x="Index Return",y="ETF Return")+
    ggtitle(paste(Title,'Return Regression Plot'))
  P1 <- ggplotly(Plot1)
  
  
  temp1 <- ggplot(data=AOR,aes(x=Date))+
    geom_line(aes(y=Underlying.Index),color='cyan')+
    labs(y="Index Return")
  temp2 <- ggplot(data=AOR,aes(x=Date))+
    geom_line(aes(y=ETF.Index), color='orange')+
    labs(y="ETF Return")+
    ggtitle(paste(Title,'Index and ETF Movement'))
  # Plot2 <- grid.arrange(temp1,temp2,nrow=1,top=paste(Title,'Movement'))    #Time Evolution Plot
  # P2 <- ggplotly(Plot2)
    P2 <- subplot(temp1,temp2)
  
  
  Plot3 <- ggplot(data=AOR,aes(x=Date))+                                   #Return Analysis Plot
    geom_line(aes(y=Index.Return,color='Index.Return'))+
    labs(y="Index Return")+
    scale_color_discrete(name='Legend',labels=c('ETF Return','Index Return'))+
    geom_line(aes(y=ETF.Return,color='ETF.Return',alpha=0.1),show.legend = FALSE)+
    scale_y_continuous(sec.axis = sec_axis(~.,name="ETF Return"))+
    ggtitle(paste(Title,'Return Series Plot'))
  P3 <- ggplotly(Plot3)
  
  
  return(list(P1,P2,P3))
  
}

Daily <- AnalysisofReturn("Daily")
Visualizations(Daily)

Yearly <- AnalysisofReturn("Yearly")
Visualizations(Yearly)

MCS <- function(Frequency,NumberofSimulation,StartingPoint)
{
  Title <- case_when(
    Frequency =="Daily" ~ "Daily",
    Frequency =="Weekly" ~ "Weekly",
    Frequency =="Monthly" ~ "Monthly",
    Frequency =="Quarterly" ~ "Quarterly",
    Frequency =="Yearly" ~ "Yearly"
  )
  
  Simulation <- as.data.frame(AnalysisofReturn(Frequency)[,4])          #Calling AnalysisofReturn to fetch results
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
  
  
  Simulation <- melt(Simulation,id.vars="TimeStamp")
  Simulation <- Simulation[,c(2,1,3)]
  names(Simulation)[1] <- "Simulation"
  
  plot4 <- ggplot(Simulation,aes(x=TimeStamp,y=value,color=Simulation))+
    geom_line(alpha=0.1)+  #,se=FALSE)
    theme(legend.position="none")+
    ggtitle(paste(Title,'Returns Monte Carlo Simulation'))
  return(list(Simulation,plot4))
}

shinyAppDir("//SPVIPH07/testvip0002/03.Individual/Ramachandran/Shiny Practice/Codes/ShinyApp/ETFAnalysis")


font_import(pattern="courbd.ttf")
font_import(pattern="courbi.ttf")
font_import(pattern="couri.ttf")
font_import(pattern="cour.ttf")
