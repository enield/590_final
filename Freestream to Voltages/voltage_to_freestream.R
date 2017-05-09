#Voltage to freestream velocity script
##For a complete explaination please see the accompanying .md file




#Load in libraries
  library(tidyverse)
  library(TTR) #used for time series data
  
#Step 1: Calculate the atmospheric density

 #Program will ask user to input the starting and end gauge pressure of the wind tunnel and the temperature inside the tunnel
 #(for purposes of knitting I have already entered in the answers to the user input questions, thus I have added a # sign in front of them)

P_start_input <- readline("What is the gauge pressure at the start of the experiment in psig?")  
P_end_input <- readline("What is the gauge pressure at the end of the experiment in psig?")
temp_input <- readline("What is the temperature inside the tunnel in degrees Celsius?")


#Convert the pressures and temperature entered by the user into a numeric format from character format
P_start<- as.numeric(unlist(strsplit(P_start_input, ",")))
P_end <- as.numeric(unlist(strsplit(P_end_input, ",")))
temp <- as.numeric(unlist(strsplit(temp_input, ",")))

#Calculate the average gauge pressure inside the tunnel
P_guage_ave <- (P_start + P_end)/2 

#The absolute pressure is calculated using the gauge pressure (psig). First the gauge pressure is converted to Pa (1 psi = 6894.75729 Pa) then its converted to absolute pressure by adding 100,000 Pa.
P_ab <- P_guage_ave*6894.75729+100000


#Convert the temperature from Celsius to Kelvin
temp<-temp+273.15

#calculate Atmospheric Density in kg/m^3 using formula 1
atm_density<-P_ab/(287.04*temp)  






#Step 2: Input the voltage data
  
  #User inputs the name of the experiment and the corresponding CSV file will be read into the program.

  experiment<-readline("What is the name of the experiment CSV file? (ex. T-16-1278.csv)")
  


# Read in the CSV file, skip the first 7 rows because data is not logged in these rows, define the names of the columns
CSV<-read.csv(experiment,skip=7,col.names=c("time","position","T1","T2","Channel3","Events1", "Events2"))






  
#Step 3: Determine the curve needed to convert voltage to dynamic pressure
  
#The manufacturer of the transducer (Tavis) provided calibration data to convert voltage to dynamic pressure. 
  #These curves were produced using the manufacturer's calibration for Tavis 1 and Tavis 2. 
  #The slope (VDC/psid) and y-intercept (VDC) were previously found for each static pressure (0-285 psig) 
  #and are recorded in the Tavis CSV file. 
  
  Tavis1<-read.csv("Tavis1.csv")
  Tavis2<-read.csv("Tavis2.csv")
  
  #In order to get the correct calibration curve for the static pressure used in the experiment, 
  #you will have to interpolate between two of these curves. An If statement is used to determine the 
  #correct two curves to interpolate between based on the gauge pressure entered in step 1 (P_gauge_ave).
  
  #Determine the distance between the gauge pressure and the line pressures in the Tavis 1 data frame. Rank the distances. Will use the two that are closest to the gauge pressure (rank 1 and 2).
  
  Tavis1 <- 
    Tavis1 %>% 
    mutate(dists = abs(LinePressure-P_guage_ave)) %>%
    mutate(rank=rank(dists))
  
  
  #List the two pressures that will be used to interpolate the slope and intercept
  
  Pressure1<- Tavis1 %>% filter(rank == 1) %>% select(LinePressure)
  Pressure2<- Tavis1 %>% filter(rank == 2) %>% select(LinePressure)
  
  #Interpolate the slope using the slopes from the two line pressures that are closest to the gauge pressure.
  Slope1 <- Tavis1 %>% filter(rank == 1) %>% select(Slope)
  Slope2 <- Tavis1 %>% filter(rank == 2) %>% select(Slope)
  
  
  Slope_Interpolated_T1<-Slope1+(P_guage_ave-Pressure1)/(Pressure2-Pressure1)*(Slope2-Slope1)
  #The slope is in a dataframe still, quickly extract it into a value
  Slope_Interpolated_T1<-Slope_Interpolated_T1[1,1]
  
  #Interpolate the intercept using the intercepts from the two line pressures that are closest to the gauge pressure.
  
  yint1 <- Tavis1 %>% filter(rank == 1) %>% select(yint)
  yint2 <- Tavis1 %>% filter(rank == 2) %>% select(yint)
  
  
  yint_Interpolated_T1<-yint1+(P_guage_ave-Pressure1)/(Pressure2-Pressure1)*(yint2-yint1)
  #The y-int is in a dataframe still, quickly extract it into a value
  yint_Interpolated_T1<-yint_Interpolated_T1[1,1]
  
  #Determine the distance between the gauge pressure and the line pressures in the Tavis 2 data frame. Rank the distances. Will use the two that are closest to the gauge pressure (rank 1 and 2).
  
  Tavis2 <- 
    Tavis2 %>% 
    mutate(dists = abs(LinePressure-P_guage_ave)) %>%
    mutate(rank=rank(dists))
  
  
  #List the two pressures that will be used to interpolate the slope and intercept
  
  Pressure1<- Tavis2 %>% filter(rank == 1) %>% select(LinePressure)
  Pressure2<- Tavis2 %>% filter(rank == 2) %>% select(LinePressure)
  
  #Interpolate the slope using the slopes from the two line pressures that are closest to the gauge pressure.
  Slope1 <- Tavis2 %>% filter(rank == 1) %>% select(Slope)
  Slope2 <- Tavis2 %>% filter(rank == 2) %>% select(Slope)
  
  
  Slope_Interpolated_T2<-Slope1+(P_guage_ave-Pressure1)/(Pressure2-Pressure1)*(Slope2-Slope1)
  #The slope is in a dataframe still, quickly extract it into a value
  Slope_Interpolated_T2<-Slope_Interpolated_T2[1,1]
  
  #Interpolate the intercept using the intercepts from the two line pressures that are closest to the gauge pressure.
  
  yint1 <- Tavis2 %>% filter(rank == 1) %>% select(yint)
  yint2 <- Tavis2 %>% filter(rank == 2) %>% select(yint)
  
  
  yint_Interpolated_T2<-yint1+(P_guage_ave-Pressure1)/(Pressure2-Pressure1)*(yint2-yint1)
  #The y-int is in a dataframe still, quickly extract it into a value
  yint_Interpolated_T2<-yint_Interpolated_T2[1,1]
  

  
  
  
  
  
  
#Step 4: Convert the voltage to dynamic pressure (psid)
    
    #From this point forward there will be two dataframes each dedicated to a different transducer.
    
    #Set up Tavis 1 dataframe
    CSV_T1<- CSV %>% 
    select(time, T1)
  
  #Set up Tavis 1 dataframe
  CSV_T2<- CSV %>% 
    select(time, T2)
  
  #If the voltage is less than the yintercept than set the dynamic pressure (dPyn) to zero.
  
  #dynamic pressure calculated using Tavis 1 transducer 
  CSV_T1 <- 
    CSV_T1 %>% 
    mutate(dynP_T1 = ifelse(CSV$T1<yint_Interpolated_T1,0,(CSV$T1-yint_Interpolated_T1)/Slope_Interpolated_T1))
  #dynamic pressure calculated using Tavis 2 transducer
  CSV_T2 <- 
    CSV_T2 %>% 
    mutate(dynP_T2 = ifelse(CSV$T2<yint_Interpolated_T2,0,(CSV$T2-yint_Interpolated_T2)/Slope_Interpolated_T2))
  

  
  
  
  
  
#Step 5: Convert dynamic pressure to freestream wind velocity
    
    #Add an additional column to the Tavis 1 dataframe for freestream wind velocity
    CSV_T1 <- 
    CSV_T1 %>% 
    mutate(u_T1 = sqrt((2*dynP_T1)/atm_density))
  
  #Add an additional column to the Tavis 2 dataframe for freestream wind velocity
  CSV_T2 <- 
    CSV_T2 %>% 
    mutate(u_T2 = sqrt((2*dynP_T2)/atm_density))
  

  
  
  
  
  
#Step 6: Smooth the freestream wind velocity 

    
    #Add an additional column to the Tavis 1 data frame to smooth the wind velocity
    CSV_T1 <-
    CSV_T1 %>%
    mutate(u_T1_Smoothed=SMA(u_T1, n=16))%>%
    #The smoothing offseted the data a little. To account for this offest, move the entire smoothed column up 8 rows(equivalent to 1 second)
    mutate(u_T1_Smoothed_Offset=c(u_T1_Smoothed[-(seq(8))], rep(NA, 8)))
  
  #Add an additional column to the Tavis 2 data frame to smooth the wind velocity
  CSV_T2 <-
    CSV_T2 %>%
    mutate(u_T2_Smoothed=SMA(u_T2, n=16))%>%
    #The smoothing offseted the data a little. To account for this offest, move the entire smoothed column up 8 rows(equivalent to 1 second)
    mutate(u_T2_Smoothed_Offset=c(u_T2_Smoothed[-(seq(8))], rep(NA, 8)))
  

  
  
  
  
  
#Step 7: Determine the seconds since the start of the experiment
    
    #Experiment begins at 0 seconds. Each time step is an 8th of a second. 
    #Create a column that will have the seconds since the start of the experiment 
    #using a sequence fill. Increase by an 1/8 of a second (0.125).
    
    #Determine the number of rows, this will be used to determine the end of the sequence. 
    n_row<-nrow(CSV_T1)
  
  #Find out the total length of time of the experiment (seconds) by multiplying the number of rows by 0.125
  Total_Time<-n_row*0.125
  
  #Create a new column for the time since the start of the experiment for bothe the Tavis 1 and Tavis 2 data frames
  CSV_T1 <-
    CSV_T1 %>%
    mutate(time_since_start=seq(0,Total_Time-0.125, by = 0.125) )
  
  
  CSV_T2 <-
    CSV_T2 %>%
    mutate(time_since_start=seq(0,Total_Time-0.125, by = 0.125) )
  

  
  
  
  
    
#Step 8: Input the time
    
    #While the wind tunnel is running, an observer is looking at the bed of grains and is 
    #visually determining when threshold speed is occuring. They record the threshold time 
    #since the start of the experiment in MM:SS format.
    
    #Create a function that will convert Minute:Second format into seconds.
    mmss_to_ss <- function  (string)
    {
      mmss <- strsplit (string, ":", T)
      mm <- as.numeric (mmss[[1]][1])
      ss <- as.numeric (mmss[[1]][2])
      return (mm * 60 + ss)
    }
  
  #Program asks user to input the time in MM:SS format
  #(for purposes of knitting I have already entered in the answers to the user input questions, thus I have added a # sign in front of them)
  # time_input <- readline("At why time is threshold occuring (answer in MM:SS format)?")
  time_input<-"1:25"
  
  #Convert into the time inputted into seconds from MM:SS format
  time_sec<-mmss_to_ss(time_input)
  
 
  
  
  
  
    
#Step 9: Find the freestream speed at threshold
    
    #Look up the freestream velocity for the time inputed in step 8 for both Tavis 1 and Tavis 2
    
    #Tavis 1
    
    #Look up speed by filtering the data frame
    u_CSV_T1<-CSV_T1%>%
    filter(time_since_start==time_sec)
  
  #The freestream velocity is in the first row and sixth column, extract this value
  u_T1<- u_CSV_T1[1,6]
  
  #Tavis 2
  
  #Look up speed by filtering the data frame
  u_CSV_T2<-CSV_T2%>%
    filter(time_since_start==time_sec)
  
  #The freestream velocity is in the first row and sixth column, extract this value
  u_T2<- u_CSV_T2[1,6]
  
  
  
  
  
  
  
  
    
  #Step 10: Create a CSV file of all the program inputs and outputs
    
    #Start by creating a data frame
    threshold_speed<- data.frame(experiment, P_start_input, P_end_input,temp_input, time_input, u_T1, u_T2 )
  
  #Create a name for the CSV file, currently experiment is a character value that we need to modify, want to eliminate the .csv format
  name<-gsub(".csv","_threshold.csv" , experiment)
  
  #export to a CSV file
  write.csv(threshold_speed, file = name)
  
  