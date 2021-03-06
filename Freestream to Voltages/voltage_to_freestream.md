Introduction
============

Since Bagnold's seminal work in the 1940's, wind tunnels have been used to study aeolian geomorphology as it allows the user to have complete control over the system. One of the primary instruments in every wind tunnel is the pitot tube that sits inside the tunnel which is connected to a transducer and is used to determine the wind speed inside the tunnel. When an experiment is run in a wind tunnel, the transducer (connected to the pitot tube) is outputting voltages for the entire length of the experiment. These voltages can fluctuate due to noise. Aeolian geomorphologists aren't interested in the voltages, we want the wind speed. To get the wind speed one has to use the calibration curves provided by the manufacturer of the transducer to convert these voltages to a dynamic pressure. These dynamic pressures are then converted to wind speed.

This whole process can typically be done in a single spreadsheet, what makes it complicated is that we are not using a normal wind tunnel. I am working with Titan Wind Tunnel, which is unique because one can change the atmospheric pressure inside the tunnel, it can go as high as 20 bar. The atmospheric pressure also slightly changes throughout the experiment. There are two transducers connected to the pitot tube inside the tunnel (Tavis 1 and Tavis 2). We have set up multiple spreadsheets to convert the voltages from the transducer to wind speed. Each spreadsheet is designed for one transducer (either Tavis 1 or Tavis 2), and a certain atmospheric pressure that we have set the tunnel to run at (ex. 3 bar, 12.5 bar, 15 bar). The manufacturer of our transducers, Tavis, has sent us calibration data to convert to dynamic pressure from a voltage. All the calibration data is subdivided by the atmospheric pressure that the calibration was done at (ex. 1 bar, 5 bar, 10 bar). We find the correct calibration for the atmospheric pressure we are running our experiment under by interpolating between two curves that Tavis provided us (ex. we want to know the correct calibration to use for 3 bar, we will interpolate this data using the 1 and 5 bar calibration curves).

After the voltages are converted to a dynamic pressure, it is easy to convert it to a wind speed. The wind speeds that are reported are filled with noise so we smooth them over a 2 second interval using a moving average to get an "interpolated wind speed."

While we are running a wind tunnel experiment we are looking inside the tunnel for the time at which the grains begin to move (threshold wind speed). We record the time that threshold occurs and then look up this time in the spreadsheet to find the wind speed of the tunnel at the moment of threshold.

Another complication in how data reduction is currently done is that we use four different units of pressure (psi, bar, mm Hg, and Pa) because the instrumentation that tells us the pressure inside the tunnel is reported in psi, and the calibration data from Tavis is mm, and we work with bars but convert everything to Pascals for calculation purposes.

This all sounds complicated and messy because it is. The purpose of this program is to streamline the process that converts tranducer voltages to free stream wind velocities.

Source of Data
--------------

Data Emily has collected from the Titan Wind Tunnel at NASA Ames in Mountain View, CA. The data is in CSV format with two columns for transducer voltages and a column for the time the data was collected. The voltages are recorded every eighth of a second.

User Inputs
-----------

The program asks the user to manually enter the following information into the console:

-   Name of the CSV file that contains the voltages and times recorded by Tavis 1 and 2.
-   Atmospheric pressure of tunnel
-   Temperature inside tunnel
-   Time threshold was observed to occur at

Program Outputs
---------------

-   Wind speed at threshold, calculated using both Tavis 1 and Tavis 2 transducer, so there will be two wind speeds
-   Summary of all the inputs that the user types into the console

Step 1: Calculate the atmospheric density
-----------------------------------------

The air density (*ρ*) inside the tunnel is calculated using the following equation:

$$\\rho = \\frac{P}{RT}$$

where *P* is the absolute pressure in Pa, *T* is the temperature in K, and *R* is the dry air constant (*R* = 287.04 Kg K). The gauge pressure shows the pressure inside the tunnel and is given in psig. The guage pressure needs to be converted to absolute pressure.

``` r
#Program will ask user to input the starting and end gauge pressure of the wind tunnel and the temperature inside the tunnel
#(for purposes of knitting I have already entered in the answers to the user input questions, thus I have added a # sign in front of them)

  #P_start_input <- readline("What is the gauge pressure at the start of the experiment in psig?")  
  #P_end_input <- readline("What is the gauge pressure at the end of the experiment in psig?")
  #temp_input <- readline("What is the temperature inside the tunnel in degrees Celsius?")

  P_start_input <- "275"
  P_end_input <- "276"
  temp_input <- "20"


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
```

Step 2: Input the voltage data
------------------------------

User inputs the name of the experiment and the corresponding CSV file will be read into the program.

``` r
#(for purposes of knitting I have already entered in the answers to the user input questions, thus I have added a # sign in front of them)
#experiment<-readline("What is the name of the experiment CSV file? (ex. T-16-1278.csv)")
experiment<-"T-16-1278.csv"


# Read in the CSV file, skip the first 7 rows because data is not logged in these rows, define the names of the columns
CSV<-read.csv(experiment,skip=7,col.names=c("time","position","T1","T2","Channel3","Events1", "Events2"))

#T-16-1278
```

Step 3: Determine the curve needed to convert voltage to dynamic pressure
-------------------------------------------------------------------------

The manufacturer of the transducer (Tavis) provided calibration data to convert voltage to dynamic pressure. These curves were produced using the manufacturer's calibration for Tavis 1 and Tavis 2. The slope (VDC/psid) and y-intercept (VDC) were previously found for each static pressure (0-285 psig) and are recorded in the Tavis CSV file.

Read in Tavis Calibration Curves:

``` r
Tavis1<-read.csv("Tavis1.csv")
Tavis2<-read.csv("Tavis2.csv")
```

In order to get the correct calibration curve for the static pressure used in the experiment, you will have to interpolate between two of these curves. An If statement is used to determine the correct two curves to interpolate between based on the gauge pressure entered in step 1 (P\_gauge\_ave).

Need to figure out which points are closest to the gauge pressure, then use these points to interpolate.

Tavis 1:

``` r
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
```

Tavis 2:

``` r
#Determine the distance between the gauge pressure and the line pressures in the Tavis 1 data frame. Rank the distances. Will use the two that are closest to the gauge pressure (rank 1 and 2).

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
```

Step 4: Convert the voltage to dynamic pressure (psid)
------------------------------------------------------

From this point forward there will be two dataframes each dedicated to a different transducer.

``` r
#Set up Tavis 1 dataframe
CSV_T1<- CSV %>% 
  select(time, T1)

#Set up Tavis 1 dataframe
CSV_T2<- CSV %>% 
  select(time, T2)
```

To convert voltage to dynamic pressure use the interpolated slope and y-intercept:

$$Dynamic Pressure = \\frac{TavisVoltage-yint Interpolated}{Slope Interpolated}$$
 If the voltage is less than the yintercept than set the dynamic pressure (dPyn) to zero.

``` r
  #dynamic pressure calculated using Tavis 1 transducer 
CSV_T1 <- 
    CSV_T1 %>% 
    mutate(dynP_T1 = ifelse(CSV$T1<yint_Interpolated_T1,0,(CSV$T1-yint_Interpolated_T1)/Slope_Interpolated_T1))
   #dynamic pressure calculated using Tavis 2 transducer
CSV_T2 <- 
    CSV_T2 %>% 
    mutate(dynP_T2 = ifelse(CSV$T2<yint_Interpolated_T2,0,(CSV$T2-yint_Interpolated_T2)/Slope_Interpolated_T2))
```

Step 5: Convert dynamic pressure to freestream wind velocity
------------------------------------------------------------

Use the following formula to convert dynamic pressure (*P*<sub>*d**y**n*</sub> in Pa) to freestream wind velocity (*u*(*z*) in m/s). Use the air density (*ρ*) calculated in Step 1. Note that *P*<sub>*d**y**n*</sub> is the dynamic pressure of the gas and is a function of *z* (wind speed height).

$$u(z)=\\sqrt{\\frac{2P\_{dyn}}{\\rho}}$$

``` r
#Add an additional column to the Tavis 1 dataframe for freestream wind velocity
CSV_T1 <- 
    CSV_T1 %>% 
    mutate(u_T1 = sqrt((2*dynP_T1)/atm_density))

#Add an additional column to the Tavis 2 dataframe for freestream wind velocity
CSV_T2 <- 
    CSV_T2 %>% 
    mutate(u_T2 = sqrt((2*dynP_T2)/atm_density))
```

Step 6: Smooth the freestream wind velocity
-------------------------------------------

The transducers take voltages every 8th of a second. There is noise in this data so we will smooth out the freestream wind velocities that were calculated in Step 5. Will use the Smoothing Moving Average (SMA) function in the TTR package. Will smooth over a 2 second interval which is 16 time steps.

The smoothing algorithm smoothes the data points below, I want it to be smoothed based on the centre. In other words for 2 second smoothing, the smoothing function will take all 16 points below and smooth them. I want it to be 8 points above and 8 points below. To achieve this an offset will be applied.

``` r
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
```

Step 7: Determine the seconds since the start of the experiment
---------------------------------------------------------------

Experiment begins at 0 seconds. Each time step is an 8th of a second. Create a column that will have the seconds since the start of the experiment using a sequence fill. Increase by an 1/8 of a second (0.125).

``` r
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
```

Step 8: Input the time
----------------------

While the wind tunnel is running, an observer is looking at the bed of grains and is visually determining when threshold speed is occuring. They record the threshold time since the start of the experiment in MM:SS format.

``` r
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
```

Step 9: Find the freestream speed at threshold
----------------------------------------------

Look up the freestream velocity for the time inputed in step 8 for both Tavis 1 and Tavis 2

``` r
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
```

Step 10: Create a CSV file of all the program inputs and outputs
----------------------------------------------------------------

Save all the user inputs and outputs of script into one CSV file. List is given below:

-   Name of the CSV file that contains the voltages and times recorded by Tavis 1 and 2. (experiment)
-   Atmospheric pressure of tunnel (P\_start\_input; P\_end\_input)
-   Temperature inside tunnel (temp\_input)
-   Time threshold was observed to occur at (time\_input)
-   Wind speed at threshold, calculated using both Tavis 1 and Tavis 2 transducer, so there will be two wind speeds (u\_T1; u\_T2)

``` r
#Start by creating a data frame
 threshold_speed<- data.frame(experiment, P_start_input, P_end_input,temp_input, time_input, u_T1, u_T2 )

#Create a name for the CSV file, currently experiment is a character value that we need to modify, want to eliminate the .csv format
name<-gsub(".csv","_threshold.csv" , experiment)

#export to a CSV file
write.csv(threshold_speed, file = name)
```
