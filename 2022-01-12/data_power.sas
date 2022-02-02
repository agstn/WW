proc power;
   twosamplesurvival test=LOGRANK   
   curve("Treatment")   = (0 12 24 36 48):(1 0.960 0.920 0.880 0.840) /* 4.0% */
   refsurvival = "Treatment"
   hazardratio =  0.75 to 0.80 by 0.005 
   accrualtime =  18 to 30 by 1 
   followuptime =  3 to 9 by 1 
   groupweights = (1 1)
   ntotal = 9900 
   sides = 2
   alpha = 0.05
   power = .;
   ods output Output=out_40;
run;

proc power;
   twosamplesurvival test=LOGRANK   
   curve("Treatment")   = (0 12 24 36 48):(1 0.955 0.910 0.865 0.820) /* 4.5% */
   refsurvival = "Treatment"
   hazardratio =  0.75 to 0.80 by 0.005 
   accrualtime =  18 to 30 by 1 
   followuptime =  3 to 9 by 1 
   groupweights = (1 1)
   ntotal = 9900 
   sides = 2
   alpha = 0.05
   power = .;
   ods output Output=out_45;
run;


proc power;
   twosamplesurvival test=LOGRANK   
   curve("Treatment")   = (0 12 24 36 48):(1 0.950 0.900 0.850 0.800) /* 5.0% */
   refsurvival = "Treatment"
   hazardratio =  0.75 to 0.80 by 0.005 
   accrualtime =  18 to 30 by 1 
   followuptime =  3 to 9 by 1 
   groupweights = (1 1)
   ntotal = 9900 
   sides = 2
   alpha = 0.05
   power = .;
   ods output Output=out_50;
run;

proc power;
   twosamplesurvival test=LOGRANK   
   curve("Treatment")   = (0 12 24 36 48):(1 0.945 0.890 0.835 0.780) /* 5.5% */
   refsurvival = "Treatment"
   hazardratio =  0.75 to 0.80 by 0.005 
   accrualtime =  18 to 30 by 1 
   followuptime =  3 to 9 by 1 
   groupweights = (1 1)
   ntotal = 9900 
   sides = 2
   alpha = 0.05
   power = .;
   ods output Output=out_55;
run;

proc power;
   twosamplesurvival test=LOGRANK   
   curve("Treatment")   = (0 12 24 36 48):(1 0.940 0.880 0.820 0.760) /* 6.0% */
   refsurvival = "Treatment"
   hazardratio =  0.75 to 0.80 by 0.005 
   accrualtime =  18 to 30 by 1 
   followuptime =  3 to 9 by 1 
   groupweights = (1 1)
   ntotal = 9900 
   sides = 2
   alpha = 0.05
   power = .;
   ods output Output=out_60;
run;

data data_power;

 set out_40 (in = out_40) 
     out_45 (in = out_45)
     out_50 (in = out_50) 
     out_55 (in = out_55)
     out_60 (in = out_60);
     
  if out_40 = 1 then hazard_rate = 4.0;
  if out_45 = 1 then hazard_rate = 4.5;
  if out_50 = 1 then hazard_rate = 5.0;
  if out_55 = 1 then hazard_rate = 5.5;
  if out_60 = 1 then hazard_rate = 6.0;
  
run;

proc export
 data = data_power
 outfile = 'data_power.csv'
 dbms = csv
 replace;
run;