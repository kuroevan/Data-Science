data cars; 
infile '/home/evangelosg0/sasuser.v94/carmpgdata_26 (2).txt' dlm= '	' firstobs=2 dsd;
length Auto $30;
input Auto MPG	CYLINDERS	SIZE	HP	WEIGHT	ACCEL	ENG_TYPE ;
proc print data=cars;
run;


*Use PROC MI to discover the missing values 
patterns and to decide what MI options to use;

Title 'Identifying Missing Patterns with PROC MI';
ODS SELECT MISSPATTERN;
PROC MI data = cars seed = 100 nimpute = 0;
VAR MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
MCMC;
RUN;
QUIT;

PROC MI DATA = cars OUT = carout SEED = 100 NIMPUTE = 5;
VAR CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
MCMC; *imputes for arbitrary missing pattern;
RUN;
QUIT;

PROC PRINT data = carout;
RUN;

PROC SORT data = carout;
BY _imputation_;
RUN;



*Use PROC REG to analyze the multiple data sets while
 outputting information to be used in MIANALYZE.;

PROC REG data = cars;
MODEL MPG = CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
OUTPUT OUT = regcars
RSTUDENT = rstudent
COOKD = cookd;
RUN;
QUIT;


TITLE 'MLR on Each Imputed Dataset';
PROC REG DATA = carout OUTEST = outreg COVOUT;
MODEL MPG = CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
BY _IMPUTATION_;
RUN;
QUIT;


PROC MIANALYZE DATA = outreg;
MODELEFFECTS CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE Intercept;
RUN;


* likewise deletion;
Title 'Predicting MPG with Listwise Deletion Imputation';
PROC REG data = cars;
MODEL MPG = CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
OUTPUT OUT = carsdelet
RSTUDENT = rstudent
COOKD = cookd;
RUN;
QUIT;


*exploring averages ;
*aveages for likewise;
PROC MEANS data = carsdelet N NMISS MEAN STD STDERR CLM Q1 MEDIAN Q3;
VAR MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
RUN;

*averages for mlr;

PROC MEANS data = regcars N NMISS MEAN STD STDERR CLM Q1 MEDIAN Q3;
VAR MPG CYLINDERS SIZE HP WEIGHT ACCEL;
RUN;



*scatter plot class by cylinders ;
PROC SGSCATTER data = cars;
title 'SP Matrix for Cars';
matrix MPG  SIZE HP WEIGHT ACCEL / group=CYLINDERS diagonal=(histogram);
RUN;

