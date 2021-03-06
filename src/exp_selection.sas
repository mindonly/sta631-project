%let outdir=W:\MyStats631\BaseballProject\;
ods pdf file="&outdir\output.pdf" startpage=no style=htmlblue;
/* import teams csv*/
proc import datafile="W:\MyStats631\BaseballProject\Teams.csv"
     out=teams
     dbms=csv
     replace;
run;

/*GETTING THE EXPONENT*/
/*******************************************************************/
data teams2; *teams2 is teams with calculated columns;
	set teams;
	if yearID=1871 then hbp=.; *needed to fix orginal change required to read in properly;
	if yearID=1871 then sf=.; *same^;
	w_l = w/l;		*win/loss ratio;
	r_ra = r/ra; 	*runs/runs allowed ratio;
	log_wl = log10(w_l);
	log_rra = log10(r_ra);
	decade = floor(yearid/10)*10;
	win_perc = w/(w+l); *winning percentage;
	run_diff = r-ra; 	*run differential;
	obp = (H+BB+HBP)/(AB+BB+HBP+SF); 	*on base percentage;
	slg = ((H-(_2b+_3b+HR))+(_2b*2)+(_3b*3)+(HR*4))/(AB); *slugging;
	ops = OBP+SLG; 		*on base plus slugging;
	_1B = H - (_2B + _3B + HR); *singles;
run;

*proc export data=teams2
	outfile='W:\MyStats631\BaseballProject\teams2.csv'
	dbms=csv
	replace;
*run;

/* obtain and plot exponent for each decade */
ods exclude all; *supresses printed output but allows output to parms;
ods output ParameterEstimates=parms;
proc reg data=teams2 alpha=0.05;
	by decade;
	model log_wl = log_rra / NOINT clb;
run;
ods exclude none;
proc print data=parms;
	title2 'Estimates of Pythogorean Exponent by Decade';
run;
proc sgplot data=parms;
	scatter x=decade y=estimate / yerrorlower=lowercl yerrorupper=uppercl;
	xaxis label='Decade' ;
	yaxis label='Exponent';
run;
ods pdf text='Error bars indicate the 95% CI for the exponent estimation. Notice 
no particular pattern throughout.';

/* exponent by year */
ods exclude all;
ods output ParameterEstimates=parms2;
proc reg data=teams2 alpha=0.05;
	by yearID;
	model log_wl = log_rra / NOINT clb;
run;
ods exclude none;
proc sgplot data=parms2;
	*scatter x=yearid y=estimate; */ yerrorlower=lowercl yerrorupper=uppercl;
	series x=yearid y=estimate;
	xaxis label='Year' ;
	yaxis label='Exponent';
	title2 'Estimates of Pythagorean Exponent by Year';
run;
ods graphics;
proc reg data=parms2;
	model estimate = yearid / clb;
	title2 'Linear Regression of Pythagorean Exponent as a Function of Year';
run;

data parms3; *remove exp 2.7 from 1878 outlier, rerun lin reg;
	set parms2;
	if yearid = 1878 then delete;
run;
proc reg data=parms3;
	model estimate = yearid / clb;
	title2 'Linear Regression of Pythagorean Exponent as a Function of Year (without 1878 outlier)';
run;
ods pdf text='When the 1878 outlier is removed, the slope is no longer significant. Safe to assume 
variation in exponent is due to error alone.';

/* overall exponent from lin reg of logs */
proc reg data=teams2 alpha=0.05 plots=none;
	model log_wl = log_rra / NOINT clb;
	title2 'Linear Regression to get Overall Exponent';
run;
ods pdf text='Make note of the differences in the overall exponent estimation from the linear 
regression method and the following nonlinear method, which iteratively determines the exponent 
as it belongs in the formula.';

/* get exponent with nonlinear method */
proc nlin data=teams2 hougaard plots; *estimates exponent x;
	parms x=2;
	model win_perc = (r**x)/((r**x)+(ra**x));
	title2 'Estimating Pythagorean Exponent with Nonlinear Method';
run;
proc nlin data=teams2 hougaard;
	parms x=2 y=2;
	model win_perc = (r**x)/((r**x)+(ra**y));
	title2 'Estimating Different Exponents Within the Formula';
run;

/* LINEAR MODEL WINNING % LINEAR */
/***************************************************/
%let allvars= r ab _1b _2b _3b hr bb so sb cs ra er era 
cg sho sv IPouts ha hra bba soa e dp fp obp slg hbp sf;

/* just run diff 2011 like in paper */
proc reg data=teams2;
	where yearID=2011;
	model win_perc = run_diff;
	title2 'Run differential vs Winning Percent for 2011 (like in paper)';
run;

/* just run diff all years since 1970 */
proc reg data=teams2;
	model win_perc = run_diff;
	where yearid >= 1970;
	title2 'Overall Linear Regression for Run Differential vs Winning Percent after 1970';
run;

proc surveyselect noprint data=teams2 
	samprate=.6667 
	out=develop_sample
	seed=44444 outall;
run;
data train valid;
	set develop_sample;
	where yearID >= 1970;
	if selected then
		output train;
	else output valid;
run;

proc glmselect data=train valdata=valid;
	where yearID >= 1970;
	model win_perc = &allvars / selection=backward choose=cv;
	title2 'Allvars Model Winning Percent: Backward Selection';
run;

proc glmselect data=train valdata=valid;
	where yearID >= 1970;
	model win_perc = &allvars / selection=forward choose=cv;
	title2 'Allvars Model Winning Percent: Forward Selection';
run;

proc glmselect data=train valdata=valid;
	where yearID >= 1970;
	model win_perc = &allvars / selection=stepwise choose=cv;
	title2 'Allvars Model Winning Percent: Stepwise Selection';
run;

/* FROM HW 5 LASSO AND ELASTIC NET */
/* linear model using least squares on the training set */
proc glmselect  data=train valdata=valid;
	model win_perc = &allvars / selection=none 
		choose= cv stat=(sl aic adjrsq bic ase);
	title2 'Least Squares Regression';
run;

/* elastic net on the training set, with
lambda chosen by cross-validation */
proc glmselect  data=train valdata=valid;
	model win_perc = &allvars / selection=elasticnet
		choose=cv /*details=all*/ stat=(sl aic adjrsq bic ase);
	title2 'Elastic Net';
run;

/* lasso model on the training set, with lambda
chosen by cross-validation */
proc glmselect  data=train valdata=valid;
	model win_perc = &allvars / selection=lasso
		choose=cv stat=(sl aic adjrsq bic ase);
	title2 'Lasso';
run;

/* extra PLS and PCR stuff */
data develop_sample2;
   set develop_sample;
   if selected=0 then do;
      win_percTest=win_perc;
	  win_perc=.;
   end;
run;

/* PCR */
proc pls data=develop_sample2 method=PCR cv=testset(valid)  cvtest(stat=press seed=12345); ;        
	model win_perc = &allvars  /  solution;
	output out=pcr_pred predicted=pcr_pred;
	title2 'PCR';
run;

/* Fit a PLS model on the training set, with M chosen by 
cross-validation*/
proc pls data=pcr_pred method=PLS cv=testset(valid)  cvtest(stat=press seed=12345);
	model win_perc = &allvars  /  solution;
	output out=both_pred predicted=pls_pred;
	title2 'PLS';
run;

/* getting the test error for pcr and pls */
data both_pred2; 
	set both_pred;
	SerrorPCR=(pcr_pred-win_perctest)**2;
	SerrorPLS=(pls_pred-win_perctest)**2;
run;
proc means data=both_pred2;
	var SerrorPCR SerrorPLS;
	title2 'PCR and PLS error';
run;

ods pdf close;