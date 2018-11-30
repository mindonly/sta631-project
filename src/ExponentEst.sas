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
run;

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
	scatter x=yearid y=estimate / yerrorlower=lowercl yerrorupper=uppercl; *can remove error bars;
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
proc nlin data=teams3 hougaard plots; *estimates exponent x;
	parms x=2;
	model win_perc = (r**x)/((r**x)+(ra**x));
	title2 'Estimating Pythagorean Exponent with Nonlinear Method';
run;
proc nlin data=teams3 hougaard;
	parms x=2 y=2;
	model win_perc = (r**x)/((r**x)+(ra**y));
	title2 'Estimating Different Exponents Within the Formula';
run;

ods pdf close;

/* LINEAR MODEL WINNING % LINEAR */
/***************************************************/

%let allvars= run_diff obp slg ops pythag h ra r er;

/* just run diff vs pythag 2011 like in paper */
proc reg data=teams2;
	where yearID=2011;
	model win_perc = run_diff;
run;
proc reg data=teams2;
	where yearID=2011;
	model win_perc = pythag;
run;

/* just run diff vs pythag all years*/
proc reg data=teams2;
	model win_perc = run_diff;
run;
proc reg data=teams2;
	model win_perc = pythag;
run;

proc reg data=teams2;
	where yearID >= 1970;
	model win_perc = pythag / noint;
run; 

proc glmselect data=teams2;
	where yearID >= 1970;
	model win_perc = r ra h ha bb bba ab slg er era e dp / selection=backward;
run;

