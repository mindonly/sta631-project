/* import teams csv*/
proc import datafile="W:\MyStats631\BaseballProject\Teams.csv"
     out=teams
     dbms=csv
     replace;
run;

/*GETTING THE EXPONENT*/
/*******************************************************************/

/* teams2 is teams with log transformations needed to get exponents */
data teams2;
	set teams;
	w_l = w/l;
	r_ra = r/ra;
	log_wl = log10(w_l);
	log_rra = log10(r_ra);
	decade = round(yearID, 10);
run;

/* obtain exponent for each decade */
ods graphics off;
ods output ParameterEstimates=parms;
proc reg data=teams2 alpha=0.05;
	by decade;
	model log_wl = log_rra / NOINT clb;
run;
proc print data=parms;
run;

/* plot exponent by decade */
proc sgplot data=parms;
	scatter x=decade y=estimate / yerrorlower=lowercl yerrorupper=uppercl;
	xaxis label='Decade' ;
	yaxis label='Exponent';
run;

/* lin regression exponent by year */
ods output ParameterEstimates=parms2;
proc reg data=teams2 alpha=0.05;
	by yearID;
	model log_wl = log_rra / NOINT clb;
run;
proc sgplot data=parms2;
	scatter x=yearid y=estimate;
	xaxis label='Year' ;
	yaxis label='Exponent';
run;
ods graphics;
proc reg data=parms2;
	model estimate = yearid / clb;
run;

/* LINEAR MODEL WINNING % LINEAR */
/***************************************************/

/* need to use correct exponent for your time interval! 
predict? */

%let allvars= run_diff obp slg ops pythag h ra r er;

data teams3;
	set teams;
	win_perc = w/(w+l);
	run_diff = r-ra;
	obp = (H+BB+HBP)/(AB+BB+HBP+SF);
	slg = ((H-(_2b+_3b+HR))+(_2b*2)+(_3b*3)+(HR*4))/(AB);
	ops = OBP+SLG;
	pythag = (r**1.84449)/((r**1.84449)+(ra**1.84449));
run;

/* just run diff vs pythag 2011 like in paper */
proc reg data=teams3;
	where yearID=2011;
	model win_perc = run_diff;
run;
proc reg data=teams3;
	where yearID=2011;
	model win_perc = pythag;
run;

/* just run diff vs pythag all years*/
proc reg data=teams3;
	model win_perc = run_diff;
run;
proc reg data=teams3;
	model win_perc = pythag;
run;

proc reg data=teams3;
	where yearID >= 1970;
	model win_perc = pythag / noint;
run; 

proc glmselect data=teams3;
	where yearID >= 1970;
	model win_perc = r ra h ha bb bba ab slg er era e dp / selection=backward;
run;
