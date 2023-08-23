
%let user = 54137;
libname in "C:\Users\54137\ICF\CDC ADIA Project - ICF Private Channel - ICF Private Channel\Study 1+3\Study 3\04_Analysis\03_SAS Data Management Files\SAS Data Files to Input";
libname out "C:\Users\54137\ICF\CDC ADIA Project - ICF Private Channel - ICF Private Channel\Study 1+3\Study 3\04_Analysis\03_SAS Data Management Files\Data";
/*libname nls 'C:\Users\55484\OneDrive - ICF\Documents\ADIA\Data';*/

/* TRADITIONAL ACES */
*creating a binary variable for household mental illness;
proc freq data= in.sample_f3;
	tables mentill_2012 mentill_2014 mentill_2016 mentill_2018/missing;
run;
data one;
	set in.sample_f3;                 
	array nvarlist _numeric_;    /* Consolidating missing values between mother and CYA datasets */
do over nvarlist;
if nvarlist = .D then nvarlist = .;  
if nvarlist = .R then nvarlist = .; 
if nvarlist = .N then nvarlist = .;  
if nvarlist = .V then nvarlist = .;  
if nvarlist = .I then nvarlist = .;
end; 
	mentill = sum(mentill_2012, mentill_2014, mentill_2016, mentill_2018);
	label mentill = "BEF AGE 18 R LIVE W/ ANYONE DEPR/MENT ILL/SUICIDAL";
run;
*checking that frequency of combined variable matched individual years;
proc freq data= one;
	tables mentill mentill_2012 mentill_2014 mentill_2016 mentill_2018/missing;
run;
proc print data=one (obs=75);
var mentill mentill_2012 mentill_2014 mentill_2016 mentill_2018;
where mentill>=2;
run;

*creating a binary variable for household alcohol/substance use;
*done;
data two;
	set one;
	subsuse = sum(subsuse_2012, subsuse_2014, subsuse_2016, subsuse_2018);
	label subsuse = "BEF AGE 18 LIVE W/ ANYONE A PROB DRINKER/ALCOHOLIC";
run;
*checking that frequency of combined variable matched individual years;
proc freq data= two;
	tables subsuse subsuse_2012 subsuse_2014 subsuse_2016 subsuse_2018/missing;
run;
proc print data=two (obs=75);
var subsuse subsuse_2012 subsuse_2014 subsuse_2016 subsuse_2018;
run;
*creating a binary variable for household physical abuse;
data three;
	set two;
	physabu = sum (physabu_2012, physabu_2014, physabu_2016, physabu_2018);
	if physabu =2 then physabu=1; /* combining responses for once and more than once */
	label physabu_2012 = "BEF AGE 18 HOW OFT ADULT IN HH PHYSICALLY HARMED R";
run;
*checking that frequency of combined variable matched individual years;
proc freq data=three;
	tables physabu physabu_2012 physabu_2014 physabu_2016 physabu_2018/missing;
run;
proc print data=three (obs=75);
var physabu physabu_2012 physabu_2014 physabu_2016 physabu_2018;
run;
*creating binary variable for parental divorce/seperation;
data four;
set three;
divorce=0;
array divorce_(*) divorce_1979-divorce_2000;
do i=1 to Dim(divorce_);
if divorce_(i) in (2,3,5) then divorce = 1;
end;
*else if divorce_(i) = . then divorce = .;
divorce_sum= sum (divorce_1979, divorce_1980, divorce_1981, divorce_1982, divorce_1983, divorce_1984, divorce_1985, divorce_1986,
divorce_1987, divorce_1988, divorce_1989, divorce_1990, divorce_1991, divorce_1992, divorce_1993, divorce_1994, divorce_1996,
divorce_1998, divorce_2000, divorce_2002, divorce_2004, divorce_2006, divorce_2008, divorce_2010, divorce_2012, divorce_2014,
divorce_2016, divorce_2018);
if divorce_sum=. then divorce=.;
run;
proc freq data=four;
tables divorce divorce_sum divorce_1979-divorce_2000/missing;
run;
proc print data=four (obs=75);
*where divorce_sum=.;
var divorce divorce_sum divorce_1979-divorce_2000;
run;

*creating binary variable for incarceration;
data five;
set four;
incarce = 0;
incarce = sum (incarce_2006, incarce_2008, incarce_2010, incarce_2012, incarce_2014, incarce_2016, incarce_2018);
if incarce => 1 then incarce=1;
run;
proc freq data=five;
tables incarce incarce_2006 incarce_2008 incarce_2010 incarce_2012 incarce_2014 incarce_2016 incarce_2018/missing;
run;
proc print data=five (obs=75);
where incarce=1; /*checking that 1s were assigned correctly*/
var incarce incarce_2006 incarce_2008 incarce_2010 incarce_2012 incarce_2014 incarce_2016 incarce_2018;
run;
/* EXPANDED ACES */

*creating a binary variable for parental love and affection;
*1 = none (0) or a little (1), 0 = quite a lot (2) or a great deal (3);
data six;
	set five;
	loveaff = sum(loveaff_2012, loveaff_2014, loveaff_2016, loveaff_2018);
	if loveaff = 0 or loveaff = 1 then loveaff =1;
	if loveaff = 2 or loveaff = 3 then loveaff = 0;
run;
proc freq data=six;
tables loveaff loveaff_2012 loveaff_2014 loveaff_2016 loveaff_2018/missing;
run;
proc print data=six (obs=75);
*where loveaff=1;
var loveaff loveaff_2012 loveaff_2014 loveaff_2016 loveaff_2018;
run;

*doing an initial check by freqyency and raw data of discrimination items;
proc freq data=six;
tables dcourte_2012 dcourte_2014 dcourte_2016 drespec_2012 drespec_2014 drespec_2016 dservic_2012 dservic_2014 
dservic_2016 dnsmart_2012 dnsmart_2014 dnsmart_2016 dafraid_2012 dafraid_2014 dafraid_2016 ddishon_2012 ddishon_2014 
ddishon_2016 dbetter_2012 dbetter_2014 dbetter_2016 dinsult_2012 dinsult_2014 dinsult_2016 dthreat_2012 dthreat_2014 
dthreat_2016 /missing;
run;
proc print data=six (obs=75);
var dcourte_2012 dcourte_2014 dcourte_2016 drespec_2012 drespec_2014 drespec_2016 dservic_2012 dservic_2014 
dservic_2016 dnsmart_2012 dnsmart_2014 dnsmart_2016 dafraid_2012 dafraid_2014 dafraid_2016 ddishon_2012 ddishon_2014 
ddishon_2016 dbetter_2012 dbetter_2014 dbetter_2016 dinsult_2012 dinsult_2014 dinsult_2016 dthreat_2012 dthreat_2014 
dthreat_2016;
run;

*creating a binary variable for reporting any form of discrimination;
data seven;
set six;
discrim=0;
array discrim_ {27} dcourte_2012 dcourte_2014 dcourte_2016 drespec_2012 drespec_2014 drespec_2016 dservic_2012 dservic_2014 dservic_2016 
dnsmart_2012 dnsmart_2014 dnsmart_2016 dafraid_2012 dafraid_2014 dafraid_2016 ddishon_2012 ddishon_2014 ddishon_2016 dbetter_2012 
dbetter_2014 dbetter_2016 dinsult_2012 dinsult_2014 dinsult_2016 dthreat_2012 dthreat_2014 dthreat_2016;
do i=1 to 27;
if discrim_(i)in (1,2,3,4) then discrim = 1;
end;
discrim_sum = sum(dcourte_2012, dcourte_2014, dcourte_2016, drespec_2012, drespec_2014, drespec_2016, dservic_2012, dservic_2014, dservic_2016, 
dnsmart_2012, dnsmart_2014, dnsmart_2016, dafraid_2012, dafraid_2014, dafraid_2016, ddishon_2012, ddishon_2014, ddishon_2016, dbetter_2012, 
dbetter_2014, dbetter_2016, dinsult_2012, dinsult_2014, dinsult_2016, dthreat_2012, dthreat_2014, dthreat_2016);
if discrim_sum=. then discrim=.;
run;
proc freq data=seven;
tables discrim dcourte_2012 dcourte_2014 dcourte_2016 drespec_2012 drespec_2014 drespec_2016 dservic_2012 dservic_2014 
dservic_2016 dnsmart_2012 dnsmart_2014 dnsmart_2016 dafraid_2012 dafraid_2014 dafraid_2016 ddishon_2012 ddishon_2014 
ddishon_2016 dbetter_2012 dbetter_2014 dbetter_2016 dinsult_2012 dinsult_2014 dinsult_2016 dthreat_2012 dthreat_2014 
dthreat_2016 /missing;
run;
proc print data=seven (obs=75);
var discrim discrim_sum dcourte_2012 dcourte_2014 dcourte_2016 drespec_2012 drespec_2014 drespec_2016 dservic_2012 dservic_2014 
dservic_2016 dnsmart_2012 dnsmart_2014 dnsmart_2016 dafraid_2012 dafraid_2014 dafraid_2016 ddishon_2012 ddishon_2014 
ddishon_2016 dbetter_2012 dbetter_2014 dbetter_2016 dinsult_2012 dinsult_2014 dinsult_2016 dthreat_2012 dthreat_2014 
dthreat_2016;
run;

*creating binary variable for mother ACE hh mental illness;
data eight;
	set seven;
	mmental = sum(mmental_2012, mmental_2014, mmental_2016, mmental_2018);
	if mmental = 2 then mmental = 1; /*merging response for once and more than once*/
	label mmental = "MOTHER - BEF AGE 18 R LIVE W/ ANYONE DEPR/MENT ILL/SUICIDAL";
run;
*checking that frequency of combined variable matched individual years;
proc freq data= eight;
	tables mmental mmental_2012 mmental_2014 mmental_2016 mmental_2018/missing;
run;
proc print data=eight (obs=75);
*where mmental=1;
var mmental mmental_2012 mmental_2014 mmental_2016 mmental_2018;
run;

*creating a binary variable for mother ACE hH alcohol/substance use;
data nine;
	set eight;
	msubstu = sum(msubstu_2012, msubstu_2014, msubstu_2016, msubstu_2018);
	label msubstu = "MOTHER - BEF AGE 18 LIVE W/ ANYONE A PROB DRINKER/ALCOHOLIC";
run;
*checking that frequency of combined variable matched individual years;
proc freq data= nine;
	tables msubstu msubstu_2012 msubstu_2014 msubstu_2016 msubstu_2018/missing;
run;
proc print data=nine (obs=75);
var msubstu msubstu_2012 msubstu_2014 msubstu_2016 msubstu_2018;
run;

*creating a binary variable for mother household physical abuse;
data ten;
	set nine;
	mphysab = sum (mphysab_2012, mphysab_2014, mphysab_2016, mphysab_2018);
	if mphysab =2 then mphysab=1; /* combining responses for once and more than once */
run;
*checking that frequency of combined variable matched individual years;
proc freq data=ten;
	tables mphysab mphysab_2012 mphysab_2014 mphysab_2016 mphysab_2018/missing;
run;
proc print data=ten (obs=75);
var mphysab mphysab_2012 mphysab_2014 mphysab_2016 mphysab_2018;
run;

*creating binary variable for mother hh emotional neglect;
*1 = a little (3) and none at all (4) and 0= a great deal (1) and quite a lot (2);
data eleven;
	set ten;
	mloveaf = sum(mloveaf_2012, mloveaf_2014, mloveaf_2016, mloveaf_2018);
	if mloveaf = 2 or mloveaf = 1 then mloveaf =0;
	if mloveaf = 3 or mloveaf = 4 then mloveaf = 1;
run;
proc freq data=eleven;
tables mloveaf mloveaf_2012 mloveaf_2014 mloveaf_2016 mloveaf_2018/missing;
run;
proc print data=eleven (obs=75);
var mloveaf mloveaf_2012 mloveaf_2014 mloveaf_2016 mloveaf_2018;
run;
 
*creating binary variable for basic needs instability - part one - food security;
*1 = "2", "3", "4", 0 = "1" (we could always eat good and nutritous meals);  
*done;
data twelve;
	set eleven;
	foodins = sum(foodins_2012, foodins_2014, foodins_2016, foodins_2018);
	if foodins = 1 then foodins=0;
	if foodins = 2 or foodins = 3 or foodins = 4 then foodins =1;
run;
*checking that combined variable matched individual years;
proc freq data= twelve;
tables foodins foodins_2012 foodins_2014 foodins_2016 foodins_2018/missing;
run;
proc print data=twelve (obs=75);
var foodins foodins_2012 foodins_2014 foodins_2016 foodins_2018;
run;

*creating binary variable for basic needs instability - part two - TANF;
data thirteen;
set twelve;
tanfas=0;
tanfas=sum(tanfas_1994, tanfas_1996, tanfas_1998, tanfas_2000);
if tanfas => 1 then tanfas=1;
run;
proc freq data=thirteen;
tables tanfas tanfas_1994 tanfas_1996 tanfas_1998 tanfas_2000/missing;
run;
proc print data=thirteen (obs=25);
where tanfas=0;
var tanfas tanfas_1994 tanfas_1996 tanfas_1998 tanfas_2000;
run;

*creating binary variable for basic needs instability - part three - food stamps;
*done;
data fourteen;
set thirteen;
foodstp=0;
foodstp = sum (foodstp_1979, foodstp_1980, foodstp_1981, foodstp_1982, foodstp_1983, foodstp_1984, foodstp_1985, foodstp_1986, foodstp_1987,
foodstp_1988, foodstp_1989, foodstp_1990, foodstp_1991, foodstp_1992, foodstp_1993, foodstp_1994, foodstp_1996, foodstp_1998, foodstp_2000);
if foodstp => 1 then foodstp = 1;
run;
proc freq data=fourteen;
tables foodstp foodstp_1979 foodstp_1980 foodstp_1981 foodstp_1982 foodstp_1983 foodstp_1984 foodstp_1985 foodstp_1986 foodstp_1987
foodstp_1988 foodstp_1989 foodstp_1990 foodstp_1991 foodstp_1992 foodstp_1993 foodstp_1994 foodstp_1996 foodstp_1998 foodstp_2000/missing;
run;
proc print data=fourteen (obs=75);
var foodstp foodstp_1979 foodstp_1980 foodstp_1981 foodstp_1982 foodstp_1983 foodstp_1984 foodstp_1985 foodstp_1986 foodstp_1987
foodstp_1988 foodstp_1989 foodstp_1990 foodstp_1991 foodstp_1992 foodstp_1993 foodstp_1994 foodstp_1996 foodstp_1998 foodstp_2000;
run;

*creating final merged variable for basic needs instability;
data fifteen;
set fourteen;
if foodins =1 |
tanfas = 1 |
foodstp = 1 then bneedin = 1;
else if foodins ne . |
tanfas ne .  |
foodstp ne .  then bneedin = 0;
run;
proc freq data=fifteen;
tables foodins tanfas foodstp bneedin/missing;
run;
proc print data=fifteen (obs=75);
var foodins tanfas foodstp bneedin;
run;

*creating binary variable for economic standing part 1, mother unemployment;
*if any memploy variables =1 that means mother was employed;
data sixteen;
set fifteen;
memploy=0;
array memploy_(*) memploy_1980-memploy_2000;
do i=1 to Dim(memploy_);
if memploy_(i)=0 then memploy = 1;
*else if memploy_(i) ne . then memploy = 0;
end;
memploy_sum=sum(memploy_1980, memploy_1981, memploy_1982, memploy_1983, memploy_1984, memploy_1985, memploy_1986, memploy_1987, memploy_1988,
memploy_1989, memploy_1990, memploy_1991, memploy_1992, memploy_1993, memploy_1994, memploy_1996, memploy_1998, memploy_2000);
if memploy_sum=. then memploy=.;
run;
proc freq data=sixteen;
tables memploy memploy_sum memploy_1980 memploy_1981 memploy_1982 memploy_1983 memploy_1984 memploy_1985 memploy_1986 memploy_1987 memploy_1988
memploy_1989 memploy_1990 memploy_1991 memploy_1992 memploy_1993 memploy_1994 memploy_1996 memploy_1998 memploy_2000/missing;
run;
proc print data=sixteen (obs=50);
var memploy memploy_sum memploy_1980 memploy_1981 memploy_1982 memploy_1983 memploy_1984 memploy_1985 memploy_1986 memploy_1987 memploy_1988
memploy_1989 memploy_1990 memploy_1991 memploy_1992 memploy_1993 memploy_1994 memploy_1996 memploy_1998 memploy_2000;
run;

*creating binary variable for economic standing part 2, mother poverty status;
*done;
data seventeen;
set sixteen;
mpovsta=0;
mpovsta=sum(mpovsta_1979, mpovsta_1980, mpovsta_1981, mpovsta_1982, mpovsta_1983, mpovsta_1984, mpovsta_1985, mpovsta_1986, mpovsta_1987,
mpovsta_1988, mpovsta_1989, mpovsta_1990, mpovsta_1991, mpovsta_1992, mpovsta_1993, mpovsta_1994, mpovsta_1996, mpovsta_1998, mpovsta_2000);
if mpovsta => 1 then mpovsta = 1;
run;
proc freq data=seventeen;
tables mpovsta mpovsta_1979 mpovsta_1980 mpovsta_1981 mpovsta_1982 mpovsta_1983 mpovsta_1984 mpovsta_1985 mpovsta_1986 mpovsta_1987
mpovsta_1988 mpovsta_1989 mpovsta_1990 mpovsta_1991 mpovsta_1992 mpovsta_1993 mpovsta_1994 mpovsta_1996 mpovsta_1998 mpovsta_2000/missing;
run;
proc print data=seventeen (obs=75);
var mpovsta mpovsta_1979 mpovsta_1980 mpovsta_1981 mpovsta_1982 mpovsta_1983 mpovsta_1984 mpovsta_1985 mpovsta_1986 mpovsta_1987
mpovsta_1988 mpovsta_1989 mpovsta_1990 mpovsta_1991 mpovsta_1992 mpovsta_1993 mpovsta_1994 mpovsta_1996 mpovsta_1998 mpovsta_2000;
run;

*creating final economic standing variable;
*EDIT 1/12/23 ecstand only represents mpovsta (removed memploy);
data eighteen;
set seventeen;
if mpovsta =1 then ecstand = 1;
else if mpovsta =0 then ecstand = 0;
run;
proc freq data= eighteen;
tables ecstand /*memploy*/ mpovsta;
run;
proc print data=eighteen (obs=75);
var ecstand /*memploy*/ mpovsta;
run;

*creating binary variable for YA community stressor;
*1 = big problem, 2 = somewhat of a problem;
data nineteen;
set eighteen;
commstr=0;
array commstr_(*) rullaws_1994-rullaws_2018 crimevi_1994-crimevi_2018 abandon_1994-abandon_2018 policep_1994-policep_2018
pubtran_1994-pubtran_2018 supervi_1994-supervi_2018 dontcar_1994-dontcar_2018 notjobs_1994-notjobs_2018;
do i=1 to Dim(commstr_);
if commstr_(i) in (1,2) then commstr = 1;
end;
commstr_sum=sum(rullaws_1994, rullaws_1996, rullaws_1998, rullaws_2000, rullaws_2002, rullaws_2004, rullaws_2006, rullaws_2008, rullaws_2010, rullaws_2012, rullaws_2014, rullaws_2016, rullaws_2018,
crimevi_1994, crimevi_1996, crimevi_1998, crimevi_2000, crimevi_2002, crimevi_2004, crimevi_2006, crimevi_2008, crimevi_2010, crimevi_2012, crimevi_2014, crimevi_2016, crimevi_2018,
abandon_1994, abandon_1996, abandon_1998, abandon_2000, abandon_2002, abandon_2004, abandon_2006, abandon_2008, abandon_2010, abandon_2012, abandon_2014, abandon_2016, abandon_2018,
policep_1994,policep_1996, policep_1998, policep_2000, policep_2002, policep_2004, policep_2006, policep_2008, policep_2010, policep_2012, policep_2014, policep_2016, policep_2018,
pubtran_1994,pubtran_1996, pubtran_1998, pubtran_2000, pubtran_2002, pubtran_2004, pubtran_2006, pubtran_2008, pubtran_2010, pubtran_2012, pubtran_2014, pubtran_2016, pubtran_2018,
supervi_1994, supervi_1996, supervi_1998, supervi_2000, supervi_2002, supervi_2004, supervi_2006, supervi_2008, supervi_2010, supervi_2012, supervi_2014, supervi_2016, supervi_2018,
dontcar_1994, dontcar_1996, dontcar_1998, dontcar_2000, dontcar_2002, dontcar_2004, dontcar_2006, dontcar_2008, dontcar_2010, dontcar_2012, dontcar_2014, dontcar_2016, dontcar_2018,
notjobs_1994, notjobs_1996, notjobs_1998, notjobs_2000, notjobs_2002, notjobs_2004, notjobs_2006, notjobs_2008, notjobs_2010, notjobs_2012, notjobs_2014, notjobs_2016, notjobs_2018);
if commstr_sum=. then commstr=.;
run;
proc freq data=nineteen;
tables commstr commstr_sum rullaws_1994 rullaws_1996 rullaws_1998 rullaws_2000 rullaws_2002 rullaws_2004 rullaws_2006 rullaws_2008 rullaws_2010 rullaws_2012 rullaws_2014 rullaws_2016 rullaws_2018
crimevi_1994 crimevi_1996 crimevi_1998 crimevi_2000 crimevi_2002 crimevi_2004 crimevi_2006 crimevi_2008 crimevi_2010 crimevi_2012 crimevi_2014 crimevi_2016 crimevi_2018
abandon_1994 abandon_1996 abandon_1998 abandon_2000 abandon_2002 abandon_2004 abandon_2006 abandon_2008 abandon_2010 abandon_2012 abandon_2014 abandon_2016 abandon_2018
policep_1994 policep_1996 policep_1998 policep_2000 policep_2002 policep_2004 policep_2006 policep_2008 policep_2010 policep_2012 policep_2014 policep_2016 policep_2018
pubtran_1994 pubtran_1996 pubtran_1998 pubtran_2000 pubtran_2002 pubtran_2004 pubtran_2006 pubtran_2008 pubtran_2010 pubtran_2012 pubtran_2014 pubtran_2016 pubtran_2018
supervi_1994 supervi_1996 supervi_1998 supervi_2000 supervi_2002 supervi_2004 supervi_2006 supervi_2008 supervi_2010 supervi_2012 supervi_2014 supervi_2016 supervi_2018
dontcar_1994 dontcar_1996 dontcar_1998 dontcar_2000 dontcar_2002 dontcar_2004 dontcar_2006 dontcar_2008 dontcar_2010 dontcar_2012 dontcar_2014 dontcar_2016 dontcar_2018
notjobs_1994 notjobs_1996 notjobs_1998 notjobs_2000 notjobs_2002 notjobs_2004 notjobs_2006 notjobs_2008 notjobs_2010 notjobs_2012 notjobs_2014 notjobs_2016 notjobs_2018
/missing;
run;
proc print data=nineteen(obs=75);
var commstr commstr_sum rullaws_1994 rullaws_1996 rullaws_1998 rullaws_2000 rullaws_2002 rullaws_2004 rullaws_2006 rullaws_2008 rullaws_2010 rullaws_2012 rullaws_2014 rullaws_2016 rullaws_2018
crimevi_1994 crimevi_1996 crimevi_1998 crimevi_2000 crimevi_2002 crimevi_2004 crimevi_2006 crimevi_2008 crimevi_2010 crimevi_2012 crimevi_2014 crimevi_2016 crimevi_2018
abandon_1994 abandon_1996 abandon_1998 abandon_2000 abandon_2002 abandon_2004 abandon_2006 abandon_2008 abandon_2010 abandon_2012 abandon_2014 abandon_2016 abandon_2018
policep_1994 policep_1996 policep_1998 policep_2000 policep_2002 policep_2004 policep_2006 policep_2008 policep_2010 policep_2012 policep_2014 policep_2016 policep_2018
pubtran_1994 pubtran_1996 pubtran_1998 pubtran_2000 pubtran_2002 pubtran_2004 pubtran_2006 pubtran_2008 pubtran_2010 pubtran_2012 pubtran_2014 pubtran_2016 pubtran_2018
supervi_1994 supervi_1996 supervi_1998 supervi_2000 supervi_2002 supervi_2004 supervi_2006 supervi_2008 supervi_2010 supervi_2012 supervi_2014 supervi_2016 supervi_2018
dontcar_1994 dontcar_1996 dontcar_1998 dontcar_2000 dontcar_2002 dontcar_2004 dontcar_2006 dontcar_2008 dontcar_2010 dontcar_2012 dontcar_2014 dontcar_2016 dontcar_2018
notjobs_1994 notjobs_1996 notjobs_1998 notjobs_2000 notjobs_2002 notjobs_2004 notjobs_2006 notjobs_2008 notjobs_2010 notjobs_2012 notjobs_2014 notjobs_2016 notjobs_2018;
run;

/* OUTCOMES */
*sum score for anxiety;
data twenty;
	set nineteen;
	anxiety = sum (anxiety_1_2018, anxiety_2_2018, anxiety_3_2018, anxiety_4_2018, anxiety_5_2018, anxiety_6_2018, anxiety_7_2018);
	label anxiety = "GAD-7 SUM SCORE";
run;
proc freq data=twenty;
tables anxiety anxiety_1_2018 anxiety_2_2018 anxiety_3_2018 anxiety_4_2018 anxiety_5_2018 anxiety_6_2018 anxiety_7_2018/missing;
run;
proc print data=twenty (obs=75);
*where anxiety ne .;
var anxiety anxiety_1_2018 anxiety_2_2018 anxiety_3_2018 anxiety_4_2018 anxiety_5_2018 anxiety_6_2018 anxiety_7_2018;
run;

*mean variable for present health;
data twentyone;
	set twenty;
preshlth = mean (preshlt_1994, preshlt_1996, preshlt_1998, preshlt_2000, preshlt_2002, preshlt_2004, preshlt_2006, preshlt_2008,
preshlt_2010, preshlt_2012, preshlt_2014, preshlt_2016, preshlt_2018);
run;
proc univariate data=twentyone;
var preshlth;
run;
proc print data=twentyone (obs=75);
var preshlth preshlt_1994 preshlt_1996 preshlt_1998 preshlt_2000 preshlt_2002 preshlt_2004 preshlt_2006 preshlt_2008
preshlt_2010 preshlt_2012 preshlt_2014 preshlt_2016 preshlt_2018;
run;

*reverse coding item :I feel happy";
*depress_2010_7 depress_2012_7 depress_2014_7 depress_2016_7 depress_2018_7;
data depressreverse;
set twentyone;
if depress_7_2010=0 then depress_7r_2010=3;       
if depress_7_2010=1 then depress_7r_2010=2;
if depress_7_2010=2 then depress_7r_2010=1;
if depress_7_2010=3 then depress_7r_2010=0;
if depress_7_2012=0 then depress_7r_2012=3;
if depress_7_2012=1 then depress_7r_2012=2;
if depress_7_2012=2 then depress_7r_2012=1;
if depress_7_2012=3 then depress_7r_2012=0;
if depress_7_2014=0 then depress_7r_2014=3;
if depress_7_2014=1 then depress_7r_2014=2;
if depress_7_2014=2 then depress_7r_2014=1;
if depress_7_2014=3 then depress_7r_2014=0;
if depress_7_2016=0 then depress_7r_2016=3;
if depress_7_2016=1 then depress_7r_2016=2;
if depress_7_2016=2 then depress_7r_2016=1;
if depress_7_2016=3 then depress_7r_2016=0;
if depress_7_2018=0 then depress_7r_2018=3;
if depress_7_2018=1 then depress_7r_2018=2;
if depress_7_2018=2 then depress_7r_2018=1;
if depress_7_2018=3 then depress_7r_2018=0;
run;
proc freq data= depressreverse;
tables depress_7_2010 depress_7_2012 depress_7_2014 depress_7_2018 depress_7r_2010 depress_7r_2012 depress_7r_2014 depress_7r_2018;
run;
proc print data= depressreverse (obs=75);
var depress_7_2010 depress_7_2012 depress_7_2014 depress_7_2018 depress_7r_2010 depress_7r_2012 depress_7r_2014 depress_7r_2018;
run;

*mean variable for depression;
data twentytwo;
set depressreverse;
depress_1994 =  mean (depress_1_1994, depress_2_1994, depress_3_1994, depress_4_1994, depress_5_1994, depress_6_1994, depress_7_1994);
depress_1996 =  mean (depress_1_1996, depress_2_1996, depress_3_1996, depress_4_1996, depress_5_1996, depress_6_1996, depress_7_1996);
depress_1998 = mean (depress_1_1998, depress_2_1998, depress_3_1998, depress_4_1998, depress_5_1998, depress_6_1998, depress_7_1998);
depress_2000 = mean (depress_1_2000, depress_2_2000, depress_3_2000, depress_4_2000, depress_5_2000, depress_6_2000, depress_7_2000);
depress_2002 = mean (depress_1_2002, depress_2_2002, depress_3_2002, depress_4_2002, depress_5_2002, depress_6_2002, depress_7_2002);
depress_2004 = mean (depress_1_2004, depress_2_2004, depress_3_2004, depress_4_2004, depress_5_2004, depress_6_2004, depress_7_2004);
depress_2006 = mean (depress_1_2006, depress_2_2006, depress_3_2006, depress_4_2006, depress_5_2006, depress_6_2006, depress_7_2006);
depress_2008 = mean (depress_1_2008, depress_2_2008, depress_3_2008, depress_4_2008, depress_5_2008, depress_6_2008, depress_7_2008);
depress_2010 = mean (depress_1_2010, depress_2_2010, depress_3_2010, depress_4_2010, depress_5_2010, depress_6_2010, depress_7r_2010,
	depress_8_2010, depress_9_2010, depress_10_2010, depress_11_2010);
depress_2012 = mean (depress_1_2012, depress_2_2012, depress_3_2012, depress_4_2012, depress_5_2012, depress_6_2012, depress_7r_2012,
	depress_8_2012, depress_9_2012, depress_10_2012, depress_11_2012);
depress_2014 = mean (depress_1_2014, depress_2_2014, depress_3_2014, depress_4_2014, depress_5_2014, depress_6_2014, depress_7r_2014,
	depress_8_2014, depress_9_2014, depress_10_2014, depress_11_2014);
depress_2016 = mean (depress_1_2016, depress_2_2016, depress_3_2016, depress_4_2016, depress_5_2016, depress_6_2016, depress_7r_2016,
	depress_8_2016, depress_9_2016, depress_10_2016, depress_11_2016);
depress_2018 = mean (depress_1_2018, depress_2_2018, depress_3_2018, depress_4_2018, depress_5_2018, depress_6_2018, depress_7r_2018,
	depress_8_2018, depress_9_2018, depress_10_2018, depress_11_2018);
depress = mean (depress_1994, depress_1996, depress_1998, depress_2000, depress_2002, depress_2004, depress_2006, depress_2008, depress_2010,
depress_2012, depress_2014, depress_2016, depress_2018);
run;
proc print data=twentytwo (obs=75);
where depress_1994 ne .;
var depress_1994 depress_1_1994 depress_2_1994 depress_3_1994 depress_4_1994 depress_5_1994 depress_6_1994 depress_7_1994;
run;
proc freq data=twentytwo;
tables depress depress_1994 depress_1996 depress_1998 depress_2000 depress_2002 depress_2004 depress_2006 depress_2008 depress_2010
depress_2012 depress_2014 depress_2016 depress_2018;
run; 
proc print data=twentytwo (obs=75);
var depress depress_1994 depress_1996 depress_1998 depress_2000 depress_2002 depress_2004 depress_2006 depress_2008 depress_2010
depress_2012 depress_2014 depress_2016 depress_2018;
proc univariate data=twentytwo;
var depress;
run;

*creating binary variable for accidents and injuries;
data twentythree;
set twentytwo;
accinju=0;
accinju=sum(accinju_1994,accinju_1996, accinju_1998, accinju_2000, accinju_2002, accinju_2004, accinju_2006, accinju_2008, accinju_2010, accinju_2012,
accinju_2014, accinju_2016);
if accinju => 1 then accinju = 1;
run;
proc freq data=twentythree;
tables accinju accinju_1994 accinju_1996 accinju_1998 accinju_2000 accinju_2002 accinju_2004 accinju_2006 accinju_2008 accinju_2010 accinju_2012
accinju_2014 accinju_2016/missing;
run;
proc print data=twentythree (obs=50);
var accinju accinju_1994 accinju_1996 accinju_1998 accinju_2000 accinju_2002 accinju_2004 accinju_2006 accinju_2008 accinju_2010 accinju_2012
accinju_2014 accinju_2016;
run;

*creating binary variable for violent crime;
data twentyfour;
set twentythree;
array agev_ {21} agevicr_2006 agevicr_2008 agevicr_2010 agevicr_2012 agevicr_2014 agevicr_2016 agevicr_2018
agerecvic_2006 agerecvic_2008 agerecvic_2010 agerecvic_2012 agerecvic_2014 agerecvic_2016 agerecvic_2018
agelastvic_2006 agelastvic_2008 agelastvic_2010 agelastvic_2012 agelastvic_2014 agelastvic_2016 agelastvic_2018;
do i=1 to 21;
if agev_(i)=>18 then agev = 1;
*else if evrvicr_(i) ne . then evrvicr = 0;
end;*/
evrvicr=0;
evrvicr = sum(evrvicr_2006, evrvicr_2008, evrvicr_2010, evrvicr_2012, evrvicr_2014, evrvicr_2016, evrvicr_2018);
if evrvicr => 1 then evrvicr = 1;
if agev ne 1 & evrvicr = 1 then evrvicr=.;
run;
proc freq data=twentythree;
tables evrvicr_2006 evrvicr_2008 evrvicr_2010 evrvicr_2012 evrvicr_2014 evrvicr_2016 evrvicr_2018
agevicr_2006 agevicr_2008 agevicr_2010 agevicr_2012 agevicr_2014 agevicr_2016 agevicr_2018
agerecvic_2006 agerecvic_2008 agerecvic_2010 agerecvic_2012 agerecvic_2014 agerecvic_2016 agerecvic_2018
agelastvic_2006 agelastvic_2008 agelastvic_2010 agelastvic_2012 agelastvic_2014 agelastvic_2016 agelastvic_2018/missing;
run;
proc freq data=twentyfour;
tables evrvicr agev evrvicr_2006 evrvicr_2008 evrvicr_2010 evrvicr_2012 evrvicr_2014 evrvicr_2016 evrvicr_2018
agevicr_2006 agevicr_2008 agevicr_2010 agevicr_2012 agevicr_2014 agevicr_2016 agevicr_2018
agerecvic_2006 agerecvic_2008 agerecvic_2010 agerecvic_2012 agerecvic_2014 agerecvic_2016 agerecvic_2018
agelastvic_2006 agelastvic_2008 agelastvic_2010 agelastvic_2012 agelastvic_2014 agelastvic_2016 agelastvic_2018/missing;
run;
proc print data=twentyfour (obs=50);
var evrvicr agev evrvicr_2006 evrvicr_2008 evrvicr_2010 evrvicr_2012 evrvicr_2014 evrvicr_2016 evrvicr_2018
agevicr_2006 agevicr_2008 agevicr_2010 agevicr_2012 agevicr_2014 agevicr_2016 agevicr_2018
agerecvic_2006 agerecvic_2008 agerecvic_2010 agerecvic_2012 agerecvic_2014 agerecvic_2016 agerecvic_2018
agelastvic_2006 agelastvic_2008 agelastvic_2010 agelastvic_2012 agelastvic_2014 agelastvic_2016 agelastvic_2018;
run;

*creating mean of drinks a day;
data twentyfive;
set twentyfour;
drinkdy = mean (drinkdy_1994, drinkdy_1996, drinkdy_1998, drinkdy_2000, drinkdy_2002, drinkdy_2004, drinkdy_2006, drinkdy_2008, drinkdy_2010,
drinkdy_2012, drinkdy_2014, drinkdy_2016, drinkdy_2018);
run;
proc freq data=twentyfive;
tables drinkdy drinkdy_1994 drinkdy_1996 drinkdy_1998 drinkdy_2000 drinkdy_2002 drinkdy_2004 drinkdy_2006 drinkdy_2008 drinkdy_2010
drinkdy_2012 drinkdy_2014 drinkdy_2016 drinkdy_2018;
run;
proc print data=twentyfive (obs=75);
var drinkdy drinkdy_1994 drinkdy_1996 drinkdy_1998 drinkdy_2000 drinkdy_2002 drinkdy_2004 drinkdy_2006 drinkdy_2008 drinkdy_2010
drinkdy_2012 drinkdy_2014 drinkdy_2016 drinkdy_2018;
run;
proc univariate data=twentyfive;
var drinkdy;
run;
proc sql;
create table drinkdyz as
select drinkdy, (drinkdy - mean(drinkdy)) / std(drinkdy) as drinkdy_Z_score
from twentyfive;
quit;
proc univariate data=drinkdyz;
var drinkdy drinkdy_Z_score;
run;
proc freq data=drinkdyz;
where drinkdy_Z_score>3.29;
tables drinkdy drinkdy_Z_score;
run;
data twentyfive1;
set twentyfive;
if drinkdy=>10 then drinkdy=.;    /* Outliers */
run;
proc freq data=twentyfive1;
tables drinkdy/missing;
*where drinkdy=.P;
run;
proc univariate data=twentyfive1;
var drinkdy;
run;

/* COVARIATES */
*EDIT 1/27/2023 0 is did not graduate high school, 1 is did graduate high school;
proc freq data=twentyfive;
tables mhighgd_xrnd/missing;
run;
data twentysix;
set twentyfive1;
if mhighgd_xrnd=. then mhighgd_bin=.;
else if mhighgd_xrnd<12 then mhighgd_bin=0;
else if mhighgd_xrnd>=12 then mhighgd_bin=1;
run;
proc freq data=twentysix;
tables mhighgd_xrnd mhighgd_bin/missing;
run;
proc print data=twentysix (obs=75);
*where mhighgd_xrnd=.;
var mhighgd_xrnd mhighgd_bin;
run;

data twentyseven;
set twentysix;
if sum (of hisplat_:)>=0 |
sum(of raceeth_:)>=0 then do;
hisp=0;
white=0;
black=0;
asian_nhpi=0;
othrace=0;
end;


if sum(of hisplat_1998-hisplat_2018) >= 1 | 
raceeth_1994 = 3 |
raceeth_1996 = 3 |
/*raceeth_1998 = 3 |*/
sum(of raceeth_7_2010, raceeth_7_2012, raceeth_7_2014, raceeth_7_2016, raceeth_7_2018) >=1 
then hisp=1;

array race11 raceeth_1_2000         raceeth_1_2002          raceeth_1_2004          raceeth_1_2006;
array race21 raceeth_2_2000         raceeth_2_2002          raceeth_2_2004          raceeth_2_2006;
array race31 raceeth_3_2000         raceeth_3_2002          raceeth_3_2004          raceeth_3_2006;
array race41 raceeth_4_2000         raceeth_4_2002          raceeth_4_2004          raceeth_4_2006;
array race51 raceeth_5_2000         raceeth_5_2002          raceeth_5_2004          raceeth_5_2006;
array race61 raceeth_6_2000         raceeth_6_2002          raceeth_6_2004          raceeth_6_2006;
do over race11 ;if race11=1 then white=1;end;
do over race21 ;if race21=1 then black=1;end;
do over race31 ;if race31=1 then asian_nhpi=1;end;
do over race41 ;if race41=1 then asian_nhpi=1;end;
do over race51 ;if race51=1 then asian_nhpi=1;end;
do over race61 ;if race61=1 then othrace=1;end;

array race1 raceeth_1_2008          raceeth_1_2010          raceeth_1_2012          raceeth_1_2014_1        raceeth_1_2016          raceeth_1_2018;
array race2 raceeth_2_2008          raceeth_2_2010          raceeth_2_2012          raceeth_2_2014_2        raceeth_2_2016          raceeth_2_2018;
array race3 raceeth_3_2008          raceeth_3_2010          raceeth_3_2012          raceeth_3_2014_3        raceeth_3_2016          raceeth_3_2018;
array race4 raceeth_4_2008          raceeth_4_2010          raceeth_4_2012          raceeth_4_2014_4        raceeth_4_2016          raceeth_4_2018;
array race5 raceeth_5_2008          raceeth_5_2010          raceeth_5_2012          raceeth_5_2014_5        raceeth_5_2016          raceeth_5_2018;
array race6 raceeth_6_2008          raceeth_6_2010          raceeth_6_2012          raceeth_6_2014_6        raceeth_6_2016          raceeth_6_2018;
do over race1 ;if race1=1 then white=1;end;
do over race2 ;if race2=1 then black=1;end;
do over race3 ;if race3=1 then asian_nhpi=1;end;
do over race4 ;if race4=1 then asian_nhpi=1;end;
do over race5 ;if race5=1 then asian_nhpi=1;end;
do over race6 ;if race6=1 then othrace=1;end;

array oldrace raceeth_1994 raceeth_1996 /*raceeth_1998*/;
do over oldrace;
if oldrace=1 then black=1;
if oldrace=2 then white=1;
if oldrace=3 then hisp=1;
if oldrace=4 then asian_nhpi=1; /*should be american indian*/
if oldrace=5 then asian_nhpi=1;
if oldrace=6 then othrace=1;
end;
if raceeth_1998 = 1 then white=1;
if raceeth_1998 = 2 then black=1;
if raceeth_1998 = 3 then asian_nhpi=1;
if raceeth_1998 = 4 then asian_nhpi=1;
if raceeth_1998 = 5 then asian_nhpi=1;
if raceeth_1998 = 6 then othrace=1;
run;

proc freq data=twentyseven;
tables black white hisp asian asian_nhpi othrace;
run; 

*average hh income;
data twentyeight;
set twentyseven;
mhhinco = mean (mhhinco_1979, mhhinco_1980, mhhinco_1981, mhhinco_1982, mhhinco_1983, mhhinco_1984,
mhhinco_1985, mhhinco_1986, mhhinco_1987, mhhinco_1988, mhhinco_1989, mhhinco_1990, mhhinco_1991,
mhhinco_1992, mhhinco_1993, mhhinco_1994, mhhinco_1996, mhhinco_1998, mhhinco_2000);
run;
proc univariate data=twentyeight;
var mhhinco;
run;
proc print data=twentyeight (obs=50);
var mhhinco mhhinco_1979 mhhinco_1980 mhhinco_1981 mhhinco_1982 mhhinco_1983 mhhinco_1984
mhhinco_1985 mhhinco_1986 mhhinco_1987 mhhinco_1988 mhhinco_1989 mhhinco_1990 mhhinco_1991
mhhinco_1992 mhhinco_1993 mhhinco_1994 mhhinco_1996 mhhinco_1998 mhhinco_2000;
run;

*urban variable;
*response options 0=rural 1=urban 2=don't know;
data twentynine;
set twentyeight;
*setting 2 (don't know) to missing);
if urbnrur_1994 =2 then urbnrur_1994 =.; 
if urbnrur_1996 =2 then urbnrur_1996 =.; 
if urbnrur_1998 =2 then urbnrur_1998 =.; 
if urbnrur_2000 =2 then urbnrur_2000 =.; 
if urbnrur_2002 =2 then urbnrur_2002 =.; 
if urbnrur_2004 =2 then urbnrur_2004 =.; 
if urbnrur_2006 =2 then urbnrur_2006 =.; 
if urbnrur_2008 =2 then urbnrur_2008 =.; 
if urbnrur_2010 =2 then urbnrur_2010 =.; 
if urbnrur_2012 =2 then urbnrur_2012 =.; 
if urbnrur_2014 =2 then urbnrur_2014 =.; 
if urbnrur_2016 =2 then urbnrur_2016 =.; 
if urbnrur_2018 =2 then urbnrur_2018 =.; 
urbn=0;
urbn=sum(urbnrur_1994, urbnrur_1996, urbnrur_1998, urbnrur_2000, urbnrur_2002, urbnrur_2004, urbnrur_2006, urbnrur_2008,
urbnrur_2010, urbnrur_2012, urbnrur_2014, urbnrur_2016, urbnrur_2018);
*creating urban variable for if respondent ever said they lived in urban area;
if urbn => 1 then urbn = 1;
run;
proc freq data=twentynine;
tables urbn urbnrur_1994 urbnrur_1996 urbnrur_1998 urbnrur_2000 urbnrur_2002 urbnrur_2004 urbnrur_2006 urbnrur_2008
urbnrur_2010 urbnrur_2012 urbnrur_2014 urbnrur_2016 urbnrur_2018/missing;
run;
proc print data=twentynine (obs=50);
var urbn urbnrur_1994 urbnrur_1996 urbnrur_1998 urbnrur_2000 urbnrur_2002 urbnrur_2004 urbnrur_2006 urbnrur_2008
urbnrur_2010 urbnrur_2012 urbnrur_2014 urbnrur_2016 urbnrur_2018;
run;

*rural variables;
* 0 = rural, 1=urban, 2=mixed;
data thirty;
set twentynine;
array urbnrur_(*) urbnrur_1994-urbnrur_2018;
do i=1 to Dim(urbnrur_);
if urbnrur_(i)=0 then rural = 1;
end;
run;
proc freq data=thirty;
tables rural urbnrur_1994-urbnrur_2018/missing;
run;
proc print data=thirty (obs=50);
var rural urbnrur_1994-urbnrur_2018;
run;
proc freq data=thirty;
tables urbn rural ;
run;
*creating final urbanrur variable using a combination of urban and rural;
data thirtyone;
set thirty;
if urbn = 1 & rural = 1 then urbnrur = 2;
else if urbn = 1 then urbnrur = 1;
else if rural = 1 then urbnrur = 0;
run;
proc freq data=thirtyone;
tables urbn rural urbnrur/missing;
run;
proc print data=thirtyone(obs=50);
var urbn rural urbnrur;
run;

*YA last grade completed;
*1=8th grade or less, 2= some high school, 3=high school graduate, 4 all post-high school degrees;
proc freq data=thirtyone;
tables lastgrd_xrnd lastgrd_2004 lastgrd_2006 lastgrd_2008 lastgrd_2010 lastgrd_2012 lastgrd_2014 lastgrd_2016 lastgrd_2018/missing;
run;
data thirtytwo;
set thirtyone;
if lastgrd_xrnd<3 then lastgrd_bin=0; /*0=did not complete high school*/
else if lastgrd_xrnd>=3 then lastgrd_bin=1;
if lastgrd_xrnd=. then lastgrd_bin=.;
run;
proc freq data=thirtytwo;
tables lastgrd_xrnd lastgrd_bin/missing;
run;


proc freq data=thirtytwo;
where discrim=1;
tables dreason_2012 dreason_2014 dreason_2016 dreason_2018/missing;
run;

*sensativity variable for reason for discrimination;
*****************************************
  1 YOUR ETHNICITY
  2 YOUR GENDER
  3 YOUR RACE
  4 YOUR AGE
  5 YOUR RELIGION   
  6 ANOTHER ASPECT OF YOUR PHYSICAL APPEARANCE
  7 YOUR SEXUAL ORIENTATION
  8 YOUR EDUCATION OR INCOME LEVEL
  9 OTHER (SPECIFY)
  10 YOUR WEIGHT
  11 YOUR NAME
*****************************************;
proc freq data = thirtytwo;
	tables discrim / missing;
run;

data thirtythree;
set thirtytwo;
/*if discrim=1 then*/ 
discrim_reason=0;
array discrim_r {4} dreason_2012 dreason_2014 dreason_2016 dreason_2018;
do i=1 to 4;
if discrim_r(i)in (1,3,7) then discrim_reason = 1;         /* reported discrimination based on race/ethnicity or sexual orientation */
end;
discrim_reason_sum = sum(dreason_2012, dreason_2014, dreason_2016, dreason_2018);
if discrim_reason_sum=. then discrim_reason=.;
run;
* This data step below is added to create the variables for discrimation reasons based on race, ethnicity, and sexual orientation separately ;
data thirtyfour;
	set thirtythree;
	/*if discrim=1 then*/
	discrim_race=0;
	discrim_ethn=0; 
	discrim_raceethn=0;
	discrim_sexualorient=0;
	array discrim_r {4} dreason_2012 dreason_2014 dreason_2016 dreason_2018;
		do i=1 to 4;
		if discrim_r(i) in (1,3) then discrim_raceethn = 1;          /* reported discrimination based on race or ethnicity */
		if discrim_r(i) in (7) then discrim_sexualorient = 1;        /* reported discrimination based on sexual orientation */
		if discrim_r(i) in (1) then discrim_ethn = 1;                /* reported discrimination based on ethnicity */
		if discrim_r(i) in (3) then discrim_race = 1;                /* reported discrimination based on race */
	end;  
	discrim_reason_sum = sum(dreason_2012, dreason_2014, dreason_2016, dreason_2018);
	if discrim_reason_sum=. then do;
		discrim_race=.;
		discrim_ethn=.;
		discrim_raceethn=.;
		discrim_sexualorient=.;
	end;
run;
proc freq data=thirtyfour;
tables discrim_reason discrim_reason_sum dreason_2012 dreason_2014 dreason_2016 dreason_2018/missing;
run;
proc print data=thirtyfour(obs=50);
var discrim_reason discrim_reason_sum dreason_2012 dreason_2014 dreason_2016 dreason_2018;
run;
proc freq data = thirtyfour;
	tables discrim_race discrim_ethn discrim_raceethn discrim_sexualorient;
run;

*for sensativity analysis;
data thirtyfive;
set thirtyfour;
if discrim=1 and discrim_reason=1 then sensitivityRES=1;
if discrim=1 and discrim_reason=. then sensitivityRES=1;
if discrim=1 and discrim_reason=0 then sensitivityRES=0;
if discrim=0 then sensitivityRES=0;
if discrim=1 and discrim_reason=1 then sensitivityNOTRES=1;
if discrim=1 and discrim_reason=. then sensitivityNOTRES=0;
if discrim=1 and discrim_reason=0 then sensitivityNOTRES=0;
if discrim=0 then sensitivityNOTRES=0;
run;
proc freq data=thirtyfive;
*where discrim=1;
tables sensitivityRES sensitivityNOTRES discrim_reason discrim discrim_reason*discrim/missing;
run;
proc print data=thirtyfive(obs=50);
where discrim=1;
var sensitivityRES discrim_reason discrim;
run;


data finalvar;
set thirtyfive;
keep motherid 
childid 
w
bimonth_xrnd 
biryear_xrnd
ygender_xrnd
black 
white 
hisp 
/*asian */
asian_nhpi 
othrace
mhighgd_bin
drinkdy
evrvicr
accinju
depress
preshlth
anxiety
commstr
ecstand
bneedin
mloveaf
mphysab
msubstu
mmental
discrim
loveaff
incarce
divorce
physabu
subsuse
mentill
mhhinco
urbnrur
lastgrd_bin
discrim_reason
discrim_ethn
discrim_race
discrim_raceethn
discrim_sexualorient 
sensitivityRES
sensitivityNOTRES;
run;
proc sort data=finalvar;
by childid;
run;

proc import file="C:\Users\&user.\ICF\CDC ADIA Project - ICF Private Channel - ICF Private Channel\Study 1+3\Analysis\SAS Data Management Files\SAS Data Files to Input\AdjustedIncomeData_9Jan2023.csv"
    out=adjinco
    dbms=csv;
run;
proc contents data=adjinco;
run;
proc sort data=adjinco;
by childid;
run;
proc transpose data=adjinco out=adjinco_f (drop=_name_) delim=_;
  by childid ;
  id mhhinco  year ;
  var Income_2018_dollars;
run;
proc contents data=adjinco_f;
run;
data adjinco_f1;
   set adjinco_f;
   mhhinco_1979_n=input(mhhinco_1979, 8.);
	mhhinco_1980_n=input(mhhinco_1980, 8.);
	mhhinco_1981_n=input(mhhinco_1981, 8.);
	mhhinco_1982_n=input(mhhinco_1982, 8.);
	mhhinco_1983_n=input(mhhinco_1983, 8.);
	mhhinco_1984_n=input(mhhinco_1984, 8.);
	mhhinco_1985_n=input(mhhinco_1985, 8.);
	mhhinco_1986_n=input(mhhinco_1986, 8.);
	mhhinco_1987_n=input(mhhinco_1987, 8.);
	mhhinco_1988_n=input(mhhinco_1988, 8.);
	mhhinco_1989_n=input(mhhinco_1989, 8.);
	mhhinco_1990_n=input(mhhinco_1990, 8.);
	mhhinco_1991_n=input(mhhinco_1991, 8.);
	mhhinco_1992_n=input(mhhinco_1992, 8.);
	mhhinco_1993_n=input(mhhinco_1993, 8.);
	mhhinco_1994_n=input(mhhinco_1994, 8.);
	mhhinco_1996_n=input(mhhinco_1996, 8.);
	mhhinco_1998_n=input(mhhinco_1998, 8.);
	mhhinco_2000_n=input(mhhinco_2000, 8.);
run;

/*reading in adjusted hh income*/
*average hh income;
data adjinco_f2;
set adjinco_f1;
mhhinco_adj = mean (mhhinco_1979_n, mhhinco_1980_n, mhhinco_1981_n, mhhinco_1982_n, mhhinco_1983_n, mhhinco_1984_n,
mhhinco_1985_n, mhhinco_1986_n, mhhinco_1987_n, mhhinco_1988_n, mhhinco_1989_n, mhhinco_1990_n, mhhinco_1991_n,
mhhinco_1992_n, mhhinco_1993_n, mhhinco_1994_n, mhhinco_1996_n, mhhinco_1998_n, mhhinco_2000_n);
run;
proc univariate data=adjinco_f2;
var mhhinco_adj;
run;
proc print data=adjinco_f2 (obs=50);
var mhhinco_adj mhhinco_1979_n mhhinco_1980_n mhhinco_1981_n mhhinco_1982_n mhhinco_1983_n mhhinco_1984_n
mhhinco_1985_n mhhinco_1986_n mhhinco_1987_n mhhinco_1988_n mhhinco_1989_n mhhinco_1990_n mhhinco_1991_n
mhhinco_1992_n mhhinco_1993_n mhhinco_1994_n mhhinco_1996_n mhhinco_1998_n mhhinco_2000_n;
run;
data adjinco_f3;
set adjinco_f2;
keep childid mhhinco_adj;
run;
proc sort data=adjinco_f3;
by childid;
run;
proc sort data=finalvar;
by childid;
run;
data out.finalvar_03232023;
   MERGE adjinco_f3 finalvar;
   BY childid;
  PROC PRINT DATA=out.finalvar_03232023 (obs=50); 
  RUN;
proc contents data=out.finalvar_03232023;
run;

  proc export data=out.finalvar_03232023
    outfile="C:\Users\&user.\ICF\CDC ADIA Project - ICF Private Channel - ICF Private Channel\Study 1+3\Study 3\Analysis\SAS Data Management Files\Data\finalvar_08112023.csv"
    dbms=csv
	replace;
	putnames=yes;
run;


proc freq data = finalvar;
	tables white;
run;
proc freq data = dat;
	tables white;
run;


/***** Generating frequency tables - Table 1 *****/

data dat;
	set out.finalvar_08112023;    /* Data updated using new race variables 8/15/2023 */
	w_0 = w/100;
	age = 2018 - biryear_xrnd;
	if discrim=1 then output;
run;

proc freq data = dat nlevels;
/*	tables ygender_xrnd w_0;*/
	tables w_0;
run;
proc means data = dat n min mean max;
	var w;
run;


/* Weighted */
proc surveyfreq data = dat missing ;
	weight w_0;
	tables

	ygender_xrnd
	biryear_xrnd
	lastgrd_bin
	mhighgd_bin

	black 
	white 
	hisp 
/*	asian */
	asian_nhpi 
	othrace

	urbnrur;
run;
proc surveyfreq data = dat;
	weight w_0;
	tables

/*	/* Binary Outcome */
	evrvicr
	accinju

/*	/* Exposures – Original ACES */
	mentill
	subsuse
	physabu
	divorce
	incarce

/*	/* Exposures – Other Childhood Stressors */
	loveaff
	discrim
	bneedin
	ecstand
	commstr
	mmental
	msubstu
	mphysab
	mloveaf;

run;




/* Unweighted */
proc freq data = dat ;
	tables

	ygender_xrnd
	biryear_xrnd
	lastgrd_bin
	mhighgd_bin

	black 
	white 
	hisp 
/*	asian */
	asian_nhpi 
	othrace

	urbnrur / missing;
run;
proc means data = dat  VARDEF=WGT;
	var age;
	weight w_0;
run;
proc freq data = dat;
	tables

/*	/* Binary Outcome */
	evrvicr
	accinju

/*	/* Exposures – Original ACES */
	mentill
	subsuse
	physabu
	divorce
	incarce

/*	/* Exposures – Other Childhood Stressors */
	loveaff
	discrim
	bneedin
	ecstand
	commstr
	mmental
	msubstu
	mphysab
	mloveaf

;
run;

proc univariate data=dat;
	var 
	evrvicr
	accinju
	drinkdy 
	depress
	preshlth
	anxiety

	mhhinco;
run;









*ods rtf file= 'C:\Users\55484\OneDrive - ICF\Documents\ADIA\NLS Data Analysis/Missin_CHISq_12.5.22.rtf' style=minimal;
proc freq data=thirtyone;
where discrim=1;
tables dreason_2012 dreason_2014 dreason_2016/missing;
run;
data thirtyone_one;
set thirtyone;
array dreason_(*) dreason_2012 dreason_2014 dreason_2016;
do i=1 to Dim(dreason_);
where discrim =1;
if dreason_(i)= . then dreason = 0;
else if dreason_(i) ne . then dreason = 1;
end;
run;
proc freq data=thirtyone_one;
tables dreason*ygender_xrnd dreason*black dreason*white dreason*hisp dreason*asian dreason*asian_nhpi dreason*othrace
dreason*urbn dreason*rural dreason*urbnrur dreason*mhighgd_bin/expected chisq;
run;
*ods rtf close;

*ods rtf file= 'C:\Users\55484\OneDrive - ICF\Documents\ADIA\NLS Data Analysis/VariableFreq12.5.22.rtf' style=minimal;
proc freq data=thirtyone;
tables mentill subsuse physabu divorce incarce loveaff discrim mmental msubstu mphysab mloveaf foodins tanfas foodstp
bneedin memploy mpovsta ecstand accinju evrvicr ygender_xrnd mhighgd_bin black white hisp asian asian_nhpi othrace urbnrur
urbn rural/missing;
run;
proc univariate data=thirtyone;
var anxiety preshlth depress drinkdy mhhinco;
histogram anxiety preshlth depress drinkdy mhhinco;
run;
proc ttest data=thirtyone_one;
where biryear_xrnd > 1994 & biryear_xrnd<2000;
class dreason;
var biryear_xrnd;
run;
proc ttest data=thirtyone_one;
class dreason;
var biryear_xrnd;
run;
proc univariate data=thirtyone_one;
var mhhinco;
histogram mhhinco;
class dreason;
run;
*ods rtf close;
*/


******** Cronbach Alpha (8/21/2023) **********;

data dat;
	set thirtyone;
	if discrim=1 then output;
run;
proc contents data=dat; run;

/*
 
DISCRIMINATION (2012-2018):
1) dcourte_2012 dcourte_2014 dcourte_2016 
2) drespec_2012 drespec_2014 drespec_2016 
3) dservic_2012 dservic_2014 dservic_2016 
4) dnsmart_2012 dnsmart_2014 dnsmart_2016 
5) dafraid_2012 dafraid_2014 dafraid_2016 
6) ddishon_2012 ddishon_2014 ddishon_2016 
7) dbetter_2012 dbetter_2014 dbetter_2016 
8) dinsult_2012 dinsult_2014 dinsult_2016 
9) dthreat_2012 dthreat_2014 dthreat_2016;

 *** All components have the same direction? - YES

*/

%MACRO discrim(year = );
Proc corr data=dat ALPHA nomiss;
     var dcourte_&year. drespec_&year. dservic_&year. dnsmart_&year. dafraid_&year. ddishon_&year. dbetter_&year. dinsult_&year. dthreat_&year.;
run;
%MEND;
%discrim(year = 2012);
%discrim(year = 2014);
%discrim(year = 2016);
%discrim(year = 2018);


/*
COMMUNITY STRESSOR:
1) rullaws_1994-rullaws_2018 
2) crimevi_1994-crimevi_2018 
3) abandon_1994-abandon_2018 
4) policep_1994-policep_2018
5) pubtran_1994-pubtran_2018 
6) supervi_1994-supervi_2018 
7) dontcar_1994-dontcar_2018 
8) notjobs_1994-notjobs_2018

 *** All components have the same direction? - YES

*/

%MACRO commstr(year = );
Proc corr data=dat ALPHA nomiss;
     var rullaws_&year. crimevi_&year. abandon_&year. policep_&year. pubtran_&year. supervi_&year. dontcar_&year. notjobs_&year. ;
run;
%MEND;

%commstr(year = 1994);
%commstr(year = 1996);
%commstr(year = 1998);
%commstr(year = 2000);
%commstr(year = 2002);
%commstr(year = 2004);
%commstr(year = 2006);
%commstr(year = 2008);
%commstr(year = 2010);
%commstr(year = 2012);
%commstr(year = 2014);
%commstr(year = 2016);
%commstr(year = 2018);


/* 
ANXIETY SEVERITY:
1) anxiety_1_2018 
2) anxiety_2_2018
3) anxiety_3_2018
4) anxiety_4_2018
5) anxiety_5_2018
6) anxiety_6_2018
7) anxiety_7_2018

 *** All components have the same direction? - YES

*/

Proc corr data=dat ALPHA nomiss;
     var anxiety_1_2018 anxiety_2_2018 anxiety_3_2018 anxiety_4_2018 anxiety_5_2018 anxiety_6_2018 anxiety_7_2018 ;
run;


/* 
DEPRESSIVE SEVERITY: 
(1994-2008: 7 items)
depress_1_1994
depress_2_1994
depress_3_1994
depress_4_1994
depress_5_1994
depress_6_1994
depress_7_1994

(2010-2018: 11 items)
depress_1_2010
depress_2_2010
depress_3_2010
depress_4_2010
depress_5_2010
depress_6_2010
depress_7r_2010
depress_8_2010
depress_9_2010
depress_10_2010
depress_11_2010

*/

proc freq data = dat;
     tables depress_1_1994 depress_2_1994 depress_3_1994 depress_4_1994 depress_5_1994 depress_6_1994 depress_7_1994;
	 tables depress_1_2010 depress_2_2010 depress_3_2010 depress_4_2010 depress_5_2010 depress_6_2010 depress_7r_2010 
			depress_8_2010  epress_9_2010 depress_10_2010 depress_11_2010; 
run;


%MACRO depress(year = );
%if &year.<=2008 %then %do;
Proc corr data=dat ALPHA nomiss;
     var depress_1_&year. depress_2_&year. depress_3_&year. depress_4_&year. depress_5_&year. depress_6_&year. depress_7_&year.;
Run;
%end;
%if &year.>=2010 %then %do;
Proc corr data=dat ALPHA nomiss;
     var depress_1_&year. depress_2_&year. depress_3_&year. depress_4_&year. depress_5_&year. depress_6_&year. depress_7r_&year.
		 depress_8_&year. depress_9_&year. depress_10_&year. depress_11_&year.;
Run;
%end;
%MEND;
%depress(year = 1994);
%depress(year = 1996);
%depress(year = 1998);
%depress(year = 2000);
%depress(year = 2002);
%depress(year = 2004);
%depress(year = 2006);
%depress(year = 2008);
%depress(year = 2010);
%depress(year = 2012);
%depress(year = 2014);
%depress(year = 2016);
%depress(year = 2018);



