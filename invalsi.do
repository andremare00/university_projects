set more off
clear all

***********************************************
/* The data-set invalsi_1415.dta (Stata data format) contained information
on both reading and math test scores and some socio-demographic
characteristics of the universe of 10th grade
students (i.e., in their second year of high school) 
in 2014-2015 school year in Italy. 
"Invalsi" is actually the name of the standardized test that Italian students undertook,
and from which the information in the dataset were recorded.
Data are at the student level.

The objective of the analysis was to understand the relationship
between test scores and class size.

***********************************************
Variable name		Description

id_school 			school id
id_class 			class id
id_student 			student id
invalsi_math 		INVALSI math score (from 0 to 100)
invalsi_ita 		INVALSI reading score (from 0 to 100)
gender 				1=male 2=female
migrant 			0=non migrant 1=migrant
edu_ma 				mother's education (1=up to junior high school; 2=high school; 3=college)
edu_fa 				father's education (1=up to junior high school; 2=high school; 3=college)
track 				1=liceo; 2=technical school; 3=vocational school
class_size 			number of students per class
age 				student's age
region 				region of residence (see variable labels)

*/
***********************************************

*********************
***DATA MANAGEMENT***
*********************
*DATA OVERVIEW
des 
des gender /*noticed that this is a categorical variable and not a dummy*/
codebook 
codebook invalsi_ita if invalsi_ita == 0
codebook invalsi_math if invalsi_math == 0

*DROPPING MISSING VALUES AND SCORES == 0 
estpost tabstat class_size invalsi_math invalsi_ita, c(stat) stat(mean sd) /*did
it in order to compare the results after I drop observations*/
esttab using "table1.tex", replace ///
cells("mean(fmt(%13.2fc)) sd(fmt(%13.2fc))") nonumber booktabs nogaps nofloat nomtitles ///
collabels("Mean" "Std. Dev.")

drop if (invalsi_ita == 0) | (invalsi_math == 0) | (invalsi_ita == .) | (invalsi_math == .) 
drop if gender == . 
drop if edu_ma == .
drop if edu_fa == .

*VARIABLE GENERATION
gen testscr = (invalsi_math+invalsi_ita)/2 /*mean of test results*/
gen ln_testscr = ln(testscr) /*natural logarithm of testscr, I use it afterwards*/
gen small_class = (class_size<=20) /*dummy equal to 1 if the class is small, 0 otherwise*/

gen North_West = (region==20) | (region==8) | (region==6) | (region==9) | (region==12)
gen North_East = (region==5) | (region==6) | (region==13) | (region==14) | (region==21)
gen Centre = (region==7)| (region==10) | (region==18) | (region==19)
gen Islands = (region==16) | (region==17)
gen South = (North_West==0) & (North_East==0) & (Centre==0) & (Islands==0)
/*set of dummies for geographic position*/

gen area5 = 1 if North_West==1
replace area5 = 2 if North_East==1
replace area5 = 3 if Centre==1
replace area5 = 4 if South==1
replace area5 = 5 if Islands==1 /*geographic position in a single variable*/

gen d_gender = (gender==1) /*equal to 1 if "Male"; "gender" was a categorical variable*/

gen fam_edu = edu_ma+edu_fa /*total family education*/

gen class_size2 = class_size^2 /*squared term that I will use afterwards*/

*LABELLING VARIABLES
label variable testscr "Mean of read and math score"

label variable d_gender `"1 if "Male", 0 if "Female""'
label define gender 1 "Male" 0 "Female"
label val d_gender gender 

label variable small_class "1 if class_size<20, 0 otherwise"
label define classsize 1 "up to 20 students" 0 "more than 20 students"
label val small_class classsize 

label variable area5 `"1 if "North West", 2 if  "North East", 3 if "Centre", 4 if "South", 5 if "Islands"'
label define area 1 "North West" 2 "North East" 3 "Centre" 4 "South" 5 "Islands"
label val area5 area

label variable fam_edu "sum of parents education"

*ORDERING THE DATASET FOR CONVENIENCE
order id_school id_class id_student invalsi* testscr class_size small_class ///
gender d_gender age migrant edu_ma edu_fa fam_edu region North_West North_East Centre South Islands area5
sort id_school id_class

******************************************
***DESCRIPTIVE AND INFERENTIAL ANALYSIS***
******************************************
*SUMMARY STATISTICS AND GRAPHS
sum class_size track invalsi_math invalsi_ita testscr, d

*TABLE COMPARISON WITH LINE 27
estpost tabstat class_size invalsi_math invalsi_ita, c(stat) stat(mean sd) /*table
that is to compare with the one above*/
esttab using "table2.tex", replace ///
cells("mean(fmt(%13.2fc)) sd(fmt(%13.2fc))") nonumber booktabs nogaps nofloat nomtitles ///
collabels("Mean" "Std. Dev.")

*GET THE NUMBER OF SCHOOLS
preserve
duplicates drop id_school, force /*since data are at student level, if I drop
duplicates of id_school I remain with 1 id per school and thus I get the number 
of school in the sample*/
codebook id_school
restore

*CLASS SIZES ACROSS TYPES OF SCHOOL
preserve /*since I need to modify temporarly the dataset, I use preserve-restore*/
duplicates drop id_class, force /*same thought of line 101*/
codebook id_class
bysort track: sum class_size, d
estpost tabstat class_size, by(track) c(stat) stat(mean sd) nototal /*average class size for each type of school*/
esttab using "class_schools.tex", replace ///
cells("mean(fmt(%13.2fc)) sd(fmt(%13.2fc))") nonumber booktabs nogaps nofloat nomtitles ///
collabels("Mean" "Std. Dev.")
restore

*TEST SCORES AND CLASS SIZE
bysort small_class: sum testscr, d /*descriptive statistics*/
graph dot (mean) testscr, over(class_size) exclude0 marker(1, mcolor(sand) ///
msymbol(circle)) yscale(range(30 75)) ylabel(#12) vertical saving(class1, replace)
graph export class1.pdf, replace
graph bar (mean) testscr, over(small_class) bar(1, fcolor(teal) lcolor(teal)) ///
ylabel(#7) saving(class2, replace) 
graph export class2.pdf, replace /*graphs for test scores by class size*/

*t-test
estpost ttest testscr, by(small_class) uneq
esttab using "ttest_class.tex", replace ///
cells("mu_1(fmt(%13.2fc)) mu_2(fmt(%13.2fc)) b(fmt(%13.2fc)) se(fmt(%13.2fc)) p(fmt(%13.2fc))")  ///
collabels("More than 20 students" "Up to 20 students" "Diff" "Std. Err. Diff" "P-value") ///
booktabs nonum nomtitles noobs title("t-test for the difference in \textit{testscr} by class size") /*t-test table
for the difference in test scores by class size*/

*TEST SCORES AND GEOGRAPHIC POSITION
bysort region: sum testscr, d /*descriptive statistics*/
graph hbar (mean) testscr, over(region) bar(1, fcolor(yellow) lcolor(yellow)) ///
ylabel(#7) saving(region, replace) nodraw
bysort area5: sum testscr, d
graph bar (mean) testscr, over(area5, relabel(1 "N. West" 2 "N. East")) bar(1, fcolor(blue) lcolor(blue)) ///
ylabel(#7) saving(area5, replace) nodraw
graph combine region.gph area5.gph
graph export geo.pdf, replace /*graph for test scores by geographic position*/

*TEST SCORES AND TYPE OF SCHOOL
estpost tabstat testscr, by(track) c(stat) ///
stat(mean sd min max p1 p25 p50 p75 p99) nototal /*average test score for each type of school*/
esttab using "tracktestsc.tex", replace ///
cells("mean(fmt(2)) sd min max p1 p25 p50 p75 p99") nonumber booktabs nogaps nofloat nomtitles ///
collabels("Mean" "Std. Dev." "Min" "Max" "p1" "p25" "p50" "p75" "p99") 

*TEST SCORES AND FAMILY BACKGROUND
estpost tabstat testscr, by(fam_edu) c(stat) ///
stat(mean sd min max p1 p25 p50 p75 p99) nototal /*descriptive statistics*/
esttab using "famtest.tex", replace ///
cells("mean(fmt(2)) sd min max p1 p25 p50 p75 p99") nonumber booktabs nogaps nofloat nomtitles ///
collabels("Mean" "Std. Dev." "Min" "Max" "p1" "p25" "p50" "p75" "p99") 

*TEST SCORES AND GENDER
tab gender track, row
tab gender track, co /*thus I get the frequencies of gender in schools*/

estpost ttest testscr, by(d_gender) uneq
esttab using "ttest_gender.tex", replace ///
cells("mu_1(fmt(%13.2fc)) mu_2(fmt(%13.2fc)) b(fmt(%13.2fc)) se(fmt(%13.2fc)) p(fmt(%13.2fc))")  ///
collabels("Female" "Male" "Diff" "Std. Err. Diff" "P-value") ///
booktabs nogaps nonum nomtitles noobs title("t-test for the difference in \textit{testscr} by gender") /*t-test table
for the difference in test scores by gender*/

*TEST SCORES AND MIGRANT
tab migrant track, row
tab migrant track, co /*thus I get the frequencies of migrant in schools*/

*t-test
estpost ttest testscr, by(migrant) uneq
esttab using "ttest_migrant.tex", replace ///
cells("mu_1(fmt(%13.2fc)) mu_2(fmt(%13.2fc)) b(fmt(%13.2fc)) se(fmt(%13.2fc)) p(fmt(%13.2fc))")  ///
collabels("Non-migrant" "Migrant" "Diff" "Std. Err. Diff" "P-value") ///
booktabs nonum noobs nomtitles title("t-test for the difference in testscr by migrant") /*t-test table 
for the difference in test scores by migrant*/

*************************
***REGRESSION ANALYSIS*** 
*************************
*PARSIMONIOUS MODEL
reg testscr class_size, r
estimates store reg1

*MODEL (2)
reg testscr class_size track i.area5, r
estimates store reg2

*MODEL (3)
reg testscr class_size i.track i.area5 fam_edu, r
estimates store reg3

*MODEL (4)
reg testscr class_size i.track i.area5 fam_edu d_gender age migrant, r
estimates store reg4

*MODEL (5)
reg testscr class_size* i.track i.area5 fam_edu d_gender age migrant, r
estimates store reg5
test class_size class_size2 

*MODEL(6)
reg ln_testscr class_size i.track i.area5 fam_edu d_gender age migrant, r
estimates store reg6

*****************
***FINAL TABLE***
*****************
esttab reg* using "regtable.tex", replace booktabs nogaps ///
cells(b(star fmt(%9.3f)) se(par)) ///
star(* 0.1 ** 0.05 *** 0.01) ///
ar2 scalars("F F-test") mlabels(, depvars) ///
refcat(2.track "track dummies:" 2.area5 "area dummies:" fam_edu "", nolabel) ///
order(class* *track *area5) drop(1.track 1.area5) ///
varlabels(2.area5 "north east" 3.area5 "centre" 4.area5 "south" 5.area5 "islands" ///
2.track "technical school" 3.track "vocational school" d_gender "gender") ///
addnotes("Robust Standard Errors in parenthesis" ///
"Significance level: *p<0.1, **p<0.05, ***p<0.01") ///
title(Relationship between test scores and class size in Italy)


