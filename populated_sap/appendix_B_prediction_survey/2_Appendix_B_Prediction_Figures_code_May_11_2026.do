 use "~DIGIn\Prediction_data_cleaned.dta", clear

*** Figure B1 - Predicted substitution rates by GPs and healthcare executives 

*** Draw kernel density functions by profession

twoway ///
(kdensity Substitution_rate if Profession == 3) ///
(kdensity Substitution_rate if Profession == 6), ///
legend(pos(6) col(1))

*** Compute mean prediction and 95% confidence interval by professions

reg Substitution_rate i.Profession, nocons, if Profession ==3

reg Substitution_rate i.Profession, nocons, if Profession ==6

*** T-test testing the the differences in predicted subtitution between GPs and executives

ttest Substitution_rate if inlist(Profession, 3, 6), by(Profession)

*******************************************************************************

* Predicted substitution rate by all professions (not included in the populated SAP)

* betterbar Substitution_rate, over(Profession) vertical ci, if Profession == 3 | Profession ==4 | Profession ==5 | Profession ==6

********************************************************************************

********************************************************************************
* Predicted substitution rate by GPs and Executives using histograms (not included in the the populated SAP)

twoway ///
(histogram Substitution_rate if Profession == 3, bin(20) start(0) color(stc1%40)) ///
(histogram Substitution_rate if Profession == 6, bin(20) start(0) color(stc2%40)) ///
,legend(pos(6) col(1)) xtitle("Predicted substitution (percent)") xline(30.0) xline(50.7)




