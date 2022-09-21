//--(2) Descriptive Analysis

//Import the dataset
//Inspect the dataset
browse
describe

//Descriptive Analysis
//Log conversion
generate log_alexa = log(alexa)

//Two-way table for all levels of seniority levels with other variables
tabstat log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period , by(year) statistics(count min max median sd) longstub column(s)
ssc install asdoc //To export the tables
asdoc tabstat log_marketcap alexa log_bing log_star log_price log_facebook log_twitter log_subsc period , by(year) statistics

//Log conversion
generate log_alexa = log(alexa)

//Correlation Matrix
pwcorr log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period, star(0.01)
asdoc  pwcorr log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period, star(0.01)

//Correlation graph matrix of the variables
graph matrix  log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period


//Significance Tests
//ttest
ttest log_marketcap, by(year)

//One-way Anova
oneway log_marketcap year


//--(3)Exploratory Analysis
// Summary Statistic graphs over years 
//bar graph for years
graph hbar (mean) log_marketcap (median) log_marketcap (sd) log_marketcap, over(year) name(e)
graph hbar (mean) alexa (median) alexa (sd) alexa, over(year) name(f)
graph hbar (mean) log_subsc (median) log_subsc (sd) log_subsc, over(year) name(g)
graph hbar (mean) log_star (median) log_star (sd) log_star, over(year) name(h)
graph hbar (mean) log_bing (median) log_bing (sd) log_bing, over(year) name(i)
graph combine e f g h i

//Distribution of main variables
histogram log_marketcap , normal name(j)
histogram log_subsc , normal name(k)
histogram log_alexa , normal name(p)
histogram log_bing , normal name(m)
histogram log_star , normal name(x)
graph combine j k p m x 

//Outliers
 graph hbox log_bing log_star log_facebook log_twitter log_subsc
 graph hbox log_twitter period log_marketcap
 graph hbox alexa

 //Relation b/w dependent & independent variables
 twoway (scatter log_marketcap log_subsc, mcolor(navy) msize(vsmall) msymbol(circle)) (lfit log_marketcap log_subsc, lcolor(maroon) lwidth(medthick)), name(r)
 twoway (scatter log_marketcap log_star , mcolor(gray) msize(vsmall) msymbol(triangle)) (lfit log_marketcap log_star , lcolor(maroon) lwidth(medthick)), name(s)
twoway (scatter log_marketcap log_alexa, mcolor(eltgreen) msize(vsmall) msymbol(diamond)) (lfit log_marketcap log_alexa, lcolor(maroon) lwidth(medthick)), name(t)
twoway (scatter log_marketcap log_bing, mcolor(erose) msize(vsmall) msymbol(circle)) (lfit log_marketcap log_bing, lcolor(maroon) lwidth(medthick)), name(u)
graph combine r s t u

//Longitudinal trend of the main variables
twoway (lfit log_subsc period), ytitle(log_subsc) name(v)
twoway (lfit log_star period), ytitle( log_star ) name(w)
twoway (lfit log_alexa period), ytitle( log_alexa ) name(z)
twoway (lfit log_bing period), ytitle( log_bing ) name(y)
graph combine v w z y

sort symbol period log_marketcap
//Bitcoin market share across periods
//Bitcoin market share (numerator)
generate bitcoin_marketcap = log_marketcap

//Bitcoin market share (denominator)
bysort period: egen sum_marketcap_sale= sum(log_marketcap)

//generate market share variable
generate Market_share = bitcoin_marketcap /sum_marketcap_sale

//Bitcoin's market share across periods
twoway (line Market_share period if symbol == "btc"), ytitle(Bitcoin's market cap) title(Bitcoin market share across periods)

//--(4)Main Regression
//OLS regression baseline model
eststo clear
reg log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period 
eststo BL1


//model 1
reg log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period if name =="Bitcoin"
eststo M2

//model 2
reg log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period if name !="Bitcoin"
eststo M3

//table comparision
esttab BL1 M2 M3 using Main4_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title (Main Regression) mtitles("Baseline model" "Model2" "Model3")

//--(5)Diagnostics and Robustness Analysis

//diagnostic analysis on baseline model to check potential of heteroskedasticity
estat hettest  
estat imtest, white
asdoc estat imtest, white
//residuals vs fitted value
rvfplot, yline(0)

//baseline ols regression model 
eststo clear
reg log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period 
eststo md1

//robust regression model
reg log_marketcap log_alexa log_bing log_star log_price log_facebook log_twitter log_subsc period,vce(robust)
eststo md2

//table comparision
esttab md1 md2 using Main_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title(Model comparision)mtitles("Baseline model" "robust")