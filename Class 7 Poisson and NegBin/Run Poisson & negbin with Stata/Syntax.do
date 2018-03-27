cd C:\6910\Lab10
set more off

log using lab10, replace

//Example 1: Doctoral publications
//Compare Poisson with negbin 
use couart4, clear
poisson art i.female i.married kid5 phd mentor
estimate store prm
nbreg art i.female i.married kid5 phd mentor
estimate store negbin
display 2*((-1560.9583)-(-1651.0563)) 
display chi2tail(1,180.196)                                         

estimate table prm negbin, b(%9.3f) se p(%9.3f) varlabel eform

//Example 2: Robust SE and exp(B) 
// Always use robust SE for the final model
nbreg art i.female i.married kid5 phd mentor, vce(robust)

//Obtain irr or exp(B) 
listcoef, help
listcoef, percent help

//Example3: Predicted probabilities (MEMs, MERs, AMEs)
//Obtain means to create predicted probabilities
sum female married kid5 phd mentor

//MEMs
mtable, atmeans pr(0/5)
//MERs to see the effect of mentor's publications
mtable, at(mentor=(1 2 3 4 5 6)) atmeans pr(0/5)

//AMEs
predict prob0, pr(0)
predict prob1, pr(1)
predict prob2, pr(2)
predict prob3, pr(3)
predict prob4, pr(4)
sum prob0-prob4
mchange, pr(0/4)

//Example 4: Draw a line chart to show group differences on a continuous variable
tab mentor
margins i.female, at(mentor=(0(10)100)) atmeans noatlegend
marginsplot, noci title("The Impact of Mentor's publications on the Expected" "Number of Publications by Gender") ///
 ytitle("Predicted/Expected Number of Publications")
 
//Example 5: Test an interaction model
tab mentor
tab phd
gen mentor_h=0
replace mentor_h=1 if mentor > 20
label define h 0 "Mentor Pub below 20" 1 "Mentor Pub > 20"
label value mentor_h h
nbreg art i.female i.married kid5 i.mentor_h i.mentor_h##c.phd, vce(robust)
margins i.mentor_h, at(phd=(0(.5)5)) atmeans noatlegend
marginsplot, noci title("The Interactive Effect of" "Mentor's Productivity & PhD Prestige") ///
  ytitle("Predicted/Expected Number of Publications")

log close
