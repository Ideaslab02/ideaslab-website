  
     ** world bank data **
	 
    use "D:\Documents\Eid Research\Co2 determinants data.dta" 
	
	**inform Stata to perform panel data analysis**
	xtset c_id year
	xtdescribe
	
	** New Code of GMM**
	** A Difference GMM model:
	xtabond2 total_co2 L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, gmm(L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, lag(2 .))
	
	**System GMM
	xtabond2 total_co2 L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, gmm(L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, lag(2 .) collapse)
	
	** Autocorrelation test
	estat abond

	
	**Step 2: Exploratory Data Analysis (EDA)**
	sum total_co2 fts mobile internet sis fdi gdp fertilizer urban
	pwcorr total_co2 fts mobile internet sis fdi gdp fertilizer urban, sig
	xtunitroot fisher total_co2 fts mobile internet sis fdi gdp fertilizer urban, dfuller lags(1)  
	
	
	**Step 3: Lag Selection & Dynamic Specification**
	reg total_co2 L.total_co2 fts mobile internet sis fdi gdp fertilizer urban
	estimates store OLS
	xtreg total_co2 L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, fe
	estimates store FE
	hausman FE OLS  // Check if FE is preferred over OLS  
	
	
	**Step 4: GMM Estimation (Difference & System GMM)**
	**(A) Difference GMM (Arellano-Bond)**
	xtabond total_co2 fts mobile internet sis fdi gdp fertilizer urban, lags(1) vce(robust)
	est store Diff1  
	**(B) System GMM (Blundell-Bond)**
	xtdpdsys total_co2 fts mobile internet sis fdi gdp fertilizer urban, lags(1) vce(robust)
	est store Sys1  
	**(C) Two-Step GMM (More Efficient)**
	xtabond total_co2 fts mobile internet sis fdi gdp fertilizer urban, lags(1) twostep vce(robust)
	est store Diff2
	xtdpdsys total_co2 fts mobile internet sis fdi gdp fertilizer urban, lags(1) twostep vce(robust)
	est store Sys2  
	**Step 5: Model Comparison & Diagnostics**
	**(1) Overidentifying Restrictions (Sargan/Hansen Test)**
	estat sargan  // For xtabond/xtdpdsys
	
	**(2) Autocorrelation Test (Arellano-Bond)**
	estat abond
	
	**(3) Instrument Strength (Weak IV Test)**
	xtabond2 total_co2 L.total_co2 fts mobile internet sis fdi gdp fertilizer urban, gmm(L.total_co2, lag(1 2)) iv(fts mobile internet sis fdi gdp fertilizer urban) twostep robust
	
	**(4) Compare Models**
	estimates table OLS FE Diff1 Sys1 Diff2 Sys2, b se stats(N r2)
	
	**Example Full Code**
	xtset country year
	xtabond total_co2 de fts mobile internet sis fdi gdp fertilizer urban, lags(1) twostep vce(robust)
	estat sargan
	estat abond
	xtdpdsys total_co2 de fts mobile internet sis fdi gdp fertilizer urban, lags(1) twostep vce(robust)
	estat sargan
	estat abond  
	
	
	** summary mean of poverty reduction indicators**
	tabstat total_co2 fts mobile internet sis fdi gdp fertilizer urban, by(country)
    
	** descriptive statistics of the variables **
	mean total_co2 fts mobile internet sis fdi gdp fertilizer urban
	xtsum total_co2 fts mobile internet sis fdi gdp fertilizer urban
    
	** normality of the data **
	histogram total_co2, normal
    histogram log_Poverty, normal
	xtsktest FDI GFCF HUMCAP lnINFL lnUNEMP lnOPEN Poverty, reps(279)
	
	** regression model **
	reg total_co2 fts mobile internet sis fdi gdp fertilizer urban
	reg log_Poverty OPEN GFCF INFL UNEMP HUMCAP FDI
	
	** Histogram normality test of residuals **
    histogram residual, normal
	
	*** formal test for multicollinearity ***
    vif
    
	** Homoscedasticity test **
	hettest residual
	
	
	
	** Fixed and random effect models **
	
	xtreg total_co2 fts mobile internet sis fdi gdp fertilizer urban, fe
	estimates store fe
	
	xtreg total_co2 fts mobile internet sis fdi gdp fertilizer urban, re
	estimates store re
	
	** Causility test FE vs OLS **
	hausman fe re
	
	** RE vs OLS **
	 xtreg total_co2 fts mobile internet sis fdi gdp fertilizer urban, re
	 estimates store re
	 xttest0
	 
	 ** Residual test **
	 xtreg total_co2 fts mobile internet sis fdi gdp fertilizer urban, re
	 predict resid, residuals
	 sktest resid
	 lmnadxt LnPoverty OPEN GFCF INFL LPR FDI GDP GIN LE, id( c_id) it( year)
	 
	 xtset c_id year
	 xtsktest resid
	 
	 ** heteroscedasticity **
	 xtset c_id year
	 xtreg total_co2 fts mobile internet sis fdi gdp fertilizer urban, re
	 xttest3
	 lmhwaldxt total_co2 fts mobile internet sis fdi gdp fertilizer urban, id( c_id) it( year)
	 lmhlmxt total_co2 fts mobile internet sis fdi gdp fertilizer urban, id( c_id) it( year)
	 
	 ** model specification **
	 xtset c_id year
	 xtreg Poverty lnOPEN GFCF lnINFL lnUNEMP HUMCAP FDI, fe
	 resetxt Poverty lnOPEN GFCF lnINFL lnUNEMP HUMCAP FDI, id( c_id) it( year) model(xtfe)
	 
	 ** Crosectional Dependence **
	 xtcsd, pesaran
	 xtcsd, friedman
	 xtcsd, frees
	
	**ARDL Model
	** Steps
	**Step 1: Indoem Stata to perform panel data analysis
	xtset c_id year
	**Step 2: specify Model
	Poverty=OPEN+GFCF+INFL+UNEMP+HUMCAP+FDI+error
	total_co2=fts+mobile+internet+sis+fdi+gdp+fertilizer+urban
	
	**Step 3: Descriptive Statistics and summary Statistics
	xtsum Poverty OPEN GFCF INFL UNEMP HUMCAP FDI
	sum Poverty OPEN GFCF INFL UNEMP HUMCAP FDI
	
	** the standard deviation is large enough to explore the variation
	sum Poverty OPEN GFCF INFL UNEMP HUMCAP FDI if cc=="CFA"
	sum Poverty OPEN GFCF INFL UNEMP HUMCAP FDI if ccy=="NCFA"
	
	**Step 3: correlation analysis
	corr Poverty lnOPEN GFCF lnINFL lnUNEMP HUMCAP FDI
	
	**Step 4: Perform unitroot test
	xtunitroot ips Poverty, lags(1)
	xtunitroot ips OPEN, lags(1)
	xtunitroot ips GFCF, lags(1)
	xtunitroot ips INFL, lags(1)
	xtunitroot ips UNEMP, lags(1)
	xtunitroot ips HUMCAP, lags(1)
	xtunitroot ips FDI, lags(1)
	
	**differencing
	xtunitroot ips d.Poverty, lags(1)
	xtunitroot ips d.OPEN, lags(1)
	xtunitroot ips d.GFCF, lags(1)
	xtunitroot ips d.INFL, lags(1)
	xtunitroot ips d.UNEMP, lags(1)
	xtunitroot ips d.HUMCAP, lags(1)
	xtunitroot ips d.FDI, lags(1)
	
	**unitroot with constant, trend and 1 lag
	xtunitroot ips Poverty, trend lags(1)
	xtunitroot ips OPEN, trend lags(1)
	xtunitroot ips GFCF, trend lags(1)
	xtunitroot ips INFL, trend lags(1)
	xtunitroot ips UNEMP, trend lags(1)
	xtunitroot ips HUMCAP, trend lags(1)
	xtunitroot ips FDI, trend lags(1)
	
	**Step 5: check for optimal lag lengths for the model and variables
	forval i= 1/9{
	ardl Poverty OPEN GFCF INFL UNEMP HUMCAP FDI if (c_id==`i'),maxlag(2 2 2 2 2 2)
	matrix list e(lags)
	di
	} 
	
	
	**Step 6: Cointegration test
	xtpedroni Poverty OPEN GFCF INFL UNEMP HUMCAP FDI, nopdols
	
	**Step 7: Determine most appropriate estimater using Hausman test
	xtpmg d(Poverty OPEN UNEMP INFL HUMCAP GFCF FDI), lr(Poverty OPEN UNEMP INFL HUMCAP GFCF FDI)replace ec(ec)
	xtpmg d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI, lr(d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI)replace pmg
	xtpmg d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI, lr(d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI)replace mg
	xtpmg d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI, lr(d.Poverty lnOPEN d.UNEMP INFL HUMCAP GFCF FDI)replace dfe
	
	**hausman
	hausman pmg MG, sigmamore
	hausman DFE pmg, sigmamore
	
	**Step 8: Estimating the models
	ARDL (1, 0, 0, 0, 0, 0, 0, 0)
	xtpmg d.Poverty d.OPEN d.GFCF d.INFL d.UNEMP d.HUMCAP FDI, lr(l.Poverty OPEN GFCF INFL UNEMP HUMCAP FDI) ec(ECT)replace pmg
	xtpmg d.Poverty d.OPEN d.GFCF d.INFL d.UNEMP d.HUMCAP FDI, lr(l.Poverty OPEN GFCF INFL UNEMP HUMCAP FDI) ec(ECT)replace pmg full
	xtpmg d.Poverty d.OPEN d.GFCF d.INFL d.UNEMP d.HUMCAP FDI, lr(l.Poverty OPEN GFCF INFL UNEMP HUMCAP FDI) ec(ECT)replace mg
	xtpmg d.Poverty d.OPEN d.GFCF d.INFL d.UNEMP d.HUMCAP FDI, lr(l.Poverty
 OPEN GFCF INFL UNEMP HUMCAP FDI) ec(ECT)replace dfe
	
	
	
	**Quantile Regression model
	
	gladder Poverty
	gladder OPEN
	gladder GFCF 
	gladder INFL 
	gladder UNEMP
	gladder FDI 
	gladder GDP 
	gladder GIN
	gladder LE
	
	
	**Step 1: Indoem Stata to perform panel data analysis
	xtset c_id year

	**Step 2: Descriptive Statistics and summary Statistics
	xtsum Poverty OPEN GFCF INFL UNEMP FDI GDP GIN LE
	sum Povverty OPEN GFCF INFL UNEMP FDI GDP GIN LE
	xtsum co2 fts mobile internet sis fdi gdp fertilizer urban
	
	**Step 3: correlation analysis
	corr Poverty OPEN GFCF INFL LPR FDI GDP GIN LE
	
	**Step 4: Quantile Model
	qregpd co2 fts mobile internet sis fdi gdp fertilizer urban, id( c_id) fix( year) optimize(mcmc) noisy draws(1000) burn(100) quantile(0.25) arate(.5)
	
	qregpd LnPoverty OPEN GFCF INFL LPR FDI GDP GIN LE, id( c_id) fix( year) optimize(mcmc) noisy draws(1000) burn(100) quantile(0.50) arate(.5)
	
	qregpd LnPoverty OPEN GFCF INFL LPR FDI GDP GIN LE, id( c_id) fix( year) optimize(mcmc) noisy draws(1000) burn(100) quantile(0.75) arate(.5)
	
	** Heatplot
	
	correlate co2 fts mobile internet sis fdi gdp fertilizer urban    
	return list
	matrix corrmatrix = r(C)
	heatplot corrmatrix
	heatplot corrmatrix, values(format(%4.3f))
	heatplot corrmatrix, values(format(%4.3f) size(medium))
	heatplot corrmatrix, values(format(%4.3f) size(medium)) legend(off)
	heatplot corrmatrix, values(format(%4.3f) size(medium)) legend(off) color(hcl diverging, intensity(.7))
	heatplot corrmatrix, values(format(%4.3f) size(medium)) legend(off) color(hcl diverging, intensity(.7))

	

	sqr_Poverty sqr_OPEN inv_INFL LnUNEMP cu_LE GFC FDI LnGDP GIN