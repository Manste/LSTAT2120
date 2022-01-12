ods graphics on;
ods pdf file="/home/u59968750/Project/sorties_projetLSTAT2120.pdf";
libname modlin "/home/u59968750/Project/";
/*creating data set in worklibrary from the dataset of the libname*/
data melbourne_housing;
	set modlin.melbourne_housing;
run;
/*visualizing dataset*/
proc contents data=melbourne_housing; run;
/*Question 1*/
/*splitting dataset randomly in 2 groups*/
/*predicting dataset 10% and estimating dataset 90%*/
proc surveyselect data=melbourne_housing samprate=0.10 seed=2021 out=Sample 
	outall method=srs noprint;
run;
/*  predicting dataset*/
data melbourne_housing_pred (drop=Selected);
	set Sample;
	where Selected=1;
run;
/*estimating dataset*/
data melbourne_housing_est (drop=Selected);
	set Sample;
	where Selected=0;
run;
/*Question2*/
/*Question3*/
/**Quantitatives variables*/
/***Statistics*/
proc means data=melbourne_housing_est mean std CV max min skew kurt;
	var Price Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
	YearBuilt Propertycount;
run;
/***boxplot*/
proc sgplot data=melbourne_housing_est;
	title "house Prices Distribution"; hbox Price;
run;
proc sgplot data=melbourne_housing_est;
	title "Number of Rooms Distribution"; hbox Rooms;
run;
proc sgplot data=melbourne_housing_est;
	title "Number of Bedroom2 Distribution"; hbox Bedroom2;
run;
proc sgplot data=melbourne_housing_est;
	title "Number of Bathroom Distribution"; hbox Bathroom;
run;
proc sgplot data=melbourne_housing_est;
	title "Landsize Distribution"; hbox Landsize;
run;
proc sgplot data=melbourne_housing_est;
	title "BuildingArea Distribution"; hbox BuildingArea;
run;
proc sgplot data=melbourne_housing_est;
	title "number of parking Cars Distribution"; hbox Car;
run;
proc sgplot data=melbourne_housing_est;
	title "Distance Distribution"; hbox Distance;
run;
proc sgplot data=melbourne_housing_est;
	title "Year of Built Distribution"; hbox YearBuilt;
run;
proc sgplot data=melbourne_housing_est;
	title "Propertycount Distribution"; hbox Propertycount;
run;
/***Correlation matrix*/
proc corr data=melbourne_housing_est;
	var Price Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
	YearBuilt Propertycount;
run;
*conclusion: room is highly correlated with bedrooms(we suspect multicollinearity;
/**qualitatives variables*/
proc freq data=melbourne_housing_est;
	tables Type Method Regionname;
run;
/**Quantitatives*qualitatives variables*/
/**Statistics*/
proc sort data=melbourne_housing_est out=data_by_Type;
	by Type;
run;
proc means data=data_by_Type mean std CV max min skew kurt;
	var Price Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
	YearBuilt Propertycount;
	by Type;
run;
/*higher mean in type=h cathegories for Landsize and BuildingArea 
can be a good candidat for interaction term: typeh*Landsize and typeh*BuildingArea*/
/*type t seems to be specialist of selling recent houses and t for older houses
interaction term typet*year*/
proc sort data=melbourne_housing_est out=data_by_Method;
	by Method;
run;
proc means data=data_by_Method mean std CV max min skew kurt;
	var Price Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
	YearBuilt Propertycount;
	by Method;
run;
proc sort data=melbourne_housing_est out=data_by_Regionname;
	by Regionname;
run;
proc means data=data_by_Regionname mean std CV max min skew kurt;
	var Price Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
	YearBuilt Propertycount;
	by Regionname;
run;
/*Region Nothern victoria has high mean for landsize and building area
interaction term RegionNV*landsize et RegionNV*buildingarea
Overall the house prices seems to have different variance and mean by group of 
qualitatives variables namely for variable region. this can be a sign of
heteroscedasticity*/
/***Boxplot*/
proc sgplot data=melbourne_housing_est;
	title "Price Distribution by Type group"; hbox Price / category=Type;
run;
proc sgplot data=melbourne_housing_est;
	title "Price Distribution by Method group"; hbox Price / category=Method;
run;
proc sgplot data=melbourne_housing_est;
	title "Price Distribution by region group"; hbox Price / category=Regionname;
run;
/*Question4*/
/*model with only quantitatives variables*/
proc reg data=melbourne_housing_est plots=none;
	model Price=Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
		YearBuilt propertycount;
run;
/*conclusion: Bedroom2 and propertycount are individually not significant*/
/*may be Bedroom is not significant because of multicollinearity stated above*/
/*let's calculate VIF to confirm multicollinearity*/
proc reg data=melbourne_housing_est plots=none;
	model Price=Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
		YearBuilt propertycount/vif;
run;
/*Indeed Rooms and bedrooms have VIF larger than 10 */
/*we can conclude that there is a multicollinearity issue*/
/*We can either use ridge regression or (keeping all the involved variable)*/
/*or simply  remove one of the variables way to deal with multicollinearity.*/
/*with large VIF.*/
/*just for illustration we run ridgeregression to see how it works*/
proc reg data=melbourne_housing_est outest=data_ridge plots(only)=ridge;
	model Price=Rooms Bedroom2 Bathroom Landsize BuildingArea Car Distance 
		YearBuilt propertycount/ridge=(0.001 to 0.2 by 0.001);
run;
/*Both VIF and standard coefficient stabilised aroung ridge_parameter =0.15*/
/*but In this project we adopt the second strategy and remove Bedrooms 
from the model for what follows*/
/*model with all quantitatives (except Bedroom2) and qualitative variables*/
/**lets first renomme modalities for Regioname to shorten them*/
data melbourne_housing_est2;
	set melbourne_housing_est;
	if Regionname="Eastern Metropolitan" then Regionname="EM";
	if Regionname="Eastern Victoria" then Regionname="EV";
	if Regionname="Northern Metropolitan" then Regionname="NM";
	if Regionname="Northern Victoria" then Regionname="NV";
	if Regionname="South-Eastern Metropolitan" then Regionname="SEM";
	if Regionname="Southern Metropolitan" then Regionname="SM";
	if Regionname="Western Metropolitan" then Regionname="WM";
	if Regionname="Western Victoria" then Regionname="WV";
run;
proc transreg data=melbourne_housing_est2 plots=none;
	model identity(Price)=identity(Rooms Bathroom Landsize BuildingArea Car 
		Distance YearBuilt Propertycount) class(Type Method Regionname)/ss2;
	output out=data_transformed;
run;
/*conclusion: propertycount and some dumy variable are not significant*/
/*variable selection type 1 using cp as criteria*/
proc reg data=data_transformed plots=none;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	propertycount Typeh Typet MethodPI MethodS MethodSA MethodSP RegionEM 
	RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM/selection=cp best=10;
run;
/*variable selection type 1 using adjRsq as criteria*/
proc reg data=data_transformed plots=none;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	propertycount Typeh Typet MethodPI MethodS MethodSA MethodSP RegionEM 
	RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM/selection=adjrsq 
	best=10;
run;
/*en plus des variables choisis précedement methodSA est aussi choisie*/
/*variable selection type2 stepwise*/
proc reg data=data_transformed outest=selected_modelforward tableout plots=none;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	propertycount Typeh Typet MethodPI MethodS MethodSA MethodSP RegionEM 
	RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM/noprint 
	selection=stepwise;
run;
proc print data=selected_modelforward; run;
/*variable selection type2 stepwise*/
proc reg data=data_transformed outest=selected_modelbackward tableout plots=none;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	propertycount Typeh Typet MethodPI MethodS MethodSA MethodSP RegionEM 
	RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM/noprint 
	selection=backward;
run;
proc print data=selected_modelbackward; run;
/*Type2 meme variable choisis que type1 avec cp comme critère*/
/*variable selection type3 Lasso*/
proc glmselect data=melbourne_housing_est2 plots(stepaxis=normb)=all seed=123;
	class Type Method Regionname;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	propertycount Type Method Regionname/ selection=lasso(stop=none choose=cvex) 
	cvmethod=random(5);
run;
/*pour la suite nous decidons de garder le model resultant de la majorité de*/
/*methode de selection ici celui avec 17 variables*/
/*analyse de residus afin de detecter s'il ya une structure dans les données: 
	homoscedasticity et normalité*/
/*normalité des termes d'erreur*/
proc reg data=data_transformed plots(MAXPOINTS=7000 only)=(qq residualhistogram);
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt Typeh 
	Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM;
run;
/*The histogramm and QQ-Plot are not good*/
/*We could conclude that the distribution of the error is not Normal */
/*test de Jarque - Bera ou test for normality*/
proc autoreg data=data_transformed plots=none;
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM 
	RegionSM RegionWM/normal;
run;
/*homoscedasticité des termes d'erreur*/
proc reg data=data_transformed plots(MAXPOINTS=7000 only)=(residuals(unpack) 
		RESIDUALBYPREDICTED);
	model Price=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt Typeh 
	Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM RegionSM RegionWM;
run;
/*le plot de residus en fonction des y predits montrent que l'accroissement 
de la variance des termes d'erreur est exponentiel en Y:  Les termes 
d'erreur sont heteroscedastique */
/*we do a white test to confirm homoscedasticity*/
proc model data=data_transformed;
	parms b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17;
	Price=b0 + b1*Rooms + b2*Bathroom + b3*Landsize + b4*BuildingArea + b5*Car 
	+ b6*Distance + b7*YearBuilt + b8*Typeh + b9*Typet + b10*MethodS 
	+ b11*MethodSP + b12*RegionEV + b13*RegionNM + b14*RegionNV 
	+ b15*RegionSEM+ b16*RegionSM + b17*RegionWM;
	fit Price/white;
run;
/*we expect the p-value to be higher than 5%, such a way H0: 
errors are homoscedastic will be rejected*/
/*remedial action*/
/*etant donné que les residus augmentent exponentiellement en fonction des valeurs 
ajustées, afin de rendre cette variance constante nous faisons la transformation 
suivante y'=logy*/
data data_transformed2;
	set data_transformed;
	logprice=log(price);
run;
/*visualisation à nouveau l'homoscedasticité des residus*/
proc reg data=data_transformed2 plots(MAXPOINTS=7000 only)=(residuals(unpack) 
		RESIDUALBYPREDICTED);
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
		Typeh Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM 
		RegionSM RegionWM;
run;
/*les erreurs semblent homoscedastique*/
/*confirmons cela par le test de white de white */
proc model data=data_transformed2;
	parms b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17;
	logprice=b0 + b1*Rooms + b2*Bathroom + b3*Landsize + b4*BuildingArea
      + b5*Car + b6*Distance + b7*YearBuilt + b8*Typeh + b9*Typet + b10*MethodS 
      + b11*MethodSP + b12*RegionEV + b13*RegionNM + b14*RegionNV + b15*RegionSEM
      + b16*RegionSM + b17*RegionWM;
	fit logprice/white;
run;
* bien que les erreurs semblent homoscédastique sur les plots des résidus, 
le test de white rejette néanmoins l'homoscedasticité(avec une p-valeur < 0.0001);
* dans le modele final, la résolution que nous adoptons est de continuer 
d'utiliser la methodes des moindres carrées ordinaires, mais nous utiliserons 
l'inférence robuste afin de pouvoir tenir compte de l'hétéroscedasticité 
ceci aura pour conséquence une augmentation des erreurs standards et donc, 
des tests statistiques et des p-valeurs, mais les valeurs des coefficients 
estimés resteront les mêmes;
/*visualisation à nouveau la normalité des residus*/
proc reg data=data_transformed2 plots(MAXPOINTS=7000 only)=(qq residualhistogram);
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM 
	RegionSM RegionWM;
run;
/*les erreurs semblent être distribuées normalement. 
Confirmons cela par le test de JB */
proc autoreg data=data_transformed2 plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM 
	RegionSM RegionWM/normal;
run;
/*we expect this time the p-value to be lower than 5%, such a way H0: 
errors are normally distributed is not rejected*/
/*Etant donné que Region NV n'est plus indivuduellement significatif 
dans ce model transformé, refaisons une selection de variables du 
nouveau model avec la selection forward*/
proc reg data=data_transformed2 outest=selected_modelforward2 tableout plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionNV RegionSEM RegionSM 
	RegionWM/noprint selection=stepwise;
run;	
proc print data=selected_modelforward2; run;
/*on voit effectivement que cette variable Region NV est rejeté */
/*donc the final model is:*/
proc reg data=data_transformed2 plots=None;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM;
run;
/*outliers observation with respect to independant variables X*/
/*consideroutliers if leverahge hii>threshol=2*p/N =2*16/6166 */
proc reg data=data_transformed2;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM 
	RegionSM RegionWM/noprint;
	plot h.*obs.;
	output out=outliers_X h=lev;
run;
proc print data=outliers_X;
	var lev;
	where lev > 2*17/6166;
run;
/*outliers observation with respect to dependant variable Y*/
/*using the Studentized residuals at 5% level*/
proc reg data=data_transformed2;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt Typeh 
	Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM/noprint;
	plot rstudent.*obs.;
	output out=outliers_Y rstudent=stud;
run;
proc print data=outliers_Y;
	var stud;
	where abs(stud) > tinv(0.975, 61669-17-1);
run;
/*Influential observation (because Xi is outlier or Yi is outliers or both of them*/
/*using norm(DFFITS)>2*sqrt(p/n)=2*sqrt(17/6166)*/
proc reg data=data_transformed2 plots(MAXPOINTS=7000 only)=dffits;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM;
	output out=influentials dffits=df;
run;
proc print data=influentials;
	var df;
	where (df > 2*sqrt(17/6166) or df < -2*sqrt(17/6166)) and df ne .;
run;
/*influential observations using the Cook’s distance
influence not only prediction i or beta k but all the prediction
and all the betas*/
proc reg data=data_transformed2 plots(MAXPOINTS=7000 only)=COOKSD;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM;
	output out=influentialscook cookd=cd;
run;
proc print data=influentialscook;
	var cd;
	where cd > finv(0.95, 17, 6166-17);
run;
/*une seule observation numero=1410*/
/*Interaction*/
data data_interaction2;
	set data_transformed2;
	th_Build=typeh*BuildingArea;
	tt_year=typet*YearBuilt;
run;
proc reg data=data_interaction2 plots=None;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year/white;
run;
proc reg data=data_interaction2 outest=selected_modelforward3 tableout plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year/white noprint selection=stepwise;
run;
proc print data=selected_modelforward3;
run;
* Bien que la variable ait une p-valeur légèrement supérieur à 5%, cette variable 
n'est pas rejetée par la méthode de sélection des variables;
*comme dit plus haut, dans ce modele final, nous utiliserons l'inférence robuste 
afin de remedier au problème persistant d'hétéroscedasticité en incluant l'option 
"white" dans le modèle Sas;
*Inférence robuste;
proc reg data=data_interaction2 plots=None;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year/white;
run;
* on retrouve les mêmes coefficients estimés mais avec des ecart-type différents
 (écart-types augmentent), ce qui modifie vraisemblablement les statistiques 
 des tests et des p-valeurs. Par exemple on voit que certaines variables 
 deviennent non significatives(le "th_build" avec un p-valeur de 15%, 
 alors que sans inférence robuste, on avait une p-valeur <0.0001 ); 
/*Question5*/
/**Test de significativité joint*/
proc reg data=data_interaction2 plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year /white noprint;
	test Rooms=0, Bathroom=0, Landsize=0, BuildingArea=0, Car=0, Distance=0, 
	YearBuilt=0, Typeh=0, Typet=0, MethodS=0, MethodSP=0, RegionEV=0, RegionNM=0, 
	RegionSEM=0, RegionSM=0, RegionWM=0, th_Build=0, tt_year=0;
run;
/*Label Price-valeur associer à Landsize F-statistique est inferieur à 5%, on rejette
l'hypothese nulle H0: tous les coefficient sont nuls ie dire aucune variable n'est 
significative ou bien la prediction se reduit à une simple moyenne des prix*/
/*interpretation des signes/interpretations des coefficients*/

/*Question6*/
/*extra room or extra bathroom has the same effect of the price*/
proc reg data=data_interaction2 plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year /white noprint;
	test Rooms-Bathroom=0;
run;
/*on ne reject pas H0, ie à dire l'idée à laquelle l'augmentation d'une 
pièce ou d'une toilette conduirait à la meme augmentation du prix*/
/*extra m2 on land or extra m2 on building area has the same effect of the price*/
proc reg data=data_interaction2 plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year /white noprint;
	test Landsize-BuildingArea=0;
run;
/*on reject H0, ie à dire l'idée à laquelle l'augmentation d'un m2 de terrain 
ou d'un m2 de la surface contruite conduirait à la meme augmentation du prix*/
/*Question7*/
/*test subset of coefficient for exemple the qualitative variables are 
jointly non significant*/
proc reg data=data_interaction2 plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year /white noprint;
	test Typeh=0, Typet=0, MethodS=0, MethodSP=0, RegionEV=0, RegionNM=0, 
		RegionSEM=0, RegionSM=0, RegionWM=0;
run;
/*H0: tous les variables qualitatives sont jointivement non significative, est rejeté*/
/*question8* and *Question 9*/
/*overall our model which consists of predicting logprice of house with respect 
to some explanatory variables is adequate for linear regression, we've been 
verified classical hypothesis: independance,normality and homoscedasticity 
of error terms. With this model we do show that one can predict a price of 
a new house dependant on its characteristics (number of rooms, bathrooms,
landsize etc...)*/
/*prediction*/
data predict;
	set sample;
	if Regionname="Eastern Metropolitan" then Regionname="EM";
	if Regionname="Eastern Victoria" then Regionname="EV";
	if Regionname="Northern Metropolitan" then Regionname="NM";
	if Regionname="Northern Victoria" then Regionname="NV";
	if Regionname="South-Eastern Metropo" then Regionname="SEM";
	if Regionname="Southern Metropolitan" then Regionname="SM";
	if Regionname="Western Metropolitan" then Regionname="WM";
	if Regionname="Western Victoria" then Regionname="WV";
run;
data predict;
	set predict;
	if Type="h" then typeh=1; else typeh=0;
	if Type="t" then typet=1; else typet=0;
	if Method="S" then MethodS=1; else MethodS=0;
	if Method="SP" then MethodSP=1; else MethodSP=0;
	if Regionname="EV" then RegionEV=1; else RegionEV=0;
 	if Regionname="NM" then RegionNM=1; else RegionNM=0;
	if Regionname="SEM" then RegionSEM=1; else RegionSEM=0;
	if Regionname="SM" then RegionSM=1; else RegionSM=0; 
	if Regionname="WM" then RegionWM=1; else RegionWM=0;
	th_Build=typeh*BuildingArea;
	tt_year=typet*BuildingArea;
run;
data predict;
	set predict;
	logprice=log(Price);
	if selected=1 then do;
		logprice_Obs=logprice;
		logprice=.;
	end;
run;
proc print data=predict;
run;
proc reg data=predict plots=none;
	model logprice=Rooms Bathroom Landsize BuildingArea Car Distance YearBuilt 
	Typeh Typet MethodS MethodSP RegionEV RegionNM RegionSEM RegionSM RegionWM 
	th_Build tt_year/white;
	output out=my_pred p=predicted ucl=upper_pred lcl=lower_pred;
run;
quit;
data toprint(keep=selected Price logprice logprice_Obs predicted lower_pred upper_pred);
	set my_pred;
run;
proc print data=toprint (obs=20);
	where selected=1 and lower_pred ne . ;
run;
quit;
* on observe que toutes les valeurs observées de la variable transformée "logprice";
*sont l'intervalle de confiance comme illustré dans les 20 premières observations 
ci dessus;
ods pdf close;
ods _all_ close;