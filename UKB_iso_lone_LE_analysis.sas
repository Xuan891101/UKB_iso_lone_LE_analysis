options nofmterr;

libname ukb 'C:\Users\DATA\UKB\SASdata';

data baseline;set ukb.ukb200318(keep=
n_eid 
n_21022_0_0 
n_31_0_0 
n_21000_0_0 
n_189_0_0 
n_738_0_0 
n_6138_0:    
n_20116_0_0		
n_21001_0_0   
n_884_0:		
n_904_0:		
n_894_0:		
n_914_0:		
n_1289_0_0  
n_1299_0_0   
n_1309_0_0   
n_1319_0_0  
n_1329_0_0   
n_1339_0_0  
n_1349_0_0   
n_1359_0_0  
n_1369_0_0  
n_1379_0_0   
n_1389_0_0  
n_1438_0_0  
n_1448_0_0  
n_1458_0_0  
n_1468_0_0  
n_1160_0_0		
n_709_:		
n_1031_:		
n_6160_:	
n_2020_:		
n_2110_:		
);

/*** covariates ***/

age=n_21022_0_0;
sex=n_31_0_0;
Townsend_Index=n_189_0_0;
income= n_738_0_0;if n_738_0_0 in (-1,-3) then income=-3;

     if n_21000_0_0=1 or 1001<=n_21000_0_0<=1003 then white =1; 
else if n_21000_0_0>0 then white =0; 
else white=-3;


array eee{6} n_6138_0_0-n_6138_0_5;
array edu{6} eduyear_0-eduyear_5;

do i=1 to 6;
     if eee{i} in (-3)  then edu{i}=.;
else if eee{i} in (1)   then edu{i}=20; 
else if eee{i} in (5)   then edu{i}=19; 
else if eee{i} in (6)   then edu{i}=15; 
else if eee{i} in (2)   then edu{i}=13; 
else if eee{i} in (3,4) then edu{i}=10; 
else if eee{i} in (-7)  then edu{i}=7; 
end;
eduyear=max(eduyear_0,eduyear_1,eduyear_2,eduyear_3,eduyear_4,eduyear_5);


/*** health lifestyles ***/

smoking=n_20116_0_0;	
     if smoking in (0,1) then smk=1;
else if smoking=2 then smk=0;

bmi=n_21001_0_0;
     if .<bmi<18.5   then bmi_grp=2;
else if 18.5<=bmi<25 then bmi_grp=1;
else if 25<=bmi<30   then bmi_grp=3;
else if 30<=bmi      then bmi_grp=4;
else bmi_grp=-3;
     if bmi_grp=1 then bmi_normal=1;
else if 1<bmi_grp<=4 then bmi_normal=0;


array vm{*} n_884_0_0 n_904_0_0 n_894_0_0  n_914_0_0;
array pp{*} mod_day   vig_day   mod_dur    vig_dur;

do i=1 to 4;pp{i}=vm{i};if vm{i} in (-1,-3) then pp{i}=.;end;

if mod_day=0 then mod_dur=0;
if vig_day=0 then vig_dur=0;
mod_min=mod_dur*mod_day;
vig_min=vig_dur*vig_day;
pa_min=sum(mod_min,vig_min*2);

     if .<pa_min<150 then pa150ggg=0;
else if pa_min>=150  then pa150ggg=1;
     if   pa_min=0    then pa150ggg3=0;
     if 0<pa_min<150  then pa150ggg3=1;
else if   pa_min>=150 then pa150ggg3=2;


     if	n_1289_0_0	>=0  then Cooked_vegetable=	n_1289_0_0; 
else if n_1289_0_0	=-10 then Cooked_vegetable=0.5;
     if	n_1299_0_0	>=0  then raw_vegetable_intake= n_1299_0_0; 
else if n_1299_0_0	=-10 then raw_vegetable_intake=0.5;
vegetable=Cooked_vegetable + raw_vegetable_intake;

     if	n_1309_0_0>=0  then fresh_fruit= n_1309_0_0; 
else if n_1309_0_0=-10 then fresh_fruit=0.5;
     if	n_1319_0_0>=0  then dried_fruit= n_1319_0_0; 
else if n_1319_0_0=-10 then dried_fruit=0.5;
fruit=fresh_fruit + dried_fruit;

     if n_1369_0_0=0 then Beef=0; 
else if n_1369_0_0=1 then Beef=0.5; 
else if n_1369_0_0=2 then Beef=1; 
else if n_1369_0_0=3 then Beef=3; 
else if n_1369_0_0=4 then Beef=5.5; 
else if n_1369_0_0=5 then Beef=7;

     if n_1379_0_0=0 then Lamb=0; 
else if n_1379_0_0=1 then Lamb=0.5; 
else if n_1379_0_0=2 then Lamb=1; 
else if n_1379_0_0=3 then Lamb=3; 
else if n_1379_0_0=4 then Lamb=5.5; 
else if n_1379_0_0=5 then Lamb=7;

     if n_1389_0_0=0 then Pork=0; 
else if n_1389_0_0=1 then Pork=0.5; 
else if n_1389_0_0=2 then Pork=1; 
else if n_1389_0_0=3 then Pork=3; 
else if n_1389_0_0=4 then Pork=5.5; 
else if n_1389_0_0=5 then Pork=7;
redmeat=Beef + Lamb + Pork;

     if n_1349_0_0=0 then procmeat=0; 
else if n_1349_0_0=1 then procmeat=0.5; 
else if n_1349_0_0=2 then procmeat=1; 
else if n_1349_0_0=3 then procmeat=3; 
else if n_1349_0_0=4 then procmeat=5.5; 
else if n_1349_0_0=5 then procmeat=7;

     if n_1329_0_0=0 then Oilyfish=0; 
else if n_1329_0_0=1 then Oilyfish=0.5; 
else if n_1329_0_0=2 then Oilyfish=1; 
else if n_1329_0_0=3 then Oilyfish=3; 
else if n_1329_0_0=4 then Oilyfish=5.5; 
else if n_1329_0_0=5 then Oilyfish=7;

     if n_1339_0_0=0 then nonOilyfish=0; 
else if n_1339_0_0=1 then nonOilyfish=0.5; 
else if n_1339_0_0=2 then nonOilyfish=1; 
else if n_1339_0_0=3 then nonOilyfish=3; 
else if n_1339_0_0=4 then nonOilyfish=5.5; 
else if n_1339_0_0=5 then nonOilyfish=7;
fish=Oilyfish + nonOilyfish;

if n_1438_0_0 >=0 then Bread=n_1438_0_0; else if n_1438_0_0=-10 then  Bread =0.5;
if n_1458_0_0 >=0 then Cereal=n_1458_0_0;else if n_1458_0_0=-10 then  Cereal=0.5;

if n_1448_0_0=3 then Whole_bread=Bread;
if n_1448_0_0<3 or n_1448_0_0>=4 then Refined_bread=Bread;

if n_1468_0_0=1 or 3 or 4 then Whole_cereal=Cereal;
if n_1468_0_0=2 or 5 or -1 or -3  then Refined_cereal=Cereal;

whole_grain= Whole_bread + Whole_cereal;
refined_grain=Refined_bread + Refined_cereal;

if n_1438_0_0	>=0 and n_1458_0_0	>=0 and whole_grain=. then whole_grain=0;
if n_1438_0_0	>=0 and n_1458_0_0	>=0 and refined_grain=. then refined_grain=0;

if Vegetable_intake>=3 then vegetable_score=1;else vegetable_score=0;
if Fruit_intake>=3     then fruit_score=1;    else fruit_score=0;
if fish_intake>=2      then fish_score=1;     else fish_score=0;
if Redmeat_intake<=1.5      then Redmeat_score=1;      else Redmeat_score=0;
if Proceeded_meat_intake<=1 then Proceeded_meat_score=1;else Proceeded_meat_score=0;
if whole_cereal>=3       then whole_cereal_score=1;     else whole_cereal_score=0;
if non_whole_cereal<=1.5 then non_whole_cereal_score=1; else non_whole_cereal_score=0;
Brief_dietaaa=vegetable_score+fruit_score+fish_score+Redmeat_score+Proceeded_meat_score+whole_cereal_score+non_whole_cereal_score;

     if 4<=Brief_detaaa    then diet4ggg=1;
else if .< Brief_detaaa<4  then diet4ggg=0;
     if 5<=Brief_detaaa    then diet4ggg3=2;
else if 4<=Brief_detaaa<5  then diet4ggg3=1;
else if .< Brief_detaaa<4  then diet4ggg3=0;

sleep_hrs=n_1160_0_0;
if Sleep_hrs in (-1,-3) then Sleep_hrs=.;
     if 7<=Sleep_hrs<9 then sleep_duration=1;
else if 0<=Sleep_hrs<7 or Sleep_hrs>=9 then sleep_duration=0;
     if .< Sleep_hrs<6 or Sleep_hrs>=10 then slpggg3=0;
else if 6<=Sleep_hrs<7  then slpggg3=1;
else if 9<=Sleep_hrs<10 then slpggg3=1;
else if 7<=Sleep_hrs<9  then slpggg3=2;

if smk>=0 and bmi_normal>=0 and pa150ggg>=0 and diet4ggg>=0 and sleep_duration>=0 
then hls =smk +bmi_normal +pa150ggg +diet4ggg +sleep_duration;

     if .< hls<3  then hls_grp=0;
else if    hls=3  then hls_grp=1;
else if 4<=hls<=5 then hls_grp=2;



/*** Isolation ***/

     if n_709_0_0<0 then iso_number=-3;
else if n_709_0_0=1 then iso_number=1;
else if n_709_0_0>1 then iso_number=0;

     if   n_1031_0_0<0  then iso_friend=-3;
else if   n_1031_0_0>4  then iso_friend=1;
else if 0<n_1031_0_0<=4 then iso_friend=0;


array iso3{5} n_6160_0_0-n_6160_0_4;

do i=1 to 5;
     if iso3{i}=-3 then iso_activity =-3;
else if iso3{i}=-7 then iso_activity =1;
else if 0<iso3{i}<6 then iso_activity =0;
end;

if iso_number>=0 and iso_friend>=0 and iso_activity>=0 then iso_score=iso_number +iso_friend +iso_activity;
if iso_score<0 then iso_score=-3;

     if    iso_score>=2 then iso_status=1;
else if 0<=iso_score<2  then iso_status=0;
else if    iso_score=-3 then iso_status=-3;

/*** loneliness ***/

if n_2020_0_0<0 then lone_often=-3;else lone_often= n_2020_0_0;

     if    n_2110_0_0<0  then lone_confide=-3;
else if    n_2110_0_0>=2 then lone_confide=0;
else if 0<=n_2110_0_0<2  then lone_confide=1;

if lone_often>=0 and lone_confide>=0 then lone_score=lone_often +lone_confide;
if lone_score<0 then lone_score=-3;

     if lone_score in (0,1) then lone_status=0;
else if lone_score=2 then lone_status=1;

/*** exposure ***/

     if iso_status=0 and lone_status=0 then joint=0;
else if iso_status=0 and lone_status=1 then joint=1;
else if iso_status=1 and lone_status=0 then joint=2;
else if iso_status=1 and lone_status=1 then joint=3;

     if hls_grp=0 and lone_status=0 then jointllls=0;
else if hls_grp=1 and lone_status=0 then jointllls=1;
else if hls_grp=2 and lone_status=0 then jointllls=2;
else if hls_grp=0 and lone_status=1 then jointllls=3;
else if hls_grp=1 and lone_status=1 then jointllls=4;
else if hls_grp=2 and lone_status=1 then jointllls=5;
     if hls_grp=0 and iso_status=0  then jointiiis=0;
else if hls_grp=1 and iso_status=0  then jointiiis=1;
else if hls_grp=2 and iso_status=0  then jointiiis=2;
else if hls_grp=0 and iso_status=1  then jointiiis=3;
else if hls_grp=1 and iso_status=1  then jointiiis=4;
else if hls_grp=2 and iso_status=1  then jointiiis=5;

     if hls^=. and smoking=2 and iso_status=0 then jointiii_smk=0;
else if hls^=. and smoking=1 and iso_status=0 then jointiii_smk=1;
else if hls^=. and smoking=0 and iso_status=0 then jointiii_smk=2;
else if hls^=. and smoking=2 and iso_status=1 then jointiii_smk=3;
else if hls^=. and smoking=1 and iso_status=1 then jointiii_smk=4;
else if hls^=. and smoking=0 and iso_status=1 then jointiii_smk=5;

     if hls^=. and bmi_grp=4 and iso_status=0 then jointiii_bmi=0;
else if hls^=. and bmi_grp=3 and iso_status=0 then jointiii_bmi=1;
else if hls^=. and bmi_grp=1 and iso_status=0 then jointiii_bmi=2;
else if hls^=. and bmi_grp=4 and iso_status=1 then jointiii_bmi=3;
else if hls^=. and bmi_grp=3 and iso_status=1 then jointiii_bmi=4;
else if hls^=. and bmi_grp=1 and iso_status=1 then jointiii_bmi=5;

     if hls^=. and pa150ggg3=0 and iso_status=0 then jointiii_pa=0;
else if hls^=. and pa150ggg3=1 and iso_status=0 then jointiii_pa=1;
else if hls^=. and pa150ggg3=2 and iso_status=0 then jointiii_pa=2;
else if hls^=. and pa150ggg3=0 and iso_status=1 then jointiii_pa=3;
else if hls^=. and pa150ggg3=1 and iso_status=1 then jointiii_pa=4;
else if hls^=. and pa150ggg3=2 and iso_status=1 then jointiii_pa=5;

     if hls^=. and diet4ggg3=0 and iso_status=0 then jointiii_diet=0;
else if hls^=. and diet4ggg3=1 and iso_status=0 then jointiii_diet=1;
else if hls^=. and diet4ggg3=2 and iso_status=0 then jointiii_diet=2;
else if hls^=. and diet4ggg3=0 and iso_status=1 then jointiii_diet=3;
else if hls^=. and diet4ggg3=1 and iso_status=1 then jointiii_diet=4;
else if hls^=. and diet4ggg3=2 and iso_status=1 then jointiii_diet=5;

     if hls^=. and slpggg3=0 and iso_status=0 then jointiii_slp=0;
else if hls^=. and slpggg3=1 and iso_status=0 then jointiii_slp=1;
else if hls^=. and slpggg3=2 and iso_status=0 then jointiii_slp=2;
else if hls^=. and slpggg3=0 and iso_status=1 then jointiii_slp=3;
else if hls^=. and slpggg3=1 and iso_status=1 then jointiii_slp=4;
else if hls^=. and slpggg3=2 and iso_status=1 then jointiii_slp=5;

     if hls^=. and smoking=2 and lone_status=0 then jointlll_smk=0;
else if hls^=. and smoking=1 and lone_status=0 then jointlll_smk=1;
else if hls^=. and smoking=0 and lone_status=0 then jointlll_smk=2;
else if hls^=. and smoking=2 and lone_status=1 then jointlll_smk=3;
else if hls^=. and smoking=1 and lone_status=1 then jointlll_smk=4;
else if hls^=. and smoking=0 and lone_status=1 then jointlll_smk=5;

     if ind=1 and bmi_grp=4 and lone_status=0 then jointlll_bmi=0;
else if ind=1 and bmi_grp=3 and lone_status=0 then jointlll_bmi=1;
else if ind=1 and bmi_grp=1 and lone_status=0 then jointlll_bmi=2;
else if ind=1 and bmi_grp=4 and lone_status=1 then jointlll_bmi=3;
else if ind=1 and bmi_grp=3 and lone_status=1 then jointlll_bmi=4;
else if ind=1 and bmi_grp=1 and lone_status=1 then jointlll_bmi=5;

     if ind=1 and pa150ggg3=0 and lone_status=0 then jointlll_pa=0;
else if ind=1 and pa150ggg3=1 and lone_status=0 then jointlll_pa=1;
else if ind=1 and pa150ggg3=2 and lone_status=0 then jointlll_pa=2;
else if ind=1 and pa150ggg3=0 and lone_status=1 then jointlll_pa=3;
else if ind=1 and pa150ggg3=1 and lone_status=1 then jointlll_pa=4;
else if ind=1 and pa150ggg3=2 and lone_status=1 then jointlll_pa=5;

     if ind=1 and diet4ggg3=0 and lone_status=0 then jointlll_diet=0;
else if ind=1 and diet4ggg3=1 and lone_status=0 then jointlll_diet=1;
else if ind=1 and diet4ggg3=2 and lone_status=0 then jointlll_diet=2;
else if ind=1 and diet4ggg3=0 and lone_status=1 then jointlll_diet=3;
else if ind=1 and diet4ggg3=1 and lone_status=1 then jointlll_diet=4;
else if ind=1 and diet4ggg3=2 and lone_status=1 then jointlll_diet=5;

     if ind=1 and slpggg3=0 and lone_status=0 then jointlll_slp=0;
else if ind=1 and slpggg3=1 and lone_status=0 then jointlll_slp=1;
else if ind=1 and slpggg3=2 and lone_status=0 then jointlll_slp=2;
else if ind=1 and slpggg3=0 and lone_status=1 then jointlll_slp=3;
else if ind=1 and slpggg3=1 and lone_status=1 then jointlll_slp=4;
else if ind=1 and slpggg3=2 and lone_status=1 then jointlll_slp=5;
;
run;



data ICD10 ;set 'C:\Users\84910\OneDrive\DATA\UKB\SASdata\ukb675245.sas7bdat';proc sort;by n_eid;run;
data ICD9 ;set ukb.ukb200318(keep=
n_eid 
s_41271: 
s_41281:
n_20001_0: 
n_20002_0: 
n_6150_0:  
n_2443_0_0 
n_2453_0_0 
n_6152_0: 
s_53_0_0 
s_42014_0_0 
s_42016_0_0 
s_42018_0_0 
s_42000_0_0 
s_42006_0_0 
s_42032_0_0
);

data ICD;merge ICD10 ICD9;by n_eid;
if s_53_0_0 ne .; 
followdate=s_191_0_0;
end_date='31DEC2022'd;

death_Date=s_40000_0_0;
if death_Date ne . then death=1;else death=0;
if death_Date=. then death_Date= end_date;
death_day=Intck('day',s_53_0_0, death_date);

array icd10(259) s_41270_0_0 - s_41270_0_258;
array icd10d(259) s_41280_0_0 - s_41280_0_258;

array cvd_date10(259) cvd_date10_0 - cvd_date10_258;
array t2d_date10(259) t2d_date10_0 - t2d_date10_258;
array t1d_date10(259) t1d_date10_0 - t1d_date10_258;
array dem_date10(259) dem_date10_0 - dem_date10_258;
array neu_date10(259) neu_date10_0 - neu_date10_258;
array can_date10(259) can_date10_0 - can_date10_258;
array crd_date10(259) crd_date10_0 - crd_date10_258;
array dep_date10(259) dep_date10_0 - dep_date10_258;
array anx_date10(259) anx_date10_0 - anx_date10_258;

do i = 1 to 259;

/************** CVD ****************/
***cvd (including CHD I20-I25, stroke I60-61 and I63-64, HF, and AF);
if icd10 (i) in (
"I20","I200","I201","I208","I209",
"I21","I210","I211","I212","I213","I214","I219",
"I22","I220","I221","I228","I229",
"I23","I230","I231","I232","I233","I234","I235","I236","I238",
"I24","I240","I241","I248","I249",
"I25","I250","I251","I252","I253","I254","I255","I256","I258","I259",

"I60","I600","I601","I602","I603","I604","I605","I606","I607","I608","I609",
"I61","I610","I611","I612","I613","I614","I615","I616","I618","I619",
"I63","I630","I631","I632","I633","I634","I635","I636","I638","I639",
"I64",

"I110","I130","I132","I500","I501","I509",
"I480","I481","I482","I483","I484","I489"
)
then cvd_date10(i)=icd10d(i);



/************** Diabetes ****************/
**T2d**;
if icd10 (i) in ("E11","E110","E111", "E112", "E113", "E114", "E115", "E116", "E117","E118", "E119")
then t2d_date10(i)=icd10d(i);

**T1d**;
if icd10 (i) in ("E100","E101", "E102", "E103", "E104", "E105", "E106", "E107","E108", "E109")
then t1d_date10(i)=icd10d(i);


/************** dementia ****************/
if icd10 (i) in (
"F00", "F000", "F001", "F002","F009","F01", "F010", "F011", "F012","F013","F018","F019",
"I673","G30","G300","G301","G308","G309",

"F02", "F020", "F021", "F022","F023","F024","F028","F03",
"F051","F106","G310","G311","G318","A810" )
then dem_date10(i)=icd10d(i);


/************** neurodegenerative disease (dementia or parkinsonism);parkinson's Disease****************/
if icd10 (i) in (
"F00", "F000", "F001", "F002","F009","F01", "F010", "F011", "F012","F013","F018","F019",
"I673","G30","G300","G301","G308","G309",

"F02", "F020", "F021", "F022","F023","F024","F028","F03",
"F051","F106","G310","G311","G318","A810", 

"G20")
then neu_date10(i)=icd10d(i);


/************** Cancer (total) ****************/
if icd10 (i) in (
"C00","C000","C001","C002","C003","C004","C005","C006","C009",
"C01",
"C02","C020","C021","C022","C023","C024","C028","C029",
"C03","C030","C031","C039",
"C04","C040","C041","C048","C049",
"C05","C050","C051","C052","C058","C059",
"C06","C060","C061","C062","C068","C069",
"C07",
"C08","C080","C081","C088","C089",
"C09","C090","C091","C098","C099",
"C10","C100","C101","C102","C103","C104","C108","C109",
"C11","C110","C111","C112","C113","C118","C119",
"C12",
"C13","C130","C131","C132","C138","C139",
"C14","C140","C148",
"C15","C150","C151","C152","C153","C154","C155","C158","C159",
"C16","C160","C161","C162","C163","C164","C165","C166","C168","C169",
"C17","C170","C171","C172","C173","C178","C179",
"C18","C180","C181","C182","C183","C184","C185","C186","C187","C188","C189",
"C19",
"C20",
"C21","C210","C218",
"C22","C220","C221","C222","C223","C224","C227","C229",
"C23",
"C24","C240","C241","C248","C249",
"C25","C250","C251","C252","C253","C254","C257","C258","C259",
"C26","C260","C261","C268","C269",
"C30","C300","C301",
"C31","C310","C311","C312","C313","C318","C319",
"C32","C320","C321","C322","C323","C328","C329",
"C33",
"C34","C340","C341","C342","C343","C348","C349",
"C37",
"C38","C380","C381","C382","C383","C384",
"C39","C390","C398","C399",
"C40","C400","C401","C402","C403","C408","C409",
"C41","C410","C411","C412","C413","C414","C419",
"C43","C430","C431","C432","C433","C434","C435","C436","C437","C438","C439",
"C45","C450","C451","C452","C457","C459",
"C46","C460","C461","C462","C463","C467","C468","C469",
"C47","C470","C471","C472","C473","C474","C475","C476","C479",
"C48","C480","C481","C482","C488",
"C49","C490","C491","C492","C493","C494","C495","C496","C498","C499",
"C50","C500","C501","C502","C503","C504","C505","C506","C508","C509",
"C51","C510","C511","C512","C518","C519",
"C52",
"C53","C530","C531","C538","C539",
"C54","C540","C541","C542","C543","C548","C549",
"C55",
"C56",
"C57","C570","C571","C574","C577","C578","C579",
"C58",
"C60","C600","C601","C602","C608","C609",
"C61",
"C62","C620","C621","C629",
"C63","C631","C632","C637","C639",
"C64","C65","C66",
"C67","C670","C671","C672","C673","C674","C675","C676","C677","C678","C679",
"C68","C680","C681","C688","C689",
"C69","C690","C691","C692","C693","C694","C695","C696","C698","C699",
"C70","C700","C701","C709",
"C71","C710","C711","C712","C713","C714","C715","C716","C717","C718","C719",
"C72","C720","C721","C722","C723","C724","C725","C728","C729",
"C73",
"C74","C740","C741","C749",
"C75","C750","C751","C752","C753","C754","C755","C758","C759",
"C76","C760","C761","C762","C763","C764","C765","C767","C768",
"C77","C770","C771","C772","C773","C774","C775","C778","C779",
"C78","C780","C781","C782","C783","C784","C785","C786","C787","C788",
"C79","C790","C791","C792","C793","C794","C795","C796","C797","C798","C799",
"C80","C800","C809",
"C81","C810","C811","C812","C813","C814","C817","C819",
"C82","C820","C821","C822","C823","C824","C825","C826","C827","C829",
"C83","C830","C831","C832","C833","C834","C835","C836","C837","C838","C839",
"C84","C840","C841","C843","C844","C845","C846","C847","C848","C849",
"C85","C850","C851","C852","C857","C859",
"C86","C860","C862","C863","C864","C865","C866",
"C88","C880","C882","C883","C884","C887","C889",
"C90","C900","C901","C902","C903",
"C91","C910","C911","C912","C913","C914","C915","C916","C917","C918","C919"
"C92","C920","C921","C922","C923","C924","C925","C926","C927","C928","C929",
"C93","C930","C931","C933","C939",
"C94","C940","C942","C944","C945","C946","C947",
"C95","C950","C951","C959",
"C96","C960","C961","C962","C963","C964","C965","C966","C967","C968","C969",
"C97"
)
then can_date10(i)=icd10d(i);


/************** respiratory disease ****************/
***CRD (Asthma J45,J450,J451,J458,J459,J46x
        COPD J43 J430 J431 J432 J438 J439 J44 J440 J441 J448 J449);
if icd10 (i) in (
"J45","J450","J451","J458","J459","J46",

"J43","J430","J431","J432","J438","J439",
"J44","J440","J441","J448","J449"
)
then crd_date10(i)=icd10d(i);


/************** Depression ****************/
if icd10 (i) in 
("F320","F321","F322","F323","F328","F329","F330","F331","F332","F333","F334","F338","F339","F340","F341","F348","F349","F380","F381","F388","F39"
)
then Dep_date10(i)=icd10d(i);


/************** Anxiety ****************/
if icd10 (i) in 
("F400","F401","F402","F408","F409","F410","F411","F412","F413","F418","F419"
)
then Anx_date10(i)=icd10d(i);

end;


/**********************************************************
     ICD9-Prevalence for CVD/Diabetes/Dementia/Cancer;
**********************************************************/

array icd9(47) s_41271_0_0 - s_41271_0_46;
array icd9d(47) s_41281_0_0 - s_41281_0_46;

array cvd_date9(47) cvd_date9_0 - cvd_date9_46;
array dia_date9(47) dia_date9_0 - dia_date9_46;
array dem_date9(47) dem_date9_0 - dem_date9_46;
array neu_date9(47) neu_date9_0 - neu_date9_46;
array can_date9(47) can_date9_0 - can_date9_46;
array crd_date9(47) crd_date9_0 - crd_date9_46;
array dep_date9(47) dep_date9_0 - dep_date9_46;
array anx_date9(47) anx_date9_0 - anx_date9_46;

do i = 1 to 47;

***cvd;
if icd9(i)in ("4109","4119","4129","4139","4140","4141","4148","4149",
              "4309","4319","4349","4369",
              "40201","40211","40291", "40401","40411","40491","40403","40413","40493",
              "4280","4281","4289",
              "4273") 
then  cvd_date9(i)=icd9d(i);


***diabetes;
if icd9(i)in ("25000","25001","25009","25010","25011","25019","25029","2503","2504","2505","25099") 
then  dia_date9(i)=icd9d(i);

***dementia;
if icd9(i)in ("3310","2904", "3331", "2902","2903","2912","2941","3312","3315") 
then  dem_date9(i)=icd9d(i);

***dementia and Pakinson's Disease;
if icd9(i)in ("3310","2904", "3331", "2902","2903","2912","2941","3312","3315","3320") 
then  neu_date9(i)=icd9d(i);

***cancer;
if icd9(i)in (
"1413","1416","1419","1420","1440","1449","1460","1505","1519","1521","1530","1532","1533","1534","1536","1537","1539","1540","1541","1542","1543",
"1551","1561","1570","1574","1579","1590","1600","1610","1613","1619","1623","1629","1649",
"1702","1707","1709","1712","1713","1717","1719","1720","1723","1725","1726","1727","1729","1740","1743","1744","1745","1748","1749",
"1799","1800","1809","1820","1830","1844","1859","1860","1869","1874","1882","1884","1889","1890",
"1906","1909","1913","1916","1919","1939","1950","1951","1960","1961","1962","1963","1965","1969","1970","1975","1976","1983","1985","1988","1990","1991",
"2001","2015","2016","2017","2019","2020","2024","2028","2029","2040","2050","2051","2059","2080",
"2100","2101","2102","2103","2104","2111","2112","2113","2114","2115","2117","2118","2119","2120","2121","2123","2125","2127","2130","2131","2134","2135","2137","2138","2139","2149",
"2150","2152","2153","2154","2155","2156","2157","2159","2160","2161","2162","2163","2164","2165","2166","2167","2168","2169","2179","2189","2190","2191","2199","2209","2210","2211","2212",
"2220","2221","2224","2230","2233","2241","2243","2251","2252","2253","2254","2269","2270","2271","2273","2275","2280","2281","2290","2298",
"2300","2325","2326","2327","2330","2331","2332","2333","2337","2348",
"2352","2354","2355","2357","2360","2362","2363","2364","2366","2367","2370","2371","2375","2377","2381","2382","2383","2384","2386","2387","2388",
"2390","2392","2393","2394","2395","2396","2397","2398"
) 
then  can_date9(i)=icd9d(i);

***crd;
if icd9(i)in (
"493","4930","4931","4939",
"492","4920","4928","4929",
"496","4969"
)
then crd_date9(i)=icd9d(i);

***dep;
if icd9(i)in ("3199")then dep_date9(i)=icd9d(i);

***anx;
if icd9(i)in ("3000","3002") then anx_date9(i)=icd9d(i);

*****ICD-9 diagnoses were not included as it does not define BPD as a distinct disorder.
ref:doi: 10.1017/S0033291721000945.;
end;

/**********************************************************
     Self-report for CVD/Diabetes/Dementia/Cancer;
**********************************************************/

******************* 20002/20001 *************************;

array spnon(29) n_20002_0_0 - n_20002_0_28;
array spcan(6)  n_20001_0_0 - n_20001_0_5;

do i = 1 to 29;

if spnon(i) in (
"1074", 
"1075",	
"1081",
"1086", 
"1491",
"1583", 
"1076", 
"1471"  
) then cvd_self=1;

if spnon(i) in (
"1220", 
"1222", 
"1223" 
) then dia_self=1;

if spnon(i) in (
"1263"  
) then dem_self=1;

if spnon(i) in (
"1263", 
"1262"  
) then neu_self=1;

if spnon(i) in (
"1111", 
"1112", 
"1113",  
"1472"  
) then crd_self=1;

if spnon(i) in (
"1286"  
) then Dep_self=1;

if spnon(i) in (
"1287"  
) then Anx_self=1;


do i = 1 to 6;
if spcan(i) ne .  then can_self=1;
if spcan(i)=1060 then can_self=.;
end;


**************** Medical conditions ***********************;
if n_6150_0_0 in (1,2,3) or 
   n_6150_0_1 in (1,2,3) or 
   n_6150_0_2 in (1,2,3) or 
   n_6150_0_3 in (1,2,3) then cvd_self=1;


*****dia;
if n_2443_0_0=1 then dia_self=1;

*****can;
if n_2453_0_0=1 then can_self=1;

*****crd;
if n_6152_0_0 in (6,8) or 
   n_6152_0_1 in (6,8) or 
   n_6152_0_2 in (6,8) or 
   n_6152_0_3 in (6,8) or 
   n_6152_0_4 in (6,8) then crd_self=1;



******************Algorithmically-defined outcomes**************;
if s_42014_0_0 in ('01Jan1900'd) then s_42014_0_0=.;
if s_42016_0_0 in ('01Jan1900'd) then s_42016_0_0=.;
if s_42018_0_0 in ('01Jan1900'd) then s_42018_0_0=.;
if s_42032_0_0 in ('01Jan1900'd) then s_42032_0_0=.;
if s_42000_0_0 in ('01Jan1900'd) then s_42000_0_0=.;
if s_42006_0_0 in ('01Jan1900'd) then s_42006_0_0=.;


**asthma;
if s_42014_0_0 ne . then asthma=1;
if asthma=1 then asthma_day=Intck('day',s_53_0_0, s_42014_0_0);
if .<asthma_day<=0 then asthma_base=1;

**copd;
if s_42016_0_0 ne . then copd=1;
if copd=1 then copd_day=Intck('day',s_53_0_0, s_42016_0_0);
if .<copd_day<=0 then copd_base=1;

if asthma_base=1 or copd_base=1 then crd_self=1;

*all cause dementia;
if s_42018_0_0 ne . then dem=1;
if dem=1 then dem_day=Intck('day',s_53_0_0, s_42018_0_0);
if .<dem_day<=0 then dem_self=1;

*Parkinson's Disease;
if s_42032_0_0 ne . then pak=1;
if pak=1 then pak_day=Intck('day',s_53_0_0, s_42032_0_0);
if .<pak_day<=0 then pak_self=1;
if dem_self=1 or pak_self=1 then neu_self=1;

*myocardial infarction;
if s_42000_0_0 ne . then mi=1;
if mi=1 then mi_day=Intck('day',s_53_0_0, s_42000_0_0);
if .<mi_day<=0 then mi_base=1;

*stroke;
if s_42006_0_0 ne . then stroke=1;
if stroke=1 then stroke_day=Intck('day',s_53_0_0, s_42006_0_0);
if .<stroke_day<=0 then stroke_base=1;

if mi_base=1 or stroke_base=1 then cvd_self=1;


data outcome;set ICD;
if cvd_icd10_base=1 or cvd_icd9_base=1 or cvd_self=1 then cvd_baseline=1;else cvd_baseline=0;
if dia_icd10_base=1 or dia_icd9_base=1 or dia_self=1 then dia_baseline=1;else dia_baseline=0;
if dem_icd10_base=1 or dem_icd9_base=1 or dem_self=1 then dem_baseline=1;else dem_baseline=0;
if neu_icd10_base=1 or neu_icd9_base=1 or neu_self=1 then neu_baseline=1;else neu_baseline=0;
if can_icd10_base=1 or can_icd9_base=1 or can_self=1 then can_baseline=1;else can_baseline=0;
if crd_icd10_base=1 or crd_icd9_base=1 or crd_self=1 then crd_baseline=1;else crd_baseline=0;

if dep_icd10_base=1 or dep_icd9_base=1 or dep_self=1 then dep_baseline=1;else dep_baseline=0;
if anx_icd10_base=1 or anx_icd9_base=1 or anx_self=1 then anx_baseline=1;else anx_baseline=0;

if dis7_icd10_base=1 or dis7_icd9_base=1 or dis7_self=1 then dis7_baseline=1;else dis7_baseline=0;
if dis5_icd10_base=1 or dis5_icd9_base=1 or dis5_self=1 then dis5_baseline=1;else dis5_baseline=0;
if dis2_icd10_base=1 or dis2_icd9_base=1 or dis2_self=1 then dis2_baseline=1;else dis2_baseline=0;

if dis8_icd10_base=1 or dis7_icd9_base=1 or dis8_self=1 then dis8_baseline=1;else dis8_baseline=0;
if dis3_icd10_base=1 or dis2_icd9_base=1 or dis3_self=1 then dis3_baseline=1;else dis3_baseline=0;

if PHQ2_depression=1 then dep_baseline=1;
if PHQ2_anxiety=1    then anx_baseline=1;
if PHQ2_depression=1 or PHQ2_anxiety=1 then do dis7_baseline=1;dis2_baseline=1;end;

format base_date  yymmdd10. death_date yymmdd10. dis7_date  yymmdd10. dis5_date  yymmdd10. dis2_date  yymmdd10. dis8_date  yymmdd10. dis3_date  yymmdd10. 
       cvd_date   yymmdd10. dia_date   yymmdd10. dem_date   yymmdd10. neu_date   yymmdd10. can_date   yymmdd10. crd_date   yymmdd10. 
       dep_date   yymmdd10. anx_date   yymmdd10. 
run;



data analysis;merge baseline outcome;
if iso_score in (., -3) then delete;
if lone_score in (., -3) then delete;
if dis7_baseline=1 then delete;

dis5_day=dis5_date-base_date;
dis7_day=dis7_date-base_date;
dis2_day=dis2_date-base_date;

cvd_day=cvd_date-base_date;
dia_day=dia_date-base_date;
neu_day=neu_date-base_date;
can_day=can_date-base_date;
crd_day=crd_date-base_date;
dep_day=dep_date-base_date;
anx_day=anx_date-base_date;
run;


data "C:\Users\analysis\analysis.sas7bdat";set analysis;run;

