* Race
rename off_race off_race2
gen off_race = 1 if off_race2=="Alaskan Native/Native American"
replace off_race = 2 if off_race2=="Asian/Pacific Islander"
replace off_race = 3 if off_race2=="Black"
replace off_race = 4 if off_race2=="Hispanic"
replace off_race = 5 if off_race2=="White"


* Gender
rename off_gender off_gender2
gen off_gender = 1 if off_gender2=="Female"
replace off_gender = 0 if off_gender2=="Male"
