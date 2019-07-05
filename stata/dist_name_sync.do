replace state = "andaman & nicobar islands" if state == "andaman and nicobar"
replace state = "puducherry" if state == "pondicherry"
replace state = "dadra & nagar haveli" if state == "dadar & nagar haveli"
replace state = "daman & diu" if state == "daman and diu"
replace state = "tamilnadu" if state == "tamil nadu"
replace state = "odisha" if state == "orissa"

* Districts
replace district = "nicobars" if district == "nicobar"
replace district = "north & middle andaman" if district == "north and middle andaman"

replace district = "y.s.r" if (state == "andhra pradesh" & district == "kadapa") | (state == "andhra pradesh" & district == "cuddapah")
replace district = "sri potti sriramulu nellore" if state == "andhra pradesh" & district == "nellore"

replace district = "balemu east kameng" if state == "arunachal pradesh" & district == "east kameng"

replace district = "tinsukia" if state == "assam" & district == "digboi"
replace district = "karbi anglong" if state == "assam" & district == "diphu"
replace district = "dima hasao" if state == "assam" & district == "north cachar hills"
replace district = "dima hasao" if state == "assam" & district == "haflong"

replace district = "kaimur (bhabua)" if state == "bihar" & district == "kaimur"
replace district = "purba champaran" if state == "bihar" & district == "east champaran"

replace district = "dakshin bastar dantewada" if state == "chhattisgarh" & district == "dantewada"
replace district = "kabeerdham" if state == "chhattisgarh" & district == "kabirdham"
replace district = "uttar bastar kanker" if state == "chhattisgarh" & district == "kanker"

replace district = "dadra & nagar haveli" if district == "dadra and nagar haveli"

replace district = "ahmadabad" if state == "gujarat" & district == "ahmedabad"
replace district = "banas kantha" if state == "gujarat" & district == "banaskantha"
replace district = "dohad" if state == "gujarat" & district == "dahod"
replace district = "the dangs" if state == "gujarat" & district == "dang"
replace district = "junagadh" if state == "gujarat" & district == "junagarh"
replace district = "kachchh" if state == "gujarat" & district == "kutch"
replace district = "panch mahals" if state == "gujarat" & district == "panchmahal"

replace district = "kangra" if state == "himachal pradesh" & district == "dharmshala"
replace district = "lahul & spiti" if district == "lahul and spiti"

replace district = "purbi singhbhum" if state == "jharkhand" & district == "east singhbhum"
replace district = "hazaribagh" if state == "jharkhand" & district == "hazaribag"
replace district = "pashchimi singhbhum" if state == "jharkhand" & district == "west singhbhum"
replace district = "saraikela-kharsawan" if (state == "jharkhand" & district == "seraikela") | (state == "jharkhand" & district == "seraikela kharsawan")

replace district = "bangalore" if state == "karnataka" & district == "bangalore urban"
replace district = "dakshina kannada" if state == "karnataka" & district == "mangalore"

replace district = "kollam" if state == "kerala" & district == "quilon"

replace district = "east nimar" if state == "madhya pradesh" & district == "khandwa"
replace district = "west nimar" if state == "madhya pradesh" & district == "khargaone"
replace district = "singrauli" if state == "madhya pradesh" & district == "singrouli"

replace district = "ahmadnagar" if state == "maharashtra" & district == "ahmednagar"
replace district = "bid" if state == "maharashtra" & district == "beed"
replace district = "palhgar" if state == "maharashtra" & district == "dahanu"
replace district = "gondiya" if state == "maharashtra" & district == "gondia"
replace district = "pune" if state == "maharashtra" & district == "junnar"
replace district = "mumbai suburban" if state == "maharashtra" & district == "mumbai (suburban)"
replace district = "mumbai" if state == "maharashtra" & district == "mumbai city"
replace district = "raigarh" if state == "maharashtra" & district == "raigad"

replace district = "imphal east" if state == "manipur" & district == "manipur(east)"
replace district = "imphal west" if state == "manipur" & district == "manipur(west)"

replace district = "baleshwar" if state == "odisha" & district == "balasore"
replace district = "baudh" if state == "odisha" & district == "boudh"
replace district = "debagarh" if state == "odisha" & district == "devgarh"
replace district = "jajapur" if state == "odisha" & district == "jajpur"
replace district = "kendujhar" if state == "odisha" & district == "keonjhar"
replace district = "nabarangapur" if state == "odisha" & district == "nabarangpur"
replace district = "subarnapur" if state == "odisha" & district == "sonapur"

replace district = "puducherry" if state == "puducherry" & district == "pondicherry"

replace district = "sahibzada ajit singh nagar" if state == "punjab" & district == "sas nagar"
replace district = "shahid bhagat singh nagar" if (state == "punjab" & district == "shahid bhagat singh nagar(nawansheher tahsil)") | (state == "punjab" & district == "nawanshahr")
replace district = "tarn taran" if state == "punjab" & district == "tran taran"
replace district = "rupnagar" if state == "punjab" & district == "ropar"
replace district = "muktsar" if state == "punjab" & district == "shri muktsar sahib"

replace district = "chittaurgarh" if state == "rajasthan" & district == "chittorgarh"
replace district = "dhaulpur" if state == "rajasthan" & district == "dholpur"
replace district = "ganganagar" if state == "rajasthan" & district == "sri ganganagar"
replace district = "jalor" if state == "rajasthan" & district == "jalore"
replace district = "jhunjhunun" if state == "rajasthan" & district == "jhunjhunu"

replace district = "north district" if state == "sikkim" & district == "north"
replace district = "south district" if state == "sikkim" & district == "south"
replace district = "east district" if state == "sikkim" & district == "east"
replace district = "west district" if state == "sikkim" & district == "west"

replace district = "kancheepuram" if (state == "tamilnadu" & district == "kanchipuram") | (state == "tamilnadu" & district == "chengalpattu")
replace district = "kanniyakumari" if state == "tamilnadu" & district == "kanyakumari"
replace district = "thiruvallur" if state == "tamilnadu" & district == "tiruvallur"
replace district = "tiruchirappalli" if state == "tamilnadu" & district == "trichy"

replace district = "dhalai" if state == "tripura" & district == "dhalai district"
replace district = "north tripura" if state == "tripura" & district == "district north"
replace district = "south tripura" if state == "tripura" & district == "district south"

replace district = "baghpat" if state == "uttar pradesh" & district == "bagpat"
replace district = "kanshiram nagar" if state == "uttar pradesh" & district == "kashiram nagar"
replace district = "kheri" if state == "uttar pradesh" & district == "lakhimpur kheri"
replace district = "mahrajganj" if state == "uttar pradesh" & district == "maharajganj"
replace district = "rae bareli" if state == "uttar pradesh" & district == "raebareli"
replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "sant ravidas nagar"
replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "bhadoi"
replace district = "mahamaya nagar" if state == "uttar pradesh" & district == "hathras"

replace district = "hardwar" if state == "uttarakhand" & district == "haridwar"
replace district = "garhwal" if state == "uttarakhand" & district == "pauri garhwal"

replace district= "barddhaman" if state == "west bengal" & district == "burdwan"
replace district= "darjiling" if state == "west bengal" & district == "darjeeling"
replace district= "paschim medinipur" if state == "west bengal" & district == "medinipur"
replace district= "paschim medinipur" if state == "west bengal" & district == "west medinipur"
replace district= "puruliya" if state == "west bengal" & district == "purulia"

