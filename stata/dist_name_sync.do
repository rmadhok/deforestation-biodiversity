**# States
replace state = "andaman & nicobar islands" if state == "andaman and nicobar"
replace state = "puducherry" if state == "pondicherry"
replace state = "dadra & nagar haveli" if state == "dadar & nagar haveli" | state == "dadar nagar haveli" 
replace state = "daman & diu" if state == "daman and diu"
replace state = "tamilnadu" if state == "tamil nadu"
replace state = "odisha" if state == "orissa"
replace state = "jammu & kashmir" if state == "jammu and kashmir"

**# Districts

* Andhra Pradesh / Telangana
replace state = "andhra pradesh" if state == "telangana"
replace district = "anantapur" if state == "andhra pradesh" & district == "ananthapur"
replace district = "y.s.r" if state == "andhra pradesh" & inlist(district, "kadapa", "cuddapah", "ysr district (kadapa)", "ysr kadapa") 
replace district = "sri potti sriramulu nellore" if state == "andhra pradesh" & district == "nellore"
replace district = "khammam" if state == "andhra pradesh" & inlist(district, "bhadradri kothagudem", "manuguru", "kahmmam")
replace district = "karimnagar" if state == "andhra pradesh" & district == "jagtiyal"
replace district =  "warangal" if state == "andhra pradesh" & district == "jayashankar bhupalapalli"
replace district = "adilabad" if state == "andhra pradesh" & district == "mancherial"
replace district = "adilabad" if state == "andhra pradesh" & district == "nirmal"
replace district = "nizamabad" if state == "andhra pradesh" & district == "komarambheem asifabad"
replace district = "karimnagar" if state == "andhra pradesh" & district == "peddapalli"
replace district = "medak" if state == "andhra pradesh" & district == "sangareddy"
replace district = "rangareddy" if state == "andhra pradesh" & district == "rangareddi"
replace district = "nizamabad" if state == "andhra pradesh" & district == "kamareddy"
replace district = "rangareddy" if state == "andhra pradesh" & district == "medchal"
replace district = "rangareddy" if state == "andhra pradesh" & district == "medchal malkajgiri" // new (TG)
replace district = "mahbubnagar" if state == "andhra pradesh" & district == "wanaparthy" //new (TG)
drop if prop_no == "fp/tg/others/957/2011" // no location

* A & N
replace district = "nicobars" if district == "nicobar"
replace district = "north & middle andaman" if district == "north and middle andaman"

* Arunachal
replace district = "balemu east kameng" if state == "arunachal pradesh" & district == "east kameng"
replace district = "lohit" if state == "arunachal pradesh" & district == "namsai"
replace district = "bhiwani" if state == "arunachal pradesh" & district == "charkhi dadri"
replace district = "kurung kumey" if state == "arunachal pradesh" & district == "kurang kumey"
replace district = "papum pare" if state == "arunachal pradesh" & district == "papumpare"

* Assam
replace district = "tinsukia" if state == "assam" & district == "digboi"
replace district = "karbi anglong" if state == "assam" & district == "diphu"
replace district = "dima hasao" if state == "assam" & district == "north cachar hills"
replace district = "dima hasao" if state == "assam" & district == "haflong"
replace district = "nagaon" if state == "assam" & district == "hojai" // new
replace district = "karbi anglong" if state == "assam" & district == "west karbi anglong" // new
drop if prop_no == "fp/as/trans/19330/2016" // multidistrict 

* Bihar
replace district = "kaimur (bhabua)" if state == "bihar" & district == "kaimur"
replace district = "purba champaran" if state == "bihar" & district == "east champaran"
replace district = "gopalganj" if state == "bihar" & district == "gopalaganj"
replace district = "jamui" if state == "bihar" & district == "jamuai"
replace district = "purba champaran" if state == "bihar" & district == "motihari"
replace district = "nalanda" if state == "bihar" & district == "nalada"
replace district = "purnia" if state == "bihar" & district == "purnea" 
replace district = "sheikhpura" if state == "bihar" & district == "shekpura"
replace district = "sitamarhi" if state == "bihar" & district == "sitamarih"

* Chhattisgarh
replace district = "dakshin bastar dantewada" if state == "chhattisgarh" & inlist(district, "dantewada", "dantewara", "south bastar")
replace district = "kabeerdham" if state == "chhattisgarh" & inlist(district, "kabirdham", "kawardha")
replace district = "uttar bastar kanker" if state == "chhattisgarh" & district == "kanker"
replace district = "surguja" if state == "chhattisgarh" & district == "balrampur" //new
replace district = "durg" if state == "chhattisgarh" & inlist(district, "bemetara", "balod") //new
replace district = "raipur" if state == "chhattisgarh" & district == "gariaband" //new
replace district = "bastar" if state == "chhattisgarh" & inlist(district, "kondagaon", "sukma", "kodagoan") //new
replace district = "surguja" if state == "chhattisgarh" & inlist(district, "surajpur", "sarguja") //new
replace district = "raigarh" if state == "chhattisgarh" & inlist(district, "dharamjaigarh", "raigad")
replace district = "janjgir - champa" if state == "chhattisgarh" & district == "janjgir-champa"

* Dadra & Nagar haveli
replace district = "dadra & nagar haveli" if state == "dadra & nagar haveli" & regexm(district, "haveli")
replace district = "dadra & nagar haveli" if district == "silvassa" //new

* Delhi
replace district = "east" if state == "delhi" & district == "east delhi"
replace district = "west" if state == "delhi" & district == "west delhi"
replace district = "south west" if state == "delhi" & district == "south west delhi"

* Gujarat
replace district = "ahmadabad" if state == "gujarat" & district == "ahmedabad"
replace district = "banas kantha" if state == "gujarat" & inlist(district, "banaskantha", "palanpur")
replace district = "dohad" if state == "gujarat" & district == "dahod"
replace district = "the dangs" if state == "gujarat" & district == "dang"
replace district = "junagadh" if state == "gujarat" & inlist(district, "junagarh", "gir-somnath", "gir somnath", "veraval")
replace district = "kachchh" if state == "gujarat" & inlist(district, "kutch", "kachh")
replace district = "panch mahals" if state == "gujarat" & inlist(district, "panchmahal", "panchamahal")
replace district = "sabar kantha" if state == "gujarat" & inlist(district, "arvalli", "himmatnagar", "sabarkantha") //new
replace district = "vadodara" if state == "gujarat" & district == "chhota udaipur" //new
replace district = "jamnagar" if state == "gujarat" & district == "devbhumi dwarka" //new
replace district = "kheda" if state == "gujarat" & district == "nadiad" // manual
replace district = "patan" if state == "gujarat" & district == "patna" 

* Haryana
replace district = "bhiwani" if state == "haryana" & district == "charkhi dadri"
replace district = "gurgaon" if state == "haryana" & inlist(district, "gurgoan", "gurugram")
replace district = "kurukshetra" if state == "haryana" & district == "kurukshtra"
replace district = "mahendragarh" if state == "haryana" & district == "mahendergarh"
replace district = "mewat" if state == "haryana" & regexm(district, "nuh")
replace district = "panchkula" if state == "haryana" & district == "morni-pinjore" // manual
replace district = "sonipat" if state == "haryana" & district == "sonepat"

* Himachal Pradesh
replace district = "kangra" if state == "himachal pradesh" & district == "dharmshala"
replace district = "lahul & spiti" if state == "himachal pradesh" & district == "lahul and spiti"

* Jharkhand
replace district = "purbi singhbhum" if state == "jharkhand" & inlist(district, "east singhbhum", "bengaluru", "dalbhum")
replace district = "hazaribagh" if state == "jharkhand" & district == "hazaribag"
replace district = "pashchimi singhbhum" if state == "jharkhand" & inlist(district, "west singhbhum", "saranda")
replace district = "saraikela-kharsawan" if state == "jharkhand" & inlist(district, "seraikela", "seraikela kharsawan", "saraikela kharsawan")
replace district = "dhanbad" if state == "jharkhand" & district == "danbhad"

* Karnataka
replace district = "bangalore" if state == "karnataka" & district == "bangalore urban"
replace district = "dakshina kannada" if state == "karnataka" & inlist(district, "mangalore", "dakshina")
replace district = "bellary" if state == "karnataka" & district == "ballari"
replace district = "chikmagalur" if state == "karnataka" & inlist(district, "chickmagalur", "chikkamagaluru")
replace district = "davanagere" if state == "karnataka" & district == "devangere"
replace district = "uttara kannada" if state == "karnataka" & (inlist(district, "hannovar", "karwar", "north kanara") | regexm(district, "uttar kan")) // manual
replace district = "chitradurga" if state == "karnataka" & district == "hosadurga" // manual
replace district = "koppal" if state == "karnataka" & district == "koppala"
replace district = "udupi" if state == "karnataka" & district == "kundapura" // manual
replace district = "ramanagara" if state == "karnataka" & regexm(district, "ramna")
replace district = "mandya" if state == "karnataka" & district == "rangareddi" // manual
replace district = "shimoga" if state == "karnataka" & district == "shivamogga"
replace district = "tumkur" if state == "karnataka" & district == "tamkur"
replace district = "yadgir" if state == "karnataka" & district == "yadagir"

* Kerala
replace district = "kollam" if state == "kerala" & district == "quilon"

* Madhya Pradesh
replace district = "east nimar" if state == "madhya pradesh" & district == "khandwa"
replace district = "west nimar" if state == "madhya pradesh" & inlist(district, "khargaone", "khargone")
replace district = "singrauli" if state == "madhya pradesh" & district == "singrouli"
replace district = "shajapur" if state == "madhya pradesh" & district == "agar malwa"
replace district = "anuppur" if state == "madhya pradesh" & district == "anappur"
replace district = "barwani" if state == "madhya pradesh" & district == "badwani"
replace district = "chhatarpur" if state == "madhya pradesh" & district == "chhattarpur"
replace district = "hoshangabad" if state == "madhya pradesh" & district == "hosangabad"
replace district = "morena" if state == "madhya pradesh" & district == "murena"
replace district = "narsimhapur" if state == "madhya pradesh" & district == "narsinghpur"
replace district = "rajgarh" if state == "madhya pradesh" & district == "rajagarh"
replace district = "shahdol" if state == "madhya pradesh" & district == "sahdol"
replace district = "tikamgarh" if state == "madhya pradesh" & district == "teekamgarh"

* Maharashtra
replace district = "ahmadnagar" if state == "maharashtra" & inlist(district, "ahmednagar", "ahamadnagar")
replace district = "bid" if state == "maharashtra" & district == "beed"
replace district = "palhgar" if state == "maharashtra" & district == "dahanu"
replace district = "gondiya" if state == "maharashtra" & district == "gondia"
replace district = "pune" if state == "maharashtra" & district == "junnar"
replace district = "mumbai suburban" if state == "maharashtra" & district == "mumbai (suburban)"
replace district = "mumbai" if state == "maharashtra" & district == "mumbai city"
replace district = "raigarh" if state == "maharashtra" & district == "raigad"
replace district = "thane" if state == "maharashtra" & district == "palghar" //new
replace district = "amravati" if state == "maharashtra" & district == "amrawati"
replace district = "buldana" if state == "maharashtra" & district == "buldhana"
replace district = "bhandara" if state == "maharashtra" & district == "chandpur"
drop if prop_no == "fp/mh/irrig/681/2009" // spans multiple states -- confusing
replace district = "yavatmal" if state == "maharashtra" & district == "yawatmal"

* Manipur
replace district = "imphal east" if state == "manipur" & district == "manipur(east)"
replace district = "imphal west" if state == "manipur" & district == "manipur(west)"

* Meghalaya
replace district = "jaintia hills" if state == "meghalaya" & district == "west jaintia hills district"
replace district = "east khasi hills" if state == "meghalaya" & district == "east khasi hills district"

* Odisha
replace district = "baleshwar" if state == "odisha" & district == "balasore"
replace district = "baudh" if state == "odisha" & district == "boudh"
replace district = "debagarh" if state == "odisha" & district == "devgarh"
replace district = "jajapur" if state == "odisha" & district == "jajpur"
replace district = "kendujhar" if state == "odisha" & district == "keonjhar"
replace district = "nabarangapur" if state == "odisha" & district == "nabarangpur"
replace district = "subarnapur" if state == "odisha" & district == "sonapur"
replace district = "anugul" if state == "odisha" & district == "angul"
replace district = "jagatsinghapur" if state == "odisha" & district == "jagatsinghpur"
replace district = "jharsuguda" if state == "odisha" & district == "jharsusuda"
replace district = "mayurbhanj" if state == "odisha" & district == "mayurbhanja"
replace district = "sundargarh" if state == "odisha" & district == "sundergarh"

* Puducherry
replace district = "puducherry" if state == "puducherry" & district == "pondicherry"

* Punjab
replace district = "sahibzada ajit singh nagar" if state == "punjab" & district == "sas nagar"
replace district = "shahid bhagat singh nagar" if state == "punjab" & inlist(district, "shahid bhagat singh nagar(nawansheher tahsil)", "nawanshahr", "nawanshahar")
replace district = "tarn taran" if state == "punjab" & regexm(district, "taran")
replace district = "rupnagar" if state == "punjab" & district == "ropar"
replace district = "muktsar" if state == "punjab" & inlist(district, "shri muktsar sahib", "mukhtsar", "muktsar sahib")
replace district = "firozpur" if state == "punjab" & district == "fazilka" //nre
replace district = "gurdaspur" if state == "punjab" & district == "pathankot" //new
replace district = "fatehgarh sahib" if state == "punjab" & inlist(district, "fategarh sahib", "sirhind") // manual

* Rajasthan
replace district = "chittaurgarh" if state == "rajasthan" & district == "chittorgarh"
replace district = "dhaulpur" if state == "rajasthan" & district == "dholpur"
replace district = "ganganagar" if state == "rajasthan" & district == "sri ganganagar"
replace district = "jalor" if state == "rajasthan" & district == "jalore"
replace district = "jhunjhunun" if state == "rajasthan" & district == "jhunjhunu"
drop if prop_no == "fp/rj/road/606/2012" // multidistrict road
replace district = "rajsamand" if state == "rajasthan" & district == "rajasmand"

* Sikkhim
replace district = "north district" if state == "sikkim" & regexm(district, "north")
replace district = "south district" if state == "sikkim" & regexm(district, "south")
replace district = "east district" if state == "sikkim" & regexm(district, "east")
replace district = "west district" if state == "sikkim" & regexm(district, "west")

* Tamil nadu
replace district = "kancheepuram" if state == "tamilnadu" & inlist(district, "kanchipuram","chengalpattu")
replace district = "kanniyakumari" if state == "tamilnadu" & district == "kanyakumari"
replace district = "thiruvallur" if state == "tamilnadu" & district == "tiruvallur"
replace district = "tiruchirappalli" if state == "tamilnadu" & district == "trichy"
replace district = "thoothukkudi" if state == "tamilnadu" & district == "thoothukudi"
replace district = "cuddalore" if state == "tamilnadu" & district == "virudhachalam" // manual

* Tripura
replace district = "dhalai" if state == "tripura" & district == "dhalai district"
replace district = "north tripura" if state == "tripura" & district == "district north"
replace district = "south tripura" if state == "tripura" & district == "district south"
replace district = "south tripura" if state == "tripura" & district == "gomati" // new
replace district = "west tripura" if district == "khowai" | district == "sepahijala" //new
replace district = "north tripura" if state == "tripura" & district == "unakoti" // new

* Uttar Pradesh
replace district = "baghpat" if state == "uttar pradesh" & district == "bagpat"
replace district = "kanshiram nagar" if state == "uttar pradesh" & district == "kashiram nagar"
replace district = "kheri" if state == "uttar pradesh" & district == "lakhimpur kheri"
replace district = "mahrajganj" if state == "uttar pradesh" & district == "maharajganj"
replace district = "rae bareli" if state == "uttar pradesh" & district == "raebareli"
replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "sant ravidas nagar"
replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "bhadoi"
replace district = "mahamaya nagar" if state == "uttar pradesh" & district == "hathras"
replace district = "jyotiba phule nagar" if state == "uttar pradesh" & district == "amroha" //new
replace district = "kanshiram nagar" if state == "uttar pradesh" & district == "kasgang" //new
replace district = "muzaffarnagar" if state == "uttar pradesh" & district == "shamli" //new

* Uttarkhand
replace district = "hardwar" if state == "uttarakhand" & district == "haridwar"
replace district = "garhwal" if state == "uttarakhand" & district == "pauri garhwal"
replace district = "pithoragarh" if state == "uttarakhand" & district == "pithorgarh"
replace district = "udham singh nagar" if state == "uttarakhand" & district == "udhamsingh nagar"

* West Bengal
replace district= "barddhaman" if state == "west bengal" & inlist(district, "burdwan", "bardhaman")
replace district= "darjiling" if state == "west bengal" & inlist(district, "darjeeling", "kalimpong") // manual
replace district= "paschim medinipur" if state == "west bengal" & district == "medinipur"
replace district= "paschim medinipur" if state == "west bengal" & district == "west medinipur"
replace district= "puruliya" if state == "west bengal" & district == "purulia"
replace district= "hugli" if state == "west bengal" & district == "hooghly" 
replace district= "north twenty four parganas" if district == "24 parganas"
replace district= "haora" if state == "west bengal" & district == "howrah"
replace district = "koch bihar" if state == "west bengal" & district == "cooch behar"
