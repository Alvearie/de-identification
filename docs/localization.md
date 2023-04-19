# Localization

The Data De-Identification service internally loads various CSV files to help with data masking or field type identification. The application comes with default values for the United States of America. If needed, you can change these values to meet the requirements of any country, region, or language. To this, the string data must conform to UTF-8 encoding standards. Follow the instructions in this topic.

### Masking providers

Several masking providers internally load CSV files. For example, to help identify or mask names, the **name** masking provider loads CSV files of male and female first names and last names. You can update CSV files to mask names in a different language and for a different region. When the Data De-Identification job first starts, the localization data loads once in memory.

Table: Masking providers and types of CSV files

| **Masking provider**   | CSV file types                     | **Description**                                                                    |
|------------------------|----------------------------------------|------------------------------------------------------------------------------------------------|
| ADDRESS                | postal_codes/country/city/street_names | ADDRESS masking provider that uses four different CSV files: postal_codes/country/city/street_names |
| ATC                    | atc_codes                              | CSV file that contains Anatomical Therapeutic Chemical (ATC) codes                           |
| CITY                   | city                          | CSV file that contains a list of cities                                                                   |
| CONTINENT              | city / country / continent    | CONTINENT masking provider that uses three different CSV files: city/country/continent                       |
| COUNTRY                | city / country                | COUNTRY masking provider that uses two different CSV files: city/country                                     |
| COUNTY                 | county                        | CSV file that contains a list of counties                                                                 |
| GENDER                 | gender                        | CSV file that contains genders                                                                          |
| HOSPITAL               | hospital_names                | CSV file that contains hospital names                                                                   |
| ICDV9                  | icdv9                         | CSV file that contains International Classification of Diseases (ICD) version 9                          |
| ICDV10                 | icdv9                         | CSV file that contains ICD version 10                         |
| MARITAL                | marital_status                | CSV file that contains marital statuses                                                                 |
| NAME                   | first_name_female/first_name_male/last_name | NAME masking provider that uses three different CSV files: first_name_female, first_name_male,last_name |
| OCCUPATION             | occupation                    | CSV file that contains a list of occupations                                                              |
| STATES                 | state                         | CSV file that contains a list of US states                                                                 |
| SWIFT                  | swift                         | CSV file that contains SWIFT codes                                                                      |
| ZIPCODE                | zip_code                      | CSV file that contains postal (ZIP) codes and their population

The CSV files are stored in ipv-core/src/main/resources/identifier. Different language files are stored in different directories.  For example, English language files are stored in the `en` directory, and Greek language files are in the `gr`directory.  Common files are in the `common` directory. In addition to supporting different languages, the Data De-identification Service supports different countries with the same language. United States files are in the `us` directory and United Kingdom files are in the `uk` directory. By default, the service only loads English and US files. To configure which countries load, change the settings in ipv-core/src/main/resources/localization.properties. Here is the format of localization.properties:
  country: Specifies which countries should be loaded
  <country>: Which language the country uses.  Example: `us = en`
  _common.<MASKING_PROVIDER>: Specifies which CSV file to use for the masking provider, which is common to all languages.  Example: `_common.ATC_CODES = /identifier/common/atc.csv`
  <lang>.<MASKING_PROVIDER>: Specifies which CSV file to use for the masking provider for a particular language. Example:  `en.CITY = /identifier/en/city_list.csv`

For an example of configuring multiple languages, see ipv-core/src/main/resources/localization-multi-lang.properties.

List of the files included:

```
├── common
│   ├── atc.csv
│   ├── generalize_categories.csv
│   ├── phone_country_codes.csv
│   ├── phone_number_digits.csv
│   ├── public_suffix_list.dat
│   └── tacdb.txt
├── en
│   ├── animals.csv
│   ├── city_list.csv
│   ├── continents.csv
│   ├── country_list.csv
│   ├── credit_card_types.csv
│   ├── days.csv
│   ├── dependent.csv
│   ├── genders.csv
│   ├── icd_list.csv
│   ├── icd_v10.csv
│   ├── marital_status.csv
│   ├── medicines.csv
│   ├── months.csv
│   ├── occupations.csv
│   ├── phone_country_calling_codes.csv
│   ├── races.csv
│   ├── religions.csv
│   ├── states_us.csv
│   ├── street_types.csv
│   ├── swift_codes.csv
│   └── vin_wmi.csv
├── gr
│   ├── city_list.csv
│   ├── continents.csv
│   ├── country_list.csv
│   ├── female_names.csv
│   ├── hospital_names.csv
│   ├── last_names.csv
│   ├── male_names.csv
│   ├── marital_status.csv
│   ├── medicines.csv
│   ├── occupations.csv
│   ├── races.csv
│   └── religions.csv
├── uk
│   ├── last_names.csv
│   └── ssnuk_prefixes.csv
└── us
    ├── counties.csv
    ├── female_names.csv
    ├── hospital_names.csv
    ├── last_names.csv
    ├── locale.properties
    ├── male_names.csv
    ├── phone_area_codes.csv
    ├── postal_codes.csv
    ├── street_names.csv
    └── zcta.csv
```

The following describes the different types of CSV files and their respective schemas as expected by the Data De-Identification service. If the information is not available for optional columns, leave the fields empty so that the total number of columns is the same.

### atc_codes
File name: atc.csv

Contains a list of Anatomical Therapeutic Chemical codes.

Mandatory column:  ATC code

Example of a CSV file:
```
A01AA01
A01AA02
A01AA03
```

### city
File name: city_list.csv

Contains a list of cities and related information.

Mandatory column: name

Optional columns: latitude, longitude, country code, population

If latitude and longitude are not provided, the Data De-Identification service cannot mask those cities with their closest neighboring cities.  If such a city is provided as input and the mask closest option is configured, the configured handling of unexpected input is applied.  Also, such cities are not considered when finding the closest neighbors of cities for which latitude and longitude have been provided.

Example of a CSV file:
```
A Coruña,43.37135,-8.396,ES,246056
Aachen,,,,
Aba,5.10658,7.36667,NG,897560
```

### continent
File name: continents.csv

Contains a list of continents and related information.

Mandatory column: name

Optional columns: latitude, longitude

If latitude and longitude are not provided, the Data De-Identification service cannot mask those continents with their closest neighboring continents.  If such a continents is provided as input and the mask closest option is configured, the configured handling of unexpected input is applied.  Also, such continents are not considered when finding the closest neighbors of continents for which latitude and longitude have been provided.


Example of a CSV file:
```
Asia,38.5208512,97.5929063
Europe,49.3613555,13.7208743
South America,,
```

### country
File name: country_list.csv

Contains a list of countries and related information.

Mandatory column: name

Optional columns: ISO 2-Letter code, ISO 3-Letter code, alias, continent, latitude, longitude

If latitude and longitude are not provided, the Data De-Identification service cannot mask those countries with their closest neighboring countries.  If such a country is provided as input and the mask closest option is configured, the configured handling of unexpected input is applied.  Also, such countries are not considered when finding the closest neighbors of countries for which latitude and longitude have been provided.

Example of a CSV file:
```
Afghanistan,AF,AFG,,Asia,34.533,69.133
Albania,,,,,,
Algeria,DZ,DZA,,Africa,28,2
United States of America,US,USA,United States,North America,38.883,-77.017
```

### county
File name: counties.csv

Contains a list of US counties and related information.

Mandatory columns: name, short name

Optional columns: state, population

Example of a CSV file:
```
Autauga County,Autauga,,55246
Baldwin County,Baldwin,Alabama,
Snyder County,Snyder,Pennsylvania,39865
```

### first_name_female
File name: female_names.csv

Contains a list of female names.

Mandatory column: name

Example of a CSV file:
```
Alecia
Trisha
Veronica
```

### first_name_male
File name: male_names.csv

Contains a list of male names.

Mandatory column: name

Example of a CSV file:
```
Blair
Leroy
Zachery
```

### gender
File name: genders.csv

Contains a list of genders.

Mandatory column: name

Example of a CSV file:
```
Male
Female
```

### hospital_names
File name: hospital_names.csv

Contains a list of hospital names and the country in which the hospital resides.

Mandatory column: name, country code

Example of a CSV file:
```
Bryce Hospital,US
Shoals Hospital,US
Eastern Health System,US
```

### icdv10
File name: icd_v10.csv

Contains a list of Internal Classification of Diseases (ICD) version 10 codes.

Mandatory columns: code, full name, category code, catgory name, chapter code, chapter name

Example of a CSV file (contrived sample data):
```
A09.3,Meaning of A09.3,A00-A09,A condition,A00-B99,Certain conditions
A09.4,"Meaning of A09.4, which might include a comma",A00-A09,Another condition,A00-B99,Certain other conditions
A09.5,"Meaning of A09.5, which is a contrived code, used as an example",A00-A09,condition category name,A00-B99,condition chapter name
```

### icdv9
File name: icd_list.csv

Contains a list of ICD version 9 codes.

Mandatory columns: code, short name, full name, chapter code, chapter name, category code, category name

Example of a CSV file:
```
006.3;Amebic Liver Abscess;Amebic Liver Abscess;001-139;infectious and parasitic diseases;006;Amebiasis
E838.0;Watercraft Acc Nec-Unpow;Other And Unspecified Water Transport Accident Injuring Occupant Of Small Boat, Unpowered;E830-E839;Water transport accidents;E838;Other and unspecified water transport accident
V89.04;Sus Fetal Growth Not Fnd;Suspected Problem With Fetal Growth Not Found;V89-V89;Other Suspected Conditions Not Found;V89;Other suspected conditions not found
```

### last_name
File name: last_names.csv

Contains a list of last names.

Mandatory column: name

Optional column: number of people with the last name

Example of a CSV file:
```
THIBOUTOT,
CARPEN,12566
ARAVJO,
```

### marital_status
File name: marital_status.csv

Contains a list of marital statuses.

Mandatory column: status

Example of a CSV file:
```
Single
Married
Divorced
```

### occupation
File name: occupations.csv

Contains a list of occupations and a category of occupations to which that occupation belongs.  If an occupation is in more than one category, an additional record for that occupation with each additional category can be entered.

Mandatory columns: occupation name, occupation category

Example of a CSV file:
```
"Accountant, certified",Chartered and certified accountants
"Accountant, certified","Book-keepers, payroll managers and wages clerks"
"Engineer, MWD",Civil engineers
"Technician, limb, artificial",Medical and dental technicians
```

### phone_area_codes
File name: phone_area_codes.csv

Contains a list of area codes and their countries.

Mandatory columns: country code, area code

Optional columns: state

Example of a CSV file:
```
USA,205,
USA,601,
USA,701,North Dakota
```

### phone_calling_codes
File name: phone_country_calling_codes

Contains a list of country codes.

Mandatory columns: country code, country name

Example of a CSV file:
```
54,Argentina
358,Finland
962,Jordan
```

### phone_num_digits
File name: phone_number_digits.csv

Contains a list of country codes and the number of telephone number digits.

Mandatory columns: country code, phone number digits

Note: Specify telephone number digits in these formats:
  - Single-digit format
    Example:  9

  - Range, formatted using two digits separated by a dash (-)
    Example:  7-9
    which means 7, 8, or 9 digits

  - Multiple digits separated by a vertical bar (|)
    Example:  7|9
    which means 7 or 9 digits


Example of a CSV file:
```
54,10
358,5-12
962,8|9
```

### postal_codes
File name: postal_codes.csv

Contains a list of country code, postal code, latitude, and longitude.

Mandatory columns: country code, postal code

Optional columns: latitude, longitude

If latitude and longitude are not provided, the Data De-Identification service cannot mask those postal codes with their closest neighboring postal codes.  If such a postal code is provided as input and the mask closest option is configured, the configured handling of unexpected input is applied.  Also, such postal codes are not considered when finding the closest neighbors of postal codes for which latitude and longitude have been provided.

Example of a CSV file:
```
US,44871,41.4918,-82.6478
US,26348, ,-80.5258
US,82637,42.7803,
```

### race_ethnicity
File name: races.csv

Contains a list of races or ethnicities.

Mandatory columns: race or ethnicity

Example of a CSV file:
```
Alaska Native
Black
African American
```

### religion
File name: religions.csv

Contains a list of religions.

Mandatory columns: religion

Example of a CSV file:
```
Christian
Muslim
Hindu
```

### states
File name: states_us.csv

Contains a list of US states.

Mandatory columns: name, abbreviation

Example of a CSV file:
```
Alabama,AL
Alaska,AK
Arizona,AZ
```

### street_names
File name: street_names.csv

Contains a list of street names.

Mandatory columns: street name

Example of a CSV file:
```
Main Street East
Valley View
Woodland
```

### swift
File name: swift_codes.csv

Contains a list of SWIFT codes.

Mandatory columns: code

Example of a CSV file (contrived sample data):
```
AAAAUSAA
BBBBUSBB
CCCCUSCC
```

### world_manufacturers_identifier
File name: vin_wmi.csv

Contains a list of World Manufacturer Identifier (WMI) numbers and manufacturers.

Mandatory columns: WMI, manufacturer

Example of a CSV file:
```
1N4,Nissan
2FA,Ford
4US,BMW
```

### zipcode
File name: zcta.csv

Contains a list of postal (ZIP) codes and their respective populations.

Mandatory column: zip code, population

Example of a CSV file:
```
00612,67010
59024,390
99901,13508
```
