# Masking Providers
## Overview

> To preserve utility in the data being de-identified, the Data de-identification service supports the following data protection methods:

```
   ADDRESS 
   ATC
   BINNING
   CITY
   CONDITIONAL
   CONTINENT
   COUNTRY
   COUNTY
   CREDIT_CARD
   DATEDEPENDENCY   
   DATETIME
   EMAIL
   GENDER
   GENERALIZE
   GUID
   HASH
   HOSPITAL
   IBAN
   ICDV9
   ICDV10
   IMEI
   IP_ADDRESS
   LATITUDE_LONGITUDE
   MAC_ADDRESS
   MAINTAIN
   MARITAL
   NAME
   NULL
   NUMBERVARIANCE
   OCCUPATION
   PHONE
   PSEUDONYM
   RACE
   RANDOM
   REDACT
   RELIGION
   REPLACE
   SSN_UK
   SSN_US
   STATE_US
   SWIFT
   URL
   VIN
   ZIPCODE
```

## List of Data Protection Methods with Configuration Options and Defaults

The following sections provide detailed information about the various 
field-level data protection methods that are currently supported by the Data
De-Identification Service, their available configuration (utility-preserving)
options, and their default values.

#### ADDRESS

>   Masks an input postal address with a random one. Various elements of the
>   address can be preserved, like street names and road types.

| **Option name**             | **Type** | **Description**                               | **Default value** |
|-----------------------------|----------|-----------------------------------------------|-------------------|
| postalCodeNearest           | Boolean  | Select nearest postal code                    | false             |
| roadTypeMask                | Boolean  | Mask road type (street, avenue, etc)          | true              |
| postalCodeNearestK          | Integer  | Number of closest postal codes to select from | 10                |
| countryMask                 | Boolean  | Mask country                                  | true              |
| postalCodeMask              | Boolean  | Mask postal code                              | true              |
| numberMask                  | Boolean  | Mask number                                   | true              |
| cityMask                    | Boolean  | Mask city                                     | true              |
| maskPseudorandom            | Boolean  | Mask based on pseudorandom function           | false             |
| streetNameMask              | Boolean  | Mask street name                              | true              |

#### ATC

>   Masks an ATC code with the option to preserve certain levels.

| **Option name**       | **Type** | **Description**          | **Default value** |
|-----------------------|----------|--------------------------|-------------------|
|  maskLevelsToKeep     | Integer  | Number of levels to keep | 4                 |

#### BINNING

>   Replaces a numerical input value with an interval that contains the value. The interval's width is configurable but is constant across the domain of values.  The range
>   is inclusive on the lower bound and exclusive on the upper bound.  See the javadoc for java.util.Formatter to see examples of valid formats.  

| **Option name**       | **Type** | **Description**                         | **Default value** |
|-----------------------|----------|-----------------------------------------|-------------------|
| binSize               | Integer  | The range of the interval               | 5                 |
| format                | String   | The format of the replacement range     | %s-%s             |
| startValue            | Integer  | The lower bound of the range            | 0                 |
| useStartValue         | Boolean  | Use the startValue as the lower bound   | false             |

#### CITY

>   Masks a city with a randomly selected city, or based on one of its
>   neighboring cities (geographical distance).

| **Option name**        | **Type** | **Description**                         | **Default value** |
|------------------------|----------|-----------------------------------------|-------------------|
| maskClosest            | Boolean  | Select one of the near cities           | false             |
| maskClosestK           | Integer  | Number of closest cities to select from | 10                |
| maskPseudorandom       | Boolean  | Mask based on pseudorandom function     | false             |

#### CONDITIONAL

   The Data De-Identification Service supports the *conditional* masking of a
   data element, based on the value of another data element that appears in the
   same FHIR Resource. That is, the masking method performed on a data element
   A (target field), depends on whether another data element B (condition
   field) in the same Resource satisfies a specified condition.

   The conditional masking provider supports a single configuration option,
   conditional.mask.ruleSet, which contains a JSON array consisting of a set of
   one or more masking providers, along with their associated conditions, as
   shown in the example below.

   **The array conditions are processed in the order that they are listed. Data
   element A is masked by the first data masking provider (and its associated
   configuration options) that its condition is met.**

   **Example 1 – Conditional masking of a FHIR data element based on another
   FHIR data element with regular path:**

```
    {
      "rules": [
        {
          "name": "CONDITIONAL_RULE",
          "maskingProviders": [
            {
              "type": "CONDITIONAL",
              "unspecifiedValueHandling": 0,
              "unspecifiedValueReturnMessage": "OTHER",
              "maskRuleSet": [
                {
                  "condition": {
                    "field": "model",
                    "operator": "equalsIgnoreCase",
                    "type": "string",
                    "value": "example_model"
                  },
                  "maskingProvider": {
                    "type": "PSEUDONYM",
                    "generateViaOptionsMinLength": 12,
                    "generateViaOptionsMaxLength": 12
                  }
                },
                {
                  "maskingProvider": {
                    "type": "HASH"
                  }
                }
              ]
            }
          ]
        }
      ],
      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [
          "Device"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Device/version",
            "rule": "CONDITIONAL_RULE"
          }
        ]
      }
    }
```
    
   Example 1 contains a CONDITIONAL ruleset for a condition field with a
   regular path (i.e., not a FHIR array data element).

   The objective of the CONDITIONAL ruleset in the provided example is to mask
   the version data element of the Device FHIR Resource using the PSEUDONYM
   masking provider, if the model data element of the Device FHIR Resource has
   value “example_model”; otherwise, the default HASH masking provider will be
   used. By construction, if none of the conditions specified in the ruleset
   are met, the specified default option will be used.

   Each array node of the conditional.mask.ruleSet JSON contains a data masking
   provider along with its optional configuration parameters, and the condition
   properties that must be satisfied in order for this provider to be applied
   (e.g., the PSEUDONYM masking provider in Example 1, along with the options
   “pseudonym.generateViaOptions.minLength = 12” and
   “pseudonym.generateViaOptions.maxLength = 12”, will be applied to mask the
   version data element only when the model data element has value
   “example_model”). The first condition that is met leads to the corresponding
   data masking provider to be applied. Optionally, a default data masking
   provider can be used, if none of the conditions of the previous rules in the
   conditional ruleset are met. The default masking provider should be placed
   last in the list. Please note that if a default data masking provider
   (without condition) is not provided and if none of the conditions of the
   specified masking providers (with conditions) are met, then the data value
   will not be masked (it will be maintained as-is).

>   The configured CONDITIONAL JSON element consists of four properties:

>-   “field” – contains the path of the condition field in the FHIR Resource
    type. The path may be a regular path (i.e., a non-array element path) or an
    array query format path as described in: 
    https://test.cloud.ibm.com/docs/services/data-de-identification?topic=data-de-identification-introduction-to-the-data-de-identification-service-configuration

>-   “value” – contains the expected value of the condition field.

>-   “type” – contains the data type of the condition field. Currently, only
    String data elements are supported.

>-   “operator” – the comparison operator to be used between the actual value of
    the condition field and the user-requested value. Currently, four String
    comparison operators are supported in CONDITIONAL: equals, equalsIgnoreCase,
    contains, and contained_in.

>   Operator equals checks if the actual value is the same as the user-requested
>   value. Operator equalsIgnoreCase performs the same check, but ignores the case
>   (capitalized vs. non-capitalized letters). The contains operator checks if the 
>   value of the condition field contains
>   (as a subset) the user-specified value. Last, the contained_in operator
>   checks if the value of the condition field is contained in the user
>   specified-value.

>   The “unspecifiedValueHandling” and “unspecifiedValueReturnMessage” properties are documented 
>   in the “Handling of Unrecognized Input Values and of Exceptions Raised by the Privacy Providers” section below.

   **Example 2 – Conditional masking of a FHIR data element based on another
   FHIR data element value in an array node:**

```
    {
      "rules": [
        {
          "name": "CONDITIONAL_RULE",
          "maskingProviders": [
            {
              "type": "CONDITIONAL",
              "maskRuleSet": [
                {
                  "condition": {
                    "field": "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)",
                    "operator": "equals",
                    "type": "string",
                    "value": "Asthma-Inhaler"
                  },
                  "maskingProvider": {
                    "type": "PSEUDONYM",
                    "generateViaOptionsMinLength": 15,
                    "generateViaOptionsMaxLength": 15
                  }
                }
              ]
            }
          ]
        }
      ],
      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [
          "Device"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Device/status",
            "rule": "CONDITIONAL_RULE"
          }
        ]
      }
    }
```
    
   Example 2 contains a CONDITIONAL ruleset for a condition field with an
   array node path. The objective of the CONDITIONAL ruleset in this example is
   to mask the status data element of the Device FHIR Resource using the
   PSEUDONYM masking provider, only if the Device Resource contains an
   extension array node with a url value of
   <http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName>
   and a valueString value “Asthma_Inhaler”; otherwise, the original value of
   the status data element will be maintained as-is.

#### CONTINENT

>   Masks a continent by replacing it with a randomly selected, or with the
>   closest continent. The distance computation is based on an approximate
>   centroid of the continent's bounding box.

| **Option name**         | **Type** | **Description**                            | **Default value** |
|-------------------------|----------|--------------------------------------------|-------------------|
| maskClosest             | Boolean  | Select one of the nearest continents       | false             |
| maskClosestK            | Integer  | Number of neighbors for nearest continents | 5                 |
 
#### COUNTRY

>   Replaces a country with a randomly chosen country, or with its nearest one
>   (calculated based on geographic distance).

| **Option name**           | **Type** | **Description**                            | **Default value** |
|---------------------------|----------|--------------------------------------------|-------------------|
| maskClosestK              | Integer  | Number of nearest countries to select from | 10                |
| maskClosest               | Boolean  | Select one of the near countries           | false             |
| maskPseudorandom          | Boolean  | Mask based on pseudorandom function        | false             |

#### COUNTY

>   Masks a county by replacing it with a random county.

| **Option name**          | **Type** | **Description**                     | **Default value** |
|--------------------------|----------|-------------------------------------|-------------------|
| maskPseudorandom         | Boolean  | Mask based on pseudorandom function | false             |

#### CREDIT_CARD

>   Masks a credit card number with the option to preserve the issuer (VISA,
>   Mastercard, AMEX, Discover, etc.)

| **Option name**            | **Type** | **Description** | **Default value** |
|----------------------------|----------|-----------------|-------------------|
| issuerPreserve             | Boolean  | Preserve issuer | true              |

#### DATEDEPENDENCY

>   Extracts two related dates (e.g., the date-of-birth and the date-of-death of
>   a patient) from the same input message: (i) the “mask date”, which is the
>   date to be masked, and (ii) the “compare date”, which is the date that the
>   mask date is compared against.  It then removes the year from the "mask date"
>   is a number of days comparison with the "compare date" is true.

   The following options are supported by this privacy provider:

| **Option name**                                 | **Type** | **Description**                                                                                    | **Default value** |
|-------------------------------------------------|----------|----------------------------------------------------------------------------------------------------|-------------------|
| datetimeYearDeleteNIntervalMaskDate             | String   | The FHIR element name of the date that will be masked.                                             | null              |
| datetimeYearDeleteNIntervalCompareDate          | String   | The FHIR element name of the date that will be compared with the mask date.                        | null              |
| dateYearDeleteNDaysValue                        | Integer  | This option must be set to the maximum allowable interval (number of days) between the two dates.  | 365               |

   In what follows, we provide an example to illustrate the use of the
   DATEDEPENDENCY provider. Specifically, we consider the Patient FHIR
   Resource and the birthDate and deceaseDateTime FHIR data elements that it
   contains.

   The rule that we want to apply regards deleting from the birthDate of a
   patient the year of birth, for those patients who died when they were below
   5 years old. For this example, the interval between the birthDate and the
   deceaseDateTime elements is calculated, and if it is found to be less than 5
   years (≅ 1825 days), then the year is removed from the birthDate element. We
   note that the values shown below are for demonstration purposes only:

**Example 3 – Protecting the year of birth for patients who died at an age below
5 years old:**

````
    {
      "rules": [
        {
          "name": "DateDependencyRule",
          "maskingProviders": [
            {
              "type": "DATEDEPENDENCY",
              "datetimeYearDeleteNIntervalMaskDate": "birthDate",
              "dateYearDeleteNDaysValue": 1825,
              "datetimeYearDeleteNIntervalCompareDate": "deceasedDateTime"
            }
          ]
        }
      ],

      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [
          "Device"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Patient/birthDate",
            "rule": "DateDependencyRule"
          }
        ]
      }
    }
```

#### DATETIME

>   Masks datetime (timestamp) objects. There are several options supported,
>   such as shifting dates, generalizing to month, generalizing to year etc. or
>   adding random offsets to the various datetime elements (years, months, days,
>   hours, seconds etc.). If multiple options are set to true in the datetime
>   masking algorithm, the following order is respected (examples are for 10th
>   of January 2016):

> 1.  Override with default or specified value (like “90+” or “Over 90 y.o.”).

> 2.  Shift date by constant amount.

> 3.  Generalize to week number/year (like 02/2016).

> 4.  Generalize to month/year (like 01/2016).

> 5.  Generalize to quarter year (like 01/2016).

> 6.  Generalize to year (2016).

> 7.  Generalize to N-year interval (like 2015-2019).

> 8.  Generalize to year (e.g., 1927) and mask any age over 90.

> 9.  Generalize to month/year (e.g., 02/1927) and mask any age over 90

> 10. Add random offsets to year, month, day, hour, minutes, seconds.

> 11. Apply maximum years ago.

> 12. Apply maximum days ago.

   If the override option of the provider is set to True, then the override
   will be processed first and if the rule criteria is met, all other options
   of the DATETIME provider will be ignored.


   The following date formats are supported by the DATETIME masking provider
   (using option datetime.format.fixed):

| **Supported date / datetime format** | **Example of recognized input value** |
|--------------------------------------|---------------------------------------|
| yyyy-mm-ddThh:mm:ss+&#124;-(hh:mm&#124;Z)    | 2008-09-15T15:53:00Z (default)        |
| dd-MM-yyyy                           | 24-12-2018                            |
| dd-MMM-yyyy                          | 24-DEC-2018                           |
| yyyy-MM-dd                           | 2018-12-24                            |
| dd/MM/yyyy                           | 24/12/2018                            |
| yyyy/MM/dd                           | 2018/12/24                            |
| dd-MM-yyyy[ HH:mm:ss ]               | 24-12-2018 12:01:12                   |
| yyyy-MM-dd[ HH:mm:ss ]               | 2018-12-24 12:01:12                   |
| dd/MM/yyyy[ HH:mm:ss ]               | 24/12/2018 12:01:12                   |
| yyyy/MM/dd[ HH:mm:ss ]               | 2018/12/24 12:01:12                   |

   The following configuration options are supported by the DATETIME masking
   provider.  
   
   Options to change the recognized date and time formats:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| formatFixed                                       | String   | Datetime format                                                                                                                                                                                                                                                  | null              |

   Options to return a fixed value if the year is a given number of years ago:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| overrideMask                                      | Boolean  | Mask the datetime value with a fixed value if the datetime is from a year that is a given number of years ago or more.                                                                                                                                                      | false             |
| overrideYearsPassed                               | Integer  | Number of years between the year of the original value and current year for the fixed value to be returned                                                                                                                                                                                                                               | 0                 |
| overrideValue                                     | String   | (optional) Value returned if the date and time meets the number of years threshold.  If not specified, the overrideYearsPassed value followed by the plus sign ('+') is returned (e.g., 90+).  | null              |
 
   Options to shift by a constant amount:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| maskShiftDate                                     | Boolean  | Shift by a constant amount                                                                                                                                                                                                                                  | false             |
| maskShiftSeconds                                  | Integer  | Number of seconds to shift                                                                                                                                                                                                                                          | 0                 |

   Options to generalize a date and time:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| generalizeWeekyear                                | Boolean  | Generalize to week/year                                                                                                                                                                                                                                          | false             |
| generalizeMonthyear                               | Boolean  | Generalize to mm/year                                                                                                                                                                                                                                            | false             |
| generalizeQuarteryear                             | Boolean  | Generalize to quarter/year                                                                                                                                                                                                                                       | false             |
| generalizeYear                                    | Boolean  | Generalize to year                                                                                                                                                                                                                                               | false             |
| generalizeNyearinterval                           | Boolean  | Generalize to n-year interval                                                                                                                                                                                                                                    | false             |
| generalizeNyearintervalvalue                      | Integer  | Value of for n-year interval generalization                                                                                                                                                                                                                      | 0                 |
| generalizeNyearintervalstart                      | Integer  | Starting year for n-year interval generalization                                                                                                                                                                                                                 | 0                 |
| generalizeNyearintervalend                        | Integer  | Ending year for n-year interval generalization                                                                                                                                                                                                                   | null              |
| generalizeYearMaskAgeOver90                       | Boolean  | Generalize to year and mask any age of \>= 90 years old, by updating the date-of-birth to reflect 90 years from the current system date.                                                                                                                         | false             |
| generalizeMonthyearMaskAgeOver90                  | Boolean  | Generalize to month/year and mask any age \>= 90 by updating the date-of-birth to reflect 90 years from the current system date.                                                                                                                                 | false             |

   Options to mask a year:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearMask                                          | Boolean  | Mask year                                                                                                                                                                                                                                                        | true              |
| yearRangeDown                                     | Integer  | Mask year range downwards                                                                                                                                                                                                                                        | 10                |
| yearRangeUp                                       | Integer  | Mask year range upwards                                                                                                                                                                                                                                          | 0                 |

   Options to mask a month:

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| monthMask                                         | Boolean  | Mask month                                                                                                                                                                                                                                                       | true              |
| monthRangeDown                                    | Integer  | Mask month range downwards                                                                                                                                                                                                                                       | 12                |
| monthRangeUp                                      | Integer  | Mask month range upwards                                                                                                                                                                                                                                         | 0                 |

   Options to mask a day:  
	 
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| dayMask                                           | Boolean  | Mask day                                                                                                                                                                                                                                                         | true              |
| dayRangeDownMin                                   | Integer  | Mask day range downwards minimum                                                                                                                                                                                                                                 | 0                 |
| dayRangeDown                                      | Integer  | Mask day range downwards maximum                                                                                                                                                                                                                                 | 7                 |
| dayRangeUpMin                                     | Integer  | Mask day range upwards minimum                                                                                                                                                                                                                                   | 0                 |
| dayRangeUp                                        | Integer  | Mask day range upwards maximum                                                                                                                                                                                                                                   | 0                 |
   
   Options to mask an hour:
	
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| hourMask                                          | Boolean  | Mask hour                                                                                                                                                                                                                                                        | true              |
| hourRangeDown                                     | Integer  | Mask hour range downwards                                                                                                                                                                                                                                        | 100               |
| hourRangeUp                                       | Integer  | Mask hour range upwards                                                                                                                                                                                                                                          | 0                 |

   Options to mask a minute:
	
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| minutesMask                                       | Boolean  | Mask minutes                                                                                                                                                                                                                                                     | true              |
| minutesRangeDown                                  | Integer  | Mask minutes range downwards                                                                                                                                                                                                                                     | 100               |
| minutesRangeUp                                    | Integer  | Mask minutes range upwards                                                                                                                                                                                                                                       | 0                 |

   Options to mask a second:
	
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| secondsMask                                       | Boolean  | Mask seconds                                                                                                                                                                                                                                                     | true              |
| secondsRangeDown                                  | Integer  | Mask seconds range downwards                                                                                                                                                                                                                                     | 100               |
| secondsRangeUp                                    | Integer  | Mask seconds range upwards                                                                                                                                                                                                                                       | 0                 |

   Options to shift the year if at a certain age:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearMaxYearsAgoMask                               | Boolean  | Mask year if it exceeds the maximum years ago from current year                                                                                                                                                                                                  | false             |
| yearMaxYearsAgo                                   | Integer  | Maximum years ago from current year                                                                                                                                                                                                                              | 0                 |
| yearShiftFromCurrentYear                          | Integer  | Years to shift current year backwards                                                                                                                                                                                                                            | 0                 |
| dayMaxDaysAgoMask                                 | Boolean  | Mask year if it exceeds the maximum days ago from current day                                                                                                                                                                                                    | false             |
| dayMaxDaysAgo                                     | Integer  | Maximum days ago from current day                                                                                                                                                                                                                                | 0                 |
| dayShiftFromCurrentDay                            | Integer  | Days to shift current date backwards                                                                                                                                                                                                                             | 0                 |
| yearMaxYearsAgoOnlyYear                           | Boolean  | Return only the shifted year value, not including month/day, if shift occurs                                                                                                                                                                                                    | false             |

   Options to delete the year:
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearDelete                                        | Boolean  | Remove the year and return only the day and month                                                                                                                                                                                                  | false             |
| yearDeleteNdays                                   | Boolean  | Remove the year and return only the day and month if date and time is after the given number of days ago                                                                                                                                                                                                                            | false             |
| yearDeleteNinterval                               | Boolean  | Remove the year and return only the day and month if date and time is within the given number of days from a given date and time                                                                                                                                                                                                                            | false             |
| yearDeleteNdaysValue                              | Integer  | The number of days ago                                                                                                                                                                                                  | 365               |
| yearDeleteNointervalComparedateValue              | Integer  | The date and time to compare                                                                                                                                                                                                                                | 0                 |


#### EMAIL

>   Masks e-mail addresses with the option to preserve certain levels of the
>   host domain.

| **Option name**        | **Type** | **Description**                                                                                             | **Default value** |
|------------------------|----------|-------------------------------------------------------------------------------------------------------------|-------------------|
| preserveDomains | Integer  | Number of domains to preserve starting from the right; if value = -1 then all domains will be preserved (i.e., everything after \@) | 1                 |
| nameLength      | Integer  | Length of username to generate; if value = -1, then the username will be 5 to 8 characters long. 0 is invalid | \-1               |

#### GENDER

>   Replaces a gender information with a randomly chosen gender. This provider
>   does not have any configuration options.

#### GENERALIZE

>   Replaces one or more specified original values with a specified general
>   category term to which these values belong. If a general category term is
>   not defined for the input values, these can be optionally replaced with a
>   *default* term which is configurable by the data owner.
>   The aim of GENERALIZE is to protect infrequent data values that appear in a
>   data element by replacing them with a general category term.

>   When a small number of values must be replaced with a general category term,
>   the IBM onboarding team can specify these sets of values and their
>   corresponding replacement values as rules of the form R: {*o*1, *o*2, …,
>   *o*n} *r*, where *o*i is an original data value and *r* is the replacement
>   value. Multiple such rules can be specified and provided as input to
>   GENERALIZE. For example, in a data element that stores the religion of
>   individuals, rule R: {“Daoism”, “Shinto”, “Confucianism”} “Eastern Asian
>   Religions”, can be used to protect the identity of the corresponding
>   individuals by associating them with the more general category of “Eastern
>   Asian Religions”. All other original values that are not captured in a rule,
>   can be maintained as-is.

>   When many values in a data element are expected to be infrequent, GENERALIZE
>   can be used to capture those original data values that must be maintained in
>   the output data. In this case, only those values that are *not* specified
>   will be replaced with a configurable general category term. For example, in
>   a data element that stores the spoken languages of individuals, GENERALIZE
>   can be configured to replace all languages that are not listed as “Spanish”
>   or “English”, with value “Other”.

   The following options are supported by GENERALIZE:

| **Option name**         | **Type** | **Description**                                                   | **Default value** |
|-------------------------|----------|-------------------------------------------------------------------|-------------------|
| maskRuleSet             | String   | The rule sets are defined in the masking JSON configuration file. | N/A               |

   In what follows, we elaborate on the different modes of operation that are
   supported by GENERALIZE for protecting infrequent data values.

   GENERALIZE can take as input a number of data generalization rules R1…Rm,
   each of which defines a set of original (or *source*) data values and a
   corresponding replacement (or *target*) value.

   Assume that *u* is an incoming data value from a FHIR data element to which
   GENERALIZE is applied and that R is a rule associated with this FHIR data
   element. GENERALIZE will check *u* against rule R and will replace it with
   the target value of R if *u* is found among the source values. Similarly, a
   rule R’ can be specified to replace value *u* only if it does *not* belong
   among the source values specified in that rule. If multiple rules are
   specified for the same FHIR data element, each rule in sequence will be
   tested against the value *u* of the FHIR data element, and the first rule
   that is found to contain *u* will be triggered. For those values *u* of the
   FHIR data element that none of the rules apply, they can be either
   maintained or be replaced with a pre-specified general category value (e.g.,
   “Other”).

   The various rules that must be enforced by GENERALIZE are defined with the
   FHIR Resource data element path in the JSON data de-identification
   configuration file. The option generalize.mask.ruleSet is used for defining
   the rules that must be applied to a FHIR data element, where each rule
   consists of two JSON element names: the targetValue element, and the
   sourceValueIn *or* the sourceValueNotIn element.

 -   The targetValue element provides the replacement value that must be applied
    to the original value of the FHIR data element.

 -   The sourceValueIn element provides a set of original values that can be
    associated with the corresponding FHIR data element. If the FHIR data
    element that is currently examined by GENERALIZE has a value that belongs to
    the sourceValueIn set, then the original value of the FHIR data element will
    be replaced with that of the corresponding targetValue element.
    

   We note that the special value “\*” (asterisk) can be used instead of any
   specific data values in the sourceValueIn set, to denote that *any* value of
   the corresponding FHIR data element must be replaced with the value in the
   targetValue element.

-   The sourceValueNotIn element can be specified instead of the sourceValueIn
    element to support rules that enforce negation logic. It captures a set of
    original values that can be associated with the corresponding FHIR data
    element and *must not* be replaced by the value of the targetValue element.
    Any FHIR data element that has a value in the sourceValueNotIn set will
    maintain its original value. All other values of the FHIR data element that
    do not belong in the sourceValueNotIn set, will be replaced by the
    corresponding value of the targetValue element.

>   In what follows, we provide some examples to demonstrate the capabilities of
>   the GENERALIZE privacy provider, by applying different rulesets to FHIR
>   resource type data elements. Please note that the values shown are for
>   demonstration purposes only.

**Example 4 – Patient address city data element:**

```
   {
     "--/fhir/Patient/address/city": {
       "type": "GENERALIZE",
       "maskRuleSet": "[{\"targetValue\": \"Asia City\", \"sourceValueIn\": [\"Bangkok\",\"Manila\",\"Shanghi\",\"TaiPei\",\"Mumbai\"]}, {\"targetValue\": \"African City\", \"sourceValueIn\": [\"Addis Ababa\",\"Cairo\",\"Cape Town\",\"Lagos\"]}]"
     }
   }
```

>   In the example above, a ruleset consisting of two rules is applied to the
>   Patient address city data element. These rules are checked sequentially as
>   follows:

> 1.  If the city value of the FHIR data element is any of the cities listed in
    the sourceValueIn element of the first rule (i.e., any of “Bangkok”,
    “Manila”, “Shanghai”, “Taipei”, or “Mumbai”), then it will be replaced with
    the value specified in the targetValue element (i.e., Asian City). Then, no
    other rule will be examined.

> 2.  Otherwise, if the city value of the FHIR data element is one of the those
    listed in the sourceValueIn element of the second rule (i.e., “Addis Ababa”,
    “Cairo”, “Cape Town”, “Lagos”), then it will be replaced with the value
    specified in the targetValue element (i.e., African City). No other rule
    will be examined.

> 3.  Otherwise, if the city value of the FHIR dta element is not among the cities
    listed in the sourceValueIn element of the first rule and is also not among
    the cities listed in the sourceValueIn element of the second rule, then the
    original value of the FHIR data element will be maintained.

**Example 5 – Practitioner address city data element:**
```
   {
     "--/fhir/Practitioner/address/city": {
       "type": "GENERALIZE",
       "maskRuleSet": "[{\"targetValue\": \"US City\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houseton\",\"Minneapolis\",\"Boston\"]}, {\"targetValue\": \"Canadian City\", \"sourceValueIn\": [\"Toronto\",\"Vancouver\",\"Montreal\",\"Calgary\"]}, {\"targetValue\": \"Other\", \"sourceValueIn\": [\"\\*\"]}]"
     }
   }
```

>   In the example above, a ruleset consisting of three rules is applied to the
>   Practitioner address city data element. As in the previous example, these
>   rules are checked sequentially and the first rule that applies to the value
>   of the FHIR data element is invoked. In this example, we demonstrate the
>   application of the third rule, wherein if the value of the input FHIR data
>   element is not among those listed in the sourceValueIn elements of the first
>   and the second rules, it will be replaced with value “Other”.

**Example 6 – Device component language code text data element:**
```
{
     "--/fhir/DeviceComponent/languageCode/text": {
       "type": "GENERALIZE",
       "maskRuleSet": "[{\"targetValue\": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]"
     }
   }
```

>   In the example above, a ruleset consisting of a single rule is applied to
>   the Device component language code text data element. This example
>   illustrates the negation logic that can be enforced in rules specified under
>   the GENERALIZE privacy provider. Specifically, if the value of the FHIR data
>   element is any of the languages listed in the sourceValueNotIn element of
>   the rule (i.e., “French” or “Spanish”), then this value will be maintained
>   in the FHIR data element. Otherwise, e.g., if the value of the FHIR data
>   element is “English”, then it will be replaced by the value specified in the
>   targetValue element, i.e. “Other”.

#### GUID 

>   Replaces a GUID with a randomly chosen GUID. This provider
>   does not have any configuration options.

#### HASH

>   Hashes the original value using an algorithm, such as SHA-256 and SHA-512.
>   The following hashing algorithms are supported: MD2 (as defined in RFC
>   1319), MD5 (as defined in RFC 1321), as well as SHA-1, SHA-256, SHA-384 and
>   SHA-512 (as defined in FIPS PUB 180-2). We note that SHA-256 is a 256-bit
>   hash function intended to provide 128 bits of security against collision
>   attacks, while SHA-512 is a 512-bit hash function intended to provide 256
>   bits of security. A 384-bit hash may be obtained by truncating the SHA-512
>   output.

>   Optionally, a part of the original data value can be maintained, by
>   specifying a start offset and / or an end offset, which define the part of
>   the value that must be replaced with the computed hash value. The start
>   offset must be greater or equal to 0 (first position in the data value) and
>   less than the end offset, when an end offset is specified. The value outside
>   the start and end offsets can be either maintained or deleted. The behavior
>   of the algorithm in the case of invalid offsets can be specified in the
>   configuration options.

| **Option name**                   | **Type** | **Description**                                                                                                                                                                                          | **Default value** |
|-----------------------------------|----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| algorithmDefault                  | String   | Default algorithm                                                                                                                                                                                        | sHA-256           |
| offsetOffsetMask                  | Boolean  | Mask using offsets                                                                                                                                                                                       | false             |
| offsetOffsetMaskDelete            | Boolean  | Mask using offsets. the part of the string that falls outside the [hashing.offset.begin, hashing.offset.end] will be deleted, and the result of hashing the string within these bounds will be returned. | false             |
| offsetBegin                       | Integer  | Offset from where to compute the hash it must be a non-negative value. if not provided, the hash will be computed from the beginning of the value (position 0). [inclusive]                             | \-1               |
| offsetEnd                         | Integer  | Offset to where to end the computation of the hash if specified, it must be greater than hashing.offset.begin. [exclusive]. if -1, it points to the end of the input string.                            | \-1               |
| salt                              | String   | Seed / salt to be used in hashing of the original data value by default, no salt is used                                                                                                               | null              |
| offsetInvalidOffsetValue          | Integer  | Value to be returned if the provided begin and/or end offsets are not valid: 1 – return null, 2 – return empty string, 3 – return hash of the entire value                                               | 1                 |

#### HOSPITAL

>   Replaces a hospital name with a randomly chosen one.

| **Option name**               | **Type** | **Description**                         | **Default value** |
|-------------------------------|----------|-----------------------------------------|-------------------|
| maskPreserveCountry           | Boolean  | Select a hospital from the same country | true              |

#### IBAN

>   Masks IBAN account numbers with the option to preserve country.

| **Option name**           | **Type** | **Description**       | **Default value** |
|---------------------------|----------|-----------------------|-------------------|
| maskPreserveCountry       | Boolean  | Preserve country code | true              |

#### ICDV9

>   Masks ICDV9 (diagnoses) codes. Codes can also be generalized to their
>   respective chapters or categories.

| **Option name**        | **Type** | **Description**           | **Default value** |
|------------------------|----------|---------------------------|-------------------|
| randomizeChapter       | Boolean  | Randomize by chapter      | false             |
| randomizeCategory      | Boolean  | Randomize by 3-digit code | true              |

#### ICDV10

>   Masks ICDV10 (diagnoses) codes. Codes can also be generalized to their
>   respective chapters or categories.

| **Option name**        | **Type** | **Description**           | **Default value** |
|------------------------|----------|---------------------------|-------------------|
| randomizeChapter       | Boolean  | Randomize by chapter      | false             |
| randomizeCategory      | Boolean  | Randomize by 3-digit code | true              |

#### IMEI

>   Masks IMEI device identifiers.

| **Option name**               | **Type** | **Description**                                                                                                  | **Default value** |
|-------------------------------|----------|------------------------------------------------------------------------------------------------------------------|-------------------|
| preserveTAC                   | Boolean  | Whether to preserve the Type Allocation Code (the first eight digits that indicate the manufacturer and model)   | true              |

#### IP_ADDRESS

>   Masks IP addresses with the option to preserve subnets.

| **Option name**            | **Type** | **Description**                | **Default value** |
|----------------------------|----------|--------------------------------|-------------------|
| subnetsPreserve            | Integer  | Number of prefixes to preserve | 0                 |

#### LATITUDE_LONGITUDE

>   Masks latitude / longitude pairs, recognizing several location formats.

| **Option name**                        | **Type** | **Description**                                            | **Default value** |
|----------------------------------------|----------|------------------------------------------------------------|-------------------|
| maskFixedRadiusRandomDirection         | Boolean  | Randomize a point with a fixed radium but random direction | false             |
| maskDonutMasking                       | Boolean  | Randomize a point in a donut-shaped                        | false             |
| maskRandomWithinCircle                 | Boolean  | Randomize a point in a circle                              | true              |
| offsetMaximumRadius                    | Integer  | Maximum offset radius (in meters)                          | 100               |
| offsetMinimumRadius                    | Integer  | Minimum offset radius (in meters)                          | 50                |

#### MAC_ADDRESS

>   Masks MAC addresses with the option to preserve the vendor information.

| **Option name**            | **Type** | **Description**             | **Default value** |
|----------------------------|----------|-----------------------------|-------------------|
| maskingPreserveVendor      | Boolean  | Preserve vendor information | true              |

#### MAINTAIN

>   Retails the current value of the field.  This provider is commonly used when the option to 
>   redact all fields without an assigned rule is specified and some fields are to be retained.

#### MARITAL 

>   Replaces a marital status with a randomly chosen marital status. This provider
>   does not have any configuration options.

#### NAME

>   Masks the first and last names of individuals.

| **Option name**           | **Type** | **Description**                           | **Default value** |
|---------------------------|----------|-------------------------------------------|-------------------|
| maskingAllowUnisex        | Boolean  | Allow unisex names to be used for masking | false             |
| tokenConsistence          | Boolean  | Provide consistence per token             | false             |
| maskPseudorandom          | Boolean  | Provide pseudorandom consistence          | false             |
| namesMaskGenderPreserve   | Boolean  | Preserve gender while masking             | true              |

#### NULL

>   Replaces the original data value with an empty string (“”), or with a NULL
>   value. The default is to return an empty string.

| **Option name**      | **Type** | **Description**                           | **Default value** |
|----------------------|----------|-------------------------------------------|-------------------|
| maskReturnNull       | Boolean  | Return NULL instead of empty string value | false             |


#### NUMBERVARIANCE

>   Masks a numeric data value by adding a random offset. There are two options
>   available and are processed in the following order:

> 1.  Calculating the offset by randomly selecting a numeric value from within a
    pre-specified interval / range.

> 2.  Calculating the offset based on given percentages signifying the acceptable
    distance of the new value from the original value.

| **Option name**                 | **Type** | **Description**                                                                                 | **Default value** |
|---------------------------------|----------|-------------------------------------------------------------------------------------------------|-------------------|
| augmentMask                     | Boolean  | Augment the numeric data value                                                                  | false             |
| augmentLowerBound               | double   | Range interval lower bound                                                                      | 1.0               |
| augmentUpperBound               | double   | Range interval upper bound                                                                      | 10.               |
| resultWithPrecision             | Boolean  | Result includes decimal digits for precision                                                    | false             |
| precisionDigits                 | Integer  | Number of decimal digits to keep when precision is set to true; -1 to provide maximum precision | -1                |
| maskLimitUp                     | double   | Up percentage limit                                                                             | 10.0              |
| maskLimitDown                   | double   | Down percentage limit                                                                           | 10.0              |

#### OCCUPATION

>   By default it replaces an occupation with a randomly selected occupation,
>   unless the option *occupation.mask.generalize* is set to *true*, in which
>   case it generalizes an occupation to its respective category. The categories
>   of occupations are based on the 2010 SOC classification.

| **Option name**            | **Type** | **Description**                   | **Default value** |
|----------------------------|----------|-----------------------------------|-------------------|
| maskGeneralize             | Boolean  | Generalize to occupation category | false             |

#### PHONE

>   Replaces a phone or fax number with a random one. There are options to
>   preserve the country code and area code of the original number.

| **Option name**            | **Type**      | **Description**                                                 | **Default value** |
|----------------------------|---------------|-----------------------------------------------------------------|-------------------|
| countryCodePreserve        | Boolean       | Preserve country code                                           | true              |
| areaCodePreserve           | Boolean       | Preserve area code                                              | true              |
| phoneRegexPatterns         | String Array  | Array of regular expression to specify phone number patterns    |                   |

>> phoneRegPatterns are used to specify phone number patterns.  It is optional.  If unspecified, the phone masking provider will use the following format:

```
  ^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\d+)
```  

>> To specify phoneRegexPatterns, the format is using Java pattern named groups.  Please see:
>>         https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
>> for additional details.
>> The supported named grouops are: countryCode/areaCode/number.   number is a required group.  countryCode/areaCode are optonal.
>> example:

```
 {
  "type": "PHONE",
  "countryCodePreserve": true,
  "areaCodePreserve": false,
  "phoneRegexPatterns": [
    "^\\+(?<countryCode>\\d{1,3}) (?<areaCode>\\d{2,4}) (?<number>[0-9\\s]{8,9})",
    "^(?<areaCode>\\(\\d{2,4}\\)) (?<number>[0-9\\s]{8,9})",
    "^\\+(?<countryCode>\\d{1,3}) (?<areaCode>\\(\\d{2,4}\\)) (?<number>[0-9]{7})"
  ]
}
```

>> The phoneRegexPatterns are used to match the following phone numbers:
```
 +380 44 123 45 67
 (044) 123 45 67
 +380 (44) 1234567
```

The named groups countryCode/areaCode are used to identify the position of country code and area code respectively.  The config options countryCodePreserve and areaCodePreserve will only work if the groups are specified.  If none of the patterns in phoneRegexPatterns matched a phone number to be masked, the output will be null by default.

#### PSEUDONYM

>   Masks an original data value by replacing it with a randomly generated data
>   pseudonym. In what follows, we elaborate on the three options that are
>   supported by PSEUDONYM for the generation of data pseudonyms.

##### Generic pseudonym generation

   Supports the generation of pseudonyms with a length that lies within a
   specified range, which may consist of one or more types of the following
   characters: uppercase and lowercase letters, digits, special characters.

   Generic pseudonym generation is supported by setting:

>  generateViaOptionsEnabled = true

   The length of the computed pseudonym lies within an interval, whose borders
   are defined using the following options:

>  generateViaOptionsMinLength = \<value\>

>  generateViaOptionsMaxLength = \<value\>

   The selection of the types of characters that can be used to produce the
   pseudonym is achieved by setting the following flags to true or false, in
   order to enable or disable a specific type of characters:

>  generateViaOptionsGenerateUppercase

>  generateViaOptionsGenerateLowercase

>  generateViaOptionsGenerateDigit

>  generateViaOptionsGenerateSpecial

##### Regular expression based pseudonym generation

>   Supports the generation of pseudonyms that follow a specified pattern,
>   provided as a regular expression. There are two ways to specify the pattern
>   that will be used to generate a data pseudonym: (a) provide the actual
>   pattern itself, or (b) provide the name of the pattern. If both the actual
>   pattern and the pattern name are provided, then the pattern is used and the
>   pattern name is ignored.

###### Specifying the pattern

>   To specify the pattern itself, the onboarding team needs to provide as input
>   a text string, which looks like a regular expression, that is used by the
>   Data De-Identification Service to produce a pseudonym.

>   The allowable syntax of the input string / pattern for producing a data
>   pseudonym is captured below:

| **Syntax Frames**     |                                                                                                                                                                              |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [                     | Begins a character class. If using a single literal (\\u1234, A, 1, etc.) then the literal can be used instead of a full character class unless a specific class is desired. |
| x-y                   | A character literal range to allow. Second index must be greater than the first.                                                                                             |
| ]                     | Ends a character class.                                                                                                                                                      |
| (                     | Begins an optional class. These can be nested and include any other item.                                                                                                    |
| \                    | Separates optional items within an optional class.                                                                                                                           |
| )                     | Closes an optional class.                                                                                                                                                    |
| {                     | Begins multiplicity. If not specified, multiplicity is 1.                                                                                                                    |
| m1                    | Minimum number of the proceeding character class. Used also as maximum if no maximum specified.                                                                              |
| , m2                  | Maximum number of this proceeding character class.                                                                                                                           |
| }                     | Close multiplicity.                                                                                                                                                          |
| **Universal Escapes** |                                                                                                                                                                              |
| \\{                   | A literal ‘{‘ character.                                                                                                                                                     |
| \\}                   | A literal ‘}‘ character.                                                                                                                                                     |
| \\]                   | A literal ‘]‘ character.                                                                                                                                                     |
| \\[                   | A literal ‘[‘ character.                                                                                                                                                     |
| \\\\                  | A literal ‘\\‘ character.                                                                                                                                                    |
| \\-                   | A literal ‘-‘ character.                                                                                                                                                     |
| \\(                   | A literal ‘(‘ character.                                                                                                                                                     |
| \\)                   | A literal ‘)‘ character.                                                                                                                                                     |
| \ \\|                   | A literal ‘\|‘ character.                                                                                                                                                    |
| \\pHHHHHHHH           | A UTF-32 character. May be included in ranges, for example: (\\pHHHHHHHH-\\pHHHHHHHH).                                                                                       |
| \\d                   | The set of digits.                                                                                                                                                           |
| \\l                   | The set of lowercase ASCII letters.                                                                                                                                          |
| \\u                   | The set of uppercase ASCII letters.                                                                                                                                          |

   As an example, to generate pseudonyms of variable lengths (from 5 to 10
   characters), consisting only of the uppercase letters “N”, “S”, “E”, and
   “W”, the onboarding team must set the following option:

>   generateViaPatternPattern = “(N\|S\|E\|W){5,10}”

###### Using a pre-specified pattern

>   Instead of specifying the pattern itself as part of the call made to the
>   PSEUDONYM provider, the pattern can be pre-specified and stored in a
>   resource file under a selected pattern name. The PSEUDONYM provider can be
>   subsequently called with input the name of the pattern that must be used for
>   pseudonym generation, and the language code (locale) that is associated with
>   this pattern.

  Resource files can contain patterns by placing an entry in the
   localization.properties file, setting the code.PATTERN property to the
   location of the patterns’ properties file, e.g.,

>   en.PATTERN = /identifier/en/patterns.properties

   The patterns’ properties file must have the following format:

>   **\<pattern name\> = \<pattern\>**

>   (e.g., phoneNumber = \\(\\d{3}\\) \\d{3}-\\d{4})

   A pattern from the resource files may be used by setting the pattern
   configuration option to null, while specifying the appropriate values for
   the language code (locale) and pattern name options. As an example,
   phoneNumber can be used as follows:

>   generateViaOptionsEnabled = false

>   generateViaPatternEnabled = true

>   generateViaPatternPattern = null

>   generateViaPatternLanguageCode = "EN"

>   generateViaPatternPatternName = "phoneNumber"

##### Encryption basedPseudonym generation

>   Supports the generation of pseudonyms by hashing the original data value
>   using the SHA-512 (default) or the SHA-256 one-way hashing algorithms.

Encryption based pseudonym generation is activated as follows:

>   generateViaOptionsEnabled = false

>   generateViaHashEnabled = true

   The options above will lead to the generation of SHA-512 pseudonyms, which
   is the default algorithm used. To generate pseudonyms using the SHA-256
   algorithm, the following configuration option must be set:

>   pseudonym.generateViaHash.useSHA256 = false

   The entire list of options and their default values for the PSEUDONYM
   provider, is presented in the following table.

| **Option name**                     | **Type** | **Description**                                                                                                                  | **Default value** |
|-------------------------------------|----------|----------------------------------------------------------------------------------------------------------------------------------|-------------------|
| generateViaOptionsEnabled           | Boolean  | Generate pseudonym via specific options                                                                                          | true              |
| generateViaOptionsMinLength         | Integer  | Minimum length of generated pseudonym                                                                                            | 10                |
| generateViaOptionsMaxLength         | Integer  | Maximum length of generated pseudonym                                                                                            | 10                |
| generateViaOptionsGenerateUppercase | Boolean  | Generated pseudonym may contain uppercase letters                                                                                | true              |
| generateViaOptionsGenerateLowercase | Boolean  | Generated pseudonym may contain lowercase letters                                                                                | true              |
| generateViaOptionsGenerateDigit     | Boolean  | Generated pseudonym may contain digits                                                                                           | true              |
| generateViaOptionsGenerateSpecial   | Boolean  | Generated pseudonym may contain the following special characters: !, \@, \#, \$, %, \^, &, \*, [, ], \\, /, ?, {, }, +, -, or \_ | false             |
| generateViaPatternEnabled           | Boolean  | Generate pseudonym via pattern                                                                                                   | false             |
| generateViaPatternPattern           | String   | Pattern used to generate pseudonym                                                                                               | null              |
| generateViaPatternLanguageCode      | String   | Language code (2-digit iSO code) of pattern used to generate pseudonym                                                           | EN                |
| generateViaPatternPatternName       | String   | Name of pattern used to generate pseudonym                                                                                       | null              |
| generateViaHashEnabled              | Boolean  | Generate pseudonym via hash algorithm                                                                                            | false             |
| generateViaHashUseSHA256            | Boolean  | Use sHA-256, instead of sHA-512, as hash algorithm to generate pseudonym                                                         | false             |

#### RACE

>   Replaces race information with a randomly chosen race. This provider does
>   not have any configuration options.

#### RANDOM

>   Replaces an original data value with random characters, by:

> 1.  replacing numerical characters with numerical characters;

> 2.  replacing letter characters with letter characters;

> 3.  maintaining other characters (like commas, dashes, asterisks, etc.)

>   If non-english characters are replaced, the characters will be replaced with an 
>   appropriately-cased latin letter. If the character does not have an associated case, 
>   such as chinese or japanese characters, it will be replaced with an uppercase latin letter.

  This provider has no configuration options.

#### REDACT

>   Redacts a value by replacing it with a character (default is the letter X). The
>   length of the value can be optionally preserved.

| **Option name**  | **Type** | **Description**               | **Default value** |
|------------------|----------|-------------------------------|-------------------|
| preserveLength   | Boolean  | Preserve token length         | true              |
| replaceCharacter | String   | Default replacement character | X                 |

#### RELIGION

>   Replaces a religion information with a randomly chosen religion. This
>   provider does not have any configuration options.

#### REPLACE

>   Replaces an original value with either asterisks or with random characters.
>   Digits are replaced with randomly selected digits, and alpha characters are
>   replaced with random alpha characters. Other characters like dashes, commas,
>   etc. are preserved.
>   If non-english characters are replaced with the maskReplaceWithRandom option,
>   the characters will be replaced with an appropriately-cased latin letter.
>   If the character does not have an associated case, such as chinese or japanese characters,
>   it will be replaced with an uppercase latin letter.

>   If neither maskReplaceWithAsterisks or maskReplaceWithRandom are selected, 
>   no making will take place since there is no replacement strategy.

| **Option name**          | **Type** | **Description**                                             | **Default value** |
|--------------------------|----------|-------------------------------------------------------------|-------------------|
| maskReplaceWithAsterisks | Boolean  | Replace the rest of the value with asterisks                | false             |
| maskPreserve             | Integer  | Number of characters to preserve                            | 3                 |
| maskOffset               | Integer  | Starting offset for preserving                              | 0                 |
| maskReplaceWithRandom    | Boolean  | Replace the rest of the value with random digits/characters | false             |

#### SSN_UK

>   Masks a social security number (SSN) based on the UK, with the option to
>   preserve its prefix.

| **Option name**    | **Type** | **Description** | **Default value** |
|--------------------|----------|-----------------|-------------------|
| maskPreservePrefix | Boolean  | Preserve prefix | true              |

#### SSN_US

>   Masks Social Security Numbers based on the US with the option to preserve
>   their prefix.

| **Option name**        | **Type** | **Description**      | **Default value** |
|------------------------|----------|----------------------|-------------------|
| maskPreserveAreaNumber | Boolean  | Preserve area number | true              |
| maskPreserveGroup      | Boolean  | Preserve group       | true              |

#### STATE_US 

>   Replaces a state with a randomly chosen state. This provider
>   does not have any configuration options.

#### SWIFT 

>    Masks SWIFT codes with the option to preserve their country.

| **Option name** | **Type** | **Description**      | **Default value** |
|-----------------|----------|----------------------|-------------------|
| preserveCountry | Boolean  | Preserve country     | false             |

#### URL

>   Masks URLs with the options to remove the query part, preserve domain
>   levels, mask ports and username / passwords that may be present.

| **Option name**      | **Type** | **Description**               | **Default value** |
|----------------------|----------|-------------------------------|-------------------|
| maskPort             | Boolean  | Mask port                     | false             |
| maskRemoveQuery      | Boolean  | Remove query part             | false             |
| preserveDomains      | Integer  | Number of domains to preserve | 1                 |
| maskUsernamePassword | Boolean  | Mask username and password    | true              |
| maskMaskQuery        | Boolean  | Mask query part               | false             |

#### VIN

>   Masks a vehicle’s identifier number with options to preserve the
>   manufacturer and the vehicle description information.

| **Option name**  | **Type** | **Description**                                | **Default value** |
|------------------|----------|------------------------------------------------|-------------------|
| wmiPreserve      | Boolean  | Preserve manufacturer information (WMI)        | true              |
| vdsPreserve      | Boolean  | Preserve vehicle description information (VDS) | false             |

#### ZIPCODE

>   Masks original zip code values while offering a number of utility-preserving
>   options: (a) zip code truncation, where the last n digits of the zip code
>   are removed to protect the association of the individual with a specific zip
>   code, (b) zeroing out of the zip code prefix, (c) zip code replacement with
>   a random zip code having the same prefix, or with a random neighboring zip
>   code to maintain spatial proximity, (d) zip code processing as per HIPAA
>   Safe Harbor, where the geographical unit formed by combining all zip codes
>   with the same first 3-digits as the original zip code is checked against the
>   current publicly available data from the US Bureau of the Census to verify
>   if it meets the set population criteria (typically if it contains more than
>   20,000 people).

>   If multiple options are set to True in the zip code masking algorithm, the
>   operations are performed in the following order:

> 1.  Replace the zip code with a neighboring zip code.


> 2.  Zero out the zip code (3-digit) prefix if the total population in the
    geographical unit formulated by combining all zip codes with the same prefix
    (first 3-digits) as the original zip code contains less than a specified
    minimum population. Additionally, the zip code may be truncated to a
    specified length if the minumum population in the formulated geographical
    unit is not reached.

> 3.  Replace the zip code digits suffix with random digits, or with digits that
    result in a random valid zip code with the same prefix.

> 4.  Truncate the zip code to the prefix.

| **Option name**                      | **Type** | **Description**                                                                   | **Default value** |
|--------------------------------------|----------|-----------------------------------------------------------------------------------|-------------------|
| maskCountryCode                      | String   | Country code for the zip codes (2-digit iSO code)                                 | US                |
| maskReplaceWithNeighbor              | Boolean  | Replace with a neighboring zip code                                               | false             |
| maskReplaceWithNeighborNearestCount  | Integer  | Number of closes zip codes to select from                                         | 10                |
| maskPrefixLength                     | Integer  | Zip code prefix length                                                            | 3                 |
| maskPrefixRequireMinPopulation       | Boolean  | Replace zip code prefix with zeros if the minimum population is not reached       | false             |
| maskPrefixMinPopulation              | Integer  | Minimum population criterion (number of people)                                   | 20000             |
| maskTruncateIfNotMinPopulation       | Boolean  | Truncate zip code if the minimum population is not reached                        | false             |
| maskTruncateLengthIfNotMinPopulation | Integer  | Number of digits to truncate the zip code to if minimum population is not reached | 2                 |
| maskSuffixTruncate                   | Boolean  | Truncate zip code to the prefix                                                   | true              |
| maskSuffixReplaceWithRandom          | Boolean  | Replace zip code suffix with random                                               | false             |
| maskSuffixReplaceWithValidOnly       | Boolean  | Replace zip code suffix resulting in valid zip code only                          | false             |


### Applying Multiple Data Protection Methods to the Same Data Element

   The Data De-Identification Service supports the application of multiple data
   protection methods (and rules) to a FHIR data element. This is a very
   powerful and useful feature, which works similarly to a Unix pipe command,
   like “ls –al \| grep ‘\^d’”. When multiple data protection methods are
   specified for the same data element, the first method is applied on the
   original value of the element and its output is provided as input to the
   next data protection method. The output of the last data protection method
   determines the new value of the FHIR data element.

   The Data De-Identification Service supports two approaches for applying
   multiple data protection methods to a data element. The first approach is
   to provide multiple values in the "maskingProviders" array. The second approach, 
   which only works for FHIR arrays with different keys, is to specify the 
   providers in separate rules in the data de-identification configuration 
   file. In what follows, we explain in detail how each of these approaches works.


#### Provide multiple values in the "maskingProviders" array 

   The Data De-Identification Service allows the application of more than one
   data protection methods (among those supported in Section 5.4.2) to the same
   FHIR data element, when multiple values are specified in the maskingProviders 
   parameter of a rule. The data protection methods are applied in the sequence 
   that they are listed in the data de-identification configuration file. 

   **This functionality is offered for both array and non-array FHIR data elements.**

   The following syntax is supported:

   **Example 12 – Masking a FHIR data element using two privacy providers:**

```
    {
      "rules": [
        {
          "name": "MaskBirthDay",
          "maskingProviders": [
            {
              "type": "DATETIME",
              "generalizeMonthyear": true
            },
            {
              "type": "HASH"
            }
          ]
        },
        {
          "name": "EMAIL_RULE",
          "maskingProviders": [
            {
              "type": "EMAIL",
              "generalizeMonthyear": true
            },
            {
              "type": "HASH"
            }
          ]
        }
      ],
      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [
          "Patient"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Patient/birthDate",
            "rule": "MaskBirthDay"
          },
          {
            "jsonPath": "/fhir/Patient/telecom[0]/value",
            "rule": "EMAIL_RULE"
          }
        ]
      }
    }
```

   In Example 12, we observe the processing of the FHIR birthDate data element
   (of the Patient FHIR Resource) by two privacy providers. First, the original
   date-of-birth of the patient is transformed to the MM/YYYY representation by
   maintaining only the month and the year of birth. Next, patients who were
   born in the same month and year are hashed to the same bucket by one-way
   hashing the birthDate data element.

   The second de-id rule applies to the first element of the telecom FHIR array
   (which is also part of the Patient FHIR Resource) and is processing it first
   by the EMAIL and subsequently by the HASH privacy providers.

   As another example, consider that we want to retrieve information about the
   State where an individual resides (for individuals of certain US States),
   based on the US zip code of their home address. The mapping of US zip codes
   to US States is provided in:
   <https://en.wikipedia.org/wiki/List_of_ZIP_code_prefixes>. Example 13
   presents the ruleset for translating 5-digit codes to states “ME”, “CT” and
   “Other”, with the latter corresponding to any US State other than Maine (ME)
   and Connecticut (CT). Observe that this is achieved using the ZIPCODE
   provider followed by GENERALIZE.

   **Example 13 – Masking a ZIP code data element using two privacy
   providers:**

```
{
  "rules": [
    {
      "name": "POSTAL_CODE",
      "maskingProviders": [
        {
          "type": "ZIPCODE"
        },
        {
          "type": "GENERALIZE",
          "maskRuleSet": "[{\"targetValue\":\"ME\", \"sourceValueIn\": [\"040\",\"041\",\"042\",\"043\",\"044\",\"045\", \"046\",\"047\",\"048\",\"049\"]}, {\"targetValue\":\"CT\", \"sourceValueIn\": [\"060\",\"061\",\"062\",\"063\",\"064\",\"065\", \"066\",\"067\",\"068\",\"069\"]}, {\"targetValue\":\"Other\", \"sourceValueIn\": [\"\\*\"]}]"
        }
      ]
    }
  ],
  "json": {
    "schemaType": "FHIR",
    "messageTypeKey": "resourceType",
    "messageTypes": [
      "Patient",
      "Practitioner"
    ],
    "maskingRules": [
      {
        "jsonPath": "/fhir/Patient/address/postalCode",
        "rule": "POSTAL_CODE"
      },
      {
        "jsonPath": "/fhir/Practitioner/address/postalCode",
        "rule": "POSTAL_CODE"
      }
    ]
  }
}
```

   **Restrictions:** We emphasize that there are restrictions enforced by the
   Data De-Identification Service to (a) the number and (b) the type of privacy
   providers that can be applied to the same FHIR data element, when using the
   comma separated format.
   
   Specifically, only two privacy providers can be assigned to a single data
   element. If this condition is violated, then an IllegalArgumentException is
   thrown and the following error message is written to the platform log file.

| **Message ID** | **Message Text**                                                                                      | **Replacement Variables**                                       |
|----------------|-------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| WPH2012E       | {0} – The rule {1} provided for key {2} contains an invalid combination of chained masking providers. | {0}: Provider class name {1}: Masking rule used {2}: Field path |

   An example of a produced log message is shown below:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR - LogManager -
>   com.ibm.research.drl.prima.providers.masking.fhir.FHIRGenericMaskingProvider
>   - The rule [RANDOM, HASH, HASH] provided for key /gender contains an invalid
>   combination of chained masking providers.

   The second restriction that is enforced by the Data De-Identification
   Service regards the type of the privacy providers that can be applied to the
   same data element, ensuring that the pair of providers that is used is
   valid. For this, the Data De-Identification Service separates the 
   privacy providers into two categories, as follows:

1.  **Category I – PII specific providers:** These providers mask a specific
    type of PII / PHI / SPI, such as ADDRESS, PHONE, VIN, SSN, COUNTY, URL,
    NAME, GENDER, RELIGION, IP ADDRESS, EMAIL, CREDIT CARD, DATETIME, IBAN,
    CITY, ICD V9/V10, MAC ADDRESS, ATC, OCCUPATION, CONTINENT, HOSPITAL, ZIP
    CODE, COUNTRY, LATITUDE / LONGITUDE, and DATEDEPENDENCY.

2.  **Category II – generic providers:** These providers are not designed to
    mask a specific type of PII / PHI / SPI, but can be used to process several
    types of identifiers. Examples are HASH, RANDOM, REPLACE, REDACT, NUMBERVARIANCE, 
    GENERALIZE, RARE_REPLACEMENT, and PSEUDONYM.

   Based on these categories, a pair of privacy providers is
   valid if the first provider belongs to Category I, and the second provider
   is from Category II.

   If an invalid pair of privacy providers is specified, the 
   Data De-Identification Service will throw an
   IllegalArgumentException and log a WPH2011E message to the platform log
   file. An example of such a log message is below:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR - LogManager -
>   com.ibm.research.drl.prima.providers.masking.fhir.FHIRGenericMaskingProvider
>   The rule [HASH, PHONE] provided for key /telecom[1]/value contains an
>   invalid combination of chained masking providers.

#### Using Separate Rules for Privacy Providers

   The same data elements of a FHIR array can be processed by multiple privacy
   providers, when more than one rules are defined for these data elements in
   the data de-identification configuration file. In this case, the rules will
   be applied to the corresponding data elements of the FHIR array in the
   sequence in which they’ve been defined, where the first rule will be applied
   on the original value of the data element, and each subsequent rule will be
   applied on the transformed value produced for this element by the previous
   rule (resembling the behavior of a Unix pipe command).

   The example below shows a valid ruleset for processing data elements of a
   FHIR array with multiple privacy providers:

   **Example 14 – Masking elements of a FHIR array with multiple masking
   providers:**
   
```
  {
        "jsonPath": "/fhir/Patient/name[1]/use",
        "rule": "HASH"
  }

  {
        "jsonPath": "/fhir/Patient/name[\*]/use",
        "rule": "RANDOM"
  }

  {
        "jsonPath": "/fhir/Patient/name[\*]/given[0]",
        "rule": "HASH_REPLACE"
  }
```

   In Example 14, both the first and the second rule will be applied to the
   name[1]/use data element. The second rule will be also applied to all the
   other name[\*]/use elements of the FHIR array. The third rule of the example
   will lead to applying the HASH followed by the REPLACE privacy providers to
   all the name[\*]/given[0] data elements of the FHIR array.

   To disable this default behavior, a new configuration option (flag), called
   “**arrayAllRules**”, has been introduced to the data de-identification
   configuration file.

   **When the “arrayAllRules” flag is set to false, only the first rule that is
   defined for a FHIR array data element will be enforced. Any additional rules
   that involve the same FHIR array data element will not be applied to this
   element. We note that “arrayAllRules” applies only to FHIR array data
   elements.**

   For instance, consider the rules in Example 14 (above) when “arrayAllRules =
   false”. In this case, name[1]/use will be processed by HASH, and all other
   name[\*]/use data elements will be processed by RANDOM. The third rule will
   apply first HASH and then REPLACE to all name[\*]/given[0] FHIR array
   elements. In other words, the “arrayAllRules” flag changes the behavior of
   the Data De-Identification Service only with respect to different de-id
   rules that are applied to the same FHIR array data element. It does not,
   however, change the behavior of the Service when two privacy providers are
   defined as part of the a de-id rule.

   **In the case of regular, non-array FHIR data elements, if multiple de-id
   rules are specified, only the last rule will be applied to the data and the
   previous ones will be discarded.**

   **Example 15 – Masking a (regular) FHIR data element using separate de-id
   rules:**

```
  {
        "jsonPath": "/fhir/Patient/gender",
        "rule": "RANDOM"
  }

  {
        "jsonPath": "/fhir/Patient/gender",
        "rule": "HASH"
  }
```

   In the example above, only the HASH (last) privacy provider will be applied
   to the gender data element of the Patient FHIR Resource.

### Handling of Unrecognized Input Values and of Exceptions Raised by the Privacy Providers

   In what follows, we explain the operation of the various privacy providers
   that are offered by the Data De-Identification Service in the case of null
   or unrecognized input data values, as well as in the case of runtime
   exceptions.

#### Handling of Null and Unrecognized Input Data Values

   The Data De-Identification Service takes special care in handling null and
   unrecognized input data values of FHIR data elements.

   In the case of data elements that have a null value, the privacy providers
   that will process them will maintain the null value of the elements, thereby
   they will not proceed to falsify the data.

   If the input value to a privacy provider is not recognized by the privacy
   provider, thereby it does not appear to be an accurate input value, the Data
   De-Identification Service will operate as specified in the data
   de-identification configuration file, in options:
   “unspecified.value.handling” and “unspecified.value.returnMessage”. These
   options are globally set and hold for all privacy providers. They are
   explained below:

| **Option name**               | **Type** | **Description**                                                                                                                                                                                  | **Default value** |
|-------------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| unspecifiedValueHandling      | Integer  | Handling options: **1** : return null **2** : return fictionalized value produced by the corresponding privacy provider **3** : return the message (String) in “unspecified.value.returnMessage” | 1                 |
| unspecifiedValueReturnMessage | String   | Value to be returned when “unspecified.value.handling” is set to **3**                                                                                                                           | “OTHER”           |

   For providers that operate using regular expressions, such as PHONE NUMBER,
   SSN\_US, SSN\_UK, MAC\_ADDRESS, IBAN, CREDIT\_CARD, DATETIME, IP\_ADDRESS, EMAIL,
   etc., an unrecognized value is a value that does not conform to the
   specified regular expression.

   Similarly, for providers that operate using lookup tables, such as NAME,
   CITY, COUNTRY, OCCUPATION, HOSPITAL, etc., an unrecognized value is a value
   that is not part of the corresponding lookup table that is used by the
   privacy provider.

   When “unspecifiedValueHandling” is set to 1 (or to a value other than 2 or
   3), any privacy provider that takes as input an unrecognized data value will
   return a null value.

   When “unspecifiedValueHandling” is set to 2, any privacy provider that
   takes as input an unrecognized data value will return a randomly generated,
   fictionalized value that is valid for the corresponding provider.

   Last, when “unspecifiedValueHandling” is set to 3, any provider that takes
   as input an unrecognized data value, will store to this value the message
   that is specified in “unspecified.value.returnMessage”. By default, this
   message is set to “OTHER”.

   **Exceptions:** The DATEDEPENDENCY, GENERALIZE, HASH, MAINTAIN, NULL, 
   PSEUDONYM, RANDOM, REDACT, and REPLACE privacy providers ignore the
   “unspecified.value.handling” option. The URL, NUMVARIANCE, and ATC providers
   treat option 2 of “unspecified.value.handling” the same as option 1, thereby
   they return null.

#### Handling of Exceptions

   The data protection methods have to deal with
   the case of runtime exceptions that may be raised during their operation in
   the Data De-Identification Service. If a data protection method encounters
   an exception while attempting to privacy-protect an input data value, it
   will catch the exception, log a WPH2002E error message to the application
   audit log, and return an empty (null) String as the value of the
   corresponding data element. That is, unless the unspecified.value.\* options
   have been set.

   The error messages that are produced have the following structure:

| **Message ID** | **Message Text**                                                                                                                                         | **Replacement Variables**                                                                                         |
|----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| WPH2002E       | {0}.{1} – {2} – The data protection method encountered an exception while attempting to transform an input value. The original value has been nullified. | {0}: Provider class name {1}: Provider method name {2}: Exception name,                                           |
|                |                                                                                                                                                          | resource type, resource ID, and field name                                                                        |

   An example of a produced log message is the following:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR -
>   com.ibm.research.drl.prima.providers.masking.DateTimeMaskingProvider.mask -
>   java.lang.NullPointerException processing field
>   Patient(id=’exampleId123’)/birthDate - The data protection method
>   encountered an exception while attempting to transform an input value. The
>   original value has been nullified.

