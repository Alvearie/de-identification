# Masking Providers
## Overview

To preserve utility in the data being de-identified, the IBM Data De-Identification service supports the following data protection methods:

* [ADDRESS](#address)
* [ATC](#atc)
* [BINNING](#binning)
* [CITY](#city)
* [CONDITIONAL](#conditional)
* [CONTINENT](#continent)
* [COUNTRY](#country)
* [COUNTY](#county)
* [CREDIT_CARD](#credit_card)
* [DATEDEPENDENCY](#datedependency)
* [DATETIME](#datetime)
* [DATETIME_CONSISTENT_SHIFT](#datetime_consistent_shift)
* [EMAIL](#email)
* [FPE](#fpe)
* [GENDER](#gender)
* [GENERALIZE](#generalize)
* [GUID](#guid)
* [HASH](#hash)
* [HOSPITAL](#hospital)
* [IBAN](#iban)
* [ICDV9](#icdv9)
* [ICDV10](#icdv10)
* [IMEI](#imei)
* [IP_ADDRESS](#ip_address)
* [LATITUDE_LONGITUDE](#latitude_longitude)
* [MAC_ADDRESS](#mac_address)
* [MAINTAIN](#maintain)
* [MARITAL](#marital)
* [NAME](#name)
* [NULL](#null)
* [NUMBERVARIANCE](#numbervariance)
* [OCCUPATION](#occupation)
* [PHONE](#phone)
* [PSEUDONYM](#pseudonym)
* [RACE](#race)
* [RANDOM](#random)
* [REDACT](#redact)
* [RELIGION](#religion)
* [REPLACE](#replace)
* [SSN_UK](#ssn_uk)
* [SSN_US](#ssn_us)
* [STATE_US](#state_us)
* [SWIFT](#swift)
* [URL](#url)
* [VIN](#vin)
* [ZIPCODE](#zipcode)

## Data protection methods with configuration options and defaults

The following sections provide information about the various
field-level data protection methods that are currently supported by the Data
De-Identification Service. This includes the methods' available configuration (utility-preserving)
options and their default values.

#### ADDRESS

>   Masks an input postal address with a random one. Various elements of the
>   address can be preserved, for example, street names and road types.

| **Option name**             | **Type** | **Description**                                     | **Default value** |
|-----------------------------|----------|-----------------------------------------------------|-------------------|
| postalCodeNearest           | Boolean  | Select among nearest postal codes (approximate geographic distance)    | false             |
| roadTypeMask                | Boolean  | Mask road type (for example, street, avenue)        | true              |
| postalCodeNearestK          | Integer  | Number of closest postal codes from which to select | 10                |
| countryMask                 | Boolean  | Mask country                                        | true              |
| postalCodeMask              | Boolean  | Mask postal code                                    | true              |
| numberMask                  | Boolean  | Mask number                                         | true              |
| cityMask                    | Boolean  | Mask city                                           | true              |
| maskPseudorandom            | Boolean  | Mask based on pseudorandom function                 | false             |
| streetNameMask              | Boolean  | Mask street name                                    | true              |

#### ATC

>   Masks an Anatomical Therapeutic Chemical (ATC) code with the option to preserve certain levels.

| **Option name**       | **Type** | **Description**                  | **Default value** |
|-----------------------|----------|----------------------------------|-------------------|
|  maskLevelsToKeep     | Integer  | Number of levels to retain (1-4) | 4                 |

#### BINNING

>   Replaces a numerical input value with an interval that contains the value. For example, the value 5
>   could be replaced with the string 0-9.  
>   The interval's width is configurable, but is constant across the domain of values.  The range
>   is inclusive on the lower bound and exclusive on the upper bound. The format in which the interval is presented is 
>   configurable.  For examples of valid formats, see the java documentation for java.util.Formatter.

| **Option name**       | **Type** | **Description**                         | **Default value** |
|-----------------------|----------|-----------------------------------------|-------------------|
| binSize               | Integer  | The range of the interval               | 5                 |
| format                | String   | The format of the replacement range     | %s-%s             |
| startValue            | Integer  | The lower bound of the range            | 0                 |
| useStartValue         | Boolean  | Use the startValue as the lower bound   | false             |

#### CITY

>   Masks a city with a randomly selected city, optionally from among its nearest 
>   neighboring cities (approximate geographical distance).

| **Option name**        | **Type** | **Description**                         | **Default value** |
|------------------------|----------|-----------------------------------------|-------------------|
| maskClosest            | Boolean  | Select one of the nearby cities.        | false             |
| maskClosestK           | Integer  | Number of closest cities to select from | 10                |
| maskPseudorandom       | Boolean  | Mask based on pseudorandom function     | false             |

#### CONDITIONAL

   The Data De-Identification Service supports the **conditional** masking of a
   data element, based on the value of another data element that appears in the
   same FHIR Resource. That is, the masking method performed on a data element
   A (target field), depends on whether another data element B (condition
   field) in the same Resource satisfies a specified condition.

   The conditional masking provider supports a single configuration option,
   conditional.mask.ruleSet, which contains a JSON array consisting of a set of
   one or more masking providers, along with their associated conditions.
   See the example below.

   **Note:** The array conditions are processed in the order that they are listed. Data
   element A is masked by the first data masking provider (and its associated
   configuration options) that meets its condition.

   **Example 1: Conditional masking of a FHIR data element based on another FHIR data element with regular path**

```
    {
      "rules": [
        {
          "name": "CONDITIONAL_RULE",
          "maskingProviders": [
            {
              "type": "CONDITIONAL"
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
   regular path, that is, not a FHIR array data element.

   In the example, the objective of the CONDITIONAL ruleset is to mask
   the version data element of the Device FHIR Resource using the PSEUDONYM
   masking provider; this applies if the model data element of the Device FHIR Resource has
   the value **example_model**. Otherwise, the default HASH masking provider is
   used. By construction, if none of the conditions specified in the ruleset
   is met, the specified default option is used.

   Each array node of the conditional.mask.ruleSet JSON contains a data masking
   provider along with its optional configuration parameters. The condition
   properties that must be satisfied in order for this provider to be applied
   (for example, the PSEUDONYM masking provider in Example 1, along with the options
   **generateViaOptionsMinLength = 12** and
   **generateViaOptionsMaxLength = 12**) are applied to mask the
   version data element only when the model data element has the value
   **example_model**. When the first condition is met, the corresponding
   data masking provider is applied. Optionally, a default data masking
   provider can be used, if none of the conditions of the previous rules in the
   conditional ruleset is met. The default masking provider should be placed
   last in the list.

   **Note:** If a default data masking provider
   (without condition) is not provided, and if none of the conditions of the
   specified masking providers (with conditions) is met, then the data value
   is not masked. It is maintained as-is.

>   The configured CONDITIONAL JSON element consists of four properties:

>-  **field**: This contains the path of the condition field in the FHIR Resource
    type. The path may be either a regular path (that is, a non-array element path) or an
    array query format path. For a description, see [Introduction to the Data De-Identification service configuration](masking-config-overview.md).

>-   **value**: This contains the expected value of the condition field.

>-   **type**: This contains the data type of the condition field. Currently, only
    String data elements are supported.

>-   **operator**: The comparison operator to be used between the actual value of
    the condition field and the user-requested value. Currently, four String
    comparison operators are supported in CONDITIONAL: equals, equalsIgnoreCase,
    contains, and contained_in.

>   Operator equals checks if the actual value is the same as the user-requested
>   value. Operator equalsIgnoreCase performs the same check, but ignores the case
>   (capitalized versus non-capitalized letters). The contains operator checks if the
>   value of the condition field contains, as a subset, the user-specified value.
>   Finally, the contained_in operator checks if the value of the condition field is
>   contained in the user specified-value.


   **Example 2: Conditional masking of a FHIR data element, based on another FHIR data element value in an array node**

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
                    "field": "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)",
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
   array node path. In this example, the objective of the CONDITIONAL ruleset is
   to mask the status data element of the Device FHIR Resource using the
   PSEUDONYM masking provider. This applies only if the Device Resource contains both an
   extension array node with a URL of
   <http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName>
   and a valueString value **Asthma_Inhaler**. Otherwise, the original value of
   the status data element is maintained as-is.

#### CONTINENT

>   Masks a continent by replacing it with a randomly selected continent, optionally 
>   from among its nearest neighboring continents (approximate geographic distance from the approximate
>   centroid of each continent's bounding box).

| **Option name**         | **Type** | **Description**                            | **Default value** |
|-------------------------|----------|--------------------------------------------|-------------------|
| maskClosest             | Boolean  | Select one of the nearest continents.      | false             |
| maskClosestK            | Integer  | Number of neighbors for nearest continents | 5                 |

#### COUNTRY

>   Replaces a country with a randomly selected country, optionally from among its nearest neighboring 
>   countries (approximate geographic distance).

| **Option name**           | **Type** | **Description**                                     | **Default value** |
|---------------------------|----------|-----------------------------------------------------|-------------------|
| maskClosestK              | Integer  | Number of nearest countries to from which to select | 10                |
| maskClosest               | Boolean  | Select one of the near countries.                   | false             |
| maskPseudorandom          | Boolean  | Mask based on pseudorandom function                 | false             |

#### COUNTY

>   Masks a county by replacing it with a random county.

| **Option name**          | **Type** | **Description**                     | **Default value** |
|--------------------------|----------|-------------------------------------|-------------------|
| maskPseudorandom         | Boolean  | Mask based on pseudorandom function | false             |

#### CREDIT_CARD

>   Masks a credit card number with the option to preserve the issuer, for example,
>   (VISA, Mastercard®, American Express®, and Discover®).

| **Option name**            | **Type** | **Description** | **Default value** |
|----------------------------|----------|-----------------|-------------------|
| issuerPreserve             | Boolean  | Preserve issuer | true              |

#### DATEDEPENDENCY

>   Masks a date and time value if it is within a given number of days of another date and time value in the same input message.
>   If masking occurs, the year and time, if any, are removed and the value returned in **dd/MM** format, as in *20/04*.
>   
>   Within the input message, the value to compare to the masked value must be present in the same parent JSON object 
>   as the masked value.  In other words, the path to the comparison value must be the same as the path to the masked value
>   except for the final element name.  If the comparison value is not found, no masking occurs.  If the comparison value is 
>   found, but cannot be parsed, the masked value is protected according to the unexpected value handling configuration as 
>   described in "Handling unexpected input values" below.

This privacy provider supports these options:

| **Option name**                                 | **Type** | **Description**                                                                                             | **Default value** |
|-------------------------------------------------|----------|-------------------------------------------------------------------------------------------------------------|-------------------|
| datetimeYearDeleteNIntervalCompareDate          | String   | The FHIR element name of the date value that will be compared with the masked date                          | null              |
| dateYearDeleteNDaysValue                        | Integer  | The maximum number of days separating the masked value and the comparison value for masking to occur        | 365               |

   The following provides an example that illustrates the use of the
   DATEDEPENDENCY provider. Specifically, we consider the Patient FHIR
   Resource and the birthDate and deceaseDateTime FHIR data elements that it
   contains.

   The rule that we want to apply regards deleting the year of birth from the birthDate of a
   patient for those patients who died when they were not more than 
   five years old. For this example, the interval between the birthDate and the
   deceaseDateTime elements is calculated, and if it is found to be less than or equal to five
   years (≅ 1825 days), then the year is removed from the birthDate element.

   **Note:** The values in the example are for demonstration purposes only.

**Example 3 – Protecting the year of birth for patients who died at no more than 
5 years old:**

```
    {
      "rules": [
        {
          "name": "DateDependencyRule",
          "maskingProviders": [
            {
              "type": "DATEDEPENDENCY",
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
          "Patient"
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

Masks datetime (timestamp) objects. There are several options supported,
for example, shifting dates, generalizing to month, and generalizing to year.
Additional options include adding random offsets to the various datetime
elements, for example, years, months, days, hours, and seconds). If
multiple options are set to true in the datetime masking algorithm, the
following order is respected.

These examples are for the 10th of January 2016:

1.  Override with default or specified value, for example, **90+** or **Over 90 y.o.**.

2.  Shift the date by constant amount.

3.  Generalize to week number/year, for example, 02/2016.

4.  Generalize to month/year, for example, 01/2016.

5.  Generalize to quarter year, for example, 01/2016.

6.  Generalize to year, for example, 2016.

7.  Generalize to N-year interval, for example, 2015-2019.

8.  Generalize to year (for example, 1927) and mask any age over 90.

9.  Generalize to month/year (for example, 02/1927) and mask any age over 90.

10. Add random offsets to year, month, day, hour, minutes, and seconds.

11. Apply maximum years ago.

12. Apply maximum days ago.

If the override option of the provider is set to **True**, then the override
is processed first. If the rule criteria are met, all other options
of the DATETIME provider are ignored.


The following formats are supported by the DATETIME masking provider by default:

| **Supported date / datetime format**         | **Example of recognized input value** |
|----------------------------------------------|---------------------------------------|
| yyyy-mm-ddThh:mm:ss+&#124;-(hh:mm&#124;Z)    | 2008-09-15T15:53:00Z (default)        |
| dd-MM-yyyy                                   | 24-12-2018                            |
| dd-MMM-yyyy                                  | 24-DEC-2018                           |
| yyyy-MM-dd                                   | 2018-12-24                            |
| dd/MM/yyyy                                   | 24/12/2018                            |
| yyyy/MM/dd                                   | 2018/12/24                            |
| dd-MM-yyyy[ HH:mm:ss ]                       | 24-12-2018 12:01:12                   |
| yyyy-MM-dd[ HH:mm:ss ]                       | 2018-12-24 12:01:12                   |
| dd/MM/yyyy[ HH:mm:ss ]                       | 24/12/2018 12:01:12                   |
| yyyy/MM/dd[ HH:mm:ss ]                       | 2018/12/24 12:01:12                   |


 The DATETIME masking provider supports the following configuration options.

**Options to change the recognized date and time formats**
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| formatFixed                                       | String   | Datetime format                                                                                                                                                                                                                                                  | null              |

If a value for `formatFixed` is provided, it will be the only format the privacy provider will recognize.  The format must at minimum include a year component.  The given value must be valid as per the java.time.format.DateTimeFormatter class.


**Options to return a fixed value if the year is a given number of years ago**
   
| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| overrideMask                                      | Boolean  | Mask the datetime value with a fixed value if the datetime is from a year that is a given number of years ago or greater.                                                                                                                                                      | false             |
| overrideYearsPassed                               | Integer  | Number of years between the year of the original value and current year for the fixed value to be returned                                                                                                                                                                                                                               | 0                 |
| overrideValue                                     | String   | (optional) Value returned if the date and time meets the number of years threshold.  If not specified, the overrideYearsPassed value followed by the plus sign ('+') is returned (for example, 90+).  | null              |

**Options to shift by a constant amount**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| maskShiftDate                                     | Boolean  | Shift by a constant amount                                                                                                                                                                                                                                  | false             |
| maskShiftSeconds                                  | Integer  | Number of seconds to shift                                                                                                                                                                                                                                          | 0                 |

**Options to generalize a date and time**
   
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
| generalizeMonthyearMaskAgeOver90                  | Boolean  | Generalize to month/year, and mask any age \>= 90 by updating the date-of-birth to reflect 90 years from the current system date.                                                                                                                                 | false             |                                                                                                              | false             |

  
   **Options to mask a year**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearMask                                          | Boolean  | Mask year                                                                                                                                                                                                                                                        | true              |
| yearRangeDown                                     | Integer  | Mask year range downwards                                                                                                                                                                                                                                        | 10                |
| yearRangeUp                                       | Integer  | Mask year range upwards                                                                                                                                                                                                                                          | 0                 |

   **Options to mask a month**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| monthMask                                         | Boolean  | Mask month                                                                                                                                                                                                                                                       | true              |
| monthRangeDown                                    | Integer  | Mask month range downwards                                                                                                                                                                                                                                       | 12                |
| monthRangeUp                                      | Integer  | Mask month range upwards                                                                                                                                                                                                                                         | 0                 |
**Options to mask a day**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| dayMask                                           | Boolean  | Mask day                                                                                                                                                                                                                                                         | true              |
| dayRangeDownMin                                   | Integer  | Mask day range downwards minimum                                                                                                                                                                                                                                 | 0                 |
| dayRangeDown                                      | Integer  | Mask day range downwards maximum                                                                                                                                                                                                                                 | 7                 |
| dayRangeUpMin                                     | Integer  | Mask day range upwards minimum                                                                                                                                                                                                                                   | 0                 |
| dayRangeUp                                        | Integer  | Mask day range upwards maximum                                                                                                                                                                                                                                   | 0                 |
   
   **Options to mask an hour**

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

   **Options to shift the year if at a certain age**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearMaxYearsAgoMask                               | Boolean  | Mask year if it exceeds the maximum years ago from the current year                                                                                                                                                                                              | false             |
| yearMaxYearsAgo                                   | Integer  | Maximum years ago from the current year                                                                                                                                                                                                                          | 0                 |
| yearShiftFromCurrentYear                          | Integer  | Years to shift current year backwards                                                                                                                                                                                                                            | 0                 |
| dayMaxDaysAgoMask                                 | Boolean  | Mask year if it exceeds the maximum days ago from the current day                                                                                                                                                                                                | false             |
| dayMaxDaysAgo                                     | Integer  | Maximum days ago from the current day                                                                                                                                                                                                                            | 0                 |
| dayShiftFromCurrentDay                            | Integer  | Days to shift current date backwards                                                                                                                                                                                                                             | 0                 |
| yearMaxYearsAgoOnlyYear                           | Boolean  | Return only the shifted year value, not including month/day, if the shift occurs                                                                                                                                                                                                | false             |

   **Options to delete the year**

| **Option name**                                   | **Type** | **Description**                                                                                                                                                                                                                                                  | **Default value** |
|---------------------------------------------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| yearDelete                                        | Boolean  | Remove the year and return only the day and month                                                                                                                                                                                                  | false             |
| yearDeleteNdays                                   | Boolean  | Remove the year and return only the day and month, if the date and time is after the given number of days ago                                                                                                                                                                                                                       | false             |
| yearDeleteNinterval                               | Boolean  | Remove the year and return only the day and month, if the date and time is within the given number of days from a given date and time                                                                                                                                                                                                                       | false             |
| yearDeleteNdaysValue                              | Integer  | The number of days ago                                                                                                                                                                                                  | 365               |
| yearDeleteNointervalComparedateValue              | String   | The date and time to compare to the value being masked                                                                                                                                                                                                                               | null              |

#### DATETIME_CONSISTENT_SHIFT

>   Masks a date or date and time value by shifting the date a number of days.  The number of days to shift the date is chosen
>   from a configured range.  However, the same number of days is used for each date related to the same principle or patient.
>   A field that identifies the principle to whom the target value relates must be present at the same location in each document
>   containing a value to be masked with this privacy provider.  

| **Option name**        | **Type** | **Description**                                                                                                                | **Default value**  |
|------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------|--------------------|
| patientIdentifierPath  | String   | A path using JSON Pointer specification syntax to the field in the source document containing the principle                    | /patient/reference |
| dateShiftMinimumDays   | Integer  | The minimum number of days (inclusive) to shift the date, must be >= 0                                                         | 1                  |
| dateShiftMaximumDays   | Integer  | The maximum number of days (inclusive) to shift the date, must be >= `dateShiftMinimumDays`                               | 365                |
| dateShiftDirection     | String   | The direction the date should be shifted in relation to the original date.  Supported values are described in the table below. | beforeOrAfter      |
| salt                   | String   | An optional, arbitrary set of characters used as part of the calculation of the number of days to shift the date.  Supplying a value prevents the principle value from being the only input to that calculation.  Null and values consisting of all whitespace characters are ignored. | null   |
| customFormats          | String Array   | Optional additional input formats to recognize when parsing date values.  More details below.                            | null               |

If more than one masking rule definition is used to mask date values related to the same principle, the values 
of the `dateShiftMinimumDays`, `dateShiftMaximumDays`, `dateShiftDirection`, and `salt` parameters 
should be the same in all of those masking rules.  Changing any of those values can cause the number of days for 
the date shift to be different even if the value at the `patientIdentifierPath` field remains the same.

If the input document does not contain a text field at the path supplied in the `patientIdentifierPath` 
parameter or if the value of the field is null or consists of only whitespace characters, the configured 
handling of unexpected input values is applied.  See the **Handling unexpected input values** section below.

**Supported values for `dateShiftDirection`**

| **Value**        | **Description**                                                |
|------------------|----------------------------------------------------------------|
| before           | Shift the date before the original date                        |
| beforeOrAfter    | Shift the date either before or after the original date        |
| after            | Shift the date after the original date                         |

**Supported and custom input formats**

The provider supports input values in any of these formats.

|**Format**                                | **Examples**                            |
|------------------------------------------|-----------------------------------------|
| yyyy-MM-ddTHH:mm:ss.nnnnnnnnn+hh:mm      | 2008-09-14T15:53:02.123456789+02:00     |
| yyyy-MM-ddTHH:mm:ss+hh:mm                | 2008-09-14T15:53:02-05:00               |
| yyyy-MM-ddTHH:mm+hh:mm                   | 2008-09-14T15:53-06:00                  |
| yyyy-MM-ddTHH:mm:ss.nnnnnnnnnZ           | 2008-09-14T15:53:02.123456789Z          |
| yyyy-MM-ddTHH:mm:ssZ                     | 2008-09-14T15:53:02Z                    |
| yyyy-MM-ddTHH:mmZ                        | 2008-09-14T15:53Z                       |
| dd-MMM-yyyy                              | 24-DEC-2018                             |
| yyyy-MM-dd                               | 2018-12-24                              |
| yyyy/MM/dd                               | 2018/12/24                              |
| yyyy-MM-dd HH:mm:ss                      | 2018-12-24 12:01:12                     |
| yyyy/MM/dd HH:mm:ss                      | 2018/12/24 12:01:12                     |
| dd-MM-yyyy                               | 16-04-1967                              |
| dd/MM/yyyy                               | 16/04/1967                              |
| dd-MM-yyyy HH:mm:ss                      | 16-04-1967 13:14:15                     |
| dd/MM/yyyy HH:mm:ss                      | 16/04/1967 13:14:15                     |

The provider formats the shifted date using the same pattern that matched the original input value.  
Minor changes to precision and formatting between the original value and the shifted value can occur, however.
For formats that include text, such dd-MMM-yyyy which contains the abbreviation for the month name,
the recognized values for the text portions are based on the default locale of the system that is
performing the masking.  For example, the recognized abbreviation for January might be **Jan** when 
the default locale of the system is US English, **Jan.** when it is Canadian English, and **janv.** 
when it is Canadian French.

If the input contains values to be protected that do not match any of these patterns, additional formats 
can be added using the `customFormats` configuration parameter.  Input is matched to custom formats 
in the order the custom formats appear in the configuration parameter and before it is matched to any of 
the built-in formats.  See the documentation for the Java *java.time.format.DateTimeFormatter* class for 
information about the syntax used to specify custom formats.  Time components and time zone name or offset 
components are optional.  The format must capture enough information about the date, however, for the 
provider to be able to calculate a year, month, and day.  For example, _yyDDD_ (Julian date, year and 
day of year) is a valid pattern because the provider can calculate the information it needs.  However, 
_MMdd_ (month and day) or _HHmm_ (hour and minute) are not adequate as the year, month, and day cannot 
be calculated.

If an input value does not match any of the available formats or is matched to a custom format that does not 
capture sufficient date information, the configured handling of unexpected input values is applied.  See the 
**Handling unexpected input values** section below.

**Principle identifier fields and ordering of rule assignments**

This provider does not apply any privacy protection to the field indicated by the `patientIdentifierPath` 
parameter, but privacy protection for that field might also be desired and privacy protection rules can be 
applied to it in other sections of the complete privacy protection configuration.  In that case, it is 
important to be aware of the order in which the privacy rules are applied.  In some cases it might be desirable 
to base the consistent date shifting on the original value of the principle field.  In others, it might be 
desirable to base the consistent date shifting on the principle field after that field has already been masked 
by other privacy protection rules.

The order in which the privacy providers are applied is controlled by the order of the members of the 
`config.json.maskingRules` array in the complete masking configuration.  The service examines each input 
document to determine its message type and then applies the rule assignments for that message type in the order 
they appear in the array.  For example, consider the following masking configuration:

````
{
    "rules": [
        {
            "name": "ConsistentShiftRule",
            "maskingProviders": [
                {
                    "type": "DATETIME_CONSISTENT_SHIFT",
                    "patientIdentifierPath": "/patient/reference",
                    "dateShiftMinimumDays": "10",
                    "dateShiftMaximumDays": "31",
                    "dateShiftDirection": "before",
                    "unexpectedInputHandling": "NULL",
                    "customFormats": [
                        "yyyy-MM-dd'T'HH:mm:ss z",
                        "yyDDD"
                    ]
                }
            ]
        },
        {
            "name": "HashRule",
            "maskingProviders": [
                {
                    "type": "HASH",
                    "algorithmDefault": "SHA-256"
                }
            ]
        }        
    ],
    "json": {
        "schemaType": "FHIR",
        "messageTypes": [
            "MedicationOrder"
        ],
        "messageTypeKey": "resourceType",       
        "maskingRules": [
            {
                "rule": "ConsistentShiftRule",
                "jsonPath": "/fhir/MedicationOrder/dateWritten"
            },
            {
                "rule": "HashRule",
                "jsonPath": "/fhir/MedicationOrder/patient/reference"
            }
        ]
    }
}
````

This configuration processes JSON input documents that have the value *MedicationOrder* at the path */resourceType*.  
Input documents that do not have this value at this path are ignored.  In the *rules* section, it contains a rule 
named *ConsistentShiftRule* that uses the DATETIME_CONSISTENT_SHIFT provider to move dates from 10 to 31 days after 
the original date.  The actual number of days is chosen by performing calculations based on the value found at the 
*/patient/reference* path.  It contains another rule named *HashRule* that uses the HASH provider to hash fields 
using the SHA-256 algorithm.  In the *maskingRules* section, the first assignment applies *ConsistentShiftRule* to 
the value at the path */dateWritten* and the second applies *HashRule* to the value at the path */patient/reference*.  

Since the assignment of the *ConsistentShiftRule* to the */dateWritten* path appears before the assignment of the 
*HashRule* to the */patient/reference* path, the value at */patient/reference* will have its original value when 
used for the consistent date shifting.

If the *maskingRules* section was written like this instead:

````
        "maskingRules": [
            {
                "rule": "HashRule",
                "jsonPath": "/fhir/MedicationOrder/patient/reference"
            },
            {
                "rule": "ConsistentShiftRule",
                "jsonPath": "/fhir/MedicationOrder/dateWritten"
            }            
        ]
````

the HASH would be applied to the value at */patient/reference* first and the DATETIME_CONSISTENT_SHIFT provider 
would use the hashed version of the value at */patient/reference* to calculate the number of days to shift the 
date at */dateWritten*.

#### EMAIL

>   Masks e-mail addresses with the option to preserve certain levels of the
>   host domain.

| **Option name**        | **Type** | **Description**                                                                                             | **Default value** |
|------------------------|----------|-------------------------------------------------------------------------------------------------------------|-------------------|
| preserveDomains | Integer  | Number of domains to preserve starting from the right; if value = -1 then all domains will be preserved (that is, everything after \@) | 1                 |
| nameLength      | Integer  | Length of username to generate; if value = -1, then the username will be 5 to 8 characters long. 0 is invalid      | \-1               |

#### FPE

FPE stands for Format-Preserving Encryption.  This provider encrypts and replaces a set of characters in the input value 
using an encryption algorithm compliant with the NIST FPE FF3-1 standard.  The encrypted character sequence is of the 
same length as the original sequence.  Any characters in the input that are not part 
of the set being encrypted are retained in their current positions.  This allows the encrypted value to be presented with 
the same formatting and separator characters as the original value.

An important aspect of the NIST FPE FF3-1 standard is that if the same value is presented for encryption multiple times
using the same encryption keys, the output is the same each time.  Additionally, when the keys are the same, no input value 
will produce the same output value as any other input value.  The standard provides unique, repeatable encryption.  Finally,
if you have access to the encryption keys, the algorithm can be used to re-produce the original value, as the encryption is
reversible.  Note, however, that the privacy provider itself does not currently offer a decryption configuration.

The implementation of the NIST FPE FF3-1 standard places some restrictions on the input value, primarily which characters can be encrypted 
and the length of the sequence that can be encrypted.  These restrictions are detailed below.  The privacy provider 
supports an option to apply the algorithm in situations where the minimum number of input characters is not met, although
the algorithm is not compliant with the NIST FPE FF3-1 standard in those cases.  That option is detailed below as well.

To illustrate the capabilities of the provider, consider the following example input value:

```
A10-B20-C30-D40-E50
```

If the provider is configured to encrypt the digits, the resulting output value would look something like this:

```
A28-B92-C67-D99-E21
```

If the provider is configured to encrypt the upper-case characters, given the same input, the resulting output value would look 
something like this:

```
X10-U20-D30-M40-P50
```

Internally, the characters not selected for encryption are removed, the remaining character sequence is encrypted, and the removed 
characters are replaced in the output in their original positions.  Because the separator characters are not part of the encryption, they 
have no impact on the encrypted sequence.  For example, when digits are being encrypted and the keys remain unchanged, all of these
input values would yield outputs that have the same sequence of digits, since the sequence to be encrypted in all of these values is
the same - 1020304050:

```
A10-B20-C30-D40-E50
A10+B20+C30:D40:E50
Z10xZ20xZ30xZ40xZ50
```


| **Option name**               | **Type**           | **Description**                                                                                             | **Default value** |
|-------------------------------|--------------------|-----------------------------------------------------------------------------------|----------------|
| key                           | string             | The encryption key - a string of 32, 48, or 64 characters, each character must be 0-9 or a-f (case-sensitive)        | none (required) |
| tweak                         | string             | Adjustment to the encryption key - a string of 16 characters, each character must be 0-9 or a-f (case-sensitive)        | none (required) |
| inputType                     | string (see below) | Determines which characters are encrypted and which are symbols or separators that are not encrypted                | DIGITS          |
| padding                       | string (see below) | Whether and where pad characters should be added to the input value to meet minimum encryption length requirements        | NONE            |


The `key` and `tweak` values are the encryption key material.  If the same values for these parameters are used, a given input input will always 
generate the same output value, even across multiple privacy protection sessions.
 

**Supported values for `inputType`**
| Value                           | Description                                                      |  Number of Encrypted Characters Required| Supports padding | Pad char |
|----------------|-----------------------------------------------------------------------------------|---------|-------|----|
| DIGITS                          | Only characters 0-9 are encrypted                                | 6-56    | true  | 0  |
| LETTERS_LOWER                   | Only characters a-z are encrypted                                | 5-40    | true  | a  | 
| LETTERS_UPPER                   | Only characters A-Z are encrypted                                | 5-40    | true  | A  | 
| LETTERS_INSENSITIVE_AS_LOWER    | Only characters a-z and A-Z are encrypted. Upper and lower case versions of the same letter are considered interchangeable.  Letters in the output are lower case.      | 5-40    | true  | a  | 
| LETTERS_INSENSITIVE_AS_UPPER    | Only characters a-z and A-Z are encrypted. Upper and lower case versions of the same letter are considered interchangeable.  Letters in the output are upper case.      | 5-40    | true  | A  | 
| LETTERS_INSENSITIVE_AS_ORIGINAL | Only characters a-z and A-Z are encrypted. Upper and lower case versions of the same letter are considered interchangeable.  Letters in the output have the case of the original letter in the same position.  | 5-40    | true  | a  | 
| LETTERS_SENSITIVE               | Only characters a-z and A-Z are encrypted. Lower case letters are replaced with lower case letters and upper case letters are replaced with upper case letters.  | 5-40 lower case letters and 5-40 upper case letters    | false   | |
| DIGITS_LETTERS_LOWER            | Only characters 0-9 and a-z are encrypted. Digits and letters are considered to be from the same character set.  A digit in the original value could be replaced with a lower case letter in the output and vice-versa.   | 4-36    | true  | 0  | 
| DIGITS_LETTERS_UPPER            | Only characters 0-9 and A-Z are encrypted. Digits and letters are considered to be from the same character set.  A digit in the original value could be replaced with an upper case letter in the output and vice-versa.  | 4-36    | true  | 0  | 
| DIGITS_LETTERS_INSENSITIVE_AS_LOWER  | Only characters 0-9, a-z, and A-Z are encrypted. Upper and lower case versions of the same letter are considered interchangeable.  Digits and letters are considered to be from the same character set.  A digit in the original value could be replaced with a letter in the output and vice-versa.  Letters in the output are lower case.  | 4-36    | true  | 0  | 
| DIGITS_LETTERS_INSENSITIVE_AS_UPPER  | Only characters 0-9, a-z, and A-Z are encrypted. Upper and lower case versions of the same letter are considered interchangeable.  Digits and letters are considered to be from the same character set.  A digit in the original value could be replaced with a letter in the output and vice-versa.  Letters in the output are upper case.  | 4-36    | true  | 0  | 
| DIGITS_LETTERS_SENSITIVE             | Only characters 0-9, a-z, and A-Z are encrypted. Digits are replaced with digits, lower case letters are replaced with lower case letters, and upper case letters are replaced with upper case letters.  | 6-56 digits, 5-40 lower case letters, and 5-40 upper case letters    | false |  | 


As indicated in the above chart, the set of characters to be encrypted determines the minimum and maximum sequence lengths that are supported.
If an input value contains more characters to be encrypted than is supported for the selected inputType, the configured handling of unexpected 
input values is applied.  See the **Handling unexpected input values** section below.  If an input value contains fewer characters than are
required for the selected inputType, unexpected input handling is applied unless Padding has been configured.

##### Padding
Padding is a convenience function offered by the privacy provider.  If configured, it automatically adds additional characters to the input sequence
to be encrypted when the input does not have enough characters itself.  This allows the algorithm to be applied to generate an encrypted sequence 
when it otherwise couldn't be.  There are some important issues to note when configuring padding:
1. The output will be longer than the input, as all the characters produced are retained.  If padding is applied to the front of the input, the 
positions of the non-encrypted characters are shifted to the right by the number of padding characters that had to be added.  For example, 
if using front padding while encrypting digits, -9 would yield a value with the format 82739-8.
1. The padded input value cannot be distinguished from an actual input value of greater length that has actual characters that match the padding 
characters.  For example, if using front padding while encrypting digits, the actual values 9, 09, and 000009 will all produce the same encrypted
sequence, assuming the encryption keys are unchanged.
1. If an external decryption process is applied to the output value, the original value plus the padding would be returned.
Without some additional knowledge of the input, there would be no way
to determine whether a padding character was added as padding or was an actual part of the original value.
1. In the cases where padding is applied, the encryption algorithm cannot be claimed to be in compliance with NIST FPE FF3-1.

**Supported values for `padding`**
| Value   | Description                                                                    |
|---------|--------------------------------------------------------------------------------|
| NONE    | No padding occurs                                                              |
| FRONT   | Any necessary padding characters are added at the beginning of the input value |
| BACK    | Any necessary padding characters are added at the end of the input value       |

#### GENDER

>   Replaces gender information with a randomly-chosen gender. This provider
>   does not have any configuration options.

#### GENERALIZE

   Replaces one or more specified original values with a specified general
   category term to which these values belong. If a general category term is
   not defined for the input values, the input can optionally be replaced with a
   default term.  The aim of
   GENERALIZE is to protect infrequent data values that appear in a
   data element by replacing them with a general category term.

   The GENERALIZE provider supports rules with value sets of the form R:

   {*o*1, *o*2, …, o*n} *r*

   where
   * *o*i is an original data value
   * *r* is the replacement value

Multiple such rules can be specified and provided as input to
GENERALIZE. For example, in a data element that stores the religion of
individuals, rule R: {“Daoism”, “Shinto”, “Confucianism”} “Eastern Asian
Religions”, can be used to protect the identity of the corresponding
individuals by associating them with the more general category of “Eastern
Asian Religions”. 

   When many values in a data element are expected to be infrequent, GENERALIZE
   can be used to capture those original data values that must be maintained in
   the output data. In this case, only those values that are **not** specified
   are replaced with a configurable general category term. For example, in
   a data element that stores the spoken languages of individuals, GENERALIZE
   can be configured to replace all languages that are not listed as **Spanish**
   or **English**, with value **Other**.

It is also possible to configure *null* as the replacement value.  This supports removing prohibited values from the input data.

   **Options supported by GENERALIZE**

| **Option name**         | **Type** | **Description**                                                   | **Default value** |
|-------------------------|----------|-------------------------------------------------------------------|-------------------|
| maskRuleSet             | String   | The value sets as a JSON array of objects in string format.       | N/A               |


   GENERALIZE can accept a number of data generalization value sets R1…Rm,
   each of which defines a set of original (or **source**) data values and a
   corresponding replacement (or **target**) value.

   Assume that **u** is an incoming data value from a data element to which
   GENERALIZE is applied and that R is a value set associated with this data
   element. GENERALIZE checks **u** against value set R and replaces it with
   the target value of R if **u** is found among the source values. Another value set
   R’ could be specified to replace value **u** only if it is **not** found
   among the source values specified in that value set. If multiple value sets are
   specified for the same data element, each value set is tested in sequence
   against the value **u** of the data element and the first value set
   that is found to contain **u** is triggered. If the value **u** does not
   match any of the value sets, the value **u** is retained.

   The value of the **maskRuleSet** parameter is a JSON array of objects written
   as a single string.  Each member of the array is a value set.  Value sets are
   evaluated in the order they appear in the array.  Each value set is a JSON object
   containing a **targetValue** property and either a 
   **sourceValueIn** **or** a **sourceValueNotIn** property.   

 -  **targetValue**: The value of this property is a string that is the replacement value 
    that will replace the original value of the data element if the value set is matched.

 -  **sourceValueIn**: The value of this property is a JSON array of original values.  
    If the incoming data
    value appears in this array, the original value of the data element is
    replaced with the **targetValue** property value.

   **Note:** If needed, you can denote that **any** value of the data element be replaced. 
   To do this, use the special value asterisk **(\*)** instead of any specific data values 
   in the sourceValueIn array.

-   **sourceValueNotIn**: The value of this property is the same as described for **sourceValueIn**
    except that the values in this array are original values that must be retained.  The
    original value is replaced with the **targetValue** property value only if the incoming
    data value is not in this array.  Note that the asterisk special value is **not** supported
    in this property.

Here are some examples of how the GENERALIZE privacy provider may be used.  
The values in the examples are for demonstration purposes only.

**Example 4: Generalize example**

```
   "rules": [
   {
      "name": "cityRule",
      "maskingProviders": [{
         "type": "GENERALIZE",
         "maskRuleSet": "[{\"targetValue\": \"Asian City\", \"sourceValueIn\": [\"Bangkok\", \"Manila\", \"Shanghi\", \"TaiPei\", \"Mumbai\"]}, {\"targetValue\": \"African City\", \"sourceValueIn\": [\"Addis Ababa\", \"Cairo\", \"Cape Town\", \"Lagos\"]}]" 
      }], 
         
         
     "maskingRules": [{
         "jsonPath": "/fhir/Patient/address/city",
         "rule": "cityRule"
     }],   
   
```

In the example above, a rule consisting of two value sets is applied to the
Patient address city data element. The value sets are examined sequentially as
follows:

1.  If the city value of the data element is one of the cities listed in
    the sourceValueIn property of the first value set (that is, any of **Bangkok**,
    **Manila**, **Shanghai**, **Taipei**, or **Mumbai**), then it is replaced with
    the value specified in the targetValue property value (that is, **Asian City**).
    No other value sets are examined.
    
1.  Otherwise, if the city value of the data element is one of the cities listed
    in the sourceValueIn property of the second value set (that is, **Addis Ababa**,
    **Cairo**, **Cape Town**, or **Lagos**), then it is replaced with the value
    specified in the targetValue property (that is, **African City**). No other value 
    sets, if any, are examined.

1.  Otherwise, since the city value of the data element has not been matched and
    there are no more value sets to examine, the original value is maintained.

**Example 5: Generalize example with special value**

```
     "maskRuleSet": "[{\"targetValue\": \"US City\", \"sourceValueIn\": [\"New York\", \"Chicago\", \"Dallas\", \"Minneapolis\", \"Boston\"]}, {\"targetValue\": \"Canadian City\", \"sourceValueIn\": [\"Toronto\", \"Vancouver\",\"Montreal\", \"Calgary\"]}, {\"targetValue\": \"Other\", \"sourceValueIn\": [\"*\"]}]"

```

>   In the example above, a rule consisting of three value sets is specified.  
>   As in Example 4, the value sets are evaluated sequentially and the first 
>   value set that applies to the data element value is used.  
>   If the incoming data value is not one of the cities listed in either of the
>   first two value sets, the incoming data value is replaced with the targetValue property 
>   (**Other**) of the third value set, since any incoming data value will match the asterisk
>   special value.

**Example 6: Generalize example with negation**

```
     "maskRuleSet": "[{\"targetValue\": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]"
```

>   In the example above, a rule with one value set is specified.  The value set 
>   contains the **sourceValueNotIn** property.  Therefore, if the value of the incoming data
>   element is any of the languages listed in the sourceValueNotIn property (**French** 
>   or **Spanish**), then the original value is maintained.  If the incoming
>   data element has any other value, it is replaced with the targetValue (**Other**).

#### GUID

>   Replaces a GUID with a randomly-chosen GUID. This provider
>   does not have any configuration options.

#### HASH

>   Hashes the original value using an algorithm, for example, SHA-256 and SHA-512.
>   The following hashing algorithms are supported: MD2 (as defined in RFC
>   1319), MD5 (as defined in RFC 1321), as well as SHA-1, SHA-256, SHA-384, and
>   SHA-512 (as defined in FIPS PUB 180-2).
>
>   **Note:** SHA-256 is a 256-bit hash function intended to provide 128 bits
>   of security against collision attacks, while SHA-512 is a 512-bit hash
>   function intended to provide 256 bits of security. To obtain a 384-bit hash,
>   truncate the SHA-512 output.
>
>   Optionally, a part of the original data value can be maintained. To do so,
>   specify a start offset and / or an end offset. This defines the part of
>   the value that must be replaced with the computed hash value. The start
>   offset must be greater or equal to zero (0, first position in the data value)
>   and less than the end offset, when an end offset is specified. The value
>   outside the start and end offsets can be either maintained or deleted. To
>   specify the behaviour of the algorithm in the case of invalid offsets, use
>   the configuration options.

| **Option name**                   | **Type** | **Description**                                                                                                                                                                                          | **Default value** |
|-----------------------------------|----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| algorithmDefault                  | String   | Default algorithm                                                                                                                                                                                        | SHA-256           |
| offsetOffsetMask                  | Boolean  | Mask using offsets                                                                                                                                                                                       | false             |
| offsetOffsetMaskDelete            | Boolean  | Mask using offsets. The part of the string that falls outside the [hashing.offset.begin, hashing.offset.end] will be deleted, and the result of hashing the string within these bounds will be returned. | false             |
| offsetBegin                       | Integer  | Offset from where to compute the hash. This must be a non-negative value. If not provided, the hash is computed from the beginning of the value (position 0). [inclusive]                             | \-1               |
| offsetEnd                         | Integer  | Offset to where to end the computation of the hash if specified, it must be greater than hashing.offset.begin. [exclusive]. If -1, it points to the end of the input string.                            | \-1               |
| salt                              | String   | Seed / salt to be used in hashing of the original data value. By default, no salt is used.                                                                                                               | null              |
| offsetInvalidOffsetValue          | Integer  | Value to be returned if the provided begin and/or end offsets are not valid: 1 – Return null, 2 – Return empty string, 3 – Return hash of the entire value                                               | 1                 |

#### HOSPITAL

>   Replaces a hospital name with a randomly-chosen one.

| **Option name**               | **Type** | **Description**                          | **Default value** |
|-------------------------------|----------|------------------------------------------|-------------------|
| maskPreserveCountry           | Boolean  | Select a hospital from the same country. | true              |

#### IBAN

>   Masks international bank account number (IBAN) account numbers with the option to preserve the country.

| **Option name**           | **Type** | **Description**       | **Default value** |
|---------------------------|----------|-----------------------|-------------------|
| maskPreserveCountry       | Boolean  | Preserve country code | true              |

#### ICDV9

>   Masks ICD-9 diagnosis codes. Codes can also be generalized to their
>   respective chapters or categories.

| **Option name**        | **Type** | **Description**                                       | **Default value** |
|------------------------|----------|-------------------------------------------------------|-------------------|
| generalizeToCategory   | Boolean  | Return the category associated with the input code    | true              |
| generalizeToChapter    | Boolean  | Return the chapter associated with the input code     | false             |

#### ICDV10

>   Masks ICD-10 diagnosis codes. Codes can also be generalized to their
>   respective chapters or categories.

>   By default, no actual version 10 ICD codes are provided with the service.  To use this provider, it will
>   be necessary to load ICD v10 codes into the service.  For more information about loading custom codes, 
>   see [Localization](localization.md).
>   Note that there might be licensing considerations for use of ICD v10 codes in some circumstances.


| **Option name**        | **Type** | **Description**                                       | **Default value** |
|------------------------|----------|-------------------------------------------------------|-------------------|
| generalizeToCategory   | Boolean  | Return the category associated with the input code    | true              |
| generalizeToChapter    | Boolean  | Return the chapter associated with the input code     | false             |

#### IMEI

>   Masks International Mobile Equipment Identity (IMEI) device identifiers.

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

| **Option name**                        | **Type** | **Description**                                              | **Default value** |
|----------------------------------------|----------|--------------------------------------------------------------|-------------------|
| maskFixedRadiusRandomDirection         | Boolean  | Randomize a point with a fixed radius but a random direction | false             |
| maskDonutMasking                       | Boolean  | Randomize a point in a donut-shaped                          | false             |
| maskRandomWithinCircle                 | Boolean  | Randomize a point in a circle                                | true              |
| offsetMaximumRadius                    | Integer  | Maximum offset radius (in meters)                            | 100               |
| offsetMinimumRadius                    | Integer  | Minimum offset radius (in meters)                            | 50                |

#### MAC_ADDRESS

>   Masks media access control (MAC) addresses with the option to preserve the vendor information.

| **Option name**            | **Type** | **Description**             | **Default value** |
|----------------------------|----------|-----------------------------|-------------------|
| maskingPreserveVendor      | Boolean  | Preserve vendor information | true              |

#### MAINTAIN

>   Retains the current value of the field.  This provider is commonly used when the option to
>   redact all fields without an assigned rule is specified, and some fields are to be retained.

#### MARITAL

>   Replaces a marital status with a randomly-chosen marital status. This provider
>   does not have any configuration options.

#### NAME

>   Masks the first and last names of individuals.

| **Option name**           | **Type** | **Description**                           | **Default value** |
|---------------------------|----------|-------------------------------------------|-------------------|
| maskingAllowUnisex        | Boolean  | Allow unisex names to be used for masking | false             |
| maskPseudorandom          | Boolean  | Provide pseudorandom consistence          | false             |
| maskGenderPreserve        | Boolean  | Preserve gender while masking             | true              |

#### NULL

>   Replaces the original data value with an empty string (“”), or with a NULL
>   value. The default is to return an empty string.

| **Option name**      | **Type** | **Description**                           | **Default value** |
|----------------------|----------|-------------------------------------------|-------------------|
| maskReturnNull       | Boolean  | Return NULL instead of empty string value | false             |

#### NUMBERVARIANCE

Masks a numeric data value by adding a random offset. There are two options
available. These are processed in the following order:

1.  Calculating the offset by randomly selecting a numeric value from within a
    pre-specified interval / range.

2.  Calculating the offset based on given percentages signifying the acceptable
    distance of the new value from the original value.

| **Option name**                 | **Type** | **Description**                                                                                 | **Default value** |
|---------------------------------|----------|-------------------------------------------------------------------------------------------------|-------------------|
| augmentMask                     | Boolean  | Augment the numeric data value                                                                  | false             |
| augmentLowerBound               | double   | Range interval lower bound                                                                      | 1.0               |
| augmentUpperBound               | double   | Range interval upper bound                                                                      | 10.0               |
| resultWithPrecision             | Boolean  | Result includes decimal digits for precision                                                    | false             |
| precisionDigits                 | Integer  | Number of decimal digits to keep when precision is set to true; -1 to provide maximum precision | -1                |
| maskLimitUp                     | double   | Up percentage limit                                                                             | 10.0              |
| maskLimitDown                   | double   | Down percentage limit                                                                           | 10.0              |

#### OCCUPATION

>   By default, this provider replaces an occupation with a randomly-selected occupation.
>   However, if the option **maskGeneralize** is set to **true**, this provider
>   generalizes an occupation to one of its associated occupational categories. The categories
>   of occupations are based on the 2010 Standard Occupational Classification (SOC).

| **Option name**            | **Type** | **Description**                   | **Default value** |
|----------------------------|----------|-----------------------------------|-------------------|
| maskGeneralize             | Boolean  | Generalize to occupation category | false             |

#### PHONE

>   Replaces a telephone number or fax number with a random one. There are options to
>   preserve the country code and area code of the original number.

| **Option name**            | **Type**      | **Description**                                                     | **Default value** |
|----------------------------|---------------|---------------------------------------------------------------------|-------------------|
| countryCodePreserve        | Boolean       | Preserve country code                                               | true              |
| areaCodePreserve           | Boolean       | Preserve area code                                                  | true              |
| phoneRegexPatterns         | String Array  | Array of regular expression to specify telephone number patterns    |                   |

>> phoneRegPatterns are used to specify telephone number patterns.  It is optional.  If unspecified, the telephone masking provider uses this format:

```
  ^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\d+)
```

>> To specify phoneRegexPatterns, the format uses Java pattern named groups. For more information, see:
>>         https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html.
>> The supported named grouops are: countryCode/areaCode/number.   number is a required group.  countryCode/areaCode are optional.
>> **Example**

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

>> The phoneRegexPatterns are used to match the following telephone numbers:
```
 +380 44 123 45 67
 (044) 123 45 67
 +380 (44) 1234567
```

The named groups countryCode/areaCode are used to identify the position of country code and area code respectively. For the configuration options countryCodePreserve and areaCodePreserve to work, the groups must be specified.  If none of the patterns in phoneRegexPatterns matched a telephone number to be masked, the output is null by default.

#### PSEUDONYM

>   Masks an original data value by replacing it with a randomly-generated data
>   pseudonym. The following describes the three options that are
>   supported by PSEUDONYM for the generation of data pseudonyms.

##### Generic pseudonym generation

   Supports the generation of pseudonyms with a length that is within a
   specified range. The range can consist of one or more types of these
   characters: uppercase and lowercase letters, digits, and special characters.

   Generic pseudonym generation is supported by setting:

>  generateViaOptionsEnabled = true

   The length of the computed pseudonym is within an interval, the borders
   of which are defined using these options:

>  generateViaOptionsMinLength

>  generateViaOptionsMaxLength

   To select the types of characters that can be used to produce the
   pseudonym, set the following flags to true or false. Doing this, in
   enables or disables a specific character type:

>  generateViaOptionsGenerateUppercase

>  generateViaOptionsGenerateLowercase

>  generateViaOptionsGenerateDigit

>  generateViaOptionsGenerateSpecial

##### Regular expression-based pseudonym generation

>   Supports the generation of pseudonyms that follow a specified pattern,
>   provided as a regular expression. There are two ways to specify the pattern
>   used to generate a data pseudonym:
>
>   - Provide the actual pattern itself.
>   - Provide the name of the pattern.
>
>   If both the actual pattern and the pattern name are provided, then the
>   pattern is used and the pattern name is ignored.

###### Specifying the pattern

>   To specify the pattern itself, the onboarding team needs to provide as a text
>   string as input, a string that looks like a regular expression. The
>   Data De-Identification Service uses this to produce a pseudonym.

>   Here is the allowable syntax of the input string / pattern for producing a data
>   pseudonym:

| **Syntax frames**     |                                                                                                                                                                              |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [                     | Begins a character class. If you use a single literal (for example, \\u1234, A, 1), then the literal can be used instead of a full-character class unless a specific class is desired.          |
| x-y                   | A character literal range to allow. The second index must be greater than the first.                  |
| ]                     | Ends a character class.                                                                                                                                                    |
| (                     | Begins an optional class. These can be nested and include any other item.                                                                                                    |
| \                     | Separates optional items within an optional class.                                                                                                                           |
| )                     | Closes an optional class.                                                                                                                                                    |
| {                     | Begins multiplicity. If not specified, multiplicity is 1.                                                                                                                    |
| m1                    | Minimum number of the proceeding character class. Used also as maximum if no maximum is specified.                                                                              |
| , m2                  | Maximum number of this proceeding character class.                                                                                                                           |
| }                     | Close multiplicity.                                                                                                                                                    |
| **Universal escapes** |                                                                                                                                                                              |
| \\{                   | A literal opening brace ‘{‘ character.                                                                                                                                                    |
| \\}                   | A literal closing brace ‘}‘ character.                                                                                                                                                    |
| \\]                   | A literal opening bracket ‘]‘ character.                                                                                                                                                    |
| \\[                   | A literal closing bracket ‘[‘ character.                                                                                                                                          |
| \\\\                  | A literal backslash ‘\\‘ character.                                                                                                                                          |
| \\-                   | A literal hyphen ‘-‘ character.                                                                                                                                          |
| \\(                   | A literal opening parenthesis ‘(‘ character.                                                                                                                                          |
| \\)                   | A literal closing parenthesis ‘)‘ character.                                                                                                                                          |
| \ \\|                 | A literal vertical bar ‘\|‘ character.                                                                                                                                           |
| \\pHHHHHHHH           | A UTF-32 character. May be included in ranges, for example: (\\pHHHHHHHH-\\pHHHHHHHH).                                                                                       |
| \\d                   | The set of digits.                                                                                                                                                           |
| \\l                   | The set of lowercase ASCII letters.                                                                                                                                          |
| \\u                   | The set of uppercase ASCII letters.                                                                                                                                          |

   **Example**

   To generate pseudonyms of variable lengths (from 5 to 10
   characters), consisting only of the uppercase letters “N”, “S”, “E”, and
   “W”, set the following option:

>   generateViaPatternPattern = “(N\|S\|E\|W){5,10}”

###### Using a pre-specified pattern

>   Instead of specifying that the pattern itself as part of the call made to the
>   PSEUDONYM provider, the pattern can be pre-specified and stored in a
>   resource file under a selected pattern name. Subsequently, the PSEUDONYM provider can be
>   called with input the name of the pattern that must be used for
>   pseudonym generation, along with the language code (locale) that is associated with
>   this pattern.

  Resource files can contain patterns by placing an entry in the
   localization.properties file, setting the code.PATTERN property to the
   location of the patterns’ properties file. Here is an example:

>   en.PATTERN = /identifier/en/patterns.properties

   The patterns’ properties file must use this format:

>   **\<pattern name\> = \<pattern\>**

>   (For example, phoneNumber = \\(\\d{3}\\) \\d{3}-\\d{4})

   A pattern from the resource files can be used by setting the pattern
   configuration option to null, while specifying the appropriate values for
   the language code (locale) and pattern name options. For example,
   phoneNumber can be used as follows:

>   generateViaOptionsEnabled = false

>   generateViaPatternEnabled = true

>   generateViaPatternPattern = null

>   generateViaPatternLanguageCode = "en"

>   generateViaPatternPatternName = "phoneNumber"

##### Encryption-based pseudonym generation

>   Supports the generation of pseudonyms by hashing the original data value
>   using either the SHA-512 (default) or the SHA-256 one-way hashing algorithms.

To activate encryption-based pseudonym generation:

>   generateViaOptionsEnabled = false

>   generateViaHashEnabled = true

   The options above generate SHA-512 pseudonyms, which
   is the default algorithm used. To generate pseudonyms using the SHA-256
   algorithm, the following configuration option must be set:

>   generateViaHashUseSHA256 = true

Here are the options and their default values for the PSEUDONYM  provider:

| **Option name**                     | **Type** | **Description**                                                                                                                  | **Default value** |
|-------------------------------------|----------|----------------------------------------------------------------------------------------------------------------------------------|-------------------|
| generateViaOptionsEnabled           | Boolean  | Generate pseudonym using specific options                                                                                          | true              |
| generateViaOptionsMinLength         | Integer  | Minimum length of generated pseudonym                                                                                            | 10                |
| generateViaOptionsMaxLength         | Integer  | Maximum length of generated pseudonym                                                                                            | 10                |
| generateViaOptionsGenerateUppercase | Boolean  | Generated pseudonym may contain uppercase letters                                                                                | true              |
| generateViaOptionsGenerateLowercase | Boolean  | Generated pseudonym may contain lowercase letters                                                                                | true              |
| generateViaOptionsGenerateDigit     | Boolean  | Generated pseudonym may contain digits                                                                                           | true              |
| generateViaOptionsGenerateSpecial   | Boolean  | Generated pseudonym may contain the following special characters: !, \@, \#, \$, %, \^, &, \*, [, ], \\, /, ?, {, }, +, -, or \_ | false             |
| generateViaPatternEnabled           | Boolean  | Generate pseudonym using a pattern                                                                                                   | false             |
| generateViaPatternPattern           | String   | Pattern used to generate pseudonym                                                                                               | null              |
| generateViaPatternLanguageCode      | String   | Language code (two-digit ISO code) of pattern used to generate pseudonym                                                           | en                |
| generateViaPatternPatternName       | String   | Name of the pattern used to generate pseudonym                                                                                       | null              |
| generateViaHashEnabled              | Boolean  | Generate pseudonym using a hash algorithm                                                                                            | false             |
| generateViaHashUseSHA256            | Boolean  | Use the SHA-256 hash algorithm to generate pseudonym, instead of SHA-512                                                          | false             |

#### RACE

>   Replaces race information with a randomly-chosen race. This provider does
>   not have any configuration options.

#### RANDOM

Replaces an original data value with random characters, by:

1.  Replacing numerical characters with numerical characters;

1.  Replacing letter characters with letter characters;

1.  Maintaining other characters, for example, commas, dashes, and asterisks.

If non-English characters are replaced, the characters are replaced with an
appropriately-cased Latin letter. If the character does not have an associated case,
such as Chinese or Japanese characters, it is replaced with an uppercase Latin letter.

This provider has no configuration options.

#### REDACT

>   Redacts a value by replacing it with a character. The default is the letter X. The
>   length of the value can be optionally preserved.

| **Option name**  | **Type** | **Description**               | **Default value** |
|------------------|----------|-------------------------------|-------------------|
| preserveLength   | Boolean  | Preserve token length         | true              |
| replaceCharacter | String   | Default replacement character | X                 |

#### RELIGION

>   Replaces a religion information with a randomly-chosen religion. This
>   provider does not have any configuration options.

#### REPLACE

>   Replaces an original value with either asterisks or with random characters.
>   Digits are replaced with randomly-selected digits, and alpha characters are
>   replaced with random alpha characters. Other characters are preserved, for
>   example, dashes and commas.

>   If non-English characters are replaced with the maskReplaceWithRandom option,
>   the characters are replaced with an appropriately-cased Latin letter.
>   If the character does not have an associated case, such as Chinese or Japanese characters,
>   it is replaced with an uppercase Latin letter.

>   If neither maskReplaceWithAsterisks nor maskReplaceWithRandom is selected,
>   the preserved portion of the value is returned.

>   If a value encountered in the data is not long enough to meet the starting offset
>   for preservation, null is returned.

| **Option name**          | **Type** | **Description**                                               | **Default value** |
|--------------------------|----------|---------------------------------------------------------------|-------------------|
| maskReplaceWithAsterisks | Boolean  | Replace the rest of the value with asterisks.                 | false             |
| maskPreserve             | Integer  | Number of characters to preserve                              | 3                 |
| maskOffset               | Integer  | Starting offset for preserving                                | 0                 |
| maskReplaceWithRandom    | Boolean  | Replace the rest of the value with random digits, characters. | false             |

#### SSN_UK

>   Masks a Social Security number (SSN) based on the UK, with the option to
>   preserve its prefix.

| **Option name**    | **Type** | **Description** | **Default value** |
|--------------------|----------|-----------------|-------------------|
| maskPreservePrefix | Boolean  | Preserve prefix | true              |

#### SSN_US

>   Masks Social Security numbers based on the US with the option to preserve
>   their prefix.

| **Option name**        | **Type** | **Description**      | **Default value** |
|------------------------|----------|----------------------|-------------------|
| maskPreserveAreaNumber | Boolean  | Preserve area number | true              |
| maskPreserveGroup      | Boolean  | Preserve group       | true              |

#### STATE_US

>   Replaces a US state with a randomly-chosen state. This provider
>   does not have any configuration options.

#### SWIFT

>   Masks SWIFT codes with the option to preserve their country.

>   By default, no actual SWIFT codes are provided with the service.  When replacing SWIFT codes using this provider,
>   the service generates a random value in a format similar to the format followed by actual SWIFT codes.  Actual
>   SWIFT codes can be added to the service, if desired.  If codes are loaded, the service randomly selects 
>   replacement values from the loaded codes, as appropriate.  For more information about loading custom codes, 
>   see [Localization](localization.md).

| **Option name** | **Type** | **Description**      | **Default value** |
|-----------------|----------|----------------------|-------------------|
| preserveCountry | Boolean  | Preserve country     | false             |

#### URL

>   Masks URLs with the options to remove the query part, preserve domain
>   levels, and mask any existing ports and username / passwords.

| **Option name**      | **Type** | **Description**                            | **Default value** |
|----------------------|----------|--------------------------------------------|-------------------|
| maskPort             | Boolean  | Mask port                                  | false             |
| maskRemoveQuery      | Boolean  | Remove query part                          | false             |
| preserveDomains      | Integer  | Number of domains to preserve (-1 for all) | 1                 |
| maskUsernamePassword | Boolean  | Mask username and password                 | true              |
| maskMaskQuery        | Boolean  | Mask query part                            | false             |

#### VIN

>   Masks a vehicle’s identifier number (VIN) with options to preserve the
>   manufacturer and the vehicle description information.

| **Option name**  | **Type** | **Description**                                | **Default value** |
|------------------|----------|------------------------------------------------|-------------------|
| wmiPreserve      | Boolean  | Preserve manufacturer information (WMI)        | true              |
| vdsPreserve      | Boolean  | Preserve vehicle description information (VDS) | false             |

#### ZIPCODE

>   Masks original postal (ZIP) code values while offering a number of utility-preserving
>   options:
>
>   - Postal (ZIP) code truncation, where the last n digits of the code
>     are removed to protect the association of the individual with a specific
>     postal code
>   - Zeroing out of the postal code prefix
>   - Postal code replacement with a random postal code that has the same prefix,
>     or with a random neighboring postal code to maintain spatial proximity
>   - Postal code processing as per HIPAA Safe Harbor, where the geographical unit
>     formed by combining postal codes with the same first three digits as the
>     original postal code is checked against the current, publicly available data
>     from the US Bureau of the Census. This check verifies whether it meets the
>     set population criteria, typically, whether it contains more than
>     20,000 people.

>   If multiple options are set to **True**, the operations are performed in this order:

1.  Replace the postal code with a neighboring postal code (approximate geographic distance)

2.  Zero out the postal code's three-digit prefix if the total population in the
    geographical unit, formulated by combining all postal codes with the same
    three-digit prefix as the original postal code, contains less than a specified
    minimum population. Additionally, the postal code may be truncated to a
    specified length if the minimum population in the formulated geographical
    unit is not reached.

3.  Replace the postal code digits suffix either with random digits, or with digits that
    result in a random valid postal code with the same prefix.

4.  Truncate the postal code to the prefix.

| **Option name**                      | **Type** | **Description**                                                                   | **Default value** |
|--------------------------------------|----------|-----------------------------------------------------------------------------------|-------------------|
| maskCountryCode                      | String   | Country code for the postal codes (two-digit ISO code)                                 | US                |
| maskReplaceWithNeighbor              | Boolean  | Replace with a neighboring postal code                                               | false             |
| maskReplaceWithNeighborNearestCount  | Integer  | Number of closes postal codes to select from                                         | 10                |
| maskPrefixLength                     | Integer  | Postal code prefix length                                                            | 3                 |
| maskPrefixRequireMinPopulation       | Boolean  | Replace the postal code prefix with zeros if the minimum population is not reached       | false             |
| maskPrefixMinPopulation              | Integer  | Minimum population criterion (number of people)                                   | 20000             |
| maskTruncateIfNotMinPopulation       | Boolean  | Truncate the postal code if the minimum population is not reached                        | false             |
| maskTruncateLengthIfNotMinPopulation | Integer  | Number of digits to truncate the postal code to if the minimum population is not reached | 2                 |
| maskSuffixTruncate                   | Boolean  | Truncate the postal code to the prefix                                                   | true              |
| maskSuffixReplaceWithRandom          | Boolean  | Replace the postal code suffix with random                                               | false             |
| maskSuffixReplaceWithValidOnly       | Boolean  | Replace the postal code suffix resulting in valid postal code only. Only used if `maskSuffixReplaceWithRandom` is `true`.     | false             |


## Applying multiple data protection methods to the same data element

   The IBM Data De-Identification Service supports the application of multiple data
   protection methods and rules to a data element. This is a
   powerful and useful feature, which works similarly to a UNIX pipe command,
   for example: ls –al \| grep ‘\^d’

   When multiple data protection methods are specified for the same data element,
   the first method is applied on the original value of the element and its output
   is provided as input to the next data protection method. The output of the
   last data protection method determines the new value of the data element.

   The Data De-Identification Service supports two approaches for applying
   multiple data protection methods to a data element. The first approach is
   to provide multiple values in the maskingProviders array. The second approach,
   which only works for JSON arrays containing JSON objects with different keys,
   is to specify the
   providers in separate rule assignments in the data de-identification configuration
   file. To learn how each approach works, continue reading this topic.

#### Provide multiple values in the maskingProviders array

   The Data De-Identification Service allows the application of more than one
   data protection method to the same data element. This applies when
   multiple values are specified in the maskingProviders parameter of a rule.
   The data protection methods are applied in the sequence that they are listed
   in the data de-identification configuration file.

   **Note:** This functionality is available for both array and non-array data elements.

   This examples shows the supported syntax.

   **Example 12: Masking a data element using two privacy providers**
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

  In Example 12, you see the processing of the FHIR birthDate data element
   (of the Patient FHIR Resource) by two privacy providers. First, the original
   date-of-birth of the patient is transformed to the MM/YYYY representation by
   maintaining only the month and the year of birth. Next, patients who were
   born in the same month and year are hashed to the same bucket by one-way
   hashing the birthDate data element.

   The second de-identification rule applies to the first element of the telecom FHIR array,
   which is also part of the Patient FHIR Resource. It is processed first
   by the EMAIL and subsequently by the HASH privacy providers.

   As another example, consider that we want to retrieve information about the
   state where an individual resides (for individuals of certain US States),
   based on the US postal (ZIP) code of their home address. The mapping of US postal codes
   to US States is provided in:
   <https://en.wikipedia.org/wiki/List_of_ZIP_code_prefixes>. Example 13
   presents the ruleset for translating five-digit codes to states **ME**, **CT**, and
   **Other**, with the latter corresponding to any US State other than Maine (ME)
   and Connecticut (CT). Observe that this is achieved using the ZIPCODE
   provider followed by GENERALIZE.

   **Example 13: Masking a postal (ZIP) code data element using two privacy providers**

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

   **Restrictions**

   It is important to note that there are restrictions enforced by the
   Data De-Identification Service to both the number and the type of privacy
   providers that can be specified in a single masking rule.

   Firstly, **only two privacy providers can be specified in a single rule**.

   The second restriction regards the type of the privacy providers that can be applied to the
   same data element, ensuring that the pair of providers that is used is
   valid. The service separates the privacy providers into two categories:

1.  **Category I – PII-specific providers:** These providers mask a specific
    type of PII / PHI / SPI. The Category I providers are:
    ADDRESS, ATC, CITY, CONTINENT, COUNTRY, COUNTY, CREDIT_CARD, DATEDEPENDENCY, 
    DATETIME, DATETIME_CONSISTENT_SHIFT, EMAIL, GENDER, HOSPITAL, IBAN, ICDV10, ICDV9, IMEI, IP_ADDRESS,
    LATITUDE_LONGITUDE, MAC_ADDRESS, MARITAL, NAME, OCCUPATION, PHONE, RACE, 
    RELIGION, SSN_UK, SSN_US, STATE_US, SWIFT, URL, VIN, ZIPCODE.

2.  **Category II – Generic providers:** These providers are not designed to
    mask a specific type of PII / PHI / SPI, but can be used to process several
    types of identifiers. The Category II providers are:
    BINNING, CONDITIONAL, GENERALIZE, GUID, HASH, MAINTAIN, NULL, NUMBERVARIANCE,
    PSEUDONYM, RANDOM, REDACT, REPLACE.

   Based on these categories, a pair of privacy providers is
   valid if the first provider belongs to Category I, and the second provider
   is from Category II.

#### Using separate rules for privacy providers
   
   The same data elements of an array can be processed by multiple privacy
   providers when more than one rule is defined for these data elements in
   the data de-identification configuration file. In this case, the rules are
   applied to the corresponding data elements of the array in the
   sequence in which the rule assignments have been specified.

   - The first rule is applied to the original value of the data element.
   - Each subsequent rule is applied to the transformed value produced for this element by the previous
   rule. This is similar to the behaviour of a UNIX pipe command.

The next example shows a valid rule set for processing data elements of an
array with multiple privacy providers.

**Example 14: Masking elements of an array with multiple masking providers**

```
  {
        "jsonPath": "/fhir/Patient/name[1]/use",
        "rule": "HASH"
  }

  {
        "jsonPath": "/fhir/Patient/name[*]/use",
        "rule": "RANDOM"
  }

  {
        "jsonPath": "/fhir/Patient/name[*]/given[0]",
        "rule": "REPLACE"
  }
```

In this example, both the first and the second rule are applied to the
_use_ element in the second member of the _name_ array.  Offsets start at 0
so [1] refers to the second array member. The second rule is also applied to 
the _use_ element in all other members of the _name_ array.
The third entry applies the REPLACE rule to
the first member of the _given_ array in all members of the _name_ array.

**Note:** This functionality is available only for indexed array data elements.  It is an 
error for multiple rules to be assigned to data elements not identified by an array index.


## Handling unexpected input values

   When an input value to a privacy provider is *null*, the privacy providers
   retain the *null* value and do no further processing for that input.

   If a privacy provider is presented an input value that is not *null*, but which it still cannot process, 
   the provider proceeds based on the values of the configuration parameters described in the table below.  
   
   These parameters can be included in the configuration of every privacy provider.     

| **Option name**               | **Type** | **Description**                                                                                                | **Default value** |
|-------------------------------|----------|---------------------------------------------------------------------------------|-------------------|
| unexpectedInputHandler        | String   | One of the handling strategies described in the table below.                    | NULL              |
| unexpectedInputReturnMessage  | String   | Value to be returned when `unexpectedInputHandler` has the value MESSAGE.  | OTHER             |

 
 
| **Handler name**        | **Description**                                                                                                    |
|-------------------------|--------------------------------------------------------------------------------------------------------------------|
| NULL                    | Set the field to *null*.                                                                                           | 
| RANDOM                  | Set the field to a random value generated by the privacy provider.  Not all privacy providers support this option. |
| MESSAGE                 | Set the field to the value provided in the `unexpectedInputReturnMessage` parameter.                        |
| ERROR_EXIT              | Returns an error message and stops the privacy protection operation.                                         |


For privacy providers that operate using regular expressions, for example, PHONE NUMBER,
SSN\_US, SSN\_UK, MAC\_ADDRESS, IBAN, CREDIT\_CARD, DATETIME, DATETIME_CONSISTENT_SHIFT, IP\_ADDRESS, and EMAIL,
an input value that cannot be processed is one that does not conform to the
specified regular expression.

For privacy providers that operate using lookup tables, for example, NAME,
CITY, COUNTRY, OCCUPATION, and HOSPITAL, an input that cannot be processed is one that 
does not appear in the table used by that provider.  

Note that based on its current configuration, a privacy provider might be able to proceed
normally even if it does not recognize the input value.  In those cases, normally processing
occurs instead of the unexpected input processing.

**Exceptions**

These privacy providers ignore the `unexpectedInputHandler` parameter as they do not require input to be in any particular format:
* CONDITIONAL
* COUNTY
* GENDER
* GENERALIZE
* GUID
* HASH
* MAINTAIN
* MARITAL
* NULL
* PSEUDONYM
* RACE
* RANDOM
* REDACT
* RELIGION
* REPLACE
* STATE_US
   
These privacy providers cannot generate random values and so treat the `unexpectedInputHandler` value **RANDOM** the same as **NULL**:
* ATC
* BINNING
* DATETIME_CONSISTENT_SHIFT
* NUMBERVARIANCE   
* URL

## Runtime exception handling

If a data protection method encounters an exception while attempting to privacy-protect an input data value, it logs the exception and stops the privacy protection process.  The service attempts to find problems in input data before privacy protection begins.  However, problems such as a sudden loss of communication with external services or databases can occur after privacy protection begins.

## Next steps

- Explore resource localization:

   [localization.md](localization.md)
