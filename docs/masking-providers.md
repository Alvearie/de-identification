# Masking Providers
## Overview

> To preserve utility in the data being de-identified, the IBM Data De-Identification service supports the following data protection methods:

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

# Data protection methods with configuration options, defaults

The following sections provide information about the various
field-level data protection methods that are currently supported by the Data
De-Identification Service. This includes the methods' available configuration (utility-preserving)
options and their default values.

#### ADDRESS

>   Masks an input postal address with a random one. Various elements of the
>   address can be preserved, for example, street names and road types.

| **Option name**             | **Type** | **Description**                                     | **Default value** |
|-----------------------------|----------|-----------------------------------------------------|-------------------|
| postalCodeNearest           | Boolean  | Select nearest postal code                          | false             |
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

| **Option name**       | **Type** | **Description**            | **Default value** |
|-----------------------|----------|----------------------------|-------------------|
|  maskLevelsToKeep     | Integer  | Number of levels to retain | 4                 |

#### BINNING

>   Replaces a numerical input value with an interval that contains the value. The interval's width is configurable, but is constant across the domain of values.  The range
>   is inclusive on the lower bound and exclusive on the upper bound. For examples of valid formats, see the javadoc for java.util.Formatter.

| **Option name**       | **Type** | **Description**                         | **Default value** |
|-----------------------|----------|-----------------------------------------|-------------------|
| binSize               | Integer  | The range of the interval               | 5                 |
| format                | String   | The format of the replacement range     | %s-%s             |
| startValue            | Integer  | The lower bound of the range            | 0                 |
| useStartValue         | Boolean  | Use the startValue as the lower bound   | false             |

#### CITY

>   Masks a city with a randomly-selected city, or based on one of its
>   neighboring cities (geographical distance).

| **Option name**        | **Type** | **Description**                         | **Default value** |
|------------------------|----------|-----------------------------------------|-------------------|
| maskClosest            | Boolean  | Select one of the near cities.          | false             |
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
   **pseudonym.generateViaOptions.minLength = 12** and
   **pseudonym.generateViaOptions.maxLength = 12**) are applied to mask the
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
    array query format path. For a description, see:
    https://test.cloud.ibm.com/docs/services/data-de-identification?topic=data-de-identification-introduction-to-the-data-de-identification-service-configuration

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

>   For information about the unspecifiedValueHandling and unspecifiedValueReturnMessage
>   properties, see “Handling of Unrecognized Input Values and of Exceptions Raised
>   by the Privacy Providers” below.

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
   array node path. In this example, the objective of the CONDITIONAL ruleset is
   to mask the status data element of the Device FHIR Resource using the
   PSEUDONYM masking provider. This applies only if the Device Resource contains both an
   extension array node with a URL of
   <http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName>
   and a valueString value **Asthma_Inhaler**. Otherwise, the original value of
   the status data element is maintained as-is.

#### CONTINENT

>   Masks a continent by replacing it either with a randomly-selected continent, or
>   with the closest continent. The distance computation is based on an approximate
>   centroid of the continent's bounding box.

| **Option name**         | **Type** | **Description**                            | **Default value** |
|-------------------------|----------|--------------------------------------------|-------------------|
| maskClosest             | Boolean  | Select one of the nearest continents.      | false             |
| maskClosestK            | Integer  | Number of neighbors for nearest continents | 5                 |

#### COUNTRY

>   Replaces a country either with a randomly-chosen country, or with its nearest
>   country. The latter is calculated based on geographic distance.

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

>   Extracts two related dates (for example, the patient's date-of-birth and the
>   date-of-death of a patient) from the same input message:
>
>   - The “mask date”: The date to be masked
>   - The “compare date”: The date to which that the mask date is compared
>
>     Then, if a number of days comparison with the "compare date" is true,
>     it removes the year from the "mask date".

This privacy provider supports these options:

| **Option name**                                 | **Type** | **Description**                                                                                    | **Default value** |
|-------------------------------------------------|----------|----------------------------------------------------------------------------------------------------|-------------------|
| datetimeYearDeleteNIntervalMaskDate             | String   | The FHIR element name of the date that will be masked                                              | null              |
| datetimeYearDeleteNIntervalCompareDate          | String   | The FHIR element name of the date that will be compared with the mask date                         | null              |
| dateYearDeleteNDaysValue                        | Integer  | This option must be set to the maximum allowable interval (number of days) between the two dates   | 365               |

   The following provides an example that illustrates the use of the
   DATEDEPENDENCY provider. Specifically, we consider both the Patient FHIR
   Resource and the birthDate and deceaseDateTime FHIR data elements that it
   contains.

   The rule that we want to apply regards deleting from the birthDate of a
   patient the year of birth, for those patients who died when they were less than
   five years old. For this example, the interval between the birthDate and the
   deceaseDateTime elements is calculated, and if it is found to be less than five
   years (≅ 1825 days), then the year is removed from the birthDate element.

   **Note:** The values in the example are for demonstration purposes only.

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
>   for example, shifting dates, generalizing to month, and generalizing to year.
>   Additional options include adding random offsets to the various datetime
>   elements, for example, years, months, days, hours, and seconds). If
>   multiple options are set to true in the datetime masking algorithm, the
>   following order is respected.

>  These examples are for the 10th of January 2016:

> 1.  Override with default or specified value, for example, **90+** or **Over 90 y.o.**.

> 2.  Shift the date by constant amount.

> 3.  Generalize to week number/year, for example, 02/2016.

> 4.  Generalize to month/year, for example, 01/2016.

> 5.  Generalize to quarter year, for example, 01/2016.

> 6.  Generalize to year, for example, 2016.

> 7.  Generalize to N-year interval, for example, 2015-2019.

> 8.  Generalize to year (for example, 1927) and mask any age over 90.

> 9.  Generalize to month/year (for example, 02/1927) and mask any age over 90.

> 10. Add random offsets to year, month, day, hour, minutes, and seconds.

> 11. Apply maximum years ago.

> 12. Apply maximum days ago.

   If the override option of the provider is set to **True**, then the override
   is processed first. If the rule criteria are met, all other options
   of the DATETIME provider are ignored.


   The following date formats are supported by the DATETIME masking provider.
   This uses the option datetime.format.fixed:

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
| yearDeleteNointervalComparedateValue              | Integer  | The date and time to compare                                                                                                                                                                                                                                | 0                 |

#### EMAIL

>   Masks e-mail addresses with the option to preserve certain levels of the
>   host domain.

| **Option name**        | **Type** | **Description**                                                                                             | **Default value** |
|------------------------|----------|-------------------------------------------------------------------------------------------------------------|-------------------|
| preserveDomains | Integer  | Number of domains to preserve starting from the right; if value = -1 then all domains will be preserved (that is, everything after \@) | 1                 |
| nameLength      | Integer  | Length of username to generate; if value = -1, then the username will be 5 to 8 characters long. 0 is invalid      | \-1               |

#### GENDER

>   Replaces gender information with a randomly-chosen gender. This provider
>   does not have any configuration options.

#### GENERALIZE

>   Replaces one or more specified original values with a specified general
>   category term to which these values belong. If a general category term is
>   not defined for the input values, these can be optionally replaced with a
>   **default** term which is configurable by the data owner. The aim of
>   GENERALIZE is to protect infrequent data values that appear in a
>   data element by replacing them with a general category term.

>   When a small number of values must be replaced with a general category term,
>   the IBM onboarding team can specify these sets of values and their
>   corresponding replacement values as rules of the form R:
>
>   {*o*1, *o*2, …, o*n} *r*
>
>   where
>   *o*i is an original data value.
>   *r* is the replacement value.
>
>   Multiple such rules can be specified and provided as input to
>   GENERALIZE. For example, in a data element that stores the religion of
>   individuals, rule R: {“Daoism”, “Shinto”, “Confucianism”} “Eastern Asian
>   Religions”, can be used to protect the identity of the corresponding
>   individuals by associating them with the more-general category of “Eastern
>   Asian Religions”. All other original values that are not captured in a rule
>   can be maintained as-is.

>   When many values in a data element are expected to be infrequent, GENERALIZE
>   can be used to capture those original data values that must be maintained in
>   the output data. In this case, only those values that are **not** specified
>   are replaced with a configurable general category term. For example, in
>   a data element that stores the spoken languages of individuals, GENERALIZE
>   can be configured to replace all languages that are not listed as **Spanish**
>   or **English**, with value **Other**.

   **Options supported by GENERALIZE**

| **Option name**         | **Type** | **Description**                                                   | **Default value** |
|-------------------------|----------|-------------------------------------------------------------------|-------------------|
| maskRuleSet             | String   | The rule sets are defined in the masking JSON configuration file. | N/A               |

   Next, we describe the different modes of operation that are
   supported by GENERALIZE for protecting infrequent data values.

   As input, GENERALIZE can take a number of data generalization rules R1…Rm,
   each of which defines a set of original (or **source**) data values and a
   corresponding replacement (or **target**) value.

   Assume that **u** is an incoming data value from a FHIR data element to which
   GENERALIZE is applied and that R is a rule associated with this FHIR data
   element. GENERALIZE checks **u** against rule R and replaces it with
   the target value of R if **u** is found among the source values. Similarly, a
   rule R’ can be specified to replace value **u** only if it does **not** belong
   among the source values specified in that rule. If multiple rules are
   specified for the same FHIR data element, each rule is tested in sequence
   against the value **u** of the FHIR data element, and the first rule
   that is found to contain **u** is triggered. For those values **u** of the
   FHIR data element to which none of the rules apply, they can either be
   maintained or be replaced with a pre-specified general category value, for
   example, **Other**.

   The various rules that must be enforced by GENERALIZE are defined with the
   FHIR Resource data element path in the JSON data de-identification
   configuration file. The option generalize.mask.ruleSet is used for defining
   the rules that must be applied to a FHIR data element. Each rule
   consists of two JSON element names: the targetValue element, and either the
   sourceValueIn element **or** the sourceValueNotIn element:

 -  **targetValue element**: This provides the replacement value that must be applied
    to the original value of the FHIR data element.

 -  **sourceValueIn element**: This provides a set of original values that can be
    associated with the corresponding FHIR data element. If the FHIR data
    element that is currently examined by GENERALIZE has a value that belongs to
    the sourceValueIn set, then the original value of the FHIR data element is
    replaced with that of the corresponding targetValue element.

   **Note:** If needed, you can denote that **any** value of the corresponding FHIR
   data element must be replaced with the value in the targetValue element. To do so,
   use the special value asterisk (\*) instead of any specific data values in the
   sourceValueIn set.

-   **sourceValueNotIn element**: To support rules that enforce negation logic, you
    can specify this instead of the sourceValueIn element. It captures a set of
    original values that can be associated with the corresponding FHIR data
    element and **must not** be replaced by the value of the targetValue element.
    Any FHIR data element that has a value in the sourceValueNotIn set maintains
        its original value. All other values of the FHIR data element that
    do not belong in the sourceValueNotIn set are replaced by the
    corresponding value of the targetValue element.

>   Next, examples show the capabilities of the GENERALIZE privacy provider,
>   by applying different rulesets to FHIR resource type data elements.
>
>   **Note:** The values in the example are for demonstration purposes only.

**Example 4: Patient address city data element**

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
    the sourceValueIn element of the first rule (that is, any of **Bangkok**,
    **Manila**, **Shanghai**, **Taipei**, or **Mumbai**), then it is replaced with
    the value specified in the targetValue element (that is, **Asian City**).
        Then, no other rule will be examined.

> 2.  Otherwise, if the city value of the FHIR data element is one of those
    listed in the sourceValueIn element of the second rule (that is, **Addis Ababa**,
    **Cairo**, **Cape Town**, **Lagos**), then it is replaced with the value
    specified in the targetValue element (that is, **African City**). No other rule
    will be examined.

> 3.  Otherwise, if the city value of the FHIR data element is not among the cities
    listed in the sourceValueIn element of the first rule and, in addition, is not
    among the cities listed in the sourceValueIn element of the second rule, then the
    original value of the FHIR data element is maintained.

**Example 5: Practitioner address city data element**

```
   {
     "--/fhir/Practitioner/address/city": {
       "type": "GENERALIZE",
       "maskRuleSet": "[{\"targetValue\": \"US City\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houseton\",\"Minneapolis\",\"Boston\"]}, {\"targetValue\": \"Canadian City\", \"sourceValueIn\": [\"Toronto\",\"Vancouver\",\"Montreal\",\"Calgary\"]}, {\"targetValue\": \"Other\", \"sourceValueIn\": [\"\\*\"]}]"
     }
   }
```

>   In the example above, a ruleset consisting of three rules is applied to the
>   Practitioner address city data element. As in the Example 4, these
>   rules are checked sequentially, and the first rule that applies to the value
>   of the FHIR data element is invoked. In this example, we demonstrate the
>   application of the third rule, wherein if the value of the input FHIR data
>   element is not among those listed in the sourceValueIn elements of the first
>   and the second rules, it is replaced with the value **Other**.

**Example 6: Device component language code text data element**

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
>   the rule (that is, **French** or **Spanish**), then this value is maintained
>   in the FHIR data element. Otherwise, for example, if the value of the FHIR data
>   element is **English**, then it is replaced by the value specified in the
>   targetValue element, that is, **Other**.

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
>   specify the behavior of the algorithm in the case of invalid offsets, use
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

| **Option name**        | **Type** | **Description**               | **Default value** |
|------------------------|----------|-------------------------------|-------------------|
| randomizeChapter       | Boolean  | Randomize by chapter          | false             |
| randomizeCategory      | Boolean  | Randomize by three-digit code | true              |

#### ICDV10

>   Masks ICD-10 diagnosis codes. Codes can also be generalized to their
>   respective chapters or categories.

| **Option name**        | **Type** | **Description**               | **Default value** |
|------------------------|----------|-------------------------------|-------------------|
| randomizeChapter       | Boolean  | Randomize by chapter          | false             |
| randomizeCategory      | Boolean  | Randomize by three-digit code | true              |

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
>   available. These are processed in the following order:

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

>   By default, this replaces an occupation with a randomly-selected occupation.
>   However, if the option **occupation.mask.generalize** is set to **true**, this
>   generalizes an occupation to its respective category. The categories
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

>  generateViaOptionsMinLength = \<value\>

>  generateViaOptionsMaxLength = \<value\>

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

>   generateViaPatternLanguageCode = "EN"

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

>   pseudonym.generateViaHash.useSHA256 = true

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
| generateViaPatternLanguageCode      | String   | Language code (two-digit ISO code) of pattern used to generate pseudonym                                                           | EN                |
| generateViaPatternPatternName       | String   | Name of the pattern used to generate pseudonym                                                                                       | null              |
| generateViaHashEnabled              | Boolean  | Generate pseudonym using a hash algorithm                                                                                            | false             |
| generateViaHashUseSHA256            | Boolean  | Use the SHA-256 hash algorithm to generate pseudonym, instead of SHA-512                                                          | false             |

#### RACE

>   Replaces race information with a randomly-chosen race. This provider does
>   not have any configuration options.

#### RANDOM

>   Replaces an original data value with random characters, by:

> 1.  Replacing numerical characters with numerical characters;

> 2.  Replacing letter characters with letter characters;

> 3.  Maintaining other characters, for example, commas, dashes, and asterisks.

>   If non-English characters are replaced, the characters are replaced with an
>   appropriately-cased Latin letter. If the character does not have an associated case,
>   such as Chinese or Japanese characters, it is replaced with an uppercase Latin letter.

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
>   no masking occurs since there is no specified replacement.

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

>    Masks SWIFT codes with the option to preserve their country.

| **Option name** | **Type** | **Description**      | **Default value** |
|-----------------|----------|----------------------|-------------------|
| preserveCountry | Boolean  | Preserve country     | false             |

#### URL

>   Masks URLs with the options to remove the query part, preserve domain
>   levels, and mask any existing ports and username / passwords.

| **Option name**      | **Type** | **Description**               | **Default value** |
|----------------------|----------|-------------------------------|-------------------|
| maskPort             | Boolean  | Mask port                     | false             |
| maskRemoveQuery      | Boolean  | Remove query part             | false             |
| preserveDomains      | Integer  | Number of domains to preserve | 1                 |
| maskUsernamePassword | Boolean  | Mask username and password    | true              |
| maskMaskQuery        | Boolean  | Mask query part               | false             |

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
>     or with a random neighboring postal code, to maintain spatial proximity
>   - Postal code processing as per HIPAA Safe Harbor, where the geographical unit
>     formed by combining postal codes with the same first three digits as the
>     original postal code is checked against the current, publicly available data
>     from the US Bureau of the Census. This check verifies whether it meets the
>     set population criteria, typically, whether it contains more than
>     20,000 people.

>   If multiple options are set to **True** in the postal code masking algorithm, the
>   operations are performed in this order:

> 1.  Replace the postal code with a neighboring postal code.

> 2.  Zero out the postal code's three-digit prefix if the total population in the
    geographical unit, formulated by combining all postal codes with the same
        three-digit prefix as the original postal code, contains less than a specified
    minimum population. Additionally, the postal code may be truncated to a
    specified length if the minumum population in the formulated geographical
    unit is not reached.

> 3.  Replace the postal code digits suffix either with random digits, or with digits that
    result in a random valid postal code with the same prefix.

> 4.  Truncate the postal code to the prefix.

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
| maskSuffixReplaceWithValidOnly       | Boolean  | Replace the postal code suffix resulting in valid postal code only                          | false             |


### Applying multiple data protection methods to the same data element

   The IBM Data De-Identification Service supports the application of multiple data
   protection methods and rules to a FHIR data element. This is a
   powerful and useful feature, which works similarly to a UNIX pipe command,
   for example: ls –al \| grep ‘\^d’

   When multiple data protection methods are specified for the same data element,
   the first method is applied on the original value of the element, and its output
   is provided as input to the next data protection method. The output of the
   last data protection method determines the new value of the FHIR data element.

   The Data De-Identification Service supports two approaches for applying
   multiple data protection methods to a data element. The first approach is
   to provide multiple values in the maskingProviders array. The second approach,
   which only works for FHIR arrays with different keys, is to specify the
   providers in separate rules in the data de-identification configuration
   file. To learn how each approach works, continue reading this topic.

#### Provide multiple values in the maskingProviders array

   The Data De-Identification Service allows the application of more than one
   data protection method to the same FHIR data element. This applies when
   multiple values are specified in the maskingProviders parameter of a rule.
   The data protection methods are applied in the sequence that they are listed
   in the data de-identification configuration file.

   **Note:** This functionality is available for both array and non-array FHIR data elements.

   This examples shows the supported syntax.

   **Example 12: Masking a FHIR data element using two privacy providers**
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
   State where an individual resides (for individuals of certain US States),
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
   providers that can be applied to the same FHIR data element, when using the
   comma-separated format.

   Specifically, **only two privacy providers can be assigned to a single data element**.
   If this condition is violated, then an IllegalArgumentException is
   generated and this error message is written to the platform log file:

| **Message ID** | **Message text**                                                                                      | **Replacement variables**                                       |
|----------------|-------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| WPH2012E       | {0} – The rule {1} provided for key {2} contains an invalid combination of chained masking providers. | {0}: Provider class name {1}: Masking rule used {2}: Field path |

   Here is an example of a produced log message:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR - LogManager -
>   com.ibm.research.drl.prima.providers.masking.fhir.FHIRGenericMaskingProvider
>   - The rule [RANDOM, HASH, HASH] provided for key /gender contains an invalid
>   combination of chained masking providers.

   The second restriction that is enforced by the Data De-Identification
   Service regards the type of the privacy providers that can be applied to the
   same data element, ensuring that the pair of providers that is used is
   valid. For service separates the privacy providers into two categories:

1.  **Category I – PII-specific providers:** These providers mask a specific
    type of PII / PHI / SPI. Examples are ADDRESS, PHONE, VIN, SSN, COUNTY, URL,
    NAME, GENDER, RELIGION, IP ADDRESS, EMAIL, CREDIT CARD, DATETIME, IBAN,
    CITY, ICD V9/V10, MAC ADDRESS, ATC, OCCUPATION, CONTINENT, HOSPITAL, ZIP
    CODE, COUNTRY, LATITUDE / LONGITUDE, and DATEDEPENDENCY.

2.  **Category II – Generic providers:** These providers are not designed to
    mask a specific type of PII / PHI / SPI, but can be used to process several
    types of identifiers. Examples are HASH, RANDOM, REPLACE, REDACT, NUMBERVARIANCE,
    GENERALIZE, RARE_REPLACEMENT, and PSEUDONYM.

   Based on these categories, a pair of privacy providers is
   valid if the first provider belongs to Category I, and the second provider
   is from Category II.

   If an invalid pair of privacy providers is specified, the
   Data De-Identification Service generates an
   IllegalArgumentException error and logs a WPH2011E message to the platform log
   file. Here is an example of such a log message:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR - LogManager -
>   com.ibm.research.drl.prima.providers.masking.fhir.FHIRGenericMaskingProvider
>   The rule [HASH, PHONE] provided for key /telecom[1]/value contains an
>   invalid combination of chained masking providers.

#### Using separate rules for privacy providers

   The same data elements of a FHIR array can be processed by multiple privacy
   providers, when more than one rule is defined for these data elements in
   the data de-identification configuration file. In this case, the rules are
   applied to the corresponding data elements of the FHIR array in the
   sequence in which they have been defined:

   - The first rule is applied to the original value of the data element.
   - Each subsequent rule is applied to the transformed value produced for this element by the previous
   rule. This is similar to the behavior of a UNIX pipe command.

   The next example shows a valid ruleset for processing data elements of a
   FHIR array with multiple privacy providers.

   **Example 14: Masking elements of a FHIR array with multiple masking providers**

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

    In this example, both the first and the second rule are applied to the
   name[1]/use data element. The second rule is also applied to all
   other name[\*]/use elements in the FHIR array. The third rule of the example
   leads to applying the HASH followed by the REPLACE privacy providers to
   all name[\*]/given[0] data elements in the FHIR array.

   To disable this default behavior, you can use a configuration option (flag),
   **arrayAllRules**, in the data de-identification configuration file.

   **When the arrayAllRules flag is set to false, only the first rule that is
   defined for a FHIR array data element is enforced. Any additional rules
   that involve the same FHIR array data element are not be applied to this
   element.**

   **Note:** arrayAllRules applies only to FHIR array data elements.

   For instance, consider the rules in Example 14 (above) when **arrayAllRules = false**.
   In this case, name[1]/use is processed by HASH, and all other
   name[\*]/use data elements are processed by RANDOM. The third rule
   applies first HASH and then REPLACE to all name[\*]/given[0] FHIR array
   elements. In other words, the arrayAllRules flag changes the behavior of
   the Data De-Identification Service only with respect to different de-identification
   rules that are applied to the same FHIR array data element. However, it does not
   change the behavior of the Service when two privacy providers are
   defined as part of the de-identification rule.

   **Note:** In the case of regular, non-array FHIR data elements, if multiple de-identification
   rules are specified, only the last rule is applied to the data. The previous rules are discarded.

   **Example 15: Masking a regular FHIR data element using separate de-identification rules**

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

  In this example, only the HASH (last) privacy provider is applied
   to the gender data element of the Patient FHIR Resource.

### Handling unrecognized input values, exceptions raised by privacy providers

   Next, this topic describes the operation of the various privacy providers
   that are offered by the Data De-Identification Service in the case of either null
   or unrecognized input data values. It also describes the case of runtime
   exceptions.

#### Handling of null and unrecognized input data values

   The Data De-Identification Service takes special care in handling null and
   unrecognized input data values of FHIR data elements.

   With data elements that have a null value, the privacy providers
   that process them maintain the null value of the elements. As a result,
   they do not proceed to falsify the data.

   If the input value to a privacy provider is not recognized by the privacy
   provider, and it does not appear to be an accurate input value, the Data
   De-Identification Service operates as specified in the data
   de-identification configuration file. This applies to the options
   unspecified.value.handling and unspecified.value.returnMessage. These
   options are globally set and persist for all privacy providers:

| **Option name**               | **Type** | **Description**                                                                                                                                                                                  | **Default value** |
|-------------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| unspecifiedValueHandling      | Integer  | Handling options: **1** : return null **2** : return fictionalized value produced by the corresponding privacy provider **3** : return the message (String) in unspecified.value.returnMessage | 1                 |
| unspecifiedValueReturnMessage | String   | Value to be returned when unspecified.value.handling is set to **3**                                                                                                                           | **OTHER**      |

   For providers that operate using regular expressions, for example, PHONE NUMBER,
   SSN\_US, SSN\_UK, MAC\_ADDRESS, IBAN, CREDIT\_CARD, DATETIME, IP\_ADDRESS, and EMAIL,
   an unrecognized value is a value that does not conform to the
   specified regular expression.

    Similarly, for providers that operate using lookup tables, for example, NAME,
   CITY, COUNTRY, OCCUPATION, and HOSPITAL, an unrecognized value is a value
   that is not part of the corresponding lookup table that is used by the
   privacy provider.

   When unspecifiedValueHandling is set to **1** (or to a value other than either **2** or
   **3**), any privacy provider that takes as input an unrecognized data value
   returns a null value.

   When unspecifiedValueHandling is set to **2**, any privacy provider that
   takes as input an unrecognized data value returns a randomly-generated,
   fictionalized value that is valid for the corresponding provider.

   Finally, when unspecifiedValueHandling is set to **3**, any provider that takes
   as input an unrecognized data value, stores to this value the message
   that is specified in unspecified.value.returnMessage. By default, this
   message is set to **OTHER**.

   **Exceptions**

   These privacy providers ignore the unspecified.value.handling option:
   DATEDEPENDENCY, GENERALIZE, HASH, MAINTAIN, NULL, PSEUDONYM, RANDOM,
   REDACT, and REPLACE. The URL, NUMVARIANCE, and ATC providers
   treat option 2 of unspecified.value.handling the same as option 1.
   As a result, each returns null.

#### Exception handling

   The data protection methods have to deal with
   the case of runtime exceptions that may be raised during their operation in
   the Data De-Identification Service. If a data protection method encounters
   an exception while attempting to privacy-protect an input data value, it
   catches the exception, logs a WPH2002E error message to the application
   audit log, and returns an empty (null) String as the value of the
   corresponding data element. This is the case unless the unspecified.value.\* options
   have been set.

   Here is the structure for the error messages that are produced:

| **Message ID** | **Message Text**                                                                                                                                         | **Replacement Variables**                                                                                         |
|----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| WPH2002E       | {0}.{1} – {2} – The data protection method encountered an exception while attempting to transform an input value. The original value has been nullified. | {0}: Provider class name {1}: Provider method name {2}: Exception name,                                           |
|                |                                                                                                                                                          | resource type, resource ID, and field name                                                                        |

   Here is an example of a produced log message:

>   17/07/17 17:00:57 - 100.10.100.10 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR -
>   com.ibm.research.drl.prima.providers.masking.DateTimeMaskingProvider.mask -
>   java.lang.NullPointerException processing field
>   Patient(id=’exampleId123’)/birthDate - The data protection method
>   encountered an exception while attempting to transform an input value. The
>   original value has been nullified.

