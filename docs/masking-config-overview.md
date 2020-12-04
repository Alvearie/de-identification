# Introduction to the Data De-Identification service configuration
This topic describes how to configure the behavior of the IBM Data De-Identification service.

## Masking configuration options

The following describes top-level attributes for service configuration.

### rules  (List, Mandatory)
A list of masking providers and associated settings used in the data de-identification process. Each entry in the list contains two sections: **name** and **maskingProviders**.

#### name (String, Mandatory) 
The name of the rule. Use any arbitrary, unique string.

#### maskingProviders (Mandatory)
Specify the "type" of the masking provider and its settings. For more information, see "Masking Providers"

   [masking-providers.md](masking-providers.md)

**Example**
```
    "rules": [
        {
          "name": "HASH",
          "maskingProviders": [
            {
              "type": "HASH",
              "algorithmDefault": "SHA-256"
            }
          ]
        },
        {
          "name": "NULL",
          "maskingProviders": [
            {
              "type": "NULL",
              "maskReturnNull": true
            }
          ]
        }
      ]
```

### JSON  (Optional)
If the input document is a JSON or FHIR document, options in this section configure how fields in the document are masked.

#### schemaType (String, Mandatory)
The schema of the JSON input to be masked

Table: Supported schema values

| **Value** | **Description**                                                                   |
|-----------|-----------------------------------------------------------------------------------|
| FHIR      | FHIR is set when input messages are FHIR JSON type.                               |
| GEN       | GEN is set when the input message is any type of generic JSON.                    |

#### messageTypeKey (Optional)
The name of a field in the top level of the JSON input that assigns a type to the JSON input. Normally, this is specified for FHIR data where different JSON documents can represent objects of different FHIR resource types.  This allows an element at the same path in one resource type to be masked differently than an element at the same path in a different resource type.

**Example**
A field might be masked differently in a Patient FHIR resource than in an Organization FHIR resource. For FHIR data, this value is commonly set to resourceType.

#### messageTypes (Array, Mandatory if `messageTypeKey` is present)
| **Value**                    | **Description**                                                                   |
|------------------------------|------------------------------------------------------------------------------------------------------|
| null or empty string         | All input uses the same set of rules, referred to as "default."                                       |
| value                        | Input uses different subsets of rules, based on the value in the messageTypeKey field in the document. |

#### maskingRules (Array, Mandatory)
A list of paths to an element in the JSON input and then the masking rule that is to be applied to that element. Each entry in the list consists of a `jsonPath` string and a `rule` string. The **jsonPath** value identifies an element in the JSON.  The syntax to specify various types of elements follows in this topic. The **rule** value is the **name** value from one of the entries in the `rules` section described earlier in this topic.


**Example**
In this example, the value in component/dataAbsentReason.coding.display in a FHIR Observation resource is masked, based on the masking rule with the name `hashRule` from the `rules` section:

```
    "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [Observation],
        "maskingRules": [
      {
        "jsonPath": "/fhir/Observation/component/dataAbsentReason.coding.display",
           "rule": "hashRule"
      },
      {
        "jsonPath": "/fhir/Observation/component/dataAbsentReason.text",
           "rule": "hashRule"
      },
      {
        "jsonPath": "/fhir/Observation/component/referenceRange",
            "rule": "referenceRange"
      }
```

If the schemaType is set to GEN, then the `jsonPath` values are:

```
      {
        "jsonPath": "/gen/Observation/component/dataAbsentReason.coding.display",
        "rule": "hashRule"
      },
      {
        "jsonPath": "/gen/Observation/component/dataAbsentReason.text",
        "rule": "hashRule"
      },
      {
        "jsonPath": "/gen/Observation/component/referenceRange",
        "rule": "referenceRange"
      }
```

**Note:** FHIR messages can still be ingested even if the masking is configured to have generic JSON input. In that case, users cannot access FHIR-specific masking providers, and messages are masked only with the generic masking providers.

Examples 1 - 3 show how to set messageTypeKey and messageTypes.

**Example 1: All messages use the same rule set**

In this example, all the default rule sets are loaded. The `messageTypes` string is ignored:

```
  {
    "rules": [
        {
          "name": "hashRule1",
          "maskingProviders": [
            {
              "type": "HASH",
            }
          ]
        }
      ],
    "json": {
        "schemaType": "GEN",
        "messageTypeKey": "",
        "messageTypes": [
        ],
        "maskingRules": [
          {
            "jsonPath": "/gen/default/Test",
            "rule": "hashRule1"
          },
        ]
      }
  }
```

**Example 2: FHIR messages**

With FHIR messages, masking rules are usually tailored to specific FHIR resource types. To select a masking rule subset, this example uses the `resourceType` key at the root of the document:

```
    {
      "rules": [
        {
          "name": "hashRule1",
          "maskingProviders": [
            {
              "type": "HASH",
            }
          ]
        }
      ],
      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "resourceType",
        "messageTypes": [
          "Device",
          "MedicationAdministration"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Device/Test",
            "rule": "hashRule1"
          }
        ]
      }
    }
``` 
**Example 3: Custom message key**

In each message, an arbitrary key defines the masking rule subset that should be used on that message. In this case, masking rules subsets depend on a document type:


```
    {
      "rules": [
        {
          "name": "HASH_RULE",
          "maskingProviders": [
            {
              "type": "HASH",
            }
          ]
        }
      ],
      "json": {
        "schemaType": "FHIR",
        "messageTypeKey": "documentinfo/type",
        "messageTypes": [
          "Clinical",
          "continuityOfCare",
          "LPR"
        ],
        "maskingRules": [
          {
            "jsonPath": "/fhir/Clinical/Test",
            "rule": "HASH_RULE"
          }
        ]
      }
    }
```

##### Support for FHIR arrays

The Data De-Identification Service supports the application of data
protection methods and rules to various data elements of FHIR arrays.

The Data De-Identification Service offers two methods to associate a
specific array element (node) with one or more privacy providers: the
FHIR Array Index method and the FHIR Array Query Condition method.

###### The FHIR Array Index method

This method allows the application of privacy providers to specific data elements of FHIR arrays, which are referenced based on their index value, that is, their position in the array. The indices of the array elements that must be processed are specified in the data de-identification configuration file.

Table: Supported formatting options

| **Element**                                                                        | **Description**                                                                                                                |
|------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| /fhir/Resource/path/to/array                                                       | Processes the entire array with the assigned privacy provider                                                                  |
| /fhir/Resource/path/to/array[i] where i = 0, 1, …, N                               | Processes the i-th element of the array with the assigned privacy provider                                                     |
| /fhir/Resource/path/to/array[\*]                                                   | Processes all the elements of the array with the assigned privacy provider                                                     |
| /fhir/Resource/path/to/array[x,y] where x, y : non-negative integers               | Processes all array elements in numeric indices x, x+1, …, y, with the assigned privacy provider                               |
| /fhir/Resource/path/to/array[x,\*] where x = 0, 1, …, N                            | Processes all array elements starting from numeric index x and until the end of the array, with the assigned privacy provider  |
| /fhir/Resource/path/to/array[{a,b, …, r}] where a, b, …, r : non-negative integers | Processes only the array elements that are in indices a, b, …, r, with the assigned privacy provider                        |

**Notes:**

- The index of the first element in an array is always ‘0’.
- Any negative indices specified are ignored.
- Spaces are permitted between the various indices.
- Nested arrays are permitted.

   Any elements of an array for which a data protection method or rule has not
   been applied will maintain their original values. Any elements of an array
   can be masked differently than other elements of the same array.

   **Note:** If the same FHIR array element is associated in the data
   de-identification configuration file with multiple data protection
   methods or rules, then all these methods or rules will be applied (in the
   order of their appearance to the configuration file) to that FHIR array
   element.

   The next examples show the masking of FHIR arrays using different
   formatting options.

   **Example 4: Masking selected elements of an array while maintaining others**

   {
        "jsonPath": "/fhir/Group/identifier[0]/value/value",
        "rule": "RANDOM"
   }

   {
        "jsonPath": "/fhir/Group/identifier[3,\*]/value/value",
        "rule": "RANDOM"
   }

   In Example 4, the data elements in indices [0], [3], …, N, where N is the
   last element of the array, are masked using the RANDOM provider. The
   rest of the elements (that is, the elements in indices [1] and [2]) are left
   unchanged.

   **Example 5: Masking elements of an array with different privacy providers**

   {
        "jsonPath": "/fhir/Group/identifier[{0,2}]/value/value",
        "rule": "HASH"
   }

   {
        "jsonPath": "/fhir/Group/identifier[1]/value/value",
        "rule": "RANDOM"
   }

   In Example 5, the data elements in indices [0] and [2] are processed by the HASH privacy provider. The element in position [1] is processed with the RANDOM masking provider. All other elements of the array are left
   unchanged.

   **Example 6: Application of multiple privacy providers to the same array
   elements**


   {
        "jsonPath": "/fhir/Group/identifier[\*]/value/value",
        "rule": "HASH"
   }

   {
        "jsonPath": "/fhir/Group/identifier[1]/value/value",
        "rule": "RANDOM"
   }

   In Example 6, due to the first rule, all data elements in the array are processed by the
   HASH privacy provider. In addition, the element in
   position [1] of the array is processed with the RANDOM masking
   provider, due to the second rule. Specifically, the data element of
   the array with index [1] is first processed by the HASH privacy
   provider. Then, its output value is processed by the RANDOM masking
   provider.

###### The FHIR Array Query Condition method

This method allows the application of privacy providers to specific data elements of FHIR arrays. These elements are referenced based on their *key* values, instead of their actual position in the array. This method supports FHIR array elements that contain multiple sibling elements. With multiple sibling elements, one sibling element (key) describes the type of data, and the other sibling element contains the actual data. Using an XQuery-like syntax, the array elements that must be processed are specified in the data de-identification configuration file based on their keys.

   As an example, consider the telecom array element of the FHIR Location resource.
   As shown in Example 7, this has a cardinality of zero-to-many contact points.
   It describes the type of data (that is, the type of contact point for the patient)
   that is contained in its sibling element (that is, the value data element).

   In this example, the key is the system data element, whose values determine
   the type of information stored in the value data element. This allows the application
   of different processing for each value.

   **Example 7: Masking elements of an array based on the key values**

   Here is a sample of the telecom array node of the Location FHIR Resource:

```
    {
      "telecom": [
        {
          "system": "phone",
          "value": "+1-3471234567"
        },
        {
          "system": "fax",
          "value": "+1-3471234589"
        },
        {
          "system": "email",
          "value": "somename@someemail.org"
          }
        ]
      }
```

  Here are FHIR Array Query Condition rules for the telecom array node:
   
```
   {
        "jsonPath": "/fhir/Location/telecom/value(system==phone)",
        "rule": "PHONE"
   }

   {
        "jsonPath": "/fhir/Location/telecom/value(system==email)",
        "rule": "EMAIL"
   }

   {
        "jsonPath": "/fhir/Location/telecom/value(system==\*)",
        "rule": "RANDOM"
   }

   {
        "jsonPath": "/fhir/Location/telecom/value(\*==\*)",
        "rule": "HASH"
   }
```

   Example 7 presents four FHIR array query condition rules for processing the
   telecom array node of the Location FHIR Resource:

1.  The first rule specifies that the PHONE privacy provider must be used to
    process the value data element of the telecom array node when the condition
    **(system==phone)** is true. Thus, when the key system has the value **phone**, the
    first rule must be applied.

2.  Similarly, the second rule applies when key system contains value **email**,
    in which case the value data element of the telecom array node is processed
    by the EMAIL privacy provider.

3.  When none of the previous (two) rules has been triggered for the value data
    element of the telecom array node (that is, because the value of key system for
    the element was different to **phone** and **email**), this rule is
    applied. The condition **(system==\*)** in this rule **applies to all values of
    the key which have not been covered by a previous rule** (for example, **fax**). In
    this example, the RANDOM privacy provider is used to process these
    values.

4.  Last, the fourth rule applies to all remaining data elements of the telecom
    array node, regardless of the key used and its value. The condition
    **(\*==\*)** in this rule specifies that the HASH privacy provider must be
    used to privacy protect any remaining elements of the telecom array.

   The FHIR Array Query Condition method supports the following syntax for
   associating a specific array node with a supported privacy provider:

**Syntax for a FHIR array query condition rule**

```
   {
        "jsonPath": "/fhir/resourceType/arrayNode/dataNode(descriptionNodeName==descriptionNodeValue)",
        "rule": "MASKING_RULE_NAME"
    }
```

>   where

> -   **resourceType** is the FHIR resource type to which this rule is applied to, for example,
    Location.

> -   **arrayNode** is the path to the array node that contains the sibling
    children nodes, for example, telecom.

> -   **dataNode** is the name of the element that contains the data value, for example, value.

> -   **(descriptionNodeName==descriptionNodeValue)** is the condition used to
    identify the specific child node. The condition is appended to the dataNode
    within a pair of parentheses.

> -   **descriptionNodeName** is the name of describing node that described the
    data type (for example, system). If this value is set to an asterisk \*
    (wildcard), then all the data child nodes are processed.

> -   **==** is the equal operator (**==** is the only operator currently supported).

> -   **descriptionNodeValue** is the value of the describing node (for example, phone).
    If the value is set to an asterisk (\*), then all the description nodes with
    the specified descriptionNodeName are processed regardless of its values.

   **Note: The order of the FHIR array query condition rules in the data de-identification configuration file is very  important.** The rules must be listed from the most specific to the most general rule. This is because each rule
   is examined (to be triggered for a data element) only if none of the previous rules was triggered for the same element. If specified, the rule with condition (\*==\*) must always be the last rule that could be applied
   to the particular FHIR array.


###### Intermixing the FHIR Array Index format and the FHIR Array Query Condition format

   Having presented the two methods that are offered by the Data
   De-Identification Service to associate FHIR array data elements with one or
   more privacy providers, **it is important to note that it is considered
   invalid syntax to intermix the FHIR Array Index method and the FHIR Array
   Query Condition method for elements of a FHIR array**.

   To illustrate this, in Example 8, the first de-identification rule is
   valid. This is because it usies the syntax of the FHIR Array Index method to process a
   FHIR array data element based on the value of a key (valueString). However, the
   second rule is not valid because the syntax used contains both the
   square brackets (used by the FHIR Array Index method to reference a FHIR
   array data element based on its index) and the key-value pair (used by the
   FHIR Array Query Condition method to reference a FHIR array data element
   based on its key value).

   **Example 8: Intermixing the syntax of the two FHIR array processing methods**

```
   {
        "jsonPath": "/fhir/Group/extension/extension/url(valueString==sample)",
        "rule": "HASH"
   }

   {
        "jsonPath": "/fhir/Group/extension[1]/extension/url(valueString==sample)",
        "rule": "HASH"
   }
```
   In the above example, the second rule triggers an
   IllegalArgumentException with the message
   **Cannot intermix FHIR arrays by index and arrays by query in the same masking rule.**
   Subsequently, this logs an error message in the application log. The error message has the
   following structure:

| **Message ID** | **Message text**                                                                                        | **Replacement variables**                         |
|----------------|---------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| WPH2013E       | {0} - The rule provided for key {1} contains features that are invalid or incompatible with each other. | {0}: Provider class name {1}: Invalid path format |

   Here is an example of the log message that is produced:

>   17/07/17 17:00:57 - 10.20.30.40 - WPHDeid - ipv-core - whc-lsf-tenant -
>   ERROR - LogManager -
>   com.ibm.research.drl.prima.providers.masking.fhir.FHIRGenericMaskingProvider

   - The rule provided for key /contact[1].telecom/value(system==email)
   contains features that are invalid or incompatible with each other.

   **Note:** To process the data elements of a FHIR array, you can use either the FHIR Array Index method or the FHIR
   Array Query Condition method. **Do not use both methods.** It is the responsibility of the onboarder
   to never process the same FHIR data elements with both methods.

### certificateId (String, Optional)
Use the optional `certificateId` field to provide a string identifying the masking configuration file in the audit trail output. This can be useful to track which configuration was used to de-identify specific records.

### defaultNoRuleResolution (Boolean, Optional)
Set to **true** to maintain any field that is not associated with a masking rule. Set to **false** to redact fields that are not associated with a masking rule.  If not specified, the default value is **true**.

