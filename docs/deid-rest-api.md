# RESTful API

You can find the RESTful API for the IBM Data De-Identification service at this location: api/v1/deidentification.

**Example of a call**
```
curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{ "config":"{\"rules\":[{\"name\":\"HASH\",\"maskingProviders\":[{\"type\":\"HASH\"}]},{\"name\":\"PHONE\",\"maskingProviders\":[{\"type\":\"PHONE\"}]},{\"name\":\"NAME\",\"maskingProviders\":[{\"type\":\"NAME\"}]},{\"name\":\"MaskBirthDay\",\"maskingProviders\":[{\"type\":\"DATETIME\",\"generalizeYear\":true}]}],\"json\":{\"schemaType\":\"FHIR\",\"messageTypeKey\":\"resourceType\",\"messageTypes\":[\"Patient\"],\"maskingRules\":[{\"jsonPath\":\"/fhir/Patient/name/given\",\"rule\":\"HASH\"},{\"jsonPath\":\"/fhir/Patient/name/family\",\"rule\":\"NAME\"},{\"jsonPath\":\"/fhir/Patient/telecom/value\",\"rule\":\"PHONE\"},{\"jsonPath\":\"/fhir/Patient/birthDate\",\"rule\":\"MaskBirthDay\"}]}}" , "data": [  "{\"resourceType\":\"Patient\",\"id\":\"example\",\"name\":[{\"use\":\"official\",\"family\":\"Chalmers\",\"given\":[\"Peter\",\"James\"]},{\"use\":\"usual\",\"given\":[\"Jim\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"+1-3471234567\",\"use\":\"work\",\"rank\":1}],\"birthDate\":\"1974-12-25\"}"  ], "schemaType": "FHIR" }' 'http://localhost:8080/api/v1/deidentification'
```

> Tips
>- To format the output JSON on the command line, append `| jq "."` to the curl command.
>- You can send a batch of messages since the `data` input field is an array.

**Response for the above example**

```
{
  "data": [
    {
      "resourceType": "Patient",
      "id": "example",
      "name": [
        {
          "use": "official",
          "family": "NATHO",
          "given": [
            "EA72C79594296E45B8C2A296644D988581F58CFAC6601D122ED0A8BD7C02E8BF",
            "9345A35A6FDF174DFF7219282A3AE4879790DBB785C70F6FFF91E32FAFD66EAB"
          ]
        },
        {
          "use": "usual",
          "given": [
            "96BD923157C731249A40C36426FC326062AD3B2904ED6792B3F404F223D35651"
          ]
        }
      ],
      "telecom": [
        {
          "system": "phone",
          "value": "+1-3478057810",
          "use": "work",
          "rank": 1
        }
      ],
      "birthDate": "1974"
    }
  ]
}
```

Table: Required parameters for the API

| **Parameter name**      | **Type**         | **Description**                                                   | **Default value** |
|-------------------------|------------------|-------------------------------------------------------------------|-------------------|
| config                  | String           | Masking configuration                                             | N/A               |
| data                    | Array of strings | Messages to de-identify                                           | N/A               |
| schemaType              | String           | One of `FHIR` for FHIR over JSON, `GEN` for generic JSON          | N/A               |

> **Note:** These APIs are not fully-hardened and finalized. Subject to change.

## Next steps

- Explore various masking configuration options:

   [masking-config-overview.md](masking-config-overview.md)
