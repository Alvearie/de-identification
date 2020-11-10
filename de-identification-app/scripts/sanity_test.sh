#!/bin/bash

# Path to De-Identification Endpoint jar
path=../target

# Expected Results
string_1="EA72C79594296E45B8C2A296644D988581F58CFAC6601D122ED0A8BD7C02E8BF"
string_2="96BD923157C731249A40C36426FC326062AD3B2904ED6792B3F404F223D35651"
string_3="9345A35A6FDF174DFF7219282A3AE4879790DBB785C70F6FFF91E32FAFD66EAB"

# Original values that are expected to be masked
string_4="Chalmers"
string_5="+1-3471234567"

# Source the test automation utils script
source test_automation_utils.sh

# Start De-Identification Endpoint
startEndpoint $path

# Check if De-Identification has started or not
checkEndpointStarted

# Calling De-Identification Endpoint to Mask data
# User must set Masking configuration and FHIR Input
echo ""
echo -e "\n########### De-Identifying data ###########"
echo ""
output=$(curl -k -v POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{ "config":"{\"rules\":[{\"name\":\"HASH\",\"maskingProviders\":[{\"type\":\"HASH\"}]},{\"name\":\"PHONE\",\"maskingProviders\":[{\"type\":\"PHONE\"}]},{\"name\":\"NAME\",\"maskingProviders\":[{\"type\":\"NAME\"}]},{\"name\":\"MaskBirthDay\",\"maskingProviders\":[{\"type\":\"DATETIME\",\"generalizeYear\":true}]}],\"json\":{\"schemaType\":\"FHIR\",\"messageTypeKey\":\"resourceType\",\"messageTypes\":[\"Patient\"],\"maskingRules\":[{\"jsonPath\":\"/fhir/Patient/name/given\",\"rule\":\"HASH\"},{\"jsonPath\":\"/fhir/Patient/name/family\",\"rule\":\"NAME\"},{\"jsonPath\":\"/fhir/Patient/telecom/value\",\"rule\":\"PHONE\"},{\"jsonPath\":\"/fhir/Patient/birthDate\",\"rule\":\"MaskBirthDay\"}]}}" , "data": [  "{\"resourceType\":\"Patient\",\"id\":\"example\",\"name\":[{\"use\":\"official\",\"family\":\"Chalmers\",\"given\":[\"Peter\",\"James\"]},{\"use\":\"usual\",\"given\":[\"Jim\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"+1-3471234567\",\"use\":\"work\",\"rank\":1}],\"birthDate\":\"1974-12-25\"}"  ], "schemaType": "FHIR" }' 'http://localhost:8080/api/v1/deidentification')

echo ""
echo "De-Identified data is: "
echo $output

# User must name their output result array as matchOutputResult, to check if their expected value is in the output
export matchOutputResult=($string_1 $string_2 $string_3)
matches

# User must name their original values array as originalValues, to check if the original value is masked in the output
export originalValues=($string_4 $string_5)
doNotMatch

# Teardown
teardown

echo -e "\n########### All Tests Successfully passed ###########"
