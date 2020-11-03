#!/bin/bash

currentDir=`pwd`

#
# Stop the application
#
function cleanup() {
   echo -e  "\n##### TEARDOWN #####"
   echo "Getting application process ID"
   processId=`ps -ef | grep 'de-id.*exec'|grep -v grep | awk 'NR==1 { print $2 }'`
   if [ -z "$processId" ]; then
       echo "Unable to find the application process id"
   else
       echo "Process ID is: $processId"
       echo "Stopping the DeID application"
       kill $processId
   fi
}

#
#  Make a curl reqest to the application and make sure the result is expected
#
function testDeidApi() {
    echo -e "\n######## VALIDATE TEST OUTPUT WITH EXPECTED RESULTS ########"

    # Expected Results
    string_1="EA72C79594296E45B8C2A296644D988581F58CFAC6601D122ED0A8BD7C02E8BF"
    string_2="96BD923157C731249A40C36426FC326062AD3B2904ED6792B3F404F223D35651"
    string_3="9345A35A6FDF174DFF7219282A3AE4879790DBB785C70F6FFF91E32FAFD66EAB"

    output=$(curl -s POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{ "config":"{\"rules\":[{\"name\":\"HASH\",\"maskingProviders\":[{\"type\":\"HASH\"}]},{\"name\":\"PHONE\",\"maskingProviders\":[{\"type\":\"PHONE\"}]},{\"name\":\"NAME\",\"maskingProviders\":[{\"type\":\"NAME\"}]},{\"name\":\"MaskBirthDay\",\"maskingProviders\":[{\"type\":\"DATETIME\",\"generalizeYear\":true}]}],\"json\":{\"schemaType\":\"FHIR\",\"messageTypeKey\":\"resourceType\",\"messageTypes\":[\"Patient\"],\"maskingRules\":[{\"jsonPath\":\"/fhir/Patient/name/given\",\"rule\":\"HASH\"},{\"jsonPath\":\"/fhir/Patient/name/family\",\"rule\":\"NAME\"},{\"jsonPath\":\"/fhir/Patient/telecom/value\",\"rule\":\"PHONE\"},{\"jsonPath\":\"/fhir/Patient/birthDate\",\"rule\":\"MaskBirthDay\"}]}}" , "data": [  "{\"resourceType\":\"Patient\",\"id\":\"example\",\"name\":[{\"use\":\"official\",\"family\":\"Chalmers\",\"given\":[\"Peter\",\"James\"]},{\"use\":\"usual\",\"given\":[\"Jim\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"+1-3471234567\",\"use\":\"work\",\"rank\":1}],\"birthDate\":\"1974-12-25\"}"  ], "schemaType": "FHIR" }' 'http://localhost:8080/api/v1/deidentification')
    echo "Output: $output"

    for string in $string_1 $string_2 $string_3
    do
        if [[ $output =~ "$string" ]]; then
            echo "Contains the expected string $string"
        else
	    echo -e "\n##### Test Failed !!!!!!!!!!!! #####\n"
	    echo "Failed to find expected string: $string"
            cleanup
	    exit 1
	fi
    done

    echo -e "\n##### Sanity test passed #####\n"
}

# 
#  Start the application
#
function startApp() {
    echo -e "\n##### STARTING THE APPLICATION #####"

    # Get jar file name
    jarName=`ls -la ../target | grep 'de-id.*exec' | awk '{ print $9 }'`

    if [ -z "$jarName" ]; then
        echo -e "\n##### Test Failed !!!!!!!!!!!! #####\n"
        echo "Executable jar file not found."
        exit 1
    else
        echo "Application jar found: $jarName"
    fi

    # Creating log files for the application 
    touch application.log

    # Starting the application 
    java -jar ../target/$jarName &> application.log &

    count=0

    # Wait for the application to start. Before doing the curl, wait 5 seconds. 
    while [ $count != 5 ]
    do
	sleep 5
	output=$(curl -s http://localhost:8080)
	
	if [ $? -eq 0 ];then
                echo "Application started successfully"
		break
	else
		echo "Waiting for application to start"
		count=$((count+1))
	fi
    done

    if [ $count == 5 ];then
	echo "Failed to start the application"
	cat application.log
        cleanup
	exit 1
    fi
}

startApp

testDeidApi

cleanup

echo "Removing the application logs"
rm -rf application.log
