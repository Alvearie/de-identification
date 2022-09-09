#!/bin/bash

# Test automation Utils script to keep functions that can be
# used to expand the testing of De-Identifcation

#
# Gets the pattern for the executable jar name
# 
# Sets global variable deid_exec_jar_pattern
# 
function getJarPattern() {
	if [ -z "$JAVA_COMPILER_RELEASE" ]; then
		deid_exec_jar_pattern="de-identification-app-.*-exec.jar"
	else
		deid_exec_jar_pattern="de-identification-app-.*-java8*-exec.jar"
	fi
}

#
# Starts the De-Identification Endpoint upon given the path to the endpoint jar
#
# Params:
# path to the De-Identification Endpoint jar
#
function startEndpoint() {
	echo -e "\n########### Starting De-Identification Endpoint ###########"
	echo ""
	# locate jar in the specified directory
	path=$1
	
	# temp
	ls -la
	
	getJarPattern	
	jarName=`ls -la ../target | grep $deid_exec_jar_pattern | awk '{ print $9 }'`
	
	if [ -z "$jarName" ]; then
        echo -e "\n##### Test Failed !!!!!!!!!!!! #####\n"
        echo "Executable jar file not found."
        exit 1
	else
        echo "De-Identification Endpoint jar: $jarName is found in directory: $path as specified by user"
	fi
	
	# Starting Endpoint
	echo ""
	java -jar $path/$jarName &> application.log &
}

#
# Stops De-Identification endpoint using its pid
#
#
function stopEndpoint() {
	echo -e  "\n########### Stopping Endpoint ###########"
	echo ""
	
	echo "Getting De-Identification Endpoint PID"
	getJarPattern
	processId=`ps -ef | grep $deid_exec_jar_pattern | grep -v grep | awk 'NR==1 { print $2 }'`
	
	if [ -z "$processId" ]; then
       echo "Unable to find the application process id"
       exit 1
	else
       echo "Process ID is: $processId"
       echo "Stopping the DeID application"
       kill -9 $processId
	fi
}

#
# Checks if De-Identification Endpoint has started
# If there is an error, Endpoint logs are printed
# 
#
function checkEndpointStarted() {
	echo "########### Checking if De-Identification Endpoint has started ###########"

	count=0
	
	# Wait for De-ID Endpoint to start. Before doing the curl, wait 5 seconds. 
	while [ $count != 5 ]
	do
		sleep 5
		output=$(curl -s http://localhost:8080)
		rc=$?
		if [ $rc -eq 0 ];then
			echo -e "\nDe-ID Endpoint started successfully"
			return "0"
			break
		else
			echo -e "\nWaiting for De-ID Endpoint to start"
			count=$((count+1))
		fi
	done
	
	echo "########### Failed to start De-Identification Endpoint ###########"
	echo ""
	cat application.log
	exit 1
}

#
# Function that checks if the expected values matches the output
# 
# User must name their output result array as matchOutputResult, to check if their expected value is in the output
#
function matches() {
	echo -e "\n########### Checking if output results match expected values ###########"
	echo ""
	
	for string in ${matchOutputResult[@]}
		do
			echo $output | grep $string
			rc=$?
			
			if [ $rc == 0  ]; then
				echo ""
				echo "##### Expected value: $string matched output #####"
				echo ""
			else
				echo "##### Test Failed #####"
				echo ""
				echo "Failed to find String: $string"
				echo ""
				exit 1
			fi
		done
}

#
# Function that checks if the new value is different from original value
# 
# User must name their original values array as originalValues, to check if the original value is masked in the output
#
function doNotMatch() {
	echo -e "########### Checking if original values were masked ###########"
	for string in ${originalValues[@]}
		do
			echo $output | grep $string
			rc=$?
			
			if [ $rc == 1  ]; then
				echo -e "\n##### Output masked. Test Passed #####"
				echo ""
				echo $output
			else
				echo "##### Test Failed #####"
				echo -e "\n##### Original value: $string was not masked #####"
				echo ""
				echo $output
				exit 1
			fi
		done
}

#
# Removes De-Identification Endpoint log file generated when Endpoint was started
#
#
function removeEndpointLogs() {
	echo -e "\n########### Removing application logs ###########"
	rm -rf application.log
}

#
# Function to clean up after running a test and shutting down De-Identificaiton Endpoint
#
#
function teardown() {
	echo -e "\n########### Starting Teardown ###########"
	stopEndpoint
	removeEndpointLogs
}
