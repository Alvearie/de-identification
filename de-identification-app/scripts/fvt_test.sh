#!/bin/bash

# Path to De-Identification Endpoint jar
path=../target

# Source the test automation utils script
source test_automation_utils.sh

# Start De-Identification Endpoint
startEndpoint $path

# Check if De-Identification has started or not
checkEndpointStarted

# Calling De-Identification Endpoint to Mask data
mvn test -Dtest=AllMaskingProviderTest

# Teardown
teardown

echo -e "\n########### All Tests Successfully passed ###########"
