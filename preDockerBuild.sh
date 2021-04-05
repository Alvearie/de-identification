#!/usr/bin/env bash
#
#  (C) Copyright IBM Corp. 2021
#
#  SPDX-License-Identifier: Apache-2.0
#
#  Script to build de-identification in IBM toolchain 
#

# Note we do not use a -e flag here. The gitCommitPomFiles function relies on return code
# to determine if a pom.xml file has been changed.  If we set -e, we 'll need to
# modify that method
set -x

rc=0

#########################################################
# Update the maven version for each build               #
#########################################################
# Remember the current directory.  Subsequent functions may go into different directories
export rootDir=`pwd`

DEVELOPER_BRANCH="${DEVELOPER_BRANCH:-master}"
curl -sSL "https://${gitApiKey}@raw.github.ibm.com/de-identification/de-id-devops/${DEVELOPER_BRANCH}/scripts/toolchain_util.sh" > toolchain_util.sh


#########################################################
# Setup the artifactory repo settings                   #
#########################################################
if [ ! -f ${HOME}/.m2/settings.xml ]; then
    mkdir ${HOME}/.m2
fi

curl -sSL "https://${gitApiKey}@raw.github.ibm.com/de-identification/de-id-devops/${DEVELOPER_BRANCH}/scripts/de-identification-settings.xml" > ${HOME}/.m2/settings.xml

DEVELOPER_ID="${DEVELOPER_ID:-}"
echo "-Drevision=1.0.0-${DEVELOPER_ID}-SNAPSHOT" >  .mvn/maven.config

#########################################################
# Main build                                            #
#########################################################
mvn -B clean install 

rc=$((rc || $? ))

if [[ ! "$rc" == "0" ]]; then
    echo "BUILD FAILURE; SEE ABOVE OUTPUT FOR DETAILS AND RESOLUTION";
    exit $rc;
fi

#########################################################
# Deploy the binaries to artifactory using maven        #
#########################################################
mvn -B deploy -DaltDeploymentRepository=snapshots::default::https://na.artifactory.swg-devops.com:443/artifactory/wh-de-id-snapshot-maven-local

rc=$((rc || $? ))

if [[ ! "$rc" == "0" ]]; then
    echo "FAILED to deploy artifacts; SEE ABOVE OUTPUT FOR DETAILS AND RESOLUTION";
    exit $rc;
fi
