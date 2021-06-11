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

# This is the branch name for de-id-devops
DEVOPS_BRANCH="${DEVOPS_BRANCH:-master}"

curl -sSL "https://${gitApiKey}@raw.github.ibm.com/de-identification/de-id-devops/${DEVELOPER_BRANCH}/scripts/de-identification-settings.xml" > ${HOME}/.m2/settings.xml

# Set the version.  If the branch is master, use the ${RELEASE_VERSION}-SNAPSHOT
# If the branch is not master, include branch name in the version
RELEASE_VERSION=1.0.1
GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
if [ "$GIT_BRANCH" == "master" ]; then
    echo "-Drevision=${RELEASE_VERSION}-SNAPSHOT" >  .mvn/maven.config
else
    echo "-Drevision=${RELEASE_VERSION}-${GIT_BRANCH}-SNAPSHOT" >  .mvn/maven.config
fi
echo "revision:"
cat .mvn/maven.config

# If we are running ci validate toolchain, just build the jar files and exit
# There is no need to build UI or deploy jar files
echo "Taskname $taskname"
if [ "$taskname" == "civalidate" ]; then
  # If we are running the CI validate toolchain, then we only
  # need to build the jar files without any test in
  # the pre docker build. The sonarqube task will run tests

  # In CIVALIDATE we need the settings.xml in same directory
  mkdir -p ./m2/repository
  curl -sSL "https://${gitApiKey}@raw.github.ibm.com/de-identification/de-id-devops/${DEVOPS_BRANCH}/scripts/de-identification-settings.xml" > ./m2/settings.xml

  mvn -B -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn clean install deploy -DaltDeploymentRepository=snapshots::default::https://na.artifactory.swg-devops.com:443/artifactory/wh-de-id-snapshot-maven-local -Dmaven.repo.local=./m2/repository
  exit 0
fi

#########################################################
# Main build                                            #
#########################################################
mvn -B -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn clean install 

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
