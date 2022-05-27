#!/usr/bin/env bash
#
#  (C) Copyright IBM Corp. 2021, 2022
#
#  SPDX-License-Identifier: Apache-2.0
#
#  Script to build de-identification in IBM toolchain 
#

# Note we do not use a -e flag here. The gitCommitPomFiles function relies on return code
# to determine if a pom.xml file has been changed.  If we set -e, we 'll need to
# modify that method
set -x

echo preDockerBuild.sh start

rc=0

# Remember the current directory.  Subsequent functions may go into different directories.
export rootDir=`pwd`

# This is the branch name for de-id-devops
DEVOPS_BRANCH="${DEVOPS_BRANCH:-master}"

curl -sSL "https://${gitApiKey}@raw.githubusercontent.com/WH-WH-de-identification/de-id-devops/${DEVOPS_BRANCH}/scripts/toolchain_util.sh" > toolchain_util.sh
source toolchain_util.sh

git submodule update --init --recursive

#########################################################
# Setup the artifactory repo settings                   #
#########################################################
if [ ! -f ${HOME}/.m2/settings.xml ]; then
    mkdir -p ${HOME}/.m2
fi

curl -sSL "https://${gitApiKey}@raw.githubusercontent.com/WH-WH-de-identification/de-id-devops/${DEVOPS_BRANCH}/scripts/de-identification-settings.xml" > ${HOME}/.m2/settings.xml

#########################################################
# Set the version                                       #
#########################################################
# If the branch is master, use the ${RELEASE_VERSION}-SNAPSHOT.
# If the branch is a release branch, use the ${RELEASE_VERSION}.
# If the branch is not master, include branch name in the version.
RELEASE_VERSION=1.1.3
RELEASE_BUILD=false
GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
if [ "$GIT_BRANCH" == "master" ]; then
    echo "-Drevision=${RELEASE_VERSION}-SNAPSHOT" > .mvn/maven.config
elif [[ "${GIT_BRANCH}" == "release"* ]]; then
    echo "-Drevision=${RELEASE_VERSION}" > .mvn/maven.config
    RELEASE_BUILD=true
else
    echo "-Drevision=${RELEASE_VERSION}-${GIT_BRANCH}-SNAPSHOT" > .mvn/maven.config
fi
cat .mvn/maven.config

#########################################################
# CI Validate                                           #
#########################################################
# If we are running ci validate toolchain, just build the jar files and exit
echo "Taskname $taskname"
if [ "$taskname" == "civalidate" ]; then
  # In CIVALIDATE we need the settings.xml in same directory
  # and a maven repository.  The jar file dependencies are download in this step
  # so that later in sonarqube stages, the jar files are available.
  mkdir -p ./m2/repository
  curl -sSL "https://${gitApiKey}@raw.githubusercontent.com/WH-WH-de-identification/de-id-devops/${DEVOPS_BRANCH}/scripts/de-identification-settings.xml" > ./m2/settings.xml

  mvn -B -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn clean install -Dmaven.repo.local=./m2/repository

  # Run the sonarqube scan.  This scan is going to fail as there is no sonarqube pod running yet.
  # The purpose is for maven to download the correct dependencies for sonarqube.
  echo "Running sonarqube to get dependencies.  This is expected to fail."
  mvn sonar:sonar -B -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn -Dmaven.repo.local=./m2/repository
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
if [ "${RELEASE_BUILD}" == "true" ]; then
    MAVEN_REPO=releases::default::https://na.artifactory.swg-devops.com:443/artifactory/wh-de-id-release-maven-local
else    
    MAVEN_REPO=snapshots::default::https://na.artifactory.swg-devops.com:443/artifactory/wh-de-id-snapshot-maven-local
fi

mvn -B deploy -DaltDeploymentRepository=${MAVEN_REPO} -DskipTests=true

rc=$((rc || $? ))
if [[ ! "$rc" == "0" ]]; then
    echo "FAILED to deploy artifacts; SEE ABOVE OUTPUT FOR DETAILS AND RESOLUTION";
    exit $rc;
fi

echo preDockerBuild.sh end
