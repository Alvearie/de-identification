#!/usr/bin/env bash
#
#  (C) Copyright IBM Corp. 2021,2022
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
# Check Java target version                             #
#                                                       #
# To change the release level used to build the jars,   #
# add this parameter to the CI toolchain after it is    #
# built.  Values are Java releases such as 8 or 11.     #
#########################################################
if [ ! -z "$JAVA_COMPILER_RELEASE" ]; then
	JAVA_RELEASE="-java${JAVA_COMPILER_RELEASE}"
	mvn --no-transfer-progress versions:set-property -Dproperty=maven.compiler.release -DnewVersion=${JAVA_COMPILER_RELEASE}
	rc=$((rc || $? ))
	if [[ ! "$rc" == "0" ]]; then
    	echo "BUILD FAILURE; COULD NOT UPDATE MAVEN JAVA COMPILER VERSION";
    	exit $rc;
	fi	
else
	JAVA_RELEASE=
    echo building jars at default Java release level
fi

#########################################################
# Set the version                                       #
#########################################################
# If the branch is master, use the ${RELEASE_VERSION}-SNAPSHOT.
# If the branch is a release branch, use the ${RELEASE_VERSION}.
# If the branch is not master, include branch name in the version.
RELEASE_VERSION=1.2.0
RELEASE_BUILD=false
GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
if [ "$GIT_BRANCH" == "master" ]; then
    echo "-Drevision=${RELEASE_VERSION}${JAVA_RELEASE}-SNAPSHOT" > .mvn/maven.config
elif [[ "${GIT_BRANCH}" == "release"* ]]; then
    echo "-Drevision=${RELEASE_VERSION}${JAVA_RELEASE}" > .mvn/maven.config
    RELEASE_BUILD=true
else
    echo "-Drevision=${RELEASE_VERSION}-${GIT_BRANCH}${JAVA_RELEASE}-SNAPSHOT" > .mvn/maven.config
fi
cat .mvn/maven.config

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
    MAVEN_REPO=releases::default::https://artifactory.commops.truvenhealth.com:443/artifactory/wh-de-id-release-maven-local
else    
    MAVEN_REPO=snapshots::default::https://artifactory.commops.truvenhealth.com:443/artifactory/wh-de-id-snapshot-maven-local
fi

mvn -B deploy -DaltDeploymentRepository=${MAVEN_REPO} -DskipTests=true

rc=$((rc || $? ))
if [[ ! "$rc" == "0" ]]; then
    echo "FAILED to deploy artifacts; SEE ABOVE OUTPUT FOR DETAILS AND RESOLUTION";
    exit $rc;
fi

echo preDockerBuild.sh end
