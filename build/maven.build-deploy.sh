#!/usr/bin/env bash
#
#
#  SPDX-License-Identifier: Apache-2.0
#
#  Maven build script 
#
set -xe

mvn --version

#########################################################
# Check Java target compiler version                    #
#                                                       #
# To change the release level used to build the jars,   #
# set the JAVA_COMPILER_RELEASE environment variable.   #
# Values are Java releases such as 8 or 11.             #
#########################################################
if [ ! -z "$JAVA_COMPILER_RELEASE" ]; then
	JAVA_RELEASE="-java${JAVA_COMPILER_RELEASE}"
	mvn --no-transfer-progress versions:set-property -Dproperty=maven.compiler.release -DnewVersion=${JAVA_COMPILER_RELEASE}
else
	JAVA_RELEASE=
    echo building jars at default Java release level
fi

#########################################################
# Set the revision                                      #
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
mvn --no-transfer-progress -s build/maven.settings.xml -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn clean install 

#########################################################
# Deploy the binaries to Artifactory                    #
#########################################################
if [[ "${DEPLOY}" == "true" ]]; then
	if [ "${RELEASE_BUILD}" == "true" ]; then
	    MAVEN_REPO=releases::default::https://artifactory.commops.truvenhealth.com:443/artifactory/wh-de-id-release-maven-local
	else    
	    MAVEN_REPO=snapshots::default::https://artifactory.commops.truvenhealth.com:443/artifactory/wh-de-id-snapshot-maven-local
	fi
	mvn --no-transfer-progress -s build/maven.settings.xml deploy -DaltDeploymentRepository=${MAVEN_REPO} -DskipTests=true
fi
