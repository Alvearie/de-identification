#!/bin/bash
set -e

# Script to create a toolchain.  The toolchain name and the GIT URL is stored in toolchain.properties

# Usage: Set the following property in the environment variable and run the script

# IBM_CLOUD_API_KEY       # IBM cloud API Key used for pipeline execution.  Note the deid function id key does not work.  Please use a personal key
# DOCKER_API_KEY          # IBM CDT API used to pull docker images
# GIT_API_KEY
# DEVELOPER_BRANCH        # name of the GIT branch, if empty or null defaults to master
# DEVELOPER_ID            # name used to specify namespace/umbrella repo

export TOOLCHAIN_BRANCH="stable-3.3.1"
export WHC_COMMONS_BRANCH=${TOOLCHAIN_BRANCH}
export INPUT_GIT_BRANCH=`git rev-parse --abbrev-ref HEAD` # get the current branch
export gitrepourl="https://github.com/dumashwang/de-identification" # CI git repo url

export TOOLCHAIN_NAME=alvearie-de-identification-CI-${INPUT_GIT_BRANCH}-${TOOLCHAIN_BRANCH}
export TOOLCHAIN_TEMPLATE_BRANCH=stable-oc-3.3.1

# if DEVELOPER_BRANCH env variable is not set or null, use master branch
export DEVELOPER_BRANCH="${DEVELOPER_BRANCH:-master}"
export DEVELOPER_ID="${DEVELOPER_ID:-ns}"

export INPUT_GIT_UMBRELLA_BRANCH="openshift"-${DEVELOPER_ID}

export CLUSTER_NAMESPACE="deid"-${DEVELOPER_ID}

# Get the createToolchain.sh from whc-toolchain/whc-commons
curl -sSL -u "${GIT_USER}:${GIT_API_KEY}" "https://raw.github.ibm.com/whc-toolchain/whc-commons/${TOOLCHAIN_BRANCH}/tools/createToolchain.sh" > createToolchain.sh
chmod 755 createToolchain.sh

# Get the property file
curl -sSL -u "${GIT_USER}:${GIT_API_KEY}" "https://raw.github.ibm.com/de-identification/de-id-devops/${DEVELOPER_BRANCH}/scripts/common.properties" > common.properties
source common.properties

./createToolchain.sh -t CI -b ${TOOLCHAIN_BRANCH} -s common.properties -c ${TOOLCHAIN_NAME} -m ${gitrepourl} -i ${INPUT_GIT_BRANCH}  -v ${INPUT_GIT_UMBRELLA_BRANCH} -b ${TOOLCHAIN_TEMPLATE_BRANCH}


