#
# Create a GIT branch from master if the GIT branch does not exist
#
#
# Params:
#   Name of the repo
#   Name of the branch in the repo
#
function createBranch () {
  repo=$1
  branch=$2
  BRANCH_EXIST=`git branch -a |grep $branch`
  if [ -z "${BRANCH_EXIST}" ]; then
     echo "Branch $branch does not exist.  Creating the branch from master now"
     git checkout master
     git pull
     git checkout -b $branch
     git push --set-upstream origin $branch
  else
     echo "Branch exist $BRANCH_EXIST"
     git checkout $branch
     git pull
  fi
}

function pushCopyrightChangesToRepo() {
  repo=$1

  echo "Push copyright changes to $repo"

  TEMP_BRANCH="TOOLING_TEMP_$(date +%Y%m%d%H%M%S)"
  createBranch $repo $TEMP_BRANCH
  
  # gather up the files
  FUNCTIONAL_ID=c3cvpfj7@ca.ibm.com
  git config --global user.email "$FUNCTIONAL_ID"
  git config --global user.name "$FUNCTIONAL_ID"
  git add .
  git commit -m "Push copyright changes"
  git push

  # Create a pull request
  createPullRequest $TEMP_BRANCH "Trigger Tooling build"

  # Delete the temporary branch
  git push origin --delete $TEMP_BRANCH
}

function createPullRequest() {
  branch=$1
  message=$2

  FUNCTIONAL_ID=c3cvpfj7@ca.ibm.com
  git config --global user.email "$FUNCTIONAL_ID"
  git config --global user.name "$FUNCTIONAL_ID"

  echo "Updating $repo with pull request: $message"
   
  # Create a pull request
  PULL_REQUEST=`curl -H "Authorization: token $GITAPIKEY " \
     --request POST\
     --data '{ "title": "Tooling create PR", "body": "Tooling create PR", "head": "'$branch'", "base": "master" }'\
     https://api.github.com/repos/Alvearie/de-identification/pulls | grep "\"url\"" |grep pulls`
  echo "PULL_REQUEST $PULL_REQUEST"
 
  # Extract the URL by removing the last character, get the second token, and trim quotes
  PULL_REQUEST_URL=`echo ${PULL_REQUEST%?} | awk '{print $2}' |tr -d '"'`
  echo "PR URL $PULL_REQUEST_URL"
  
  # Approve the pull request
  curl -H "Authorization: token $GITAPIKEY" \
       --request POST\
       --data '{ "body": "Tooling approve PR", "event": "APPROVE" }'      $PULL_REQUEST_URL/reviews

  # Merge the PR
  curl -H "Authorization: token $GITAPIKEY" \
      --request PUT\
      --data '{"commit_title": "Tooling merge PR", "commit_message": "Tooling merge PR", "merge_method": "merge"  }'\
      $PULL_REQUEST_URL/merge
}


#Handle files that have the wrong copyright 
find . -type f | grep .java | xargs grep -Pzl 'The source code for this program' | awk -F: '{ print $1 }' > filesWithIncorrectCopyrights.txt
for f in `cat filesWithIncorrectCopyrights.txt`; do awk -i inplace '/package/{i++}i' ${f}; done

#Handle files that do not have a copyright
find . -type f | grep .java | xargs grep -Pzvl '/\*\s* \* \(C\) Copyright IBM Corp\. 2016\,2020\s*\*\s*\* SPDX\-License\-Identifier\: Apache\-2\.0\s*\*/' | xargs grep -Pzvl '/\*\s* \* \(C\) Copyright IBM Corp\. 2020\s*\*\s*\* SPDX\-License\-Identifier\: Apache\-2\.0\s*\*/' | awk -F: '{ print $1 }' > filesWithoutCopyrights.txt
for f in `cat filesWithoutCopyrights.txt`; do echo ${f}; sed -i '1s/^/\/*\n \* \(C\) Copyright IBM Corp\. 2016,2020\n \*\n \* SPDX-License-Identifier: Apache-2\.0\n \*\/\n/' ${f}; done

# Create a pull request and commit it, if there are files
git diff --exit-code
if [ $? != "0" ]; then
    pushCopyrightChangesToRepo de-identification
else
    echo "No files needed their copyrights adjusted"
fi
