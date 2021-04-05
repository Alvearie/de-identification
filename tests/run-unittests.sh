#!/bin/bash
#
#  (C) Copyright IBM Corp. 2021
#
#  SPDX-License-Identifier: Apache-2.0
# 
#  Format junit test result for IBM toolchain
#
#

echo -e '<?xml version="1.0" encoding="UTF-8" ?>\n<testsuites>' > unittest.xml; for i in `find ../ -name surefire-reports`; do for j in `ls $i/*xml`; do cat $j | grep -v '<?xml version' >> unittest.xml; echo >> unittest.xml; done; done; echo '</testsuites>' >> unittest.xml; cat unittest.xml

