/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.util.Tuple;

public interface IdentifierWithOffset {
  Tuple<Boolean, Tuple<Integer, Integer>> isOfThisTypeWithOffset(String data);
}
