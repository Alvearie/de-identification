/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.util.ContinentManager;
import org.junit.Test;

public class ContinentManagerTest {
  @Test
  public void testLocalization() {
    // this test assumes that GR is loaded by default
    ContinentManager continentManager = new ContinentManager(null);

    String english = "Europe";
    assertTrue(continentManager.isValidKey(english));

    String greek = "Ευρώπη";
    assertTrue(continentManager.isValidKey(greek));
  }
}
