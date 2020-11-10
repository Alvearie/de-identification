/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.util.ReligionManager;
import org.junit.Test;

public class ReligionManagerTest {
  @Test
  public void testLookupSuccessful() throws Exception {
    ReligionManager religionManager = new ReligionManager(null);
    String religion = "Catholic";
    assertTrue(religionManager.isValidKey(religion));

    religion = "caTHolic";
    assertTrue(religionManager.isValidKey(religion));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    ReligionManager religionManager = new ReligionManager(null);
    assertTrue(religionManager.isValidKey(religionManager.getRandomKey()));
  }
}
