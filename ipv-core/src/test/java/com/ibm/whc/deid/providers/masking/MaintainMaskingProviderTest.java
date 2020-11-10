/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class MaintainMaskingProviderTest extends TestLogSetUp {

  @Test
  public void testMaintain_NonEmptyInput() throws Exception {

		MaintainMaskingProvider maskingProvider = new MaintainMaskingProvider();

    String originalValue = "*A-Acc,DDD123455;>";
    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.equals(originalValue));
  }

  @Test
  public void testMaintain_NullInput() throws Exception {

		MaintainMaskingProvider maskingProvider = new MaintainMaskingProvider();

    String originalValue = null;
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals(null, maskedValue);
  }
}
