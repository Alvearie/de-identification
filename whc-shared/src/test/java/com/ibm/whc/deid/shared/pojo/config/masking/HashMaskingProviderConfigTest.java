/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class HashMaskingProviderConfigTest {

  @Test
  public void testSetSalt() {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    assertEquals("", config.getSalt());
    config.setSalt("xx");
    assertEquals("xx", config.getSalt());
    config.setSalt(null);
    assertEquals("", config.getSalt());
  }

}
