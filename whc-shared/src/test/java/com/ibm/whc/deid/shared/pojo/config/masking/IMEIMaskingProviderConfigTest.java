/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class IMEIMaskingProviderConfigTest {


  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testEqualsHashCode() {
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    assertFalse(config.equals(null));
    assertFalse(config.equals("test"));
    assertFalse(config.equals(new IBANMaskingProviderConfig()));
    assertTrue(config.equals(config));
    // multiple calls, same value
    assertEquals(config.hashCode(), config.hashCode());

    IMEIMaskingProviderConfig other = new IMEIMaskingProviderConfig();
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setUnspecifiedValueHandling(2);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setUnspecifiedValueHandling(2);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setUnspecifiedValueReturnMessage("x");
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setUnspecifiedValueReturnMessage("x");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setPreserveTAC(!config.getPreserveTAC());
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setPreserveTAC(config.getPreserveTAC());
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());
  }

}
