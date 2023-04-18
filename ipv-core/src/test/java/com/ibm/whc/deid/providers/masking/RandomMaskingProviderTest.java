/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class RandomMaskingProviderTest extends TestLogSetUp {
  /*
   * Note: Test cases for alphanumeric values and also for a sample of other characters (* - , > ;)
   * to verify they have been preserved in the masked value.
   */
  @Test
  public void testAlphaNumeric() throws Exception {

    RandomMaskingProvider maskingProvider = new RandomMaskingProvider();

    String value = "AAAAcccDDD12345566";
    String maskedValue = maskingProvider.mask(value);

    assertFalse(maskedValue.equals(value));
    System.out.println(maskedValue);
    assertTrue(maskedValue.length() == value.length());

    for (int i = 0; i < maskedValue.length(); i++) {
      char c1 = maskedValue.charAt(i);
      char c2 = value.charAt(i);
      assertTrue(Character.getType(c1) == Character.getType(c2));
    }
  }

  @Test
  public void testOtherChar() throws Exception {

    RandomMaskingProvider maskingProvider = new RandomMaskingProvider();

    String value = "*A-Acc,DDD123455;>";
    String maskedValue = maskingProvider.mask(value);

    assertFalse(maskedValue.equals(value));
    System.out.println(maskedValue);
    assertTrue(maskedValue.length() == value.length());

    for (int i = 0; i < maskedValue.length(); i++) {
      char c1 = maskedValue.charAt(i);
      char c2 = value.charAt(i);
      assertTrue(Character.getType(c1) == Character.getType(c2));
      if (i == 0 || i == 2 || i == 6 || i == 16 || i == 17) {
        assertTrue(c1 == c2);
      }
    }
  }

  @Test
  public void testMaskNullRandomInputReturnNull() throws Exception {
    MaskingProvider maskingProvider = new RandomMaskingProvider();

    String invalidRandom = null;
    String maskedRandom = maskingProvider.mask(invalidRandom);

    assertEquals(null, maskedRandom);
  }
}
