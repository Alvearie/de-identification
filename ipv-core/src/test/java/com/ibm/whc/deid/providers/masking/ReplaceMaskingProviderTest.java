/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.shared.pojo.config.masking.ReplaceMaskingProviderConfig;

public class ReplaceMaskingProviderTest extends TestLogSetUp {
  /*
   * Tests all boolean options for both true and false values. Tests for various range of offsets
   * and number of characters to preserve. It also tests for alphanumeric and sample of other
   * characters (* - ; , >).
   */

  @Test
  public void testMask() {
        ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
        MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.equals("asd"));
  }

  @Test
  public void testMaskOffset() {
    // check offset
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(1);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.equals("sda"));
  }

  @Test
  public void testMaskOffsetLargerThanDataLength() {
    // check when offset is greater than data length
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(10);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals(null, maskedValue);
  }

  @Test
  public void testMaskOffsetEqualsDataLength() {
    // check when offset is equal to data length
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(8);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "12345678";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals(null, maskedValue);
  }

  @Test
  public void testRandom() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(0);
    configuration.setMaskPreserve(2);
    configuration.setMaskReplaceWithAsterisks(true);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertNotEquals(maskedValue, originalValue);
    assertEquals("as", maskedValue.substring(0, 2));

    for (int i = 2; i < maskedValue.length(); i++) {
      assertTrue(Character.isLowerCase(maskedValue.charAt(i)));
    }
  }

  @Test
  public void testRandomMiddle() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(1);
    configuration.setMaskPreserve(3);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertNotEquals(maskedValue, originalValue);
    assertEquals("sda", maskedValue.substring(1, 4));

    assertTrue(Character.isLowerCase(maskedValue.charAt(0)));
    for (int i = 4; i < maskedValue.length(); i++) {
      assertTrue(Character.isLowerCase(maskedValue.charAt(i)));
    }
  }

  @Test
  public void testRandomEnd() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(7);
    configuration.setMaskPreserve(3);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);

    assertNotEquals(maskedValue, originalValue);
    assertEquals("d", maskedValue.substring(7, 8));

    for (int i = 0; i < 7; i++) {
      assertTrue(Character.isLowerCase(maskedValue.charAt(i)));
    }
  }

  @Test
  public void testRandomEndByOne() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(6);
    configuration.setMaskPreserve(1);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);

    assertNotEquals(maskedValue, originalValue);
    assertEquals("a", maskedValue.substring(6, 7));

    for (int i = 0; i < 6; i++) {
      assertTrue(Character.isLowerCase(maskedValue.charAt(i)));
    }
    assertTrue(Character.isLowerCase(maskedValue.charAt(7)));
  }

  @Test
  public void testRandomAlphaNumeric() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskPreserve(0);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "AAAAcccDDD12345566";
    String maskedValue = maskingProvider.mask(originalValue);

    assertFalse(maskedValue.equals(originalValue));
    System.out.println(maskedValue);
    assertTrue(maskedValue.length() == originalValue.length());

    for (int i = 0; i < maskedValue.length(); i++) {
      char c1 = maskedValue.charAt(i);
      char c2 = originalValue.charAt(i);
      assertTrue(Character.getType(c1) == Character.getType(c2));
    }
  }

  @Test
  public void testRandomOtherChar() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskPreserve(0);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "*A-Acc,DDD123455;>";
    String maskedValue = maskingProvider.mask(originalValue);

    assertFalse(maskedValue.equals(originalValue));
    System.out.println(maskedValue);
    assertTrue(maskedValue.length() == originalValue.length());

    for (int i = 0; i < maskedValue.length(); i++) {
      char c1 = maskedValue.charAt(i);
      char c2 = originalValue.charAt(i);
      assertTrue(Character.getType(c1) == Character.getType(c2));
      if (i == 0 || i == 2 || i == 6 || i == 16 || i == 17) {
        assertTrue(c1 == c2);
      }
    }
  }

  @Test
  public void testAsterisks() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(0);
    configuration.setMaskPreserve(2);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals("as******", maskedValue);
  }

  @Test
  public void testAsterisksMiddle() {
    // check the case we replace something in the middle
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(1);
    configuration.setMaskPreserve(3);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals("*sda****", maskedValue);
  }

  @Test
  public void testAsterisksEnd() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(7);
    configuration.setMaskPreserve(3);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals("*******d", maskedValue);
  }

  @Test
  public void testAsterisksEndByOne() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(6);
    configuration.setMaskPreserve(1);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals("******a*", maskedValue);
  }

  @Test
  public void testMaskOverrun() {
    // check offset
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(7);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasdad";
    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.equals("d"));
  }

  @Test
  public void testMaskNullReplaceInputReturnNull() throws Exception {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);

    String invalidReplace = null;
    String maskedReplace = maskingProvider.mask(invalidReplace);

    assertEquals(null, maskedReplace);
  }

  @Test
  public void testMaintainSymbolsAsterisk() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(0);
    configuration.setMaskPreserve(2);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asdasd-ad";
    String maskedValue = maskingProvider.mask(originalValue);
    System.out.println(maskedValue);
    assertEquals("as****-**", maskedValue);
  }

  @Test
  public void testOtherLanguagesStillMasked() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskOffset(0);
    configuration.setMaskPreserve(0);
    configuration.setMaskReplaceWithAsterisks(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asßda-sdü-ad表意文字";
    String maskedValue = maskingProvider.mask(originalValue);
    System.out.println(maskedValue);
    assertEquals("*****-***-******", maskedValue);
  }

  @Test
  public void testOtherLanguagesStillMaskedWithoutAsterisks() {
    ReplaceMaskingProviderConfig configuration = new ReplaceMaskingProviderConfig();
    configuration.setMaskPreserve(0);
    configuration.setMaskReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ReplaceMaskingProvider(configuration);
    String originalValue = "asßda-sdü-ad表意文字";
    String maskedValue = maskingProvider.mask(originalValue);
    System.out.println(maskedValue);
    assertFalse(maskedValue.equals(originalValue));
    System.out.println(maskedValue);
    assertTrue(maskedValue.length() == originalValue.length());
    assertTrue(!maskedValue.contains("表"));
    assertTrue(!maskedValue.contains("ß"));
    assertTrue(!maskedValue.contains("ü"));
    assertTrue(!maskedValue.contains("字"));
  }
}
