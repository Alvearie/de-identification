/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;

public class HashMaskingProviderTest extends TestLogSetUp {
  /*
   * Note: In addition to default SHA-256 algorithm, also tests for the SHA-512 algorithm.
   */
  @Test
  public void testMask() throws Exception {
    HashMaskingProvider maskingProvider = new HashMaskingProvider();

    String value = "test";
    // sha-256 by default
    String mask = maskingProvider.mask(value).toLowerCase();
    // System.out.println("testMask() masked: [" + masked + "]");
    assertTrue(mask.equals("9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"));
  }

  @Test
  public void testMask_SHA512() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setAlgorithmDefault("SHA-512");
    HashMaskingProvider maskingProvider = new HashMaskingProvider(config);

    String value = "test";
    // sha-512
    String mask = maskingProvider.mask(value).toLowerCase();

    System.out.println("SHA-512 masked:" + mask);
    assertTrue(mask.equals(
        "ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff"));

    System.out.println("=======>" + this.getClass().getName() + ", testMask_SHA512() Done ");
  }

  @Test
  public void testMask_withSalt() throws Exception {
    String value = "test";
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setAlgorithmDefault("SHA-512");
    config.setSalt("whooohooosalt");
    HashMaskingProvider BlandMaskingProvider = new HashMaskingProvider(config);
    String maskSalt = BlandMaskingProvider.mask(value).toLowerCase();

    config = new HashMaskingProviderConfig();
    config.setAlgorithmDefault("SHA-512");
    HashMaskingProvider SaltyMaskingProvider = new HashMaskingProvider(config);
    String maskBland = SaltyMaskingProvider.mask(value).toLowerCase();

    System.out.println("=======> no salt  : " + maskBland);
    System.out.println("=======> with salt: " + maskSalt);
    assertFalse(maskSalt.equals(maskBland));

    System.out.println("=======>" + this.getClass().getName() + ", testMask_withSalt() Done ");
  }

  @Test
  public void testMaskNullHashInputReturnNull() {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    String invalidHash = null;
    String maskedHash = maskingProvider.mask(invalidHash);

    assertEquals(null, maskedHash);
  }

  @Test(expected = RuntimeException.class)
  public void testMaskInvalidAlgorithm() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setAlgorithmDefault("unknown");
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    String originalValue = "test";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals(null, maskedValue);
  }

  @Test(expected = RuntimeException.class)
  public void testExceptionOnMasking() {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setAlgorithmDefault("INVALID");
    HashMaskingProvider maskingProvider = new HashMaskingProvider(config);

    String originalValue = "test";
    maskingProvider.mask(originalValue);
  }

  @Test
  public void testMaskWithBeginOffset_inMiddle() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(8);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with begin offset in the middle of the value and default end
    // offset to the end of value.
    // This should in a partially masked value: "partOne/<masked value>"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue
        .equals("partOne/6FD8B6744F04D4F948CE40FC1DD650CD8F8407B9F194B245E8DD13C491C5201F"));
  }

  @Test
  public void testMaskWithBeginOffset_startOfValue() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(0);
    HashMaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with begin offset as the start of the value and default end
    // offset to the end of value.
    // This should result in a fully masked value.
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        maskedValue.equals("E56892CED6DC5C3FBBF94E44D27B75F714D2BE51EE25C2F8DBE5F5D7C0293BB1"));
  }

  @Test
  public void testMaskWithBeginOffset_endOfValue_invalidOffsetOption1() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(25);
    config.setOffsetInvalidOffsetValue(1);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with begin offset as the end of the value and default end offset
    // to the end of value.
    // This should result in a null value, using option 1 for
    // "hashing.offset.invalidOffsetValue"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertNull(maskedValue);
  }

  @Test
  public void testMaskWithEndOffset_inMiddle() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetEnd(7);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with end offset in the middle of the value and default begin
    // offset at the start of value.
    // This should result in a partially masked value:
    // "<masked value>/partTwo/partThree"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.equals(
        "1E3D8AE50B00D2527A5E74FCC0EC721B2CDCB8CB715B43FD8DA15E29EFB0A976/partTwo/partThree"));
  }

  @Test
  public void testMaskWithEndOffset_startOfValue_invalidOffsetOption2() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetEnd(0);
    config.setOffsetInvalidOffsetValue(2);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with end offset as start of the value and default begin offset
    // at the start of value.
    // This should result in a null value, using option 2 for
    // "hashing.offset.invalidOffsetValue"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.isEmpty());
  }

  @Test
  public void testMaskWithEndOffset_endOfValue() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetEnd(25);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with end offset as the end of the value and default begin offset
    // at the start of value.
    // This should result in a fully masked value.
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        maskedValue.equals("E56892CED6DC5C3FBBF94E44D27B75F714D2BE51EE25C2F8DBE5F5D7C0293BB1"));
  }

  @Test
  public void testMaskWithBeginAndEndOffset_inMiddle() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(8);
    config.setOffsetEnd(15);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with a begin and end offset in the middle of the value.
    // This should result in a partially masked value:
    // "partOne/<masked value>/partThree"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.equals(
        "partOne/D28B0E46851E018BB5FFE89FC12BA94C0B78E4A87C0CA57FE2365F62937949ED/partThree"));
  }

  @Test
  public void testMaskWithBeginAndEndOffset_startAndEndOfValue() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(0);
    config.setOffsetEnd(25);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with a begin and end offset set to the start and end of the
    // value
    // This should result in a fully masked value.
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        maskedValue.equals("E56892CED6DC5C3FBBF94E44D27B75F714D2BE51EE25C2F8DBE5F5D7C0293BB1"));
  }

  @Test
  public void testMaskWithBeginAndEndOffset_sameOffsetValue_invalidOffsetOption3()
      throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetBegin(7);
    config.setOffsetEnd(7);
    config.setOffsetInvalidOffsetValue(3);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with a begin and end offset set to the same offset in the middle
    // of the value
    // This should result in a fully masked value, using option 3 for
    // "hashing.offset.invalidOffsetValue"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        maskedValue.equals("E56892CED6DC5C3FBBF94E44D27B75F714D2BE51EE25C2F8DBE5F5D7C0293BB1"));
  }

  @Test
  public void testMaskWithHashingOffsetOffsetMaskDeelte_setToTrue() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.setOffsetOffsetMask(true);
    config.setOffsetOffsetMaskDelete(true);
    config.setOffsetBegin(8);
    config.setOffsetEnd(15);
    config.setOffsetInvalidOffsetValue(3);
    MaskingProvider maskingProvider = new HashMaskingProvider(config);

    // Test with a begin and end offset in the middle of the value.
    // This should result in a partially masked value: "<masked value>"
    String originalValue = "partOne/partTwo/partThree";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        maskedValue.equals("D28B0E46851E018BB5FFE89FC12BA94C0B78E4A87C0CA57FE2365F62937949ED"));
  }
}
