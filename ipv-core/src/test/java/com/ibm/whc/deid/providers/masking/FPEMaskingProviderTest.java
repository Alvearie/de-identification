/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;
import com.privacylogistics.FF3Cipher;

public class FPEMaskingProviderTest {

  @Test
  public void testDigitsSuccess() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[0-9]+");
    String original = "897435847";
    String result = provider.mask(original);
    System.out.println(result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 10);
    assertEquals(original, cipher.decrypt(result));
  }

  @Test
  public void testDigitsSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[0-9]{3}+a&[0-9]{6}+@");
    String original = "897a&435847@";
    String result = provider.mask(original);
    System.out.println(result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 10);
    String resultNoSymbols = result.substring(0, 3) + result.substring(5, 11);
    assertEquals("897435847", cipher.decrypt(resultNoSymbols));
  }

  @Test
  public void testDigitsSuccessPadFront() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.FRONT);
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[0-9]+");
    String original = "6789";
    String result = provider.mask(original);
    System.out.println(result);
    // verify length and content
    assertEquals(6, result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 10);
    assertEquals("00" + original, cipher.decrypt(result));
  }

  @Test
  public void testDigitsSuccessSymbolsPadFront() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.FRONT);
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[0-9]{3}+a-A[0-9]{3}+@");
    String original = "6a-A789@";
    String result = provider.mask(original);
    System.out.println(result);
    // verify length and content
    assertEquals(10, result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 10);
    String resultNoSymbols = result.substring(0, 3) + result.substring(6, 9);
    assertEquals("006789", cipher.decrypt(resultNoSymbols));
  }

}
