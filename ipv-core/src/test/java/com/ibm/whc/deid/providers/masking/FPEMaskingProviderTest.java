/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.fpe.Radix;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
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
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    assertEquals("756378309", result); // result with same keys should always be the same
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS.value());
    assertEquals(original, cipher.decrypt(result));
  }

  @Test
  public void testDigitsSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.BACK); // not needed
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[0-9]{3}+a&[0-9]{6}+@");
    String original = "897a&435847@";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    assertEquals("756a&378309@", result); // result with same keys should always be the same
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS.value());
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
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(6, result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    assertEquals("900369", result); // result with same keys should always be the same
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS.value());
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
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(10, result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    assertEquals("900a-A369@", result); // result with same keys should always be the same
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS.value());
    String resultNoSymbols = result.substring(0, 3) + result.substring(6, 9);
    assertEquals("006789", cipher.decrypt(resultNoSymbols));
  }

  @Test
  public void testLettersLowerSuccess() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[a-z]+");
    String original = "abcdefghijklmnopqrstuvwxyz";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("qxjybsegrisimahnabsipxqnug", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.LOWER.value());
    assertEquals(original, cipher.decrypt(result));
  }

  @Test
  public void testLettersLowerSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile(
        "0[a-z]{3}+-[a-z]{3}-[a-z]{3}-[a-z]{3}-[a-z]{3}-[a-z]{3}-[a-z]{3}-[a-z]{3}-[a-z]{2}Z");
    String original = "0abc-def-ghi-jkl-mno-pqr-stu-vwx-yzZ";
    String originalNoSym = "abcdefghijklmnopqrstuvwxyz";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("0qxj-ybs-egr-isi-mah-nab-sip-xqn-ugZ", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(1, 4));
    buffer.append(result.substring(5, 8));
    buffer.append(result.substring(9, 12));
    buffer.append(result.substring(13, 16));
    buffer.append(result.substring(17, 20));
    buffer.append(result.substring(21, 24));
    buffer.append(result.substring(25, 28));
    buffer.append(result.substring(29, 32));
    buffer.append(result.substring(33, 35));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.LOWER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testLettersUpperSuccess() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile("[A-Z]+");
    String original = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("QXJYBSEGRISIMAHNABSIPXQNUG", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.UPPER.value());
    assertEquals(original, cipher.decrypt(result));
  }

  @Test
  public void testLettersSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    Pattern pattern = Pattern.compile(
        "z[A-Z]{3}+-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{2}9");
    String original = "zABC-DEF-GHI-JKL-MNO-PQR-STU-VWX-YZ9";
    String originalNoSym = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("zQXJ-YBS-EGR-ISI-MAH-NAB-SIP-XQN-UG9", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(1, 4));
    buffer.append(result.substring(5, 8));
    buffer.append(result.substring(9, 12));
    buffer.append(result.substring(13, 16));
    buffer.append(result.substring(17, 20));
    buffer.append(result.substring(21, 24));
    buffer.append(result.substring(25, 28));
    buffer.append(result.substring(29, 32));
    buffer.append(result.substring(33, 35));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.UPPER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testLettersInsensitiveLowerSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "abc$8XYZ \t";
    String originalNoSym = "abcxyz";
    Pattern pattern = Pattern.compile("[a-z]{3}+\\$8[a-z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // result with same keys should always be the same
    assertEquals("bcg$8ski \t", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.LOWER.value());
    // recapitalization from original as desired
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testLettersInsensitiveLowerSymbolsPadFront() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.FRONT);
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "c$8XYZ \t";
    String originalNoSym = "acxyz"; // pad character added
    Pattern pattern = Pattern.compile("[a-z]{2}+\\$8[a-z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " - " + result);
    // verify length and content
    // one pad character
    assertEquals(original.length() + 1, result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // result with same keys should always be the same
    assertEquals("ii$8rek \t", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 2));
    buffer.append(result.substring(4, 7));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.LOWER.value());
    // recapitalization from original as desired
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testLettersInsensitiveUpperSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "abc$8XYZ \t";
    String originalNoSym = "ABCXYZ";
    Pattern pattern = Pattern.compile("[A-Z]{3}+\\$8[A-Z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // result with same keys should always be the same
    assertEquals("BCG$8SKI \t", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.UPPER.value());
    // recapitalization from original as desired
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testLettersInsensitiveOriginalSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_ORIGINAL);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "def$8JKL \t";
    String originalNoSym = "defjkl";
    Pattern pattern = Pattern.compile("[a-z]{3}+\\$8[A-Z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // result with same keys should always be the same
    assertEquals("ghl$8NKM \t", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.LOWER.value());
    // recapitalization from original as desired
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString().toLowerCase()));
  }

  @Test
  public void testDigitsLettersLowerSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "A#9876543210#abc-def-ghi-jkl-mno-pqr-stu-vwx-yzZ";
    String originalNoSym = "9876543210abcdefghijklmnopqrstuvwxyz";
    Pattern pattern = Pattern.compile(
        "A#[0-9a-z]{10}+#[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{2}Z");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("A#8hwyecapjs#jmz-dru-t2u-zl2-tp1-yg7-slo-7o4-qzZ", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(2, 12));
    buffer.append(result.substring(13, 16));
    buffer.append(result.substring(17, 20));
    buffer.append(result.substring(21, 24));
    buffer.append(result.substring(25, 28));
    buffer.append(result.substring(29, 32));
    buffer.append(result.substring(33, 36));
    buffer.append(result.substring(37, 40));
    buffer.append(result.substring(41, 44));
    buffer.append(result.substring(45, 47));
    FF3Cipher cipher =
        new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS_LOWER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testDigitsLettersUpperSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "#a0123456789#ABC-DEF-GHI-JKL-MNO-PQR-STU-VWX-YZz";
    String originalNoSym = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    Pattern pattern = Pattern.compile(
        "#a[0-9A-Z]{10}+#[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{2}z");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("#aWXDQSQH7NM#V9C-940-CE3-3RG-4X8-XK8-E2A-DPY-E2z", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(2, 12));
    buffer.append(result.substring(13, 16));
    buffer.append(result.substring(17, 20));
    buffer.append(result.substring(21, 24));
    buffer.append(result.substring(25, 28));
    buffer.append(result.substring(29, 32));
    buffer.append(result.substring(33, 36));
    buffer.append(result.substring(37, 40));
    buffer.append(result.substring(41, 44));
    buffer.append(result.substring(45, 47));
    FF3Cipher cipher =
        new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS_UPPER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testDigitsLettersInsensitiveLowerSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "@0123456789#abc-DEF-ghi-JKL-MNO-PQR-STU-VWX-YZ@";
    String originalNoSym = "0123456789abcdefghijklmnopqrstuvwxyz";
    Pattern pattern = Pattern.compile(
        "@[0-9a-z]{10}+#[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{2}@");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("@wxdqsqh7nm#v9c-940-ce3-3rg-4x8-xk8-e2a-dpy-e2@", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(1, 11));
    buffer.append(result.substring(12, 15));
    buffer.append(result.substring(16, 19));
    buffer.append(result.substring(20, 23));
    buffer.append(result.substring(24, 27));
    buffer.append(result.substring(28, 31));
    buffer.append(result.substring(32, 35));
    buffer.append(result.substring(36, 39));
    buffer.append(result.substring(40, 43));
    buffer.append(result.substring(44, 46));
    FF3Cipher cipher =
        new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS_LOWER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString()));
  }

  @Test
  public void testDigitsLettersInsensitiveUpperSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "@0123456789#abc-DEF-ghi-JKL-MNO-PQR-STU-VWX-YZ@";
    String originalNoSym = "0123456789abcdefghijklmnopqrstuvwxyz";
    Pattern pattern = Pattern.compile(
        "@[0-9A-Z]{10}+#[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{3}-[0-9A-Z]{2}@");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("@WXDQSQH7NM#V9C-940-CE3-3RG-4X8-XK8-E2A-DPY-E2@", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(1, 11));
    buffer.append(result.substring(12, 15));
    buffer.append(result.substring(16, 19));
    buffer.append(result.substring(20, 23));
    buffer.append(result.substring(24, 27));
    buffer.append(result.substring(28, 31));
    buffer.append(result.substring(32, 35));
    buffer.append(result.substring(36, 39));
    buffer.append(result.substring(40, 43));
    buffer.append(result.substring(44, 46));
    FF3Cipher cipher =
        new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS_LOWER.value());
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString().toLowerCase()));
  }

  @Test
  public void testLettersSensitiveSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_SENSITIVE);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "@0123456789#abc-DEF-ghi-JKL-MNO-PQR-STU-vwx-YZ@";
    Pattern pattern = Pattern.compile(
        "@0123456789#[a-z]{3}-[A-Z]{3}-[a-z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[a-z]{3}-[A-Z]{2}@");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("@0123456789#zhl-MQD-luv-DRG-XCO-ZLO-SQU-wdb-QP@", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
  }

  @Test
  public void testLettersSensitiveLengths() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_SENSITIVE);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.setUnexpectedInputReturnMessage("X");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    
    // minimum lower and upper
    String original = "abc-123-DEF-gh-JK";
    Pattern pattern = Pattern.compile("[a-z]{3}-123-[A-Z]{3}-[a-z]{2}-[A-Z]{2}");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("brp-123-CXG-yu-FF", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    
    // one lower too few
    assertEquals("X", provider.mask(original.substring(1)));

    // one upper too few
    assertEquals("X", provider.mask(original.substring(0, original.length() - 1)));

    // maximum lower and upper
    original = "abcdefghijklmnopqrstuvwxyzabcdefghijklmn-ABCDEFGHIJKLMNOPQRSTUVXXYZABCDEFGHIJKLMN";
    pattern = Pattern.compile("[a-z]{40}+-[A-Z]{40}+");
    result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }

    // one lower too many
    assertEquals("X", provider.mask(original + "a"));
    
    // one upper too many
    assertEquals("X", provider.mask(original + "A"));
  }

  @Test
  public void testDigitsLettersSensitiveSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_SENSITIVE);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "@0123456789#abc-DEF-ghi-JKL-MNO-PQR-STU-vwx-YZ@";
    Pattern pattern = Pattern.compile(
        "@[0-9]{10}#[a-z]{3}-[A-Z]{3}-[a-z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[A-Z]{3}-[a-z]{3}-[A-Z]{2}@");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("@2482145309#zhl-MQD-luv-DRG-XCO-ZLO-SQU-wdb-QP@", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
  }

  @Test
  public void testDigitsLettersSensitiveLengths() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_SENSITIVE);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.setUnexpectedInputReturnMessage("X");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);

    // minimum lengths
    String original = "abc-123-DEF-gh-456-JK";
    Pattern pattern = Pattern.compile("[a-z]{3}-[0-9]{3}-[A-Z]{3}-[a-z]{2}-[0-9]{3}-[A-Z]{2}");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // result with same keys should always be the same
    assertEquals("brp-837-CXG-yu-581-FF", result);
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }

    // one digit too few
    assertEquals("X", provider.mask(original.replace('1', '*')));

    // one lower too few
    assertEquals("X", provider.mask(original.substring(1)));

    // one upper too few
    assertEquals("X", provider.mask(original.substring(0, original.length() - 1)));

    // maximum lengths
    original =
        "abcdefghijklmnopqrstuvwxyzabcdefghijklmn-12345678901234567890123456789012345678901234567890123456-ABCDEFGHIJKLMNOPQRSTUVXXYZABCDEFGHIJKLMN";
    pattern = Pattern.compile("[a-z]{40}-[0-9]{56}-[A-Z]{40}");
    result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }

    // one lower too many
    assertEquals("X", provider.mask(original + "a"));

    // one upper too many
    assertEquals("X", provider.mask(original + "A"));

    // one digit too many
    assertEquals("X", provider.mask("2" + original));
  }

  @Test
  public void testNothingToMask() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    for (UsageType usage : UsageType.values()) {
      config.setInputType(usage);
      FPEMaskingProvider provider = new FPEMaskingProvider(config);
      assertNull("usage:" + usage.name(), provider.mask(null));
      String original = "";
      assertEquals("usage:" + usage.name(), original, provider.mask(original));
      original = " $$$ ";
      assertEquals("usage:" + usage.name(), original, provider.mask(original));
    }
  }

  @Test
  public void testLength() throws Exception {
    for (UsageType usage : UsageType.values()) {
      switch (usage) {
        case DIGITS:
          doTestLength(6, 56, '5', usage);
          break;
        case LETTERS_LOWER:
        case LETTERS_INSENSITIVE_AS_LOWER:
        case LETTERS_INSENSITIVE_AS_UPPER:
        case LETTERS_INSENSITIVE_AS_ORIGINAL:
          doTestLength(5, 40, 'v', usage);
          break;
        case LETTERS_UPPER:
          doTestLength(5, 40, 'R', usage);
          break;
        case DIGITS_LETTERS_LOWER:
        case DIGITS_LETTERS_UPPER:
        case DIGITS_LETTERS_INSENSITIVE_AS_LOWER:
        case DIGITS_LETTERS_INSENSITIVE_AS_UPPER:
          doTestLength(4, 36, '3', usage);
          break;
        case LETTERS_SENSITIVE:          
        case DIGITS_LETTERS_SENSITIVE:
          // not tested here
          break;
        default:
          fail("unexpected usage: " + usage.name());
      }
    }
  }

  private void doTestLength(int min, int max, char valid, UsageType usage) {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(usage);
    config.setKey("aaaabbbbccccdddd11111111222222223333333344444444aaaabbbbccccdddd");
    config.setTweak("aaaabbbbccccdddd");
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.setUnexpectedInputReturnMessage("bad length");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    StringBuilder buffer = new StringBuilder(max + 5);

    // try one less than min
    for (int i = 0; i < min - 1; i++) {
      buffer.append(valid);
    }
    assertEquals("bad length", provider.mask(buffer.toString()));

    // try adding a symbol
    buffer.append("-");
    assertEquals("bad length", provider.mask(buffer.toString()));

    // try at min
    buffer.setLength(buffer.length() - 1);
    buffer.append(valid);
    String in = buffer.toString();
    String result = provider.mask(in);
    assertEquals(min, result.length());
    assertNotEquals(in, result);

    // try at max
    for (int i = min; i < max; i++) {
      buffer.append(valid);
    }
    in = buffer.toString();
    result = provider.mask(in);
    assertEquals(max, result.length());
    assertNotEquals(in, result);

    // try with one more char
    buffer.append(valid);
    assertEquals("bad length", provider.mask(buffer.toString()));

    // try encryption with max length and a symbol
    buffer.setLength(max);
    buffer.append('$');
    String resultSym = provider.mask(buffer.toString());
    assertEquals(result + "$", resultSym);
  }

  @Test
  public void testPadding() throws Exception {
    for (UsageType usage : UsageType.values()) {
      switch (usage) {
        case DIGITS:
          doTestPad(6, '5', usage);
          break;
        case LETTERS_LOWER:
        case LETTERS_INSENSITIVE_AS_LOWER:
        case LETTERS_INSENSITIVE_AS_UPPER:
        case LETTERS_INSENSITIVE_AS_ORIGINAL:
          doTestPad(5, 'v', usage);
          break;
        case LETTERS_UPPER:
          doTestPad(5, 'R', usage);
          break;
        case DIGITS_LETTERS_LOWER:
        case DIGITS_LETTERS_UPPER:
        case DIGITS_LETTERS_INSENSITIVE_AS_LOWER:
        case DIGITS_LETTERS_INSENSITIVE_AS_UPPER:
          doTestPad(4, '3', usage);
          break;
        case LETTERS_SENSITIVE:
        case DIGITS_LETTERS_SENSITIVE:
          // no padding allowed
          break;
        default:
          fail("unexpected usage: " + usage.name());
      }
    }
  }

  private void doTestPad(int min, char valid, UsageType usage) {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(usage);
    config.setKey("aaaabbbbccccdddd11111111222222223333333344444444aaaabbbbccccdddd");
    config.setTweak("aaaabbbbccccdddd");
    StringBuilder buffer = new StringBuilder(min + 2);
    buffer.append('-').append('-');

    for (int count = 1; count <= min; count++) {
      buffer.insert(1, valid);
      String in = buffer.toString();

      config.setPadding(Pad.FRONT);
      FPEMaskingProvider provider = new FPEMaskingProvider(config);
      String result = provider.mask(in);
      System.out.println(in + " -> " + result);
      assertEquals(min + 2, result.length());
      assertEquals('-', result.charAt(min + 2 - 1));
      assertEquals('-', result.charAt(min + 2 - 1 - count - 1));

      config.setPadding(Pad.BACK);
      provider = new FPEMaskingProvider(config);
      result = provider.mask(in);
      System.out.println(in + " -> " + result);
      assertEquals(min + 2, result.length());
      assertEquals('-', result.charAt(0));
      assertEquals('-', result.charAt(count + 1));
    }
  }

  @Test
  @Ignore("Long running test, not routinely required")
  public void testEngine1() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS);
    config.setKey("aaaabbbbccccdddd11111111222222223333333344444444aaaabbbbccccdddd");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.NONE);
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    HashSet<String> set = new HashSet<>(1600000);
    int originalLength = 7;
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), Radix.DIGITS.value());
    String original = null;
    String result = null;

    long start = System.currentTimeMillis();
    for (int i = 0; i < 1000000; i++) {
      original = String.format("%07d", i);
      result = provider.mask(original);
      // expected length
      assertEquals(originalLength, result.length());
      // unique
      assertTrue(set.add(result));
      // reverses
      assertEquals(original, cipher.decrypt(result));
    }
    long end = System.currentTimeMillis();
    System.out.println("duration = " + ((end - start) / 1000.0) + " sec");
    System.out.println("last = " + original + " <-> " + result);
    System.out.println(set.size() + " values");
  }

  @Test
  @Ignore("Long running test, not routinely required")
  public void testEngine2() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_UPPER);
    config.setKey("aaaabbbbccccdddd11111111222222223333333344444444aaaabbbbccccdddd");
    config.setTweak("aaaabbbbccccdddd");
    config.setPadding(Pad.NONE);
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    HashSet<String> set = new HashSet<>(1600000);
    int originalLength = 7;
    StringBuilder original = new StringBuilder("AAAAAAA");
    String result = null;

    long start = System.currentTimeMillis();
    for (int i = 0; i < 1000000; i++) {
      inc(original, 0);
      result = provider.mask(original.toString());
      // expected length
      assertEquals(originalLength, result.length());
      // unique
      assertTrue(set.add(result));
    }
    long end = System.currentTimeMillis();
    System.out.println("duration = " + ((end - start) / 1000.0) + " sec");
    System.out.println("last = " + original + " <-> " + result);
    System.out.println(set.size() + " values");
  }

  private void inc(StringBuilder original, int position) {
    char ch = original.charAt(position);
    if (ch == 'Z') {
      original.setCharAt(position, 'A');
      inc(original, position + 1);
    } else {
      original.setCharAt(position, (char) (ch + 1));
    }
  }
}
