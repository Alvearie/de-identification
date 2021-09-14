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
import com.ibm.whc.deid.providers.masking.fpe.FPEDriverBase;
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
    System.out.println(original + " -> " + result);
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
    System.out.println(original + " -> " + result);
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
    System.out.println(original + " -> " + result);
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    assertEquals(original, FPEDriverBase
        .shiftBase26ToLetters(cipher.decrypt(FPEDriverBase.shiftLettersToBase26(result))));
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    assertEquals(originalNoSym, FPEDriverBase.shiftBase26ToLetters(
        cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString()))));
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    assertEquals(original,
        FPEDriverBase
            .shiftBase26ToLetters(
                cipher.decrypt(FPEDriverBase.shiftLettersToBase26(result.toLowerCase())))
            .toUpperCase());
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    assertEquals(originalNoSym, FPEDriverBase.shiftBase26ToLetters(
        cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString().toLowerCase())))
        .toUpperCase());
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    // recapitalization from original as desired
    assertEquals(originalNoSym, FPEDriverBase.shiftBase26ToLetters(
        cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString()))));
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 2));
    buffer.append(result.substring(4, 7));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    // recapitalization from original as desired
    assertEquals(originalNoSym,
        FPEDriverBase
            .shiftBase26ToLetters(
                cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString()))));
  }

  @Test
  public void testLettersInsensitiveUpperSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "abc$8XYZ \t";
    String originalNoSym = "abcxyz";
    Pattern pattern = Pattern.compile("[A-Z]{3}+\\$8[A-Z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    // recapitalization from original as desired
    assertEquals(originalNoSym, FPEDriverBase.shiftBase26ToLetters(
        cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString().toLowerCase()))));
  }

  @Test
  public void testLettersInsensitiveOriginalSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.LETTERS_INSENSITIVE_AS_ORIGINAL);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "abc$8XYZ \t";
    String originalNoSym = "abcxyz";
    Pattern pattern = Pattern.compile("[a-z]{3}+\\$8[A-Z]{3} \t");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue("`" + result + "`", matcher.matches());
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
    // verify reversible
    StringBuilder buffer = new StringBuilder(originalNoSym.length());
    buffer.append(result.substring(0, 3));
    buffer.append(result.substring(5, 8));
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 26);
    // recapitalization from original as desired
    assertEquals(originalNoSym, FPEDriverBase.shiftBase26ToLetters(
        cipher.decrypt(FPEDriverBase.shiftLettersToBase26(buffer.toString().toLowerCase()))));
  }

  @Test
  public void testDigitsLettersLowerSuccessSymbols() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_LOWER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "A#0123456789#abc-def-ghi-jkl-mno-pqr-stu-vwx-yzZ";
    String originalNoSym = "0123456789abcdefghijklmnopqrstuvwxyz";
    Pattern pattern = Pattern.compile(
        "A#[0-9a-z]{10}+#[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{3}-[0-9a-z]{2}Z");
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    // verify length and content
    assertEquals(original.length(), result.length());
    Matcher matcher = pattern.matcher(result);
    assertTrue(result, matcher.matches());
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 36);
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 36);
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString().toLowerCase()).toUpperCase());
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 36);
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString().toLowerCase()));
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
    FF3Cipher cipher = new FF3Cipher(config.getKey(), config.getTweak(), 36);
    assertEquals(originalNoSym, cipher.decrypt(buffer.toString().toLowerCase()));
  }

  @Test
  public void testTooLong() throws Exception {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    config.setInputType(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_UPPER);
    config.setKey("11111111222222223333333344444444");
    config.setTweak("aaaabbbbccccdddd");
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.setUnexpectedInputReturnMessage("longlong");
    FPEMaskingProvider provider = new FPEMaskingProvider(config);
    String original = "#a0123456789#ABC-DEF-GHI-JKL-MNO-PQR-STU-VWX-YZz";
    String result = provider.mask(original);
    System.out.println(original + " -> " + result);
    assertEquals("longlong", result);
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
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
    // verify repeatable
    for (int i = 0; i < 3; i++) {
      assertEquals(result, provider.mask(original));
    }
  }

  // check null
  // check empty string
  // check just symbols
}
