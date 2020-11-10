/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.CharacterRange;
import com.ibm.whc.deid.models.TokenClass;
import java.security.SecureRandom;
import java.util.Arrays;
import org.junit.Test;

public class TokenClassTest {
  SecureRandom random = new SecureRandom();

  /** Test numeric ranges */
  @Test
  public void testParseTokenCharacterRange() {
    // Test simple ascii range
    assertTrue(checkToken("a-z", "[a-z]"));

    // Test reversed ascii range
    assertTrue(checkToken("z-a", "[a-z]"));

    // Test UTF16 character range
    assertTrue(checkToken("\u1234-\u1235", "[\u1234-\u1235]"));

    // Test English language shortcut ranges
    assertTrue(checkToken("\\d", "\\d"));
    assertTrue(checkToken("\\l", "[a-z]"));
    assertTrue(checkToken("\\u", "[A-Z]"));

    // Test UNICODE UTF16 surrogate range
    assertTrue(checkToken("\uD800\uDF00-\uD800\uDF01", "[\uD800\uDF00-\uD800\uDF01]"));

    // Test discrete class
    assertTrue(checkToken("abc", "[a-c]"));

    // Assert range size
    TokenClass tc = getTokenClassByString("a-z");
    assertTrue(tc.getSize() == 26);

    tc = new TokenClass(new CharacterRange('a'));
    assertTrue(tc.getRandomToken(random).matches("a"));
    assertTrue(tc.getSize() == 1);
    tc.addCharacterSet(new CharacterRange("b"));
    assertTrue(tc.getSize() == 2);
    assertTrue(tc.getRandomToken(random).matches("[a-b]"));
    assertTrue(checkToken("\\p00001234-\\p00001235", "[\u1234-\u1235]"));
  }

  /** Test discrete ranges */
  @Test
  public void testParseSingleToken() {
    // Test UNICODE escape
    TokenClass tc = null;
    assertTrue(checkToken("\\p00001234", "\u1234"));

    // Test single character
    tc = getTokenClassByString("a");
    assertTrue(tc.getRandomToken(random).matches("a"));
    assertTrue(tc.getSize() == 1);

    // Test escaped non-special
    tc = getTokenClassByString("\\a");
    assertTrue(tc.getRandomToken(random).matches("a"));
    // Assert size
    assertTrue(tc.getSize() == 1);

    // Test nonrange dash
    assertTrue(checkToken("-", "-"));
    assertTrue(checkToken("\\-", "[\\-]"));

    // Test square bracket escape
    assertTrue(checkToken("\\[", "\\["));
    assertTrue(checkToken("\\]", "\\]"));
  }

  /** Test invalid cases */
  @Test
  public void testInvalidClass() {
    TokenClass tc = null;

    // Test invalid point
    tc = getTokenClassByString("\\p000");
    assertTrue(tc.getRandomToken(random).matches(""));
    assertTrue(tc.getTokenAt(-1).matches(""));

    assertTrue(checkToken("\\p00001234-\\p000", "\u1234"));
    System.out.println(getTokenClassByString("\\p0000abcga").getRandomToken(random));
    assertTrue(checkToken("\\p0000gggga", "a"));
    // Test empty class
    tc = new TokenClass();
    assertTrue(tc.getSize() == 0);
    assertTrue(tc.getRandomToken(random) == "");

    // Test bad range (Should use first character)
    assertTrue(checkToken("a-", "a"));

    // Test missing surrogate (Should return hald unicode point)
    assertTrue(checkToken("\uD800", "\uD800"));
  }

  private TokenClass getTokenClassByString(String pattern) {
    return new TokenClass(Arrays.asList(pattern.split("")));
  }

  private boolean checkToken(String tokenClass, String regex) {
    return getTokenClassByString(tokenClass).getRandomToken(random).matches("^" + regex + "$");
  }
}
