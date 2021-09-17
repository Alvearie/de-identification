/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;

public class FPEDriverBaseTest {

  @Test
  public void testBase26Conversion() {
    assertEquals("0", FPEDriverBase.shiftLettersToBase26("a"));
    assertEquals("1", FPEDriverBase.shiftLettersToBase26("b"));
    assertEquals("2", FPEDriverBase.shiftLettersToBase26("c"));
    assertEquals("3", FPEDriverBase.shiftLettersToBase26("d"));
    assertEquals("4", FPEDriverBase.shiftLettersToBase26("e"));
    assertEquals("5", FPEDriverBase.shiftLettersToBase26("f"));
    assertEquals("6", FPEDriverBase.shiftLettersToBase26("g"));
    assertEquals("7", FPEDriverBase.shiftLettersToBase26("h"));
    assertEquals("8", FPEDriverBase.shiftLettersToBase26("i"));
    assertEquals("9", FPEDriverBase.shiftLettersToBase26("j"));
    assertEquals("a", FPEDriverBase.shiftLettersToBase26("k"));
    assertEquals("b", FPEDriverBase.shiftLettersToBase26("l"));
    assertEquals("c", FPEDriverBase.shiftLettersToBase26("m"));
    assertEquals("d", FPEDriverBase.shiftLettersToBase26("n"));
    assertEquals("e", FPEDriverBase.shiftLettersToBase26("o"));
    assertEquals("f", FPEDriverBase.shiftLettersToBase26("p"));
    assertEquals("g", FPEDriverBase.shiftLettersToBase26("q"));
    assertEquals("h", FPEDriverBase.shiftLettersToBase26("r"));
    assertEquals("i", FPEDriverBase.shiftLettersToBase26("s"));
    assertEquals("j", FPEDriverBase.shiftLettersToBase26("t"));
    assertEquals("k", FPEDriverBase.shiftLettersToBase26("u"));
    assertEquals("l", FPEDriverBase.shiftLettersToBase26("v"));
    assertEquals("m", FPEDriverBase.shiftLettersToBase26("w"));
    assertEquals("n", FPEDriverBase.shiftLettersToBase26("x"));
    assertEquals("o", FPEDriverBase.shiftLettersToBase26("y"));
    assertEquals("p", FPEDriverBase.shiftLettersToBase26("z"));
    assertEquals("", FPEDriverBase.shiftLettersToBase26(""));
    assertEquals("02p", FPEDriverBase.shiftLettersToBase26("acz"));

    assertEquals("a", FPEDriverBase.shiftBase26ToLetters("0"));
    assertEquals("b", FPEDriverBase.shiftBase26ToLetters("1"));
    assertEquals("c", FPEDriverBase.shiftBase26ToLetters("2"));
    assertEquals("d", FPEDriverBase.shiftBase26ToLetters("3"));
    assertEquals("e", FPEDriverBase.shiftBase26ToLetters("4"));
    assertEquals("f", FPEDriverBase.shiftBase26ToLetters("5"));
    assertEquals("g", FPEDriverBase.shiftBase26ToLetters("6"));
    assertEquals("h", FPEDriverBase.shiftBase26ToLetters("7"));
    assertEquals("i", FPEDriverBase.shiftBase26ToLetters("8"));
    assertEquals("j", FPEDriverBase.shiftBase26ToLetters("9"));
    assertEquals("k", FPEDriverBase.shiftBase26ToLetters("a"));
    assertEquals("l", FPEDriverBase.shiftBase26ToLetters("b"));
    assertEquals("m", FPEDriverBase.shiftBase26ToLetters("c"));
    assertEquals("n", FPEDriverBase.shiftBase26ToLetters("d"));
    assertEquals("o", FPEDriverBase.shiftBase26ToLetters("e"));
    assertEquals("p", FPEDriverBase.shiftBase26ToLetters("f"));
    assertEquals("q", FPEDriverBase.shiftBase26ToLetters("g"));
    assertEquals("r", FPEDriverBase.shiftBase26ToLetters("h"));
    assertEquals("s", FPEDriverBase.shiftBase26ToLetters("i"));
    assertEquals("t", FPEDriverBase.shiftBase26ToLetters("j"));
    assertEquals("u", FPEDriverBase.shiftBase26ToLetters("k"));
    assertEquals("v", FPEDriverBase.shiftBase26ToLetters("l"));
    assertEquals("w", FPEDriverBase.shiftBase26ToLetters("m"));
    assertEquals("x", FPEDriverBase.shiftBase26ToLetters("n"));
    assertEquals("y", FPEDriverBase.shiftBase26ToLetters("o"));
    assertEquals("z", FPEDriverBase.shiftBase26ToLetters("p"));
    assertEquals("", FPEDriverBase.shiftBase26ToLetters(""));
    assertEquals("abc", FPEDriverBase.shiftBase26ToLetters("012"));

    try {
      FPEDriverBase.shiftLettersToBase26("A");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }
    try {
      FPEDriverBase.shiftLettersToBase26(" ");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }

    try {
      FPEDriverBase.shiftLettersToBase26("$");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }

    try {
      FPEDriverBase.shiftBase26ToLetters("q");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }

    try {
      FPEDriverBase.shiftBase26ToLetters(" ");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }

    try {
      FPEDriverBase.shiftBase26ToLetters("$");
      fail("expected exception");
    } catch (ArrayIndexOutOfBoundsException e) {
      // good
    }

  }

}
