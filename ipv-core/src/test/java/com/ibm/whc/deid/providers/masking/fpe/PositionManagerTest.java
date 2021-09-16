/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.CharType;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.Position;

public class PositionManagerTest {

  @Test
  public void testCategorization() {
    int count = 512;
    StringBuilder buffer = new StringBuilder(count);
    for (int i = 0; i < count; i++) {
      buffer.append((char) i);
    }
    PositionManager pm = new PositionManager(buffer.toString());
    Position[] positions = pm.getPositions();
    assertEquals(count, positions.length);
    for (int i = 0; i < count; i++) {
      assertEquals((char) i, positions[i].getOriginal());
      CharType ct = positions[i].getType();
      if ((char) i < '0') {
        assertEquals(CharType.OTHER, ct);
      } else if ((char) i <= '9') {
        assertEquals(CharType.DIGIT, ct);
      } else if ((char) i < 'A') {
        assertEquals(CharType.OTHER, ct);
      } else if ((char) i <= 'Z') {
        assertEquals(CharType.UPPER, ct);
      } else if ((char) i < 'a') {
        assertEquals(CharType.OTHER, ct);
      } else if ((char) i <= 'z') {
        assertEquals(CharType.LOWER, ct);
      } else {
        assertEquals(CharType.OTHER, ct);
      }
    }
  }

  @Test
  public void testExtract() {
    PositionManager pm = new PositionManager("$%^abcABC123");
    assertEquals("", pm.extract(false, false, false));
    assertEquals("123", pm.extract(true, false, false));
    assertEquals("abc", pm.extract(false, true, false));
    assertEquals("ABC", pm.extract(false, false, true));
    assertEquals("abc123", pm.extract(true, true, false));
    assertEquals("ABC123", pm.extract(true, false, true));
    assertEquals("abcABC", pm.extract(false, true, true));
    assertEquals("abcABC123", pm.extract(true, true, true));

    pm = new PositionManager("$%^");
    assertEquals("", pm.extract(true, true, true));

    pm = new PositionManager("");
    assertEquals("", pm.extract(true, true, true));

    pm = new PositionManager("$%^abcABC");
    assertEquals("", pm.extract(false, false, false));
    assertEquals("", pm.extract(true, false, false));
    assertEquals("abc", pm.extract(false, true, false));
    assertEquals("ABC", pm.extract(false, false, true));
    assertEquals("abc", pm.extract(true, true, false));
    assertEquals("ABC", pm.extract(true, false, true));
    assertEquals("abcABC", pm.extract(false, true, true));
    assertEquals("abcABC", pm.extract(true, true, true));

    pm = new PositionManager("$%^ABC123");
    assertEquals("", pm.extract(false, false, false));
    assertEquals("123", pm.extract(true, false, false));
    assertEquals("", pm.extract(false, true, false));
    assertEquals("ABC", pm.extract(false, false, true));
    assertEquals("123", pm.extract(true, true, false));
    assertEquals("ABC123", pm.extract(true, false, true));
    assertEquals("ABC", pm.extract(false, true, true));
    assertEquals("ABC123", pm.extract(true, true, true));

    pm = new PositionManager("$%^abc123");
    assertEquals("", pm.extract(false, false, false));
    assertEquals("123", pm.extract(true, false, false));
    assertEquals("abc", pm.extract(false, true, false));
    assertEquals("", pm.extract(false, false, true));
    assertEquals("abc123", pm.extract(true, true, false));
    assertEquals("123", pm.extract(true, false, true));
    assertEquals("abc", pm.extract(false, true, true));
    assertEquals("abc123", pm.extract(true, true, true));
  }

  @Test
  public void testReplaceSymbols() {
    PositionManager pm = new PositionManager("$%^abcABC123");
    String encrypted = "zyxwvutsr";
    assertEquals("$%^abcABC123", pm.replaceSymbols(encrypted, false, false, false));
    assertEquals("$%^abcABCzyx", pm.replaceSymbols(encrypted, true, false, false));
    assertEquals("$%^zyxABC123", pm.replaceSymbols(encrypted, false, true, false));
    assertEquals("$%^abczyx123", pm.replaceSymbols(encrypted, false, false, true));
    assertEquals("$%^zyxABCwvu", pm.replaceSymbols(encrypted, true, true, false));
    assertEquals("$%^abczyxwvu", pm.replaceSymbols(encrypted, true, false, true));
    assertEquals("$%^zyxwvu123", pm.replaceSymbols(encrypted, false, true, true));
    assertEquals("$%^zyxwvutsr", pm.replaceSymbols(encrypted, true, true, true));
    try {
      pm.replaceSymbols(encrypted.substring(0, encrypted.length() - 1), true, true, true);
      fail("expected exception");
    } catch (StringIndexOutOfBoundsException e) {
      // good
    }
  }
}
