/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
/**
 * 
 */
public class FileUtilsTest {

  @Test
  public void testParseRequiredDouble() {

    try {
      FileUtils.parseRequiredDouble(null);
      fail("expected exception");
    } catch (NumberFormatException e) {
      assertEquals("null", e.getMessage());
    }

    try {
      FileUtils.parseRequiredDouble(" ");
      fail("expected exception");
    } catch (NumberFormatException e) {
      // good
    }

    try {
      FileUtils.parseRequiredDouble("arg");
      fail("expected exception");
    } catch (NumberFormatException e) {
      // good
    }

    try {
      FileUtils.parseRequiredDouble("4.5.6");
      fail("expected exception");
    } catch (NumberFormatException e) {
      // good
    }

    assertTrue(4.56 == FileUtils.parseRequiredDouble("4.56"));
  }
}
