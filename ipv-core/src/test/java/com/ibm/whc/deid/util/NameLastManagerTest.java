/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.LastName;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class NameLastManagerTest implements MaskingProviderTest {

  @Test
  public void testLoadRecord() {
    String locale = "us";
    String[] record = new String[] {"Smith", "x"};
    NameLastManager manager = new NameLastManager();
    NameLastManager.loadRecord(locale, manager, record);
    LastName resource = manager.getValue("smith");
    assertNotNull(resource);
    assertEquals("Smith", resource.getName());
    assertSame(resource, manager.getValue(locale, "SMITH"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      NameLastManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("last name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      NameLastManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("last name"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      NameLastManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("last name locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      NameLastManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("last name locale"));
    }
    locale = temp;
  }
}
