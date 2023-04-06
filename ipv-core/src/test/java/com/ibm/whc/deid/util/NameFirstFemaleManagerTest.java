/*
 * © Merative US L.P. 2016,2021
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
import com.ibm.whc.deid.models.FirstName;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class NameFirstFemaleManagerTest implements MaskingProviderTest {

  @Test
  public void testLoadRecord() {
    String locale = "us";
    String[] record = new String[] {"Smith", "x"};
    NameFirstFemaleManager manager = new NameFirstFemaleManager();
    NameFirstFemaleManager.loadRecord(locale, manager, record);
    FirstName resource = manager.getValue("smith");
    assertNotNull(resource);
    assertEquals("Smith", resource.getName());
    assertSame(resource, manager.getValue(locale, "SMITH"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      NameFirstFemaleManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("first name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      NameFirstFemaleManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("first name"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      NameFirstFemaleManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("first name locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      NameFirstFemaleManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("first name locale"));
    }
    locale = temp;
  }
}
