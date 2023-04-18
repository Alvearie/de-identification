/*
 * © Merative US L.P. 2021
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
import com.ibm.whc.deid.models.Sex;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class GenderManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      GenderManager.buildGenderManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=2, values=[   ]] from /localization/test.gender.bad.csv: The value \"   \" for \"gender name\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"Gender1"};
    GenderManager manager = new GenderManager();
    String locale = "en";

    GenderManager.loadRecord(locale, manager, record);
    Sex resource = manager.getValue("gender1");
    assertNotNull(resource);
    assertEquals("Gender1", resource.getName());
    assertSame(resource, manager.getValue("eN", "GENDER1"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      GenderManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("gender name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      GenderManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("gender name"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      GenderManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("gender locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      GenderManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("gender locale"));
    }
    locale = temp;
  }
}
