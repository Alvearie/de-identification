/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.Religion;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ReligionManagerTest implements MaskingProviderTest {

  @Test
  public void testLookupSuccessful() throws Exception {
    ReligionManager religionManager = ReligionManager.buildReligionManager(localizationProperty);
    String religion = "Catholic";
    assertTrue(religionManager.isValidKey(religion));

    religion = "caTHolic";
    assertTrue(religionManager.isValidKey(religion));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    ReligionManager religionManager = ReligionManager.buildReligionManager(localizationProperty);
    assertTrue(religionManager.isValidKey(religionManager.getRandomKey()));
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      ReligionManager.buildReligionManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=7, values=[  ]] from /localization/test.religion.bad.csv: The value \"  \" for \"religion\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    ReligionManager manager = new ReligionManager();
    String locale = "en";
    String[] record = new String[] {"Religion1"};
    ReligionManager.loadRecord(locale, manager, record);
    Religion resource = manager.getValue("religion1");
    assertNotNull(resource);
    assertEquals("Religion1", resource.getName());
    assertSame(resource, manager.getValue(locale, "RELIGION1"));
    assertNull(manager.getValue("us", "RELIGION1"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      ReligionManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("religion"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      ReligionManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("religion"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      ReligionManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("religion locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      ReligionManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("religion locale"));
    }
    locale = temp;
  }
}
