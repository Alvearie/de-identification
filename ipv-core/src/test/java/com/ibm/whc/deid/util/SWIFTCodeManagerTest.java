/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Test;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.providers.masking.SWIFTCodeMaskingProviderTestSetup;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.utils.log.LogCodes;

public class SWIFTCodeManagerTest extends SWIFTCodeMaskingProviderTestSetup {

  @Test
  public void testWithCodesLoaded() {
    SWIFTCodeManager mgr = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(tenantId,
        Resource.SWIFT, null, TEST_LOCALIZATION_PROPERTIES);

    assertNull(mgr.getValue("ABCDEFGH"));
    SWIFTCode code = mgr.getValue("hhHHUShh");
    assertNotNull(code);
    assertEquals("HHHHUSHH", code.getCode());

    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    HashSet<String> codeset = new HashSet<>(keys);
    assertEquals(REPLACEMENTS_LIST.size(), codeset.size());
    codeset.removeAll(REPLACEMENTS_LIST);
    assertEquals(0, codeset.size());

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom("CCCCCACC");
    assertNotNull(ps);
    assertTrue(REPLACEMENTS_LIST.contains(ps));

    assertNull(mgr.getRandomValueFromCountry(null));
    assertNull(mgr.getRandomValueFromCountry("fr"));
    assertNull(mgr.getRandomValueFromCountry("en"));
    String value = mgr.getRandomValueFromCountry("ca");
    assertNotNull(value);
    assertTrue(CA_REPLACEMENTS_LIST.contains(value));
    value = mgr.getRandomValueFromCountry("us");
    assertNotNull(value);
    assertTrue(US_REPLACEMENTS_LIST.contains(value));

    value = mgr.getRandomKey();
    assertNotNull(value);
    assertTrue(REPLACEMENTS_LIST.contains(value));

    code = mgr.getRandomValue();
    assertNotNull(code);
    assertTrue(REPLACEMENTS_LIST.contains(code.getCode()));

    List<SWIFTCode> items = mgr.getValues();
    assertNotNull(items);
    assertEquals(REPLACEMENTS_LIST.size(), items.size());
    codeset = new HashSet<>();
    for (SWIFTCode codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }
    codeset.removeAll(REPLACEMENTS_LIST);
    assertEquals(0, codeset.size());

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertTrue(mgr.isValidKey("FFFFUSFF"));
    assertTrue(mgr.isValidKey("bbbbCabB"));
  }

  @Test
  public void testWithoutCodesLoaded() {
    SWIFTCodeManager mgr = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(tenantId,
        Resource.SWIFT, null, localizationProperty);

    assertNull(mgr.getValue("ABCDEFGH"));

    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    assertEquals(0, keys.size());

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    assertNull(mgr.getRandomValueFromCountry(null));
    assertNull(mgr.getRandomValueFromCountry("en"));

    assertNull(mgr.getRandomKey());

    assertNull(mgr.getRandomValue());

    List<SWIFTCode> list = mgr.getValues();
    assertNotNull(list);
    assertEquals(0, list.size());

    assertFalse(mgr.isValidKey("ABCDEFGH"));
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      SWIFTCodeManager.buildSWIFTCodeManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=10, values=[jjjjJJJ]] from /localization/test.swift.codes.bad.csv: The value \"jjjjJJJ\" for \"SWIFT code\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"FFFFUSFF"};
    SWIFTCodeManager manager = new SWIFTCodeManager();

    SWIFTCodeManager.loadRecord(manager, record);
    SWIFTCode swiftCode = manager.getValue("FFFFUSFF");
    assertNotNull(swiftCode);
    assertEquals("US", swiftCode.getCountry());
    // only value loaded for US
    assertEquals(record[0], manager.getRandomValueFromCountry("US"));

    record[0] = null;
    try {
      SWIFTCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("SWIFT code"));
    }
    record[0] = " ";
    try {
      SWIFTCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("SWIFT code"));
    }
    record[0] = "FFFF77ff";
    try {
      SWIFTCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("SWIFT code"));
      assertTrue(e.getMessage().contains("FFFF77ff"));
    }
    record[0] = "FFFF77ffABCD";
    try {
      SWIFTCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("SWIFT code"));
      assertTrue(e.getMessage().contains("FFFF77ffABCD"));
    }
  }
}
