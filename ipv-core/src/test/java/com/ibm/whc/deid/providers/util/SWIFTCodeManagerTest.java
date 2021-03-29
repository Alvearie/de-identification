/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.providers.masking.SWIFTCodeMaskingProviderTestSetup;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeManagerTest extends SWIFTCodeMaskingProviderTestSetup {

  @Test
  public void testYesCodes() {
    SWIFTCodeManager mgr = new SWIFTCodeManager(tenantId, TEST_LOCALIZATION_PROPERTIES);

    Collection<SWIFTCode> items = mgr.getItemList();
    assertNotNull(items);
    assertEquals(REPLACEMENTS_LIST.size(), items.size());
    HashSet<String> codeset = new HashSet<>();
    for (SWIFTCode code : items) {
      assertTrue(codeset.add(code.getCode()));
    }
    codeset.removeAll(REPLACEMENTS_LIST);
    assertEquals(0, codeset.size());

    assertNull(mgr.getKey("ABCDEFGH"));
    SWIFTCode code = mgr.getKey("hhHHUShh");
    assertNotNull(code);
    assertEquals("HHHHUSHH", code.getCode());

    // swift stores all keys only in the _all_ map, so nothing is found by country/locale
    assertNull(mgr.getKey("en", "ABCDEFGH"));
    assertNull(mgr.getKey("fr", "AAAACAAA"));
    assertNull(mgr.getKey("us", "AAAACAAA"));
    assertNull(mgr.getKey("ca", "AAAACAAA"));
    assertNull(mgr.getKey("en", "AAAACAAA"));

    List<String> keys = mgr.getKeys();
    assertNotNull(keys);
    codeset = new HashSet<>(keys);
    assertEquals(REPLACEMENTS_LIST.size(), codeset.size());
    codeset.removeAll(REPLACEMENTS_LIST);
    assertEquals(0, codeset.size());

    // manager returns all keys if no data for given locale/country
    assertEquals(keys, mgr.getKeys("fr"));
    assertEquals(keys, mgr.getKeys("ca"));
    assertEquals(keys, mgr.getKeys("us"));
    assertEquals(keys, mgr.getKeys("en"));

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    // swift is not localized, so not swift code country specific
    ps = mgr.getPseudorandom("CCCCCACC");
    assertNotNull(ps);
    assertTrue(REPLACEMENTS_LIST.contains(ps));

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

    // not localized
    assertNull(mgr.getRandomKey("gb"));
    assertNull(mgr.getRandomKey("en"));
    assertNull(mgr.getRandomKey("Ca"));
    assertNull(mgr.getRandomKey("uS"));

    code = mgr.getRandomValue();
    assertNotNull(code);
    assertTrue(REPLACEMENTS_LIST.contains(code.getCode()));

    // not localized
    assertNull(mgr.getRandomValue("fr"));
    assertNull(mgr.getRandomValue("us"));
    assertNull(mgr.getRandomValue("ca"));
    assertNull(mgr.getRandomValue("en"));

    items = mgr.getValues();
    assertNotNull(items);
    assertEquals(REPLACEMENTS_LIST.size(), items.size());
    codeset = new HashSet<>();
    for (SWIFTCode codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }
    codeset.removeAll(REPLACEMENTS_LIST);
    assertEquals(0, codeset.size());

    // not localized, manager returns all values if no data for given locale/country
    assertEquals(items, mgr.getValues("en"));
    assertEquals(items, mgr.getValues("fr"));
    assertEquals(items, mgr.getValues("us"));
    assertEquals(items, mgr.getValues("ca"));
    assertEquals(items, mgr.getValues("xx"));

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertTrue(mgr.isValidKey("FFFFUSFF"));
    assertTrue(mgr.isValidKey("bbbbCabB"));

    // not localized, manager doesn't revert to "all" list for isValidKey()
    assertFalse(mgr.isValidKey("en", "ABCDEFGH"));
    assertFalse(mgr.isValidKey("fr", "FFFFUSFF"));
    assertFalse(mgr.isValidKey("ca", "FFFFUSFF"));
    assertFalse(mgr.isValidKey("us", "FFFFUSFF"));
    assertFalse(mgr.isValidKey("en", "FFFFUSFF"));
  }

  @Test
  public void testNoCodes() {
    SWIFTCodeManager mgr = new SWIFTCodeManager(tenantId, localizationProperty);

    assertNull(mgr.getItemList());

    assertNull(mgr.getKey("ABCDEFGH"));
    assertNull(mgr.getKey("en", "ABCDEFGH"));
    assertNull(mgr.getKeys());
    assertNull(mgr.getKeys("en"));

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    assertNull(mgr.getRandomValueFromCountry("en"));

    assertNull(mgr.getRandomKey());
    assertNull(mgr.getRandomKey("en"));
    assertNull(mgr.getRandomValue());
    assertNull(mgr.getRandomValue("en"));

    assertNull(mgr.getValues());
    assertNull(mgr.getValues("en"));

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertFalse(mgr.isValidKey("en", "ABCDEFGH"));
  }
}
