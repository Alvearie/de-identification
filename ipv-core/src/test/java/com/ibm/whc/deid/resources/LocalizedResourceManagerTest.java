/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.LocalizedEntity;

public class LocalizedResourceManagerTest {

  private static class TestResource implements ManagedResource {

    private String code;
    
    public TestResource(String code) {
      this.code = code;
    }

    @Override
    public String getKey() {
      return code.toUpperCase();
    }    
    
    public String getCode() {
      return code;
    }
  }

  private static class TestLocalizedResource extends TestResource implements LocalizedEntity {

    private String cc;
    
    public TestLocalizedResource(String code, String cc) {
      super(code);
      this.cc = cc;
    }

    @Override
    public String getNameCountryCode() {
      return cc;
    }
  }

  private static class TestLocalizedResourceManager extends LocalizedResourceManager<TestResource> {
    
    public TestLocalizedResourceManager() {      
      // nothing required here
    }
  }
  
  @Test
  public void testWithoutResourcesLoaded() {
    TestLocalizedResourceManager mgr = new TestLocalizedResourceManager();

    assertNull(mgr.getValue("ABCDEFGH"));
    assertNull(mgr.getValue(null));
    assertNull(mgr.getValue("en", "ABCDEFGH"));
    assertNull(mgr.getValue(null, "ABCDEFGH"));

    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    assertEquals(0, keys.size());
    keys = mgr.getKeys("en");
    assertNotNull(keys);
    assertEquals(0, keys.size());
    keys = mgr.getKeys(null);
    assertNotNull(keys);
    assertEquals(0, keys.size());
    
    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom(null);
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    assertNull(mgr.getRandomKey());
    assertNull(mgr.getRandomKey("en"));
    assertNull(mgr.getRandomKey(null));

    assertNull(mgr.getRandomValue());
    assertNull(mgr.getRandomValue("en"));
    assertNull(mgr.getRandomValue(null));

    List<TestResource> list = mgr.getValues();
    assertNotNull(list);
    assertEquals(0, list.size());
    list = mgr.getValues("en");
    assertNotNull(list);
    assertEquals(0, list.size());
    list = mgr.getValues(null); 
    assertNotNull(list);
    assertEquals(0, list.size());

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertFalse(mgr.isValidKey(null)); 
    assertFalse(mgr.isValidKey("en", "ABCDEFGH"));
    assertFalse(mgr.isValidKey(null, "ABCDEFGH"));    
  }

  @Test
  public void testWithCodesLoaded() {
    List<TestResource> EN_REPLACEMENTS_LIST = Arrays.asList(new TestResource("first"), new TestResource("second"), new TestResource("third"), new TestResource("lASt"));
    List<String> EN_REPLACEMENTS_KEYS = Arrays.asList("FIRST", "SECOND", "THIRD", "LAST");
    List<String> EN_REPLACEMENTS_VALUES = Arrays.asList("first", "second", "third", "lASt");
    List<TestResource> FR_REPLACEMENTS_LIST = Arrays.asList(new TestResource("FRfirst"), new TestResource("FRsecond"), new TestResource("FRthird"), new TestResource("last"));
    List<String> FR_REPLACEMENTS_KEYS = Arrays.asList("FRFIRST", "FRSECOND", "FRTHIRD", "LAST");
    List<String> FR_REPLACEMENTS_VALUES = Arrays.asList("FRfirst", "FRsecond", "FRthird", "last");    

    TestLocalizedResourceManager mgr = new TestLocalizedResourceManager();
    for (TestResource r : EN_REPLACEMENTS_LIST) {
      mgr.add(r);
      mgr.add("en", r);
    }
    for (TestResource r : FR_REPLACEMENTS_LIST) {
      mgr.add(r);
      mgr.add("fr", r);
    }
    
    assertNull(mgr.getValue("ABCDEFGH"));
    TestResource code = mgr.getValue("second");
    assertNotNull(code);
    assertEquals("second", code.getCode());
    code = mgr.getValue("last");
    assertNotNull(code);
    assertEquals("last", code.getCode());  // FR code added last
    code = mgr.getValue("LAST");
    assertNotNull(code);
    assertEquals("last", code.getCode());  // FR code added last
    assertNull(mgr.getValue(null));

    assertNull(mgr.getValue("gb", "ABCDEFGH"));
    assertNull(mgr.getValue("gb", "third"));
    assertNull(mgr.getValue(null, "ABCDEFGH"));    
    assertNull(mgr.getValue("en", "ABCDEFGH"));
    assertNull(mgr.getValue("en", null));
    assertNull(mgr.getValue("eN", "frsecond"));
    code = mgr.getValue("eN", "second");
    assertEquals("second", code.getCode());
    code = mgr.getValue("eN", "last");
    assertNotNull(code);
    assertEquals("lASt", code.getCode());
    code = mgr.getValue("EN", "LAST");
    assertNotNull(code);
    assertEquals("lASt", code.getCode());
    code = mgr.getValue("Fr", "frsecond");
    assertNotNull(code);
    assertEquals("FRsecond", code.getCode());
    code = mgr.getValue("fR", "last");
    assertNotNull(code);
    assertEquals("last", code.getCode());
    code = mgr.getValue("fr", "LAST");
    assertNotNull(code);
    assertEquals("last", code.getCode());

    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    HashSet<String> codeset = new HashSet<>(keys);
    for (String s : EN_REPLACEMENTS_KEYS) {
      assertTrue(codeset.remove(s));
    }
    for (String s : FR_REPLACEMENTS_KEYS) {
      if (!"LAST".equals(s)) {        
        assertTrue(codeset.remove(s));
      }
    }
    assertTrue(codeset.isEmpty());
    
    keys = mgr.getKeys("gb");
    assertNotNull(keys);
    assertEquals(0, keys.size());
    keys = mgr.getKeys(null); 
    assertNotNull(keys);
    assertEquals(0, keys.size());
    
    keys = mgr.getKeys("EN");
    assertNotNull(keys);
    codeset = new HashSet<>(keys);
    for (String s : EN_REPLACEMENTS_KEYS) {
      assertTrue(codeset.remove(s));
    }
    assertTrue(codeset.isEmpty());
    keys = mgr.getKeys("fr");
    assertNotNull(keys);
    codeset = new HashSet<>(keys);
    for (String s : FR_REPLACEMENTS_KEYS) {
      assertTrue(codeset.remove(s));
    }
    assertTrue(codeset.isEmpty());
    
    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom("third");
    assertNotNull(ps);
    // TestResource is not a LocalizedEntity
    assertTrue(EN_REPLACEMENTS_KEYS.contains(ps) || FR_REPLACEMENTS_KEYS.contains(ps));
    ps = mgr.getPseudorandom(null);
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom("frthird");
    assertNotNull(ps);
    assertTrue(FR_REPLACEMENTS_KEYS.contains(ps));

    String key = mgr.getRandomKey();
    assertNotNull(key);
    assertTrue(EN_REPLACEMENTS_KEYS.contains(key) || FR_REPLACEMENTS_KEYS.contains(key));
    assertNull(mgr.getRandomKey("gb"));
    assertNull(mgr.getRandomKey(null)); 
    key = mgr.getRandomKey("En");
    assertNotNull(key);
    assertTrue(EN_REPLACEMENTS_KEYS.contains(key));
    key = mgr.getRandomKey("fR");
    assertNotNull(key);
    assertTrue(FR_REPLACEMENTS_KEYS.contains(key));

    code = mgr.getRandomValue();
    assertNotNull(code);
    assertTrue(EN_REPLACEMENTS_LIST.contains(code) || FR_REPLACEMENTS_LIST.contains(code));
    assertNull(mgr.getRandomValue("gb"));
    assertNull(mgr.getRandomValue(null));
    code = mgr.getRandomValue("eN");
    assertNotNull(code);
    assertTrue(EN_REPLACEMENTS_LIST.contains(code));
    code = mgr.getRandomValue("Fr");
    assertNotNull(code);
    assertTrue(FR_REPLACEMENTS_LIST.contains(code));

    List<TestResource> items = mgr.getValues();
    assertEquals(EN_REPLACEMENTS_LIST.size() + FR_REPLACEMENTS_LIST.size(), items.size());
    assertNotNull(items);
    codeset = new HashSet<>();
    for (TestResource codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }    
    codeset.removeAll(EN_REPLACEMENTS_VALUES);
    codeset.removeAll(FR_REPLACEMENTS_VALUES);
    assertEquals(0, codeset.size());
    
    items = mgr.getValues("gb");
    assertNotNull(items);
    assertEquals(0, items.size());
    items = mgr.getValues(null);
    assertNotNull(items);
    assertEquals(0, items.size());
    items = mgr.getValues("eN");
    assertEquals(EN_REPLACEMENTS_LIST.size(), items.size());
    assertNotNull(items);
    codeset = new HashSet<>();
    for (TestResource codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }    
    codeset.removeAll(EN_REPLACEMENTS_VALUES);
    assertEquals(0, codeset.size());
    items = mgr.getValues("Fr");
    assertEquals(FR_REPLACEMENTS_LIST.size(), items.size());
    assertNotNull(items);
    codeset = new HashSet<>();
    for (TestResource codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }    
    codeset.removeAll(FR_REPLACEMENTS_VALUES);
    assertEquals(0, codeset.size());
    
    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertTrue(mgr.isValidKey("first"));
    assertTrue(mgr.isValidKey("fIRst"));
    assertTrue(mgr.isValidKey("FIRST"));
    assertFalse(mgr.isValidKey(null));
    assertFalse(mgr.isValidKey("gb", "FIRST"));
    assertFalse(mgr.isValidKey(null, "FIRST"));
    assertTrue(mgr.isValidKey("en", "first"));
    assertTrue(mgr.isValidKey("en", "fIRst"));
    assertTrue(mgr.isValidKey("en", "FIRST"));
    assertFalse(mgr.isValidKey("en", "FRFIRST"));
    assertTrue(mgr.isValidKey("fr", "frfirst"));
    assertTrue(mgr.isValidKey("fr", "fRfIRst"));
    assertTrue(mgr.isValidKey("FR", "FRFIRST"));
    assertTrue(mgr.isValidKey("Fr", "frfirst"));
    assertTrue(mgr.isValidKey("fR", "fRfIRst"));
    assertTrue(mgr.isValidKey("fr", "FRFIRST"));
    assertFalse(mgr.isValidKey("fr", "FIRST"));
    assertFalse(mgr.isValidKey("en", null));
    assertFalse(mgr.isValidKey("fr", null));
  }

  @Test
  public void testLocalizedPseudorandom() {
    List<TestResource> EN_REPLACEMENTS_LIST = Arrays.asList(new TestLocalizedResource("first", "en"), new TestLocalizedResource("second", "en"), new TestLocalizedResource("third", "en"), new TestLocalizedResource("lASt", "en"));
    List<String> EN_REPLACEMENTS_KEYS = Arrays.asList("FIRST", "SECOND", "THIRD", "LAST");
    List<TestResource> FR_REPLACEMENTS_LIST = Arrays.asList(new TestLocalizedResource("FRfirst", "fr"), new TestLocalizedResource("FRsecond", null), new TestLocalizedResource("FRthird", "xx"), new TestLocalizedResource("last", "fr"));
    List<String> FR_REPLACEMENTS_KEYS = Arrays.asList("FRFIRST", "FRSECOND", "FRTHIRD", "LAST");

    TestLocalizedResourceManager mgr = new TestLocalizedResourceManager();
    for (TestResource r : EN_REPLACEMENTS_LIST) {
      mgr.add(r);
      mgr.add("en", r);
    }
    for (TestResource r : FR_REPLACEMENTS_LIST) {
      mgr.add(r);
      mgr.add("fr", r);
    }
    
    String ps = mgr.getPseudorandom("third");
    assertNotNull(ps);
    assertTrue(EN_REPLACEMENTS_KEYS.contains(ps));
    ps = mgr.getPseudorandom("frfirst");
    assertNotNull(ps);
    assertTrue(FR_REPLACEMENTS_KEYS.contains(ps));
    // error case - unexpected locale from resource
    ps = mgr.getPseudorandom("frthird");
    assertNotNull(ps);
    assertTrue(EN_REPLACEMENTS_KEYS.contains(ps) || FR_REPLACEMENTS_KEYS.contains(ps));
    // error case - no locale from resource
    ps = mgr.getPseudorandom("frsecond");
    assertNotNull(ps);
    assertTrue(EN_REPLACEMENTS_KEYS.contains(ps) || FR_REPLACEMENTS_KEYS.contains(ps));
  }
}
