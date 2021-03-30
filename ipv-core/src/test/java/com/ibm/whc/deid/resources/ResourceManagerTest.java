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
import org.junit.Test;

public class ResourceManagerTest {

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
  
  private static class TestResourceManager extends ResourceManager<TestResource> {
    
    public TestResourceManager() {      
      // nothing required here
    }
  }
  
  @Test
  public void testWithoutResourcesLoaded() {
    TestResourceManager mgr = new TestResourceManager();

    assertNull(mgr.getValue("ABCDEFGH"));
    assertNull(mgr.getValue(null));

    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    assertEquals(0, keys.size());

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom(null);
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    assertNull(mgr.getRandomKey());

    assertNull(mgr.getRandomValue());

    List<TestResource> list = mgr.getValues();
    assertNotNull(list);
    assertEquals(0, list.size());

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertFalse(mgr.isValidKey(null));
  }

  @Test
  public void testWithCodesLoaded() {
    List<TestResource> REPLACEMENTS_LIST = Arrays.asList(new TestResource("first"), new TestResource("second"), new TestResource("third"), new TestResource("lASt"));
    List<String> REPLACEMENTS_KEYS = Arrays.asList("FIRST", "SECOND", "THIRD", "LAST");
    List<String> REPLACEMENTS_VALUES = Arrays.asList("first", "second", "third", "lASt");    
    TestResourceManager mgr = new TestResourceManager();
    for (TestResource r : REPLACEMENTS_LIST) {
      mgr.add(r);
    }
    
    assertNull(mgr.getValue("ABCDEFGH"));
    TestResource code = mgr.getValue("second");
    assertNotNull(code);
    assertEquals("second", code.getCode());
    code = mgr.getValue("last");
    assertNotNull(code);
    assertEquals("lASt", code.getCode());
    code = mgr.getValue("LAST");
    assertNotNull(code);
    assertEquals("lASt", code.getCode());
    assertNull(mgr.getValue(null));
    
    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    HashSet<String> codeset = new HashSet<>(keys);
    for (String s : REPLACEMENTS_KEYS) {
      assertTrue(codeset.remove(s));
    }
    assertTrue(codeset.isEmpty());

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom("third");
    assertNotNull(ps);
    assertTrue(REPLACEMENTS_KEYS.contains(ps));
    ps = mgr.getPseudorandom(null);
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    String key = mgr.getRandomKey();
    assertNotNull(key);
    assertTrue(REPLACEMENTS_KEYS.contains(key));

    code = mgr.getRandomValue();
    assertNotNull(code);
    assertTrue(REPLACEMENTS_LIST.contains(code));

    List<TestResource> items = mgr.getValues();
    assertNotNull(items);
    assertEquals(REPLACEMENTS_LIST.size(), items.size());
    codeset = new HashSet<>();
    for (TestResource codex : items) {
      assertTrue(codeset.add(codex.getCode()));
    }    
    codeset.removeAll(REPLACEMENTS_VALUES);
    assertEquals(0, codeset.size());

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertTrue(mgr.isValidKey("first"));
    assertTrue(mgr.isValidKey("fIRst"));
    assertTrue(mgr.isValidKey("FIRST"));
    assertFalse(mgr.isValidKey(null));
  }
  
  @Test
  public void testWithSingleCodeLoaded() {
    TestResourceManager mgr = new TestResourceManager();
    TestResource one = new TestResource("One");
    mgr.add(one);
    
    assertNull(mgr.getValue("ABCDEFGH"));
    assertTrue(one == mgr.getValue("one"));
    assertTrue(one == mgr.getValue("ONE"));
    assertTrue(one == mgr.getValue("oNe"));
    assertNull(mgr.getValue(null));
    
    Set<String> keys = mgr.getKeys();
    assertNotNull(keys);
    assertEquals(1, keys.size());
    assertTrue(keys.contains("ONE"));

    String ps = mgr.getPseudorandom("ABCDEFGH");
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());
    ps = mgr.getPseudorandom("oNe");
    assertNotNull(ps);
    assertEquals("ONE", ps);
    ps = mgr.getPseudorandom(null);
    assertNotNull(ps);
    assertFalse(ps.trim().isEmpty());

    assertEquals("ONE", mgr.getRandomKey()); 

    assertTrue(one == mgr.getRandomValue());

    List<TestResource> items = mgr.getValues();
    assertNotNull(items);
    assertEquals(1, items.size());
    assertTrue(one == items.get(0));

    assertFalse(mgr.isValidKey("ABCDEFGH"));
    assertTrue(mgr.isValidKey("one"));
    assertTrue(mgr.isValidKey("OnE"));
    assertTrue(mgr.isValidKey("ONE"));
    assertFalse(mgr.isValidKey(null));
  }
}
