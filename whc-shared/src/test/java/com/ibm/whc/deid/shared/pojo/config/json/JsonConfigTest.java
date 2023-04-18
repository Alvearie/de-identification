/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.json;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class JsonConfigTest {
  
  @Test
  public void testMaskingRules() {    
    JsonConfig jc = new JsonConfig();    
    assertNotNull(jc.getMaskingRules());
    assertEquals(0, jc.getMaskingRules().size());
    
    jc.addMaskingRule(null, null);
    assertNotNull(jc.getMaskingRules());
    assertEquals(1, jc.getMaskingRules().size());
    assertNull(jc.getMaskingRules().get(0).getJsonPath());
    assertNull(jc.getMaskingRules().get(0).getRule());
    
    List<JsonMaskingRule> rules = new ArrayList<>();
    rules.add(null);
    rules.add(new JsonMaskingRule("path", "rule"));
    jc.setMaskingRules(rules);
    assertNotNull(jc.getMaskingRules());
    assertEquals(2, jc.getMaskingRules().size());
    assertNull(jc.getMaskingRules().get(0));
    assertEquals("path", jc.getMaskingRules().get(1).getJsonPath());    
    assertEquals("rule", jc.getMaskingRules().get(1).getRule());
    
    jc.setMaskingRules(null);
    assertNull(jc.getMaskingRules());
    
    jc.addMaskingRule("path2", "rule2");
    assertNotNull(jc.getMaskingRules());
    assertEquals(1, jc.getMaskingRules().size());
    assertEquals("path2", jc.getMaskingRules().get(0).getJsonPath());    
    assertEquals("rule2", jc.getMaskingRules().get(0).getRule());
  }
  
}
