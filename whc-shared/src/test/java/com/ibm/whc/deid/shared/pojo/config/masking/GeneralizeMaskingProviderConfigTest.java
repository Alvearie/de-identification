/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Iterator;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig.GeneralizeRule;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class GeneralizeMaskingProviderConfigTest {

  @Test
  public void testParseValidate() throws Exception {
    GeneralizeMaskingProviderConfig config = new GeneralizeMaskingProviderConfig();
    config.validate(null);
    assertNull(config.getMaskRuleSet());

    config.setUnspecifiedValueHandling(4);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(2);
    config.validate(null);

    config.setMaskRuleSet("");
    config.validate(null); // OK - no rules found
    List<GeneralizeRule> rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    assertEquals(0, rules.size());

    config.setMaskRuleSet(" ");
    config.validate(null); // OK - no rules found
    rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    assertEquals(0, rules.size());

    config.setMaskRuleSet("null");  // generates a null node
    config.validate(null); // OK - no rules found
    rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    assertEquals(0, rules.size());

    config.setMaskRuleSet("\t");
    config.validate(null); // OK - no rules found
    rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    assertEquals(0, rules.size());

    config.setMaskRuleSet("false"); // generates boolean value node
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet` value must be a valid json array", e.getMessage());
    }

    config.setMaskRuleSet("value");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().startsWith("`maskRuleSet` is not valid json - "));
    }

    config.setMaskRuleSet("[}");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().startsWith("`maskRuleSet` is not valid json - "));
    }

    config.setMaskRuleSet("{}");  // generates object node
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet` value must be a valid json array", e.getMessage());
    }

    config.setMaskRuleSet("[]");
    config.validate(null); // OK - no rules found
    rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    assertEquals(0, rules.size());

    config.setMaskRuleSet("[{}]");  // generates object node
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`targetValue` is null for value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"Other\", \"sourceValueIn\": [\"Italian\",\"English\"], \"sourceValueNotIn\": null}, {}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`targetValue` is null for value set 1", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": null, \"sourceValueIn\": [\"Italian\",\"English\"], \"sourceValueNotIn\": null}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`targetValue` is null for value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"\", \"sourceValueIn\": [\"Italian\",\"English\"], \"sourceValueNotIn\": null}, {\"targetValue\": \" \", \"sourceValueIn\": [\"Italian\",\"English\"]}, {\"targetValue\": \"a\", \"sourceValueIn\": null, \"sourceValueNotIn\": [\"Italian\", \"\", \"French\"]}, {\"targetValue\": \"b\", \"sourceValueNotIn\": [\" \", \"\", \"French\", false, 4]}]");
    config.validate(null);
    rules = config.parseMaskRuleSet(config.getMaskRuleSet());
    assertNotNull(rules);
    Iterator<GeneralizeRule> it = rules.iterator();
    assertTrue(it.hasNext());
    GeneralizeRule rule = it.next();
    assertEquals("", rule.getCategory());
    assertFalse(rule.getLogicalNegation());
    assertEquals(2, rule.getValueSet().size());    
    assertTrue(rule.getValueSet().contains("Italian"));
    assertTrue(rule.getValueSet().contains("English"));
    assertTrue(it.hasNext());
    rule = it.next();
    assertEquals(" ", rule.getCategory());
    assertFalse(rule.getLogicalNegation());
    assertEquals(2, rule.getValueSet().size());
    assertTrue(rule.getValueSet().contains("Italian"));
    assertTrue(rule.getValueSet().contains("English"));
    assertTrue(it.hasNext());
    rule = it.next();
    assertEquals("a", rule.getCategory());
    assertTrue(rule.getLogicalNegation());
    assertEquals(3, rule.getValueSet().size());
    assertTrue(rule.getValueSet().contains("Italian"));
    assertTrue(rule.getValueSet().contains("French"));
    assertTrue(rule.getValueSet().contains(""));
    assertTrue(it.hasNext());
    rule = it.next();
    assertEquals("b", rule.getCategory());
    assertTrue(rule.getLogicalNegation());
    assertEquals(5, rule.getValueSet().size());
    assertTrue(rule.getValueSet().contains(" "));
    assertTrue(rule.getValueSet().contains("French"));
    assertTrue(rule.getValueSet().contains(""));
    assertTrue(rule.getValueSet().contains("false"));
    assertTrue(rule.getValueSet().contains("4"));
    assertFalse(it.hasNext());

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": null, \"sourceValueNotIn\": null}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("one of `sourceValueIn` or `sourceValueNotIn` must be specified in value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": [\"v1\"]}, {\"targetValue\": \"value2\", \"sourceValueIn\": [\"v2\"]}, {\"targetValue\": \"value3\"}, {\"targetValue\": \"value4\", \"sourceValueIn\": [\"v4\"]}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("one of `sourceValueIn` or `sourceValueNotIn` must be specified in value set 2", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": [\"v1\"], \"sourceValueNotIn\": [\"v2\"]}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("only one of `sourceValueIn` and `sourceValueNotIn` can be specified in value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": \"v1\"}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`sourceValueIn` must be a json array in value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": null, \"sourceValueNotIn\": \"v1\"}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`sourceValueNotIn` must be a json array in value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": null, \"sourceValueNotIn\": []}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("at least one value must be supplied in list of values in value set 0", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": null, \"sourceValueNotIn\": [null]}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("null object specified in list of values in value set 0 - only textual data is allowed", e.getMessage());
    }

    config.setMaskRuleSet("[{\"targetValue\": \"value1\", \"sourceValueIn\": [3]}, {\"targetValue\": \"value2\", \"sourceValueIn\": [null]}]");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("null object specified in list of values in value set 1 - only textual data is allowed", e.getMessage());
    }

  }

}
