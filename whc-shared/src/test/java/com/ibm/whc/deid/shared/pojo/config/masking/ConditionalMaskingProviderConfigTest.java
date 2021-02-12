/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import org.junit.Test;

public class ConditionalMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ConditionalMaskingProviderConfig config = new ConditionalMaskingProviderConfig();

    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet` must be specified with at least one entry", e.getMessage());
    }

    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<>();
    config.setMaskRuleSet(maskRuleSet);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet` must be specified with at least one entry", e.getMessage());
    }

    ConditionalMaskRuleSet ruleset = new ConditionalMaskRuleSet();
    maskRuleSet.add(ruleset);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(
          "entry at offset 0 in `maskRuleSet` is not valid: `maskRuleSet.maskingProvider` is missing",
          e.getMessage());
    }

    ruleset.setMaskingProvider(new HashMaskingProviderConfig());
    config.validate();

    ConditionalMaskRuleSet ruleset2 = new ConditionalMaskRuleSet();
    maskRuleSet.add(ruleset2);
    BinningMaskingProviderConfig provider = new BinningMaskingProviderConfig();
    provider.setBinSize(-2);
    ruleset2.setMaskingProvider(provider);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(
          "entry at offset 1 in `maskRuleSet` is not valid: `maskRuleSet.maskingProvider` is invalid: `binSize` must be greater than 0",
          e.getMessage());
    }

    provider.setBinSize(10);
    config.validate();

    config.setUnspecifiedValueHandling(5);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(1);
    config.validate();

    maskRuleSet.add(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("entry at offset 2 in `maskRuleSet` is null", e.getMessage());
    }
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testEqualsHashCode() throws Exception {
    ConditionalMaskingProviderConfig config = new ConditionalMaskingProviderConfig();
    assertFalse(config.equals(null));
    assertFalse(config.equals("x"));
    assertFalse(config.equals(new AddressMaskingProviderConfig()));
    assertTrue(config.equals(config));
    assertEquals(config.hashCode(), config.hashCode()); // same value called twice

    ConditionalMaskingProviderConfig other = new ConditionalMaskingProviderConfig();
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setUnspecifiedValueHandling(2);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setUnspecifiedValueHandling(2);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setUnspecifiedValueReturnMessage("x");
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setUnspecifiedValueReturnMessage("x");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    List<ConditionalMaskRuleSet> configRules = new ArrayList<>();
    config.setMaskRuleSet(configRules);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());

    List<ConditionalMaskRuleSet> otherRules = new LinkedList<>();
    other.setMaskRuleSet(otherRules);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    ConditionalMaskRuleSet ruleSet = new ConditionalMaskRuleSet();
    configRules.add(ruleSet);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    Condition condition = new Condition();
    ruleSet2.setCondition(condition);
    otherRules.add(ruleSet2);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());

    configRules.clear();
    configRules.add(ruleSet2);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());
  }

}
