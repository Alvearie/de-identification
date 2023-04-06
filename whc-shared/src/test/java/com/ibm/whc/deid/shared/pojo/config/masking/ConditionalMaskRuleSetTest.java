/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionOperator;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ConditionalMaskRuleSetTest {

  @Test
  public void testValidate() throws Exception {
    ConditionalMaskRuleSet ruleset = new ConditionalMaskRuleSet();
    try {
      ruleset.validate(null, null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`null.maskingProvider` is missing", e.getMessage());
    }

    AddressMaskingProviderConfig address = new AddressMaskingProviderConfig();
    address.setPostalCodeNearestK(-2);
    ruleset.setMaskingProvider(address);
    try {
      ruleset.validate("maskRuleSet", null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(
          "`maskRuleSet.maskingProvider` is invalid: `postalCodeNearestK` must be greater than 0",
          e.getMessage());
    }

    address.setPostalCodeNearestK(12);
    ruleset.validate(null, null);

    Condition condition = new Condition();
    condition.setOperator(ConditionOperator.CONTAINED_IN);
    condition.setValue("value2");
    ruleset.setCondition(condition);
    try {
      ruleset.validate("maskRuleSet", null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.field` is missing", e.getMessage());
    }

    condition.setField("field2");
    ruleset.validate("maskRuleSet", null);
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testEqualsHashCode() {
    ConditionalMaskRuleSet set = new ConditionalMaskRuleSet();
    assertNull(set.getCondition());
    assertNull(set.getMaskingProvider());

    int code = set.hashCode();
    assertEquals(code, set.hashCode());
    assertFalse(set.equals(null));
    assertFalse(set.equals("x"));
    assertTrue(set.equals(set));

    ConditionalMaskRuleSet other = new ConditionalMaskRuleSet();
    assertTrue(set.equals(other));
    assertEquals(set.hashCode(), other.hashCode());

    set.setCondition(new Condition());
    assertFalse(set.equals(other));
    assertNotEquals(set.hashCode(), other.hashCode());
    other.setCondition(new Condition());
    assertTrue(set.equals(other));
    assertEquals(set.hashCode(), other.hashCode());
    set.getCondition().setField("field");
    assertFalse(set.equals(other));
    assertNotEquals(set.hashCode(), other.hashCode());
    other.getCondition().setField("field");
    assertTrue(set.equals(other));
    assertEquals(set.hashCode(), other.hashCode());

    set.setMaskingProvider(new AddressMaskingProviderConfig());
    assertFalse(set.equals(other));
    assertNotEquals(set.hashCode(), other.hashCode());
    other.setMaskingProvider(new HashMaskingProviderConfig());
    assertFalse(set.equals(other));
    assertNotEquals(set.hashCode(), other.hashCode());
    other.setMaskingProvider(new AddressMaskingProviderConfig());
    assertTrue(set.equals(other));
    assertEquals(set.hashCode(), other.hashCode());
    set.getMaskingProvider().setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    assertFalse(set.equals(other));
    assertNotEquals(set.hashCode(), other.hashCode());
    other.getMaskingProvider().setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    assertTrue(set.equals(other));
    assertEquals(set.hashCode(), other.hashCode());
  }
}
