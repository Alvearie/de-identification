/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class ConditionTest {

  @Test
  public void testValidate() throws Exception {
    Condition condition = new Condition();
    condition.setField("field1");
    condition.setOperator(ConditionOperator.EQUALS_IGNORE_CASE);
    condition.setValue("value2");
    condition.setType(ConditionType.STRING);
    condition.validate(null);

    condition.setField(null);
    try {
      condition.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`null.field` is missing", e.getMessage());
    }
    condition.setField("  ");
    try {
      condition.validate("a.b");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`a.b.field` is missing", e.getMessage());
    }
    condition.setField("field3");

    condition.setOperator(null);
    try {
      condition.validate("path2");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`path2.operator` is missing", e.getMessage());
    }
    condition.setOperator(ConditionOperator.CONTAINED_IN);

    condition.setValue(null);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.value` is missing", e.getMessage());
    }
    condition.setValue("value3");

    condition.setType(null);
    condition.validate("x"); // not used, not validated
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testEqualsHashCode() {
    Condition condition = new Condition();
    assertNull(condition.getField());
    assertNull(condition.getOperator());
    assertNull(condition.getType());
    assertNull(condition.getValue());

    int code = condition.hashCode();
    assertEquals(code, condition.hashCode());
    assertFalse(condition.equals(null));
    assertFalse(condition.equals("x"));
    assertTrue(condition.equals(condition));

    Condition other = new Condition();
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());

    condition.setField("field2");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setField(null);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setField("Field2");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setField("field2");
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());

    condition.setOperator(ConditionOperator.EQUALS);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setOperator(ConditionOperator.EQUALS_IGNORE_CASE);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setOperator(null);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setOperator(ConditionOperator.EQUALS);
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());

    condition.setType(ConditionType.STRING);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setType(null);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setType(ConditionType.STRING);
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());

    condition.setValue("value2");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setValue("value3");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setValue(null);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setValue("value2");
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());
  }

}
