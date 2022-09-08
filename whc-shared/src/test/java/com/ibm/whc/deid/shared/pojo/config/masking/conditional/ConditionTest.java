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
import java.util.ArrayList;
import java.util.Arrays;
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

    condition.setOperator(ConditionOperator.CONTAINS);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.value` is missing", e.getMessage());
    }

    condition.setOperator(ConditionOperator.EQUALS);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.value` is missing", e.getMessage());
    }

    condition.setOperator(ConditionOperator.EQUALS_IGNORE_CASE);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.value` is missing", e.getMessage());
    }

    condition.setValue("value3");
    condition.validate("x"); // not used, not validated

    // value not checked for certain operators
    condition.setValueList(Arrays.asList("value1"));
    condition.setOperator(ConditionOperator.ANY_OF);
    condition.validate("x"); // not used, not validated
    condition.setOperator(ConditionOperator.ANY_OF_IGNORE_CASE);
    condition.validate("x"); // not used, not validated
    condition.setOperator(ConditionOperator.NOT_ANY_OF);
    condition.validate("x"); // not used, not validated
    condition.setOperator(ConditionOperator.NOT_ANY_OF_IGNORE_CASE);
    condition.validate("x"); // not used, not validated

    condition.setValueList(Arrays.asList("value1", "value2", null));
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList[2]` is missing", e.getMessage());
    }

    condition.setValueList(null);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList` is missing", e.getMessage());
    }

    condition.setValueList(new ArrayList<>());
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList` is missing", e.getMessage());
    }

    condition.setOperator(ConditionOperator.NOT_ANY_OF);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList` is missing", e.getMessage());
    }

    condition.setOperator(ConditionOperator.ANY_OF_IGNORE_CASE);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList` is missing", e.getMessage());
    }

    condition.setOperator(ConditionOperator.ANY_OF);
    try {
      condition.validate("maskRuleSet.condition");
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskRuleSet.condition.valueList` is missing", e.getMessage());
    }

    condition.setValueList(Arrays.asList("value1", "value2"));
    condition.validate("x"); // not used, not validated

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
    assertNull(condition.getValueList());

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

    ArrayList<String> list = new ArrayList<>();
    condition.setValueList(list);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    other.setValueList(list);
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());

    ArrayList<String> list2 = new ArrayList<>();
    list2.add("value1");
    condition.setValueList(list2);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    ArrayList<String> list2a = new ArrayList<>();
    list2a.add("value2");
    other.setValueList(list2a);
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    list2a.clear();
    list2a.add("Value1");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    list2a.add(0, "value1");
    assertFalse(condition.equals(other));
    assertNotEquals(condition.hashCode(), other.hashCode());
    list2a.remove(1);
    assertTrue(condition.equals(other));
    assertEquals(condition.hashCode(), other.hashCode());
  }
}
