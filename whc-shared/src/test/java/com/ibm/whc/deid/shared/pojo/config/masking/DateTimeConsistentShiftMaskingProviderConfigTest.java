/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig.DateShiftDirection;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class DateTimeConsistentShiftMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.validate(null);

    config.setPatientIdentifierPath(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`patientIdentifierPath` is missing", e.getMessage());
    }

    config.setPatientIdentifierPath("");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`patientIdentifierPath` is missing", e.getMessage());
    }

    config.setPatientIdentifierPath("   \n\t  ");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`patientIdentifierPath` is missing", e.getMessage());
    }

    config.setPatientIdentifierPath("path1");
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setDateShiftMinimumDays(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(
          e.getMessage().contains("`dateShiftMinimumDays` must be greater than or equal to 0"));
    }
    config.setDateShiftMinimumDays(0);
    config.validate(null);

    config.setDateShiftMaximumDays(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "`dateShiftMaximumDays` must be greater than or equal to `dateShiftMinimumDays`"));
    }
    config.setDateShiftMaximumDays(0);
    config.validate(null);

    config.setDateShiftMinimumDays(5);
    config.setDateShiftMaximumDays(365);
    config.validate(null);

    List<String> formats = new ArrayList<>();
    config.setCustomFormats(formats);
    formats.add("TTT");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().startsWith("format at offset 0 in `customFormats` is not valid: "));
    }

    formats.clear();
    formats.add("dd-MM-yyyy");
    formats.add("yyDDD");
    config.validate(null);

    formats.add(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("format at offset 2 in `customFormats` is missing", e.getMessage());
    }
    formats.remove(2);

    formats.add("  \t");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("format at offset 2 in `customFormats` is missing", e.getMessage());
    }
    formats.remove(2);

    formats.add("HH:mm:ss"); // valid, but will cause failure later - no date
    config.validate(null);

    formats.add("UQW");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().startsWith("format at offset 3 in `customFormats` is not valid: "));
    }
  }

  @Test
  public void testSetDateShiftDirection() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    assertEquals(DateShiftDirection.BEFORE_OR_AFTER, config.getDateShiftDirection());
    config.setDateShiftDirection(DateShiftDirection.AFTER);
    assertEquals(DateShiftDirection.AFTER, config.getDateShiftDirection());
    config.setDateShiftDirection(null);
    assertEquals(DateShiftDirection.BEFORE_OR_AFTER, config.getDateShiftDirection());
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void testEqualsHashcode() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.hashCode(); // no exception

    assertFalse(config.equals("other"));

    DateTimeConsistentShiftMaskingProviderConfig other = null;
    assertFalse(config.equals(other));

    other = new DateTimeConsistentShiftMaskingProviderConfig();
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setDateShiftMinimumDays(5);
    assertFalse(config.equals(other));
    other.setDateShiftMinimumDays(5);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setDateShiftMaximumDays(10);
    assertFalse(config.equals(other));
    other.setDateShiftMaximumDays(10);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setPatientIdentifierPath("p2");
    assertFalse(config.equals(other));
    other.setPatientIdentifierPath("p2");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setDateShiftDirection(DateShiftDirection.BEFORE);
    assertFalse(config.equals(other));
    other.setDateShiftDirection(DateShiftDirection.AFTER);
    assertFalse(config.equals(other));
    other.setDateShiftDirection(DateShiftDirection.BEFORE);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setSalt("salt1");
    assertFalse(config.equals(other));
    other.setSalt("Salt1");
    assertFalse(config.equals(other));
    other.setSalt(" salt1 ");
    assertFalse(config.equals(other));
    other.setSalt("salt1");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    List<String> formats = new ArrayList<>();
    List<String> formats2 = new ArrayList<>();
    config.setCustomFormats(formats);
    assertFalse(config.equals(other));
    other.setCustomFormats(formats2);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    formats.add(null);
    assertFalse(config.equals(other));
    assertNull(config.getCustomFormats().get(0));
    formats2.add(null);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    formats.add("f1");
    assertFalse(config.equals(other));
    formats2.add("F1");
    assertFalse(config.equals(other));
    formats2.add(1, "f1");
    assertFalse(config.equals(other));
    formats2.remove(2);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());
  }
}
