/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class DateDependencyMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    DateDependencyMaskingProviderConfig config = new DateDependencyMaskingProviderConfig();
    config.setDatetimeYearDeleteNIntervalMaskDate("maskDate");
    config.setDatetimeYearDeleteNIntervalCompareDate("compDate");
    config.setDateYearDeleteNDaysValue(0);
    assertNull(config.getDatetimeyearDeleteNIntervalCompareDateValue());
    config.validate();

    config.setUnspecifiedValueHandling(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(1);
    config.validate();

    config.setDatetimeYearDeleteNIntervalMaskDate(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`datetimeYearDeleteNIntervalMaskDate` is required", e.getMessage());
    }
    config.setDatetimeYearDeleteNIntervalMaskDate("maskDate2");

    config.setDatetimeYearDeleteNIntervalCompareDate(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`datetimeYearDeleteNIntervalCompareDate` is required", e.getMessage());
    }
    config.setDatetimeYearDeleteNIntervalCompareDate("compDate2");

    config.setDateYearDeleteNDaysValue(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`dateYearDeleteNDaysValue` must be greater than or equal to 0", e.getMessage());
    }
    config.setDateYearDeleteNDaysValue(55);
  }

}
