/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
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

    config.setDatetimeYearDeleteNIntervalMaskDate(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`datetimeYearDeleteNIntervalMaskDate` is required"));
    }
    config.setDatetimeYearDeleteNIntervalMaskDate("maskDate2");

    config.setDatetimeYearDeleteNIntervalCompareDate(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`datetimeYearDeleteNIntervalCompareDate` is required"));
    }
    config.setDatetimeYearDeleteNIntervalCompareDate("compDate2");

    config.setDateYearDeleteNDaysValue(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(
          e.getMessage().contains("`dateYearDeleteNDaysValue` must be greater than or equal to 0"));
    }
    config.setDateYearDeleteNDaysValue(55);
  }

}
