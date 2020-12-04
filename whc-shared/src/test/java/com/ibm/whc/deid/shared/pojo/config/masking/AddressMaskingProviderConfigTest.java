/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class AddressMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    AddressMaskingProviderConfig config = new AddressMaskingProviderConfig();
    config.validate();

    config.setUnspecifiedValueHandling(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);
    config.validate();
    config.setUnspecifiedValueHandling(1);
    config.validate();
    config.setUnspecifiedValueHandling(2);
    config.validate();
    config.setUnspecifiedValueHandling(3);
    config.validate();
    config.setUnspecifiedValueHandling(4);    
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);
    
    config.setPostalCodeNearestK(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`postalCodeNearestK` must be greater than 0", e.getMessage());
    }

    config.setPostalCodeNearestK(5);
    config.validate();

    config.setPostalCodeNearestK(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`postalCodeNearestK` must be greater than 0", e.getMessage());
    }
  }

}
