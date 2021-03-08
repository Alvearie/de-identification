/*
 * (C) Copyright IBM Corp. 2021
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
    config.validate(null);

    config.setUnspecifiedValueHandling(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);
    config.validate(null);
    config.setUnspecifiedValueHandling(1);
    config.validate(null);
    config.setUnspecifiedValueHandling(2);
    config.validate(null);
    config.setUnspecifiedValueHandling(3);
    config.validate(null);
    config.setUnspecifiedValueHandling(4);    
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);
    
    config.setPostalCodeNearestK(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`postalCodeNearestK` must be greater than 0", e.getMessage());
    }

    config.setPostalCodeNearestK(5);
    config.validate(null);

    config.setPostalCodeNearestK(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`postalCodeNearestK` must be greater than 0", e.getMessage());
    }
  }

}
