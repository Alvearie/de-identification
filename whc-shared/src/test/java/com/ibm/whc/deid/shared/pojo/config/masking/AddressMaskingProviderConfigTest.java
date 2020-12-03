/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class AddressMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    AddressMaskingProviderConfig config = new AddressMaskingProviderConfig();
    config.validate();

    config.setPostalCodeNearestK(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`postalCodeNearestK`"));
    }

    config.setPostalCodeNearestK(5);
    config.validate();

    config.setPostalCodeNearestK(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`postalCodeNearestK`"));
    }
  }

}
