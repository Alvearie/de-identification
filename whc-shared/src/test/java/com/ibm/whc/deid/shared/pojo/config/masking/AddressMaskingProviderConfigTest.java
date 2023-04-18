/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class AddressMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    AddressMaskingProviderConfig config = new AddressMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.validate(null);

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
