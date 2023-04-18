/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertFalse;

import com.ibm.whc.deid.shared.pojo.config.masking.GUIDMaskingProviderConfig;
import org.junit.Test;

public class GUIDMaskingProviderTest extends TestLogSetUp {

  @Test
  public void testMask() {
    GUIDMaskingProviderConfig config = new GUIDMaskingProviderConfig();
    GUIDMaskingProvider maskingProvider = new GUIDMaskingProvider(config);

    String guid = "fe672ffd-da08-4052-9074-721d070d1b47";
    String maskedValue = maskingProvider.mask(guid);

    assertFalse(maskedValue.equals(guid));
  }
}
