/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.Serializable;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class BasicMaskingProviderFactoryTest {

  private static final LogManager log = LogManager.getInstance();
  
  private final String tenantId = "TEST_TENANT";

  @Test
  public void testSimple() {
    DeidMaskingConfig deidMaskingConfig = new DeidMaskingConfig();
    MaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory();
    MaskingProvider maskingProvider = maskingProviderFactory.getProviderFromType(
        MaskingProviderType.REDACT, deidMaskingConfig, new RedactMaskingProviderConfig(), tenantId,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    assertEquals("XXXXXXXXXX", maskingProvider.mask("John Smith")); 
  }

  @Test
  public void verifySerialization() throws Exception {
    MaskingProviderFactory mpf = new BasicMaskingProviderFactory();
    DeidMaskingConfig deidMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();

    for (MaskingProviderType providerType : MaskingProviderType.values()) {
      MaskingProvider maskingProvider = mpf.getProviderFromType(providerType, deidMaskingConfig,
          MaskingProviderConfig.getDefaultMaskingProviderConfig(providerType), tenantId,
          LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
      assertTrue(maskingProvider instanceof Serializable);
      try {
        MaskingProvider clone = SerializationUtils.clone(maskingProvider);
        assertNotSame(maskingProvider, clone);
      } catch (Exception e) {
        log.logError(LogCodes.WPH1013E, e, "non-Serializable type: " + providerType);
        // TODO: update these providers
        if (providerType != MaskingProviderType.CREDIT_CARD
            && providerType != MaskingProviderType.PHONE) {
          fail("non-Serializable type: " + providerType);
        }
      }
    }
  }
}
