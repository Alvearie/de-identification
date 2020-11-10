/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import java.io.IOException;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;

public class ConfigGenerator {

  private static final Logger logger = LoggerFactory.getLogger(ConfigGenerator.class);

  protected DeidMaskingConfig getNewMaskingConfig() {
    return new DeidMaskingConfig();
  }

  /**
   * Get a default masking config for testing FHIR
   *
   * @return
   */
  public DeidMaskingConfig getTestDeidConfig() {
    DeidMaskingConfig config = getNewMaskingConfig();
    try {
      String maskingProviders =
          MaskingConfigUtils.readResourceFileAsString("/template/fhir_masking_providers.json");
      String maskingRules =
          MaskingConfigUtils.readResourceFileAsString("/template/fhir_masking_rules.json");

      List<Rule> rules = MaskingConfigUtils.getFhirRules(maskingProviders);
      config.setRules(rules);

      config.setJson(MaskingConfigUtils.getDefaultFhirConfig(maskingRules));

    } catch (IOException e) {
      logger.error(e.getMessage(), e);
    }
    return config;
  }

}
