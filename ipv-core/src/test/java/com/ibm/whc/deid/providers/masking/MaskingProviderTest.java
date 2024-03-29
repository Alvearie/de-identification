/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.util.localization.LocalizationManager;

public interface MaskingProviderTest {

  String tenantId = "TEST_TENANT";

  String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  String ERROR_LOCALIZATION_PROPERTIES = "/localization/test.localization.error.properties";
  String TEST_LOCALIZATION_PROPERTIES = "/localization/test.localization.properties";
}
