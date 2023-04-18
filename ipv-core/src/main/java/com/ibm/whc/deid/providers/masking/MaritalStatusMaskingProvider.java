/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.MaritalStatus;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.MaritalStatusMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.MaritalStatusManager;


/**
 * Privacy provider to protect marital status.
 */
public class MaritalStatusMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -1898529887780962978L;

  protected transient volatile MaritalStatusManager maritalStatusResourceManager = null;

  /**
   * Instantiates a new marital status masking provider.
   * 
   * @param configuration the configuration
   * @param tenantId identifier of the tenant associated with the current request
   * @param localizationProperty location of the localization property file
   */
  public MaritalStatusMaskingProvider(MaritalStatusMaskingProviderConfig configuration,
      String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    MaritalStatus status = getManager().getRandomValue();
    return status == null ? null : status.getName();
  }

  protected MaritalStatusManager getManager() {
    if (maritalStatusResourceManager == null) {
      maritalStatusResourceManager =
          (MaritalStatusManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.MARITAL_STATUS, null, localizationProperty);
    }
    return maritalStatusResourceManager;
  }
}
