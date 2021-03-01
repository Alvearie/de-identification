/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.MaritalStatus;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.MaritalStatusMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.MaritalStatusManager;


/** The type Marital status masking provider. */
public class MaritalStatusMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -1898529887780962978L;

  protected MaritalStatusManager statusManager;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  /**
   * Instantiates a new Marital status masking provider.
   * 
   * @param configuration the configuration
   * @param localizationProperty TODO
   * @param random the random
   */
  public MaritalStatusMaskingProvider(MaritalStatusMaskingProviderConfig configuration,
      String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    MaritalStatus maritalStatus = statusManager.getKey(identifier);

    if (maritalStatus == null) {
      debugFaultyInput("maritalStatus");
      if (unspecifiedValueHandling == 2) {
        return statusManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return statusManager.getRandomKey(maritalStatus.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      statusManager = (MaritalStatusManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.MARITAL_STATUS, null, localizationProperty);

      initialized = true;
    }
  }
}
