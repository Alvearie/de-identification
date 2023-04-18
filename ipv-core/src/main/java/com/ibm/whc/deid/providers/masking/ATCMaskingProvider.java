/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ATCMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.util.ATCManager;
import com.ibm.whc.deid.util.ManagerFactory;

public class ATCMaskingProvider extends AbstractMaskingProvider {
  
  private static final long serialVersionUID = 2743280303243192992L;

  protected transient volatile ATCManager atcManager = null;
  
  protected final int prefixPreserveLength;

  public ATCMaskingProvider(ATCMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    
    int levelsToKeep = configuration.getMaskLevelsToKeep();
    if (levelsToKeep == 1) {
      this.prefixPreserveLength = 1;
    } else if (levelsToKeep == 2) {
      this.prefixPreserveLength = 3;
    } else if (levelsToKeep == 3) {
      this.prefixPreserveLength = 4;
    } else if (levelsToKeep == 4) {
      this.prefixPreserveLength = 5;
    } else {
      // should not occur - validation should catch
      throw new RuntimeException(
          new InvalidMaskingConfigurationException("`maskLevelsToKeep` must be [1..4]"));
    }
  }

  protected ATCManager getManager() {
    if (atcManager == null) {
      atcManager = (ATCManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ATC_CODES, null, localizationProperty);
    }
    return atcManager;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!getManager().isValidKey(identifier)) {
      // For this provider, we do not return random ATC
      return applyUnexpectedValueHandling(identifier, null);
    }

    return identifier.substring(0, prefixPreserveLength);
  }
}
