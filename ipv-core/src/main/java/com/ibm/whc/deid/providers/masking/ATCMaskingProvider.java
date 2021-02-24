/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ATCMaskingProviderConfig;
import com.ibm.whc.deid.util.ATCManager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ATCMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 2743280303243192992L;

  protected ATCManager atcManager;
  protected final int levelsToKeep;
  protected final int prefixPreserveLength;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  public ATCMaskingProvider(ATCMaskingProviderConfig configuration, String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
		
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
    this.levelsToKeep = configuration.getMaskLevelsToKeep();

    if (this.levelsToKeep == 1) {
      this.prefixPreserveLength = 1;
    } else if (this.levelsToKeep == 2) {
      this.prefixPreserveLength = 3;
    } else if (this.levelsToKeep == 3) {
      this.prefixPreserveLength = 4;
    } else if (this.levelsToKeep == 4) {
      this.prefixPreserveLength = 5;
    } else {
      this.prefixPreserveLength = 7;
    }
  }

  protected volatile boolean initialized = false;
  protected void initialize() {
    if (!initialized) {
      atcManager =
					(ATCManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ATC_CODES, null, localizationProperty);

      initialized = true;
    }
  }

  @Override
  public String mask(String identifier) {
    initialize();

    try {
      if (identifier == null) {
        log.logWarn(LogCodes.WPH1011W, "identifier", "ATC masking");
        return null;
      }

      if (!atcManager.isValidKey(identifier) || prefixPreserveLength == 7) {
        // For this provider, we do not return random ATC
        log.logWarn(LogCodes.WPH1011W, "identifier", "ATC masking");
        if (unspecifiedValueHandling == 3) {
          return unspecifiedValueReturnMessage;
        } else {
          return null;
        }
      }

      return identifier.substring(0, prefixPreserveLength);
    } catch (Exception e) {
      logException(e);
      return null;
    }
  }
}
