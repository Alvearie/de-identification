/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Hospital;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.HospitalMaskingProviderConfig;
import com.ibm.whc.deid.util.HospitalManager;
import com.ibm.whc.deid.util.ManagerFactory;


public class HospitalMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 7964959969532210677L;

  protected HospitalManager hospitalManager;
  protected final boolean preserveCountry;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public HospitalMaskingProvider(HospitalMaskingProviderConfig configuration, String tenantId) {
    this.preserveCountry = configuration.isMaskPreserveCountry();
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

    if (!this.preserveCountry) {
      return hospitalManager.getRandomKey();
    }

    Hospital hospital = hospitalManager.getKey(identifier);

    if (hospital == null) {
      // TODO: verify is hospital is an essential field
      warnFaultyInput("hospital");
      if (unspecifiedValueHandling == 2) {
        return hospitalManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return hospitalManager.getRandomKey(hospital.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      hospitalManager = (HospitalManager) ManagerFactory.getInstance().getManager(null,
          Resource.HOSPITAL_NAMES, null);
      initialized = true;
    }
  }

}
