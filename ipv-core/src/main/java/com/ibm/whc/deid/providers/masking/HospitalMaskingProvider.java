/*
 * (C) Copyright IBM Corp. 2016,2021
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

  private static final long serialVersionUID = 7964959969532210677L;

  protected final boolean preserveCountry;

  protected transient volatile HospitalManager hospitalResourceManager = null;

  public HospitalMaskingProvider(HospitalMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.preserveCountry = configuration.isMaskPreserveCountry();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    HospitalManager hospitalManager = getHospitalManager();

    if (!this.preserveCountry) {
      return getRandomHospitalName(hospitalManager);
    }

    // preserveCountry was specified
    Hospital hospital = hospitalManager.getValue(identifier);
    if (hospital == null) {
      return applyUnexpectedValueHandling(identifier, () -> getRandomHospitalName(hospitalManager));
    }
    Hospital randomHospital = hospitalManager.getRandomValue(hospital.getNameCountryCode());
    return randomHospital == null ? null : randomHospital.getName();
  }

  protected String getRandomHospitalName(HospitalManager hospitalManager) {
    Hospital hospital = hospitalManager.getRandomValue();
    return hospital == null ? null : hospital.getName();
  }

  protected HospitalManager getHospitalManager() {
    if (hospitalResourceManager == null) {
      hospitalResourceManager = (HospitalManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.HOSPITAL_NAMES, null, localizationProperty);
    }
    return hospitalResourceManager;
  }
}
