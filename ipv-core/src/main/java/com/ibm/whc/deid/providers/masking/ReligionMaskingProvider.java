/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Religion;
import com.ibm.whc.deid.shared.pojo.config.masking.ReligionMaskingProviderConfig;
import com.ibm.whc.deid.util.ReligionManager;

/** The type Religion masking provider. */
public class ReligionMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 4551913201853798655L;

  protected ReligionManager religionManager;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public ReligionMaskingProvider(ReligionMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
    this.localizationProperty = localizationProperty;
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Religion religion = religionManager.getKey(identifier);

    if (religion == null) {
      debugFaultyInput("religion");
      if (unspecifiedValueHandling == 2) {
        return religionManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return religionManager.getRandomKey(religion.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      religionManager = new ReligionManager(tenantId, localizationProperty);
      initialized = true;
    }
  }

}
