/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;

import com.ibm.whc.deid.models.Occupation;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;
import com.ibm.whc.deid.util.OccupationManager;

public class OccupationMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 118800423304584769L;

  /** The constant occupationManager. */
  OccupationManager occupationManager;

  protected final boolean generalizeToCategory;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public OccupationMaskingProvider(OccupationMaskingProviderConfig configuration, String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
		this.generalizeToCategory = configuration.isMaskGeneralize();
    this.random = new SecureRandom();
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

    Occupation occupation = occupationManager.getKey(identifier);

    if (occupation == null) {
      debugFaultyInput("occupation");
      if (unspecifiedValueHandling == 2) {
        return occupationManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    if (this.generalizeToCategory) {
      List<String> categories = occupation.getCategories();
      int randomIndex = random.nextInt(categories.size());
      return categories.get(randomIndex);
    }

    return occupationManager.getRandomKey(occupation.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      occupationManager = new OccupationManager(null, localizationProperty);
      initialized = true;
    }
  }
}
